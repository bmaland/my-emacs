"""
Python dot expression completion using Pymacs.

When you hit TAB it will try to complete the dot expression
before point.
For example, given this import at the top of the file:

    import time

typing 'time.cl' then hitting TAB should complete 'time.clock'.

See pycomplete.el for the Emacs Lisp side of things.
"""

import sys
import types
import inspect
import StringIO
import os.path

_HELPOUT = StringIO.StringIO
_STDOUT = sys.stdout

try:
    x = set
except NameError:
    from sets import Set as set
else:
    del x

from Pymacs import lisp

COLWIDTH = 20


def pycomplete(s, fname=None, imports=None, debug=False):
    '''Display completion in Emacs window'''
    completions = _get_all_completions(s, fname, imports)
    dots = s.split('.')
    result = os.path.commonprefix([k[len(dots[-1]):] for k in completions])

    if result == '' or result not in completions:
        if completions:
            if debug:
                width = 80
            else:
                width = lisp.window_width() - 2

            column = width / COLWIDTH
            white = ' ' * COLWIDTH
            msg = ''

            counter = 0
            for completion in completions :
                if len(completion) < COLWIDTH:
                    msg += completion + white[len(completion):]
                    counter += 1
                else :
                    msg += completion + white[len(completion) - COLWIDTH:]
                    counter += 2

                if counter >= column:
                    counter = 0
                    msg += '\n'
        else:
            msg = 'no completions!'
        if debug:
            print msg
        else:
            lisp.message(msg)
    return result       


def pyhelp(s, imports=None, debug=False):
    '''Return object description'''
    doc = ''
    try:
        doc = _gethelp(s, imports)
    except Exception, ex:
        return '%s' % ex
    return doc
        

def pysignature(s):
    '''Return info about function parameters'''

    obj = None
    sig = ""
    try:
        obj = _load_symbol(s, globals(), locals())
    except Exception, ex:
        return '%s' % ex

    if type(obj) in (types.ClassType, types.TypeType):
        # Look for the highest __init__ in the class chain.
        obj = _find_constructor(obj)
    elif type(obj) == types.MethodType:
        # bit of a hack for methods - turn it into a function
        # but we drop the "self" param.
        obj = obj.im_func
   
    if type(obj) in [types.FunctionType, types.LambdaType]:
        (args, varargs, varkw, defaults) = inspect.getargspec(obj)
        sig = ('%s: %s' % (obj.__name__,
                           inspect.formatargspec(args, varargs, varkw,
                                                 defaults)))
    doc = getattr(obj, '__doc__', '')
    if doc and not sig:
        doc = doc.lstrip()
        pos = doc.find('\n')
        if pos < 0 or pos > 70:
            pos = 70
        sig = doc[:pos]
    return sig


def _load_symbol(s, dglobals, dlocals):
    sym = None
    dots = s.split('.')
    if not s or len(dots) == 1:
        try:
            sym = eval(s, dglobals, dlocals)
        except Exception:
            pass
    else:
        for i in range(1, len(dots)+1):
            s = '.'.join(dots[:i])
            if not s:
                continue
            try:
                sym = eval(s, dglobals, dlocals)
            except NameError:
                try:
                    sym = __import__(s, dglobals, dlocals, [])
                    dglobals[s] = sym
                except ImportError:
                    pass
            except Exception:
                pass
    return sym


def _gethelp(s, imports=None):
    '''Return string printed by `help` function'''
    _import_modules(imports, globals())
    obj = _load_symbol(s, globals(), locals())
    if not obj:
        obj = s
    out = _HELPOUT()
    try:
        sys.stdout = out
        help(obj)
    finally:
        sys.stdout = _STDOUT
    return out.getvalue()


def _import_modules(imports, dglobals):
    '''If given, execute import statements'''
    if imports is not None:
        for stmt in imports:
            try:
                exec stmt in dglobals
            except TypeError:
                raise TypeError, 'invalid type: %s' % stmt
            except Exception, ex:
                continue


def _get_all_completions(s, fname=None, imports=None):
    '''Return contextual completion of s (string of >= zero chars)'''

    dlocals = {}
    _import_modules(imports, globals())

    dots = s.split('.') 
    if not s or len(dots) == 1:
        keys = set()
        keys.update(dlocals.keys())
        keys.update(globals().keys())
        import __builtin__
        keys.update(dir(__builtin__))
        keys = list(keys)
        keys.sort()
        if s:
            return [k for k in keys if k.startswith(s)]
        else:
            return keys

    sym = None
    for i in range(1, len(dots)):
        s = '.'.join(dots[:i])
        if not s:
            continue
        try:
            sym = eval(s, globals(), dlocals)
        except NameError:
            try:
                sym = __import__(s, globals(), dlocals, [])
            except ImportError:
                return []
    if sym is not None:  
        s = dots[-1]     
        return [k for k in dir(sym) if k.startswith(s)]


def _find_constructor(class_ob):
    # Given a class object, return a function object used for the
    # constructor (ie, __init__() ) or None if we can't find one.
    try:
        return class_ob.__init__.im_func
    except AttributeError:
        for base in class_ob.__bases__:
            rc = _find_constructor(base)
            if rc is not None: return rc
    return None


def _test_signature():
    print pysignature('os.path.join')
    print pysignature('urllib.urlopen')
    print pysignature('httplib.HTTPConnection.request')
    print pysignature('httplib.HTTPConnection')
    print pysignature('csv.reader')
    # no signature
    #print pysignature('open')


def _test_help():
    print 'MODULE: os START'
    print pyhelp('os', debug=True)
    print 'MODULE: os END'

    # bug in pydoc
    print 'MODULE: logging START'
    print pyhelp('logging', ('import logging',), debug=True)
    print 'MODULE: logging END'

    print 'MODULE: csv START'
    print pyhelp('csv', ('import csv',), debug=True)
    print 'MODULE: csv END'

    print 'KEYWORD: import START'
    print pyhelp('import', debug=True)
    print 'KEYWORD: import END'

    print 'METHOD: os.path.join START'
    print pyhelp('os.path.join', debug=True)
    print 'METHOD: os.path.join END'


def _test_complete():
    print ' ->', pycomplete('', debug=True)
    print 'sys.get ->', pycomplete('sys.get', debug=True)
    print 'settr ->', pycomplete('settr', debug=True)
    print 'settr (plat in context) ->',
    print pycomplete('settr', imports=['from sys import settrace'], debug=True)
    print 'foo. ->', pycomplete('foo.', debug=True)
    print 'Enc (email * imported) ->', 
    print pycomplete('Enc', imports=['from email import *'], debug=True)
    print 'E (email * imported) ->',
    print pycomplete('E', imports=['from email import *'], debug=True)
    print 'Enc ->', pycomplete('Enc', debug=True)
    print 'E ->', pycomplete('E', debug=True)


if __name__ == "__main__":
    _test_complete()
    _test_help()
    _test_signature()
    
