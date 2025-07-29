/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2022 The MOPSA Project.                                    */
/*                                                                          */
/* This program is free software: you can redistribute it and/or modify     */
/* it under the terms of the GNU Lesser General Public License as published */
/* by the Free Software Foundation, either version 3 of the License, or     */
/* (at your option) any later version.                                      */
/*                                                                          */
/* This program is distributed in the hope that it will be useful,          */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/* GNU Lesser General Public License for more details.                      */
/*                                                                          */
/* You should have received a copy of the GNU Lesser General Public License */
/* along with this program.  If not, see <http://www.gnu.org/licenses/>.    */
/*                                                                          */
/****************************************************************************/

#include <Python.h>
#include "python_stubs.h"

#undef Py_BEGIN_ALLOW_THREADS

PyObject*
PyErr_NoMemory()
{
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = PyExc_MemoryError;
    exc->exc_msg = NULL;
    _mopsa_pyerr_bind_cs_to(exc);
    return NULL;
}

void
PyErr_Clear()
{
    if(exc != &no_exc)
        free(exc);
    exc = &no_exc;
}

void PyErr_SetNone(PyObject* o) {
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = o;
    exc->exc_msg = NULL;
    _mopsa_pyerr_bind_cs_to(exc);
}

void PyErr_SetString(PyObject* o, const char* msg){
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = o;
    exc->exc_msg = msg;
    _mopsa_pyerr_bind_cs_to(exc);
}

PyObject*
PyErr_Format(PyObject* o, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    char* msg;
    vasprintf(&msg, fmt, args);
    va_end(args);
    PyErr_SetString(o, msg);
    return NULL;
}

int PyErr_BadArgument(void)
{
    PyErr_SetString(PyExc_TypeError,
                    "bad argument type for built-in operation");
    return 0;
}

void PyErr_BadInternalCall(void)
{
    PyErr_SetString(PyExc_SystemError,
                    "bad argument to internal function");
}


int
PyErr_ExceptionMatches(PyObject *lexc)
{
    return exc->exc_state == lexc;
}

PyObject* PyType_GenericAlloc_Helper(PyTypeObject *type, size_t size);

PyObject*
PyType_GenericAlloc(PyTypeObject *type, Py_ssize_t nitems)
{
    PyObject *obj;
    const size_t size = _PyObject_VAR_SIZE(type, nitems+1);

    obj = PyType_GenericAlloc_Helper(type, size);
    /* FIXME */
    if (obj == NULL)
        return PyErr_NoMemory();
    // FIXME: the helper should do the memset I think
    memset(obj, '\0', size);

    /* somehow PyObject_INIT? */
    Py_SET_TYPE(obj, type); // FIXME now done by the boundary. Maybe the refcnt should be too?
//    obj->ob_refcnt = 1;

    return obj;
}

PyObject *
PyType_GenericNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    return type->tp_alloc(type, 0);
}


PyObject*
_PyObject_New(PyTypeObject *type)
{
    PyObject *obj;
    const size_t size = _PyObject_SIZE(type);
    obj = PyType_GenericAlloc_Helper(type, size);
    memset(obj, '\0', size);

    if (obj == NULL)
        return PyErr_NoMemory();

    Py_SET_TYPE(obj, type);
//    obj->ob_refcnt = 1;

    return obj;
}

static PyObject *
// remove member_get and directly call PyMember_GetOne?
// was PyMemberDescrObject, we're cheating again
member_get(PyMemberDef *descr, PyObject *obj, PyObject *type)
{
    /* PyObject *res; */

    /* if (descr_check((PyDescrObject *)descr, obj, &res)) */
    /*     return res; */

    /* if (descr->d_member->flags & READ_RESTRICTED) { */
    /*     if (PySys_Audit("object.__getattr__", "Os", */
    /*         obj ? obj : Py_None, descr->d_member->name) < 0) { */
    /*         return NULL; */
    /*     } */
    /* } */
    return PyMember_GetOne((char *)obj, descr); // was descr->d_member, we're cheating
}

// FIXME: we want to check the flag at creation rather than first call
// FIXME: simplified version, check the real one in Python/structmember.c
PyObject *
PyMember_GetOne(const char *addr, PyMemberDef *l)
{
    PyObject *v;

    addr += l->offset;
    switch (l->type) {
    case T_INT:
        v = PyLong_FromLong(*(int*)addr);
        break;
    case T_OBJECT:
        v = *(PyObject **)addr;
        if (v == NULL)
            v = Py_None;
       /* Py_INCREF(v); */
        break;
    case T_OBJECT_EX:
        v = *(PyObject **)addr;
        if (v == NULL)
            PyErr_SetString(PyExc_AttributeError, l->name);
        Py_XINCREF(v);
        break;
    default:
        PyErr_SetString(PyExc_SystemError, "bad memberdescr type");
        v = NULL;
    }
    return v;
}

// FIXME: returns values with wrap_descr_set
int PyMember_SetOne(char *addr, PyMemberDef *l, PyObject *v) {
        PyObject *oldv;
        addr += l->offset;

        if ((l->flags & READONLY)) {
            PyErr_SetString(PyExc_AttributeError, "readonly attribute");
            return -1;
        }
        if (v == NULL) {
            if (l->type == T_OBJECT_EX) {
              /* Check if the attribute is set. */
              if (*(PyObject **)addr == NULL) {
                PyErr_SetString(PyExc_AttributeError, l->name);
                return -1;
            }
        }
        else if (l->type != T_OBJECT) {
            PyErr_SetString(PyExc_TypeError,
                            "can't delete numeric/char attribute");
            return -1;
        }
    }
    switch (l->type) {
    case T_INT:{
        long long_val = PyLong_AsLong(v);
        if ((long_val == -1) && PyErr_Occurred())
            return -1;
        *(int *)addr = (int)long_val;
        if ((long_val > INT_MAX) || (long_val < INT_MIN))
            WARN("Truncation of value to int");
        break;
        }
    case T_OBJECT:
    case T_OBJECT_EX:
        Py_XINCREF(v);
        oldv = *(PyObject **)addr;
        *(PyObject **)addr = v;
        Py_XDECREF(oldv);
        break;
    default:
        PyErr_Format(PyExc_SystemError,
                     "bad memberdescr type for %s", l->name);
        return -1;
    }
    return 0;
}


PyObject*
PyModuleDef_Init(struct PyModuleDef* def)
{
    return PyModule_Create(def);
}

int PyModule_AddIntConstant(PyObject *m, const char *name, long value)
{
    PyObject *o = PyLong_FromLong(value);
    if(!o)
        return -1;
    if (PyModule_AddObject(m, name, o) == 0)
    {
        return 0;
    }
    Py_DECREF(o);
    return -1;
}


static int
check_num_args(PyObject *ob, int n)
{
    if (!PyTuple_CheckExact(ob)) {
        PyErr_SetString(PyExc_SystemError,
            "PyArg_UnpackTuple() argument list is not a tuple");
        return 0;
    }
    if (n == PyTuple_GET_SIZE(ob))
        return 1;
    PyErr_Format(
        PyExc_TypeError,
        "expected %d argument%s, got %zd", n, n == 1 ? "" : "s", PyTuple_GET_SIZE(ob));
    return 0;
}

static PyObject *
wrap_init(PyObject *self, PyObject *args, void *wrapped, PyObject *kwds)
{
    initproc func = (initproc)wrapped;
    int r = func(self, args, kwds);
    if (r < 0)
        return NULL;
    Py_RETURN_NONE;
}

static PyObject *
wrap_lenfunc(PyObject *self, PyObject *args, void *wrapped)
{
    lenfunc func = (lenfunc)wrapped;
    Py_ssize_t res;

    /* if (!check_num_args(args, 0)) */
    /*     return NULL; */
    res = (*func)(self);
    if (res == -1 && PyErr_Occurred())
        return NULL;
    return PyLong_FromSsize_t(res);
}

static Py_ssize_t
getindex(PyObject *self, PyObject *arg)
{
    Py_ssize_t i;

    i = PyNumber_AsSsize_t(arg, PyExc_OverflowError);
    if (i == -1 && PyErr_Occurred())
        return -1;
    if (i < 0) {
        PySequenceMethods *sq = Py_TYPE(self)->tp_as_sequence;
        if (sq && sq->sq_length) {
            Py_ssize_t n = (*sq->sq_length)(self);
            if (n < 0) {
                assert(PyErr_Occurred());
                return -1;
            }
            i += n;
        }
    }
    return i;
}

static PyObject *
wrap_sq_item(PyObject *self, PyObject *args, void *wrapped)
{
    ssizeargfunc func = (ssizeargfunc)wrapped;
    PyObject *arg;
    Py_ssize_t i;

    if (PyTuple_GET_SIZE(args) == 1) {
        arg = PyTuple_GET_ITEM(args, 0);
        i = getindex(self, arg);
        if (i == -1 && PyErr_Occurred())
            return NULL;
        return (*func)(self, i);
    }
    check_num_args(args, 1);
    assert(PyErr_Occurred());
    return NULL;
}


static PyObject *
wrap_objobjproc(PyObject *self, PyObject *args, void *wrapped)
{
    objobjproc func = (objobjproc)wrapped;
    int res;
    PyObject *value;

    // FIXME
    /* if (!check_num_args(args, 1)) */
    /*     return NULL; */
    value = PyTuple_GetItem(args, 0);// was GET_ITEM
    res = (*func)(self, value);
    if (res == -1 && PyErr_Occurred())
        return NULL;
    else
        return PyBool_FromLong(res);
}

static PyObject *
wrap_objobjargproc(PyObject *self, PyObject *args, void *wrapped)
{
    objobjargproc func = (objobjargproc)wrapped;
    int res;
    PyObject *key, *value;

    if (!PyArg_UnpackTuple(args, "", 2, 2, &key, &value))
        return NULL;
    res = (*func)(self, key, value);
    if (res == -1) {
        if (!PyErr_Occurred())
            PyErr_SetString(PyExc_SystemError, "error return without exception set");
        return NULL;
    }
    Py_RETURN_NONE;
}


static PyObject *
wrap_sq_setitem(PyObject *self, PyObject *args, void *wrapped)
{
    ssizeobjargproc func = (ssizeobjargproc)wrapped;
    Py_ssize_t i;
    int res;
    PyObject *arg, *value;

    if (!PyArg_UnpackTuple(args, "", 2, 2, &arg, &value))
        return NULL;
    i = getindex(self, arg);
    if (i == -1 && PyErr_Occurred())
        return NULL;
    res = (*func)(self, i, value);
    if (res == -1 && PyErr_Occurred())
        return NULL;
    Py_RETURN_NONE;
}

static PyObject *
wrap_sq_delitem(PyObject *self, PyObject *args, void *wrapped)
{
    ssizeobjargproc func = (ssizeobjargproc)wrapped;
    Py_ssize_t i;
    int res;
    PyObject *arg;

    if (!check_num_args(args, 1))
        return NULL;
    arg = PyTuple_GET_ITEM(args, 0);
    i = getindex(self, arg);
    if (i == -1 && PyErr_Occurred())
        return NULL;
    res = (*func)(self, i, NULL);
    if (res == -1 && PyErr_Occurred())
        return NULL;
    Py_RETURN_NONE;
}

static PyObject *
wrap_unaryfunc(PyObject *self, PyObject *args, void *wrapped)
{
    unaryfunc func = (unaryfunc)wrapped;

    /* if (!check_num_args(args, 0)) */
    /*     return NULL; */
    return (*func)(self);
}

static PyObject *
wrap_call(PyObject *self, PyObject *args, void *wrapped, PyObject *kwds)
{
    ternaryfunc func = (ternaryfunc)wrapped;

    return (*func)(self, args, kwds);
}


static PyObject *
wrap_richcmpfunc(PyObject *self, PyObject *args, void *wrapped, int op)
{
    richcmpfunc func = (richcmpfunc)wrapped;
    PyObject *other;

    if (!check_num_args(args, 1))
        return NULL;
    other = PyTuple_GET_ITEM(args, 0);
    PyObject *result = (*func)(self, other, op);
    return result;
}

#undef RICHCMP_WRAPPER
#define RICHCMP_WRAPPER(NAME, OP) \
static PyObject * \
richcmp_##NAME(PyObject *self, PyObject *args, void *wrapped) \
{ \
    PyObject *result = wrap_richcmpfunc(self, args, wrapped, OP); \
    return result; \
}

RICHCMP_WRAPPER(lt, Py_LT)
RICHCMP_WRAPPER(le, Py_LE)
RICHCMP_WRAPPER(eq, Py_EQ)
RICHCMP_WRAPPER(ne, Py_NE)
RICHCMP_WRAPPER(gt, Py_GT)
RICHCMP_WRAPPER(ge, Py_GE)


static PyObject *
wrap_next(PyObject *self, PyObject *args, void *wrapped)
{
    unaryfunc func = (unaryfunc)wrapped;
    PyObject *res;

    /* if (!check_num_args(args, 0)) */
    /*     return NULL; */
    res = (*func)(self);
    if (res == NULL && !PyErr_Occurred())
        PyErr_SetNone(PyExc_StopIteration);
    return res;
}


static PyObject *
wrap_binaryfunc(PyObject *self, PyObject *args, void *wrapped)
{
    binaryfunc func = (binaryfunc)wrapped;
    PyObject *other;

    if (!check_num_args(args, 1))
        return NULL;
    other = PyTuple_GET_ITEM(args, 0);
    return (*func)(self, other);
}

static PyObject *
wrap_binaryfunc_l(PyObject *self, PyObject *args, void *wrapped)
{
    binaryfunc func = (binaryfunc)wrapped;
    PyObject *other;

    if (!check_num_args(args, 1))
        return NULL;
    other = PyTuple_GET_ITEM(args, 0);
    return (*func)(self, other);
}

static PyObject *
wrap_binaryfunc_r(PyObject *self, PyObject *args, void *wrapped)
{
    binaryfunc func = (binaryfunc)wrapped;
    PyObject *other;

    if (!check_num_args(args, 1))
        return NULL;
    other = PyTuple_GET_ITEM(args, 0);
    return (*func)(other, self);
}

static PyObject *
wrap_indexargfunc(PyObject *self, PyObject *args, void *wrapped)
{
    ssizeargfunc func = (ssizeargfunc)wrapped;
    PyObject* o;
    Py_ssize_t i;

    if (!PyArg_UnpackTuple(args, "", 1, 1, &o))
        return NULL;
    i = PyNumber_AsSsize_t(o, PyExc_OverflowError);
    if (i == -1 && PyErr_Occurred())
        return NULL;
    return (*func)(self, i);
}

static PyObject *
tp_new_wrapper(PyObject *self, PyObject *args, PyObject *kwds)
{
    PyTypeObject *type, *subtype, *staticbase;
    PyObject *arg0, *res;
    PyTypeObject *type1;

    /* if (self == NULL || !PyType_Check(self)) */
    /*     Py_FatalError("__new__() called with non-type 'self'"); */
    type = (PyTypeObject *)self;
    /* if (!PyTuple_Check(args) || PyTuple_GET_SIZE(args) < 1) { */
    /*     PyErr_Format(PyExc_TypeError, */
    /*                  "%s.__new__(): not enough arguments", */
    /*                  type->tp_name); */
    /*     return NULL; */
    /* } */
    arg0 = PyTuple_GetItem(args, 0);
    /* if (!PyType_Check(arg0)) { */
    /*     PyErr_Format(PyExc_TypeError, */
    /*                  "%s.__new__(X): X is not a type object (%s)", */
    /*                  type->tp_name, */
    /*                  Py_TYPE(arg0)->tp_name); */
    /*     return NULL; */
    /* } */
    subtype = (PyTypeObject *)arg0;
    /* if (!PyType_IsSubtype(subtype, type)) { */
    /*     PyErr_Format(PyExc_TypeError, */
    /*                  "%s.__new__(%s): %s is not a subtype of %s", */
    /*                  type->tp_name, */
    /*                  subtype->tp_name, */
    /*                  subtype->tp_name, */
    /*                  type->tp_name); */
    /*     return NULL; */
    /* } */

    args = PyTuple_GetSlice(args, 1, PyTuple_Size(args));
    /* if (args == NULL) */
    /*     return NULL; */
    res = type->tp_new(subtype, args, kwds);
    /* Py_DECREF(args); */
    return res;
}


// FIXME: incomplete, defined in abstract.c
Py_ssize_t
PyNumber_AsSsize_t(PyObject *item, PyObject *err)
{
    Py_ssize_t result;
    PyObject *runerr;
    PyObject *value = PyNumber_Index(item);
    if (value == NULL)
        return -1;

    /* We're done if PyLong_AsSsize_t() returns without error. */
    result = PyLong_AsSsize_t(value);
    return result;
}


static PyObject *
null_error(void)
{
    if (!PyErr_Occurred())
        PyErr_SetString(PyExc_SystemError,
                        "null argument to internal routine");
    return NULL;
}

// FIXME: incomplete, defined in abstract.c
PyObject*
PyNumber_Index(PyObject *item)
{
    PyObject *result = NULL;
    if (item == NULL) {
        return null_error();
    }

    if (PyLong_Check(item)) {
        Py_INCREF(item);
        return item;
    }//    if (!PyIndex_Check(item))
    else
    {
        PyErr_Format(PyExc_TypeError,
                     "'%.200s' object cannot be interpreted "
                     "as an integer", item->ob_type->tp_name);
        return NULL;
    }
}



void *
PyMem_Malloc(size_t size)
{
    if (size == 0)
        size = 1;
    return malloc(size);
}

void *
PyMem_Calloc(size_t nelem, size_t elsize)
{
    if (nelem == 0 || elsize == 0) {
        nelem = 1;
        elsize = 1;
    }
    return calloc(nelem, elsize);
}

void *
PyMem_Realloc(void *ptr, size_t size)
{
    if (size == 0)
        size = 1;
    return realloc(ptr, size);
}

void
PyMem_Free(void *ptr)
{
    free(ptr);
}

Py_UCS4* PyUnicode_AsUCS4Copy(PyObject *unicode)
{
    // let's be imprecise: we malloc a buffer of the good size, but don't copy anything
    Py_ssize_t s = PyUnicode_GetLength(unicode);
    if(s == -1 && PyErr_Occurred())
        return NULL;
    Py_UCS4* ret = (Py_UCS4*) malloc(s * sizeof(Py_UCS4));
    if(ret)
    {
//        ret[s] = 0; or no delimiter?
        return ret;
    }
    else
    {
        PyErr_NoMemory();
        return NULL;
    }
}



PyObject *PyBool_FromLong(long ok)
{
    PyObject *result;

    if (ok)
        result = Py_True;
    else
        result = Py_False;
    Py_INCREF(result);
    return result;
}



int
PySequence_Check(PyObject *s)
{
    if (PyDict_Check(s))
        return 0;
    // Force disjonction this way (if multiple values for s), while waiting for partitioning
    if(s->ob_type->tp_as_sequence) return s->ob_type->tp_as_sequence->sq_item != NULL;
    else return 0;
    /* return s->ob_type->tp_as_sequence && */
    /*     s->ob_type->tp_as_sequence->sq_item != NULL; */
}

Py_ssize_t
PySequence_Size(PyObject *s)
{
    PySequenceMethods *m;

    if (s == NULL) {
        null_error();
        return -1;
    }

    m = s->ob_type->tp_as_sequence;
    if (m && m->sq_length) {
        Py_ssize_t len = m->sq_length(s);
        assert(len >= 0 || PyErr_Occurred());
        return len;
    }

    if (s->ob_type->tp_as_mapping && s->ob_type->tp_as_mapping->mp_length) {
        type_error("%.200s is not a sequence", s);
        return -1;
    }
    type_error("object of type '%.200s' has no len()", s);
    return -1;
}

PyObject *
PySequence_GetItem(PyObject *s, Py_ssize_t i)
{
    PySequenceMethods *m;

    if (s == NULL) {
        return null_error();
    }

    m = s->ob_type->tp_as_sequence;
    if (m && m->sq_item) {
        if (i < 0) {
            if (m->sq_length) {
                Py_ssize_t l = (*m->sq_length)(s);
                if (l < 0) {
                    assert(PyErr_Occurred());
                    return NULL;
                }
                i += l;
            }
        }
        return m->sq_item(s, i);
    }

    if (s->ob_type->tp_as_mapping && s->ob_type->tp_as_mapping->mp_subscript) {
        return type_error("%.200s is not a sequence", s);
    }
    return type_error("'%.200s' object does not support indexing", s);
}

/* PyObject * */
/* PySequence_GetSlice(PyObject *s, Py_ssize_t i1, Py_ssize_t i2) */
/* { */
/*     PyMappingMethods *mp; */

/*     if (!s) { */
/*         return null_error(); */
/*     } */

/*     mp = s->ob_type->tp_as_mapping; */
/*     if (mp && mp->mp_subscript) { */
/*         PyObject *res; */
/*         PyObject *slice = _PySlice_FromIndices(i1, i2); */
/*         if (!slice) */
/*             return NULL; */
/*         res = mp->mp_subscript(s, slice); */
/*         Py_DECREF(slice); */
/*         return res; */
/*     } */

/*     return type_error("'%.200s' object is unsliceable", s); */
/* } */

PyObject *
PySequence_Fast(PyObject *v, const char *m)
{
    PyObject *it;

    if (v == NULL) {
        return null_error();
    }

    if (PyList_CheckExact(v) || PyTuple_CheckExact(v)) {
        Py_INCREF(v);
        return v;
    }

    it = PyObject_GetIter(v);
    if (it == NULL) {
        if (PyErr_ExceptionMatches(PyExc_TypeError))
            PyErr_SetString(PyExc_TypeError, m);
        return NULL;
    }

    v = PySequence_List(it);
    Py_DECREF(it);

    return v;
}


/* PyObject * */
/* PyIter_Next(PyObject *iter) */
/* { */
/*     PyObject *result; */
/*     result = (*iter->ob_type->tp_iternext)(iter); */
/*     if (result == NULL && */
/*         PyErr_Occurred() && */
/*         PyErr_ExceptionMatches(PyExc_StopIteration)) */
/*         PyErr_Clear(); */
/*     return result; */
/* } */

PyObject *
PyObject_SelfIter(PyObject *obj)
{
    Py_INCREF(obj);
    return obj;
}

PyObject *
PyObject_GetIter(PyObject *o)
{
    PyTypeObject *t = o->ob_type;
    getiterfunc f;

    f = t->tp_iter;
    if (f == NULL) {
        if (PySequence_Check(o))
            return PySeqIter_New(o);
        return type_error("'%.200s' object is not iterable", o);
    }
    else {
        PyObject *res = (*f)(o);
        if (res != NULL && !PyIter_Check(res)) {
            PyErr_Format(PyExc_TypeError,
                         "iter() returned non-iterator "
                         "of type '%.100s'",
                         res->ob_type->tp_name);
            Py_DECREF(res);
            res = NULL;
        }
        return res;
    }
}

#define call_method PyObject_CallMethod
#undef PyObject_CallMethod
PyObject *
PyObject_CallMethod(PyObject *obj, const char *name, const char *format, ...);

static PyObject *
slot_tp_iternext(PyObject *self)
{
    _Py_IDENTIFIER(__next__);
    const char* name = PyId___next__.string;
    return PyObject_CallMethod(self, name, NULL, 0);
}

PyObject *
_PyObject_NextNotImplemented(PyObject *self)
{
    PyErr_Format(PyExc_TypeError,
                 "'%.200s' object is not iterable",
                 Py_TYPE(self)->tp_name);
    return NULL;
}


PyObject *
PyLong_FromVoidPtr(void *p)
{
//#if SIZEOF_VOID_P <= SIZEOF_LONG
    return PyLong_FromUnsignedLong((unsigned long)(uintptr_t)p);
/* #else */

/* #if SIZEOF_LONG_LONG < SIZEOF_VOID_P */
/* #   error "PyLong_FromVoidPtr: sizeof(long long) < sizeof(void*)" */
/* #endif */
/*     return PyLong_FromUnsignedLongLong((unsigned long long)(uintptr_t)p); */
/* #endif /\* SIZEOF_VOID_P <= SIZEOF_LONG *\/ */

}
