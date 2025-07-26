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

#include "python_stubs.h"
// not in CPython

void
_PyType_Assign_Helper(PyObject* obj, PyTypeObject* type)
// in practice, only used to get the types PyObject and PyTypeObject...
{
    *(PyTypeObject**) ((char*)obj + offsetof(PyObject, ob_type)) = type;
}


int PyType_ReadyCheat(PyTypeObject *type)
{
    if(type->tp_flags & Py_TPFLAGS_READY) {
        return 1;
    }

    Py_SET_TYPE(type, &PyType_Type);
    type->tp_flags =
        (type->tp_flags & ~Py_TPFLAGS_READYING) | Py_TPFLAGS_READY;
    type->tp_alloc = PyType_GenericAlloc;
    return 0;
}


static PySequenceMethods unicode_as_sequence =
{
    .sq_length=(lenfunc)PyObject_Size,
    .sq_item=(ssizeargfunc)PyUnicode_GetItem,
//    .sq_ass_item=???
};

static PyMappingMethods unicode_as_mapping = {
    .mp_length=(lenfunc)PyObject_Size,
    .mp_subscript=(binaryfunc)PyObject_GetItem,  /* mp_subscript */
    .mp_ass_subscript=(objobjargproc)0,           /* mp_ass_subscript */
};



static PySequenceMethods list_as_sequence =
{
    .sq_length=(lenfunc)PyList_Size,
    .sq_item=(ssizeargfunc)PyList_GetItem,
//    .sq_ass_item=???
};

static PySequenceMethods tuple_as_sequence =
{
    .sq_length=(lenfunc)PyTuple_Size,
    .sq_item=(ssizeargfunc)PyTuple_GetItem,
};

static PySequenceMethods range_as_sequence =
{
    .sq_length=(lenfunc)PyObject_Size,
    .sq_item=(ssizeargfunc)PyList_GetItem,
// these two functions are not defined in the C api
};

static PySequenceMethods set_as_sequence =
{
    .sq_length=(lenfunc)PySet_Size,
    .sq_item=(ssizeargfunc)PyList_GetItem,
// these two functions are not defined in the C api
};


void
init_flags()
{
    Py_SET_TYPE(&PyType_Type, &PyType_Type);
    Py_SET_TYPE(&PyBaseObject_Type, &PyType_Type);
    Py_SET_TYPE(&PyLong_Type, &PyType_Type);
    Py_SET_TYPE(&PyFloat_Type, &PyType_Type);
    Py_SET_TYPE(&PyUnicode_Type, &PyType_Type);
    Py_SET_TYPE(&PyList_Type, &PyType_Type);
    Py_SET_TYPE(&PyListIter_Type, &PyType_Type);
    Py_SET_TYPE(&PySlice_Type, &PyType_Type);
    Py_SET_TYPE(&PySet_Type, &PyType_Type);
    Py_SET_TYPE(&PySetIter_Type, &PyType_Type);
    Py_SET_TYPE(&PyRange_Type, &PyType_Type);
    Py_SET_TYPE(&PyRangeIter_Type, &PyType_Type);
    Py_SET_TYPE(&PyTuple_Type, &PyType_Type);
    Py_SET_TYPE(&PyTupleIter_Type, &PyType_Type);
    Py_SET_TYPE(&_PyNone_Type, &PyType_Type);
    Py_SET_TYPE(&_PyNotImplemented_Type, &PyType_Type);
    Py_SET_TYPE(&PyBytes_Type, &PyType_Type);
    Py_SET_TYPE(&PyDict_Type, &PyType_Type);

    PyType_Type.tp_as_sequence = 0;
    PyBaseObject_Type.tp_as_sequence = 0;
    PyLong_Type.tp_as_sequence = 0;
    PyFloat_Type.tp_as_sequence = 0;
    PyUnicode_Type.tp_as_sequence = &unicode_as_sequence;
    PyUnicode_Type.tp_as_mapping = &unicode_as_mapping;
    PyTuple_Type.tp_as_sequence = &tuple_as_sequence;
    PySet_Type.tp_as_sequence = &set_as_sequence;
    PyList_Type.tp_as_sequence = &list_as_sequence;
    PyRange_Type.tp_as_sequence = &range_as_sequence;
    PyTupleIter_Type.tp_as_sequence = 0;
    PySetIter_Type.tp_as_sequence = 0;
    PyListIter_Type.tp_as_sequence = 0;
    PyRangeIter_Type.tp_as_sequence = 0;
    PySlice_Type.tp_as_sequence = 0;
    _PyNone_Type.tp_as_sequence = 0;
    _PyNotImplemented_Type.tp_as_sequence = 0;
    // FIXME: bytes_as_sequence
    // FIXME: dict_as_sequence

    PyType_Type.tp_iternext = 0;
    PyBaseObject_Type.tp_iternext = 0;
    PyLong_Type.tp_iternext = 0;
    PyFloat_Type.tp_iternext = 0;
    PyUnicode_Type.tp_iternext = 0;
    PyTuple_Type.tp_iternext = 0;
    PySet_Type.tp_iternext = 0;
    PyList_Type.tp_iternext = 0;
    PySlice_Type.tp_iternext = 0;
    PyRange_Type.tp_iternext = 0;
    _PyNone_Type.tp_iternext = 0;
    _PyNotImplemented_Type.tp_iternext = 0;
    PyTupleIter_Type.tp_iternext = PyIter_Next;
    PySetIter_Type.tp_iternext = PyIter_Next;
    PyListIter_Type.tp_iternext = PyIter_Next;
    PyRangeIter_Type.tp_iternext = PyIter_Next;

    PyType_Type.tp_iter = 0;
    PyBaseObject_Type.tp_iter = 0;
    PyLong_Type.tp_iter = 0;
    PyFloat_Type.tp_iter = 0;
    PyUnicode_Type.tp_iter = PyObject_GetIter;
    PyTuple_Type.tp_iter = PyObject_GetIter;
    PyList_Type.tp_iter = PyObject_GetIter;
    PySlice_Type.tp_iter = 0;
    PySet_Type.tp_iter = PyObject_GetIter;
    PyRange_Type.tp_iter = PyObject_GetIter;
    PyTupleIter_Type.tp_iter = PyObject_GetIter; // FIXME should be selfiter for all Py*Iter_Type
    PyListIter_Type.tp_iter = PyObject_GetIter;
    PySetIter_Type.tp_iter = PyObject_GetIter;
    PyRangeIter_Type.tp_iter = PyObject_GetIter;
    _PyNone_Type.tp_iter = 0;
    _PyNotImplemented_Type.tp_iter = 0;


    PyFloat_Type.tp_as_number = 0;

    // all flags are in the object declaration except Ready, which is
    // added upon completion of PyType_Ready
    PyType_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_TYPE_SUBCLASS | Py_TPFLAGS_READY;
    PyBaseObject_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY;
    PyBool_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_READY;
    PyLong_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_LONG_SUBCLASS | Py_TPFLAGS_READY;
    PyFloat_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY;
    PyUnicode_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_UNICODE_SUBCLASS | Py_TPFLAGS_READY;
    PyList_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_LIST_SUBCLASS | Py_TPFLAGS_READY;
    PySet_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY;
    PyRange_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_READY;
    PyTuple_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_TUPLE_SUBCLASS | Py_TPFLAGS_READY;
    PyTupleIter_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_READY;
    PyListIter_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_READY;
    PyRangeIter_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_READY;
    PySetIter_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_READY;
    _PyNone_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_READY;
    _PyNotImplemented_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_READY;
    PyBytes_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_BYTES_SUBCLASS | Py_TPFLAGS_READY;
    PyDict_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_DICT_SUBCLASS | Py_TPFLAGS_READY;
    PySlice_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_READY;
}

struct _longobject _Py_FalseStruct = {
    PyVarObject_HEAD_INIT(&PyBool_Type, 0)
    { 0 }
};

struct _longobject _Py_TrueStruct = {
    PyVarObject_HEAD_INIT(&PyBool_Type, 1)
    { 1 }
};


PyObject _Py_NoneStruct = {
  _PyObject_EXTRA_INIT
  1, &_PyNone_Type
};

PyObject _Py_NotImplementedStruct = {
    _PyObject_EXTRA_INIT
    1, &_PyNotImplemented_Type
};

// this is all the work done by type_new actually
void set_tp_alloc_py_class(PyTypeObject *t, PyTypeObject *base)
{
    t->tp_alloc = PyType_GenericAlloc;
    t->tp_basicsize = base->tp_basicsize;
    t->tp_itemsize = base->tp_itemsize;
    t->tp_iternext = base->tp_iternext;
}


void set_default_flags(PyTypeObject *t)
{
    // should be 284160
    // 9, 10, 12, 14, 18
    t->tp_flags = Py_TPFLAGS_HEAPTYPE | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_HAVE_VERSION_TAG;
    t->tp_as_sequence = 0; // FIXME: more complicated, if a class has a __getitem__
    t->tp_as_number = 0;
    t->tp_as_mapping = 0;
}

int PyParseTuple_int_helper(PyObject *obj, int *result)
{
    /* small adaption of case 'i' in convertsimple in getargs.c */
    long ival = PyLong_AsLong(obj);
    if(ival == -1 && PyErr_Occurred()) {
        return 0;
    }
    else if (ival > INT_MAX) {
        PyErr_SetString(PyExc_OverflowError,
                        "signed integer is greater than maximum");
        return 0;
    }
    else if (ival < INT_MIN) {
        PyErr_SetString(PyExc_OverflowError,
                        "signed integer is less than minimum");
        return 0;
    }
    else {
        *result = ival;
        return 1;
    }
}

int PyParseTuple_shortint_helper(PyObject *obj, short *result)
{
    /* small adaption of case 'h' in convertsimple in getargs.c */
    long ival;
    /* if (float_argument_error(arg)) */
    /*     RETURN_ERR_OCCURRED; */
    ival = PyLong_AsLong(obj);
    if (ival == -1 && PyErr_Occurred())
        return 0;
    else if (ival < SHRT_MIN) {
        PyErr_SetString(PyExc_OverflowError,
                        "signed short integer is less than minimum");
        return 0;
    }
    else if (ival > SHRT_MAX) {
        PyErr_SetString(PyExc_OverflowError,
                        "signed short integer is greater than maximum");
        return 0;
    }
    else {
        *result = (short) ival;
        return 1;
    }
}


static PyObject *
type_error(const char *msg, PyObject *obj)
{
    PyErr_Format(PyExc_TypeError, msg, obj->ob_type->tp_name);
    return NULL;
}
