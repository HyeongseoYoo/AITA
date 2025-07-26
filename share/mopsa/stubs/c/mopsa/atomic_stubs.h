/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2019 The MOPSA Project.                                    */
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

#ifndef _ATOMIC_STUBS_H
#define _ATOMIC_STUBS_H


/*
  Replaces Clang's __c11_atomic_ intrisincs with regular (non-atomic) 
  operations to allow analysis until we support concurrency and atomicity 
  in Mopsa.
 */

#define __c11_atomic_init(obj, value)           \
  (*(obj) = (value))

#define __c11_atomic_load(obj, order)           \
  ({ (order); (*(obj)) })

#define __c11_atomic_store(obj, value, order)   \
  ({ (order); (*(obj) = (value)) })

/*
  "typeof(*(obj)+0)" is a trick to get the type of the objet pointed by obj
  with _Atomic stripped (to avoid type error in assignment)
 */
#define __c11_atomic_exchange(obj, value, order)                        \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = (value);                                                   \
    __tmp;                                                              \
  })

#define __c11_atomic_compare_exchange_strong(obj1, obj2, val, order1, order2) \
  ({ (order1);                                                          \
    (order2);                                                           \
    typeof(obj1) __obj1 = (obj1);                                       \
    typeof(obj2) __obj2 = (obj2);                                       \
    typeof(val) __val = (val);                                          \
    int __tmp = *__obj1 == *__obj2;                                     \
    if (__tmp) *__obj1 = __val; else *__obj2 = *__obj1;                 \
    __tmp; })

#define __c11_atomic_compare_exchange_weak(obj1, obj2, val, order1, order2) \
  __c11_atomic_compare_exchange_strong(obj1, obj2, val, order1, order2)

#define __c11_atomic_fetch_add(obj, arg, order)                         \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(arg) __arg = (arg);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp + __arg;                                             \
    __tmp; })

#define __c11_atomic_fetch_sub(obj, arg, order)                         \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(arg) __arg = (arg);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp - __arg;                                             \
    __tmp; })

#define __c11_atomic_fetch_and(obj, arg, order)                         \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp & (arg);                                             \
    __tmp; })

#define __c11_atomic_fetch_or(obj, arg, order)                          \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp | (arg);                                             \
    __tmp; })

#define __c11_atomic_fetch_xor(obj, arg, order)                         \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp ^ (arg);                                             \
    __tmp; })

#define __c11_atomic_fetch_max(obj, arg, order)                         \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(arg) __arg = (arg);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp > __arg ? __tmp : __arg                              \
    __tmp; })

#define __c11_atomic_fetch_min(obj, arg, order)                         \
  ({ (order);                                                           \
    typeof(obj) __obj = (obj);                                          \
    typeof(arg) __arg = (arg);                                          \
    typeof(*(obj)+0) __tmp = *__obj;                                    \
    *__obj = __tmp < __arg ? __tmp : __arg                              \
    __tmp; })


/* Now for GCC's atomics */

#define __atomic_load_n(ptr, memorder)          \
  ({ (memorder); (*(ptr)); })

#define __atomic_load(ptr, ret, memorder) \
  ({ (memorder); (*(ret) = *(ptr)); 0; })

#define __atomic_store_n(ptr, val, memorder) \
  ({ (memorder); (*(ptr)) = (val); 0; })

#define __atomic_store(ptr, val, memorder) \
  ({ (memorder); (*(ptr) = *(val)); 0; })

#define __atomic_exchange_n(ptr, val, memorder)                         \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = (val);                                                     \
    __tmp; })

#define __atomic_exchange(ptr, val, ret, memorder)                      \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = *(val);                                                    \
    *(ret) =__tmp;                                                      \
    0; })

#define __atomic_compare_exchange_n(ptr, expected, desired, weak, success_memorder, failure_memorder) \
  ({ (success_memorder);                                                \
    (failure_memorder);                                                 \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(expected) __expected = (expected);                           \
    typeof(desired) __desired = (desired);                              \
    int __tmp = *__ptr == *__expected;                                  \
    if (__tmp) *__ptr = __desired; else *__expected = *__ptr;           \
    __tmp; })

#define __atomic_compare_exchange(ptr, expected, desired, weak, success_memorder, failure_memorder) \
  ({ (success_memorder);                                                \
    (failure_memorder);                                                 \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(expected) __expected = (expected);                           \
    typeof(desired) __desired = (desired);                              \
    int __tmp = *__ptr == *__expected;                                  \
    if (__tmp) *__ptr = *__desired; else *__expected = *__ptr;          \
    __tmp; })

#define __atomic_add_fetch(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr + (val);                            \
    *__ptr = __tmp;                                                     \
    __tmp; })

#define __atomic_sub_fetch(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr - (val);                            \
    *__ptr = __tmp;                                                     \
    __tmp; })

#define __atomic_and_fetch(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr & (val);                            \
    *__ptr = __tmp;                                                     \
    __tmp; })

#define __atomic_or_fetch(ptr, val, memorder)                           \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr | (val);                            \
    *__ptr = __tmp;                                                     \
    __tmp; })

#define __atomic_xor_fetch(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr ^ (val);                            \
    *__ptr = __tmp;                                                     \
    __tmp; })

#define __atomic_nand_fetch(ptr, val, memorder)                         \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = ~(*__ptr & (val));                         \
    *__ptr = __tmp;                                                     \
    __tmp; })

#define __atomic_fetch_add(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = __tmp + (val);                                             \
    __tmp; })

#define __atomic_fetch_sub(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = __tmp - (val);                                             \
    __tmp; })

#define __atomic_fetch_and(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = __tmp & (val);                                             \
    __tmp; })

#define __atomic_fetch_or(ptr, val, memorder)                           \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = __tmp | (val);                                             \
    __tmp; })

#define __atomic_fetch_xor(ptr, val, memorder)                          \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = __tmp ^ (val);                                             \
    __tmp; })

#define __atomic_fetch_nand(ptr, val, memorder)                         \
  ({ (memorder);                                                        \
    typeof(ptr) __ptr = (ptr);                                          \
    typeof(*(ptr)+0) __tmp = *__ptr;                                    \
    *__ptr = ~(__tmp & (val));                                          \
    __tmp; })

#define __atomic_test_and_set(ptr, memorder)   \
  ({ (memorder);                               \
    char* __ptr = (char*) (ptr);               \
    _Bool __tmp = *__ptr;                      \
    *__ptr = _mopsa_rand_char();               \
    __tmp; })

#define __atomic_clear(ptr, memorder)           \
  ({ (memorder); *(ptr) = 0; 0; })

#define __atomic_thread_fence(memorder)         \
  ({ (memorder); 0; })

#define __atomic_signal_fence(memorder)         \
  ({ (memorder); 0; })

#define __atomic_always_lock_free(size, ptr)    \
  ({ (memorder); (ptr); _mopsa_rand_s8(); })

#define __atomic_is_lock_free(size, ptr)        \
  ({ (memorder); (ptr); _mopsa_rand_s8(); })

#endif

