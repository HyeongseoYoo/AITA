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

#ifndef _MOPSA_H
#define _MOPSA_H

#define __MOPSA__

typedef unsigned long long _mopsa_size_t;

// Generators of typed random values
extern signed char _mopsa_rand_s8();
extern unsigned char _mopsa_rand_u8();
#define _mopsa_rand_char _mopsa_rand_s8

extern signed short _mopsa_rand_s16();
extern unsigned short _mopsa_rand_u16();

extern signed int _mopsa_rand_s32();
extern unsigned int _mopsa_rand_u32();
#define _mopsa_rand_int _mopsa_rand_s32

extern signed long _mopsa_rand_s64();
extern unsigned long _mopsa_rand_u64();

extern float _mopsa_rand_float();
extern double _mopsa_rand_double();

extern void *_mopsa_rand_void_pointer();
#define _mopsa_rand_pointer _mopsa_rand_void_pointer

// Generators of typed random ranges
extern signed char _mopsa_range_s8(signed char l, signed char u);
extern unsigned char _mopsa_range_u8(unsigned char l, unsigned char u);
#define _mopsa_range_char _mopsa_range_s8

extern signed short _mopsa_range_s16(signed short l, signed short u);
extern unsigned short _mopsa_range_u16(unsigned short l, unsigned short u);

extern signed int _mopsa_range_s32(signed int l, signed int u);
extern unsigned int _mopsa_range_u32(unsigned int l, unsigned int u);
#define _mopsa_range_int _mopsa_range_s32

extern signed long _mopsa_range_s64(signed long l, signed long u);
extern unsigned long _mopsa_range_u64(unsigned long l, unsigned long u);

extern float _mopsa_range_float(float l, float u);
extern double _mopsa_range_double(double l, double u);

extern float _mopsa_valid_float();
extern double _mopsa_valid_double();


// Invalid pointer
extern void *_mopsa_invalid_pointer();

// Raise Exception.Panic exception with a given message
extern void _mopsa_panic(const char* msg);


// Printing
extern void _mopsa_print();


// Filters
extern void _mopsa_assume(int cond);

// Assertions
extern void _mopsa_assert(int cond);
extern void _mopsa_assert_exists(int cond);
extern void _mopsa_assert_unreachable();
extern void _mopsa_assert_reachable();
extern void _mopsa_assert_safe();
extern void _mopsa_assert_unsafe();

// stdatomic.h
#include "atomic_stubs.h"

// Memory
extern void _mopsa_assert_valid_ptr(void *p);
extern void _mopsa_assert_valid_bytes(void *p, _mopsa_size_t n);
extern void _mopsa_memrand(char *s, _mopsa_size_t i, _mopsa_size_t j);
extern void _mopsa_memset(char *s, char c, _mopsa_size_t i, _mopsa_size_t j);
extern void _mopsa_memcpy(char *dst, char *src, _mopsa_size_t i, _mopsa_size_t j);

// Strings
extern void _mopsa_assert_valid_string(char *s);
extern void _mopsa_assert_valid_substring(char *s, _mopsa_size_t n);
extern char *_mopsa_new_valid_string();
extern char **_mopsa_new_valid_string_array();
extern char *_mopsa_new_readonly_string();
extern char *_mopsa_new_valid_string_max(_mopsa_size_t max);
extern char *_mopsa_new_readonly_string_max(_mopsa_size_t max);
extern void _mopsa_strrand(char *s);
extern void _mopsa_strnrand(char *s, _mopsa_size_t n);

// Files
extern void _mopsa_assert_valid_stream(void* stream);
extern void _mopsa_assert_valid_file_descriptor(int fd);


#endif //_MOPSA_H
