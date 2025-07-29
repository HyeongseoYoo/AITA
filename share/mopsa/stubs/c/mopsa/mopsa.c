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


#include <stddef.h>
#include <limits.h>
#include "mopsa.h"

/*******************/
/* Base predicates */
/*******************/

/*$=
 * predicate valid_base(p):
 *   (p != NULL) and
 *   (p != INVALID) and
 *   alive(p);
 *
 *   predicate valid_base_or_fail(p):
 *     (p != NULL otherwise raise("NULL pointer dereference")) and
 *     (p != INVALID otherwise raise("Invalid pointer dereference")) and
 *     (alive(p) otherwise (
 *       resource(p)?raise("Use after free")
 *                  :raise("Dangling pointer dereference")));
 */

/**********************/
/* Pointer predicates */
/**********************/

/*$=
 * predicate valid_ptr(p):
 *   valid_base(p) and
 *   offset(p) >= 0 and
 *   bytes(p) >= offset(p) + sizeof_expr(*p);
 *
 * predicate null_or_valid_ptr(p):
 *   if p != NULL then valid_ptr(p) end;
 *
 * predicate valid_ptr_or_fail(p):
 *   valid_base_or_fail(p) and
 *   ( ( offset(p) >= 0 and
 *       bytes(p) >= offset(p) + sizeof_expr(*p) )
 *     otherwise raise("Out-of-bound access") );
 *
 * predicate null_or_valid_ptr_or_fail(p):
 *   if p != NULL then valid_ptr_or_fail(p) end;
 *
 * predicate valid_ptr_range(p, i, j):
 *   forall _mopsa_size_t k in [i, j]: valid_ptr(p+k);
 *
 * predicate valid_ptr_range_or_fail(p, i, j):
 *   forall _mopsa_size_t k in [i, j]: valid_ptr_or_fail(p+k);
 */

/*******************/
/* Byte predicates */
/*******************/

/*$=
 * predicate valid_bytes(p, n):
 *   if n > 0 then
 *     valid_base(p) and
 *     offset(p) >= 0 and
 *     bytes(p) >= offset(p) + n
 *   end;
 *
 * predicate valid_bytes_or_fail(p, n):
 *   if n > 0 then
 *     valid_base_or_fail(p) and
 *     ( ( offset(p) >= 0 and
 *         bytes(p) >= offset(p) + n )
 *       otherwise raise("Out-of-bound access"))
 *   end;
 *
 * predicate null_or_valid_bytes(p, n):
 *   if p != NULL then valid_bytes(p, n) end;
 *
 * predicate null_or_valid_bytes_or_fail(p, n):
 *   if p != NULL then valid_bytes_or_fail(p, n) end;
 *
 * predicate in_bytes(r, x, n):
 *   exists _mopsa_size_t _i in [0, n]: r == x + _i;
 */

/*********************/
/* String predicates */
/*********************/

/*$=
 * predicate valid_string(s):
 *   valid_ptr(s) and
 *   exists _mopsa_size_t _i in [0, bytes(s) - offset(s)): s[_i] == 0;
 *
 * predicate valid_string_or_fail(s):
 *   valid_ptr_or_fail(s) and
 *   ( (exists _mopsa_size_t _i in [0, bytes(s) - offset(s)): s[_i] == 0) otherwise raise("Non-terminating string") );
 *
 * predicate null_or_valid_string(s):
 *   if s != NULL then valid_string(s) end;
 *
 * predicate null_or_valid_string_or_fail(s):
 *   if s != NULL then valid_string_or_fail(s) end;
 *
 * predicate valid_primed_string(s):
 *   valid_ptr(s) and
 *   exists _mopsa_size_t _i in [0, (bytes(s) - offset(s)) ): (s[_i])' == 0;
 *
 * predicate valid_substring(s, n):
 *   if n > 0 then
 *     ( valid_ptr(s) and
 *     exists _mopsa_size_t _i in [0, n): s[_i] == 0 )
 *   end;
 *
 * predicate valid_substring_or_fail(s, n):
 *   if n > 0 then
 *     valid_ptr_or_fail(s) and
 *     exists _mopsa_size_t _i in [0, n): s[_i] == 0
 *   end;
 *
 * predicate valid_primed_substring(s, n):
 *   if n > 0 then
 *     valid_ptr(s) and
 *     exists _mopsa_size_t _i in [0, n): (s[_i])' == 0
 *   end;
 *
 * predicate in_string(x,s):
 *   exists _mopsa_size_t _i in [0, (bytes(s) - offset(s))):
 *     ( x == s + _i and
 *       forall _mopsa_size_t _j in [0, _i): s[_j] != 0 );
 */

/***************************/
/* Wide strings predicates */
/***************************/

/*$=
 * predicate valid_wide_string(s):
 *   valid_ptr(s) and
 *   bytes(s) >= 1 and
 *   exists _mopsa_size_t _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))): s[_i] == 0;
 *
 * predicate valid_wide_string_or_fail(s):
 *   valid_ptr_or_fail(s) and
 *   bytes(s) >= 1 and
 *   exists _mopsa_size_t _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))): s[_i] == 0;
 *
 * predicate valid_wide_substring(s, n):
 *   if n > 0 then valid_ptr(s) end and
 *   exists _mopsa_size_t _i in [0, n): s[_i] == 0;
 *
 * predicate null_or_valid_wide_string(s):
 *   if s != NULL then valid_wide_string(s) end;
 *
 * predicate valid_primed_wide_string(s):
 *   valid_ptr(s) and
 *   exists _mopsa_size_t _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))): (s[_i])' == 0;
 *
 * predicate valid_primed_wide_substring(s, n):
 *   if n > 0 then valid_ptr(s) end and
 *   exists _mopsa_size_t _i in [0, n): (s[_i])' == 0;
 *
 * predicate in_wide_string(x,s):
 *   exists _mopsa_size_t _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))):
 *     ( x == s + _i and
 *       forall _mopsa_size_t _j in [0, _i): s[_j] != 0 );
 *
 * predicate valid_wchars(s, n):
 *   if n > 0 then
 *     valid_ptr(s) and
 *     bytes(s) >= offset(s) + n * sizeof_type(wchar_t)
 *   end;
 *
 * predicate valid_wchars_or_fail(s, n):
 *   if n > 0 then
 *     valid_ptr_or_fail(s) and
 *     bytes(s) >= offset(s) + n * sizeof_type(wchar_t)
 *   end;
 *
 * predicate null_or_valid_wchars(s, n):
 *   if n > 0 then
 *     if s != NULL then
 *       valid_ptr(s) and
 *       bytes(s) >= offset(s) + n * sizeof_type(wchar_t)
 *     end
 *   end;
 *
 *  predicate in_wchars(r, x, n):
 *    exists _mopsa_size_t _i in [0, n]: r == x + _i;
 */

/***********************/
/* Resource predicates */
/***********************/

/*$=
 * predicate alive_resource(p, r):
 *   p in r and alive(p);
 */


/*$
 * local:   char * str = new Memory;
 * ensures: size(str) == INT_MAX;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_valid_string();

/*$
 * local:  char ** ar = new Memory;
 * local:  char* s1 = _mopsa_new_valid_string();
 * local:  char* s2 = _mopsa_new_valid_string();
 * local:  char* s3 = _mopsa_new_valid_string();
 * ensures: size(ar) == sizeof_type(char*) * 4;
 * ensures: ar[0] == s1;
 * ensures: ar[1] == s2;
 * ensures: ar[2] == s3;
 * ensures: ar[3] == NULL;
 * ensures: return == ar;
 * unsound: "stubs only create string arrays of 3 strings";
 */
char **_mopsa_new_valid_string_array();

/*$
 * local:   char * str = new ReadOnlyMemory;
 * ensures: size(str) == INT_MAX;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_readonly_string();

/*$
 * local:   char * str = new Memory;
 * ensures: size(str) == max;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_valid_string_max(_mopsa_size_t max);

/*$
 * local:   char * str = new ReadOnlyMemory;
 * ensures: size(str) == max;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_readonly_string_max(_mopsa_size_t max);


/*$
 * requires: valid_ptr_or_fail(p);
 */
void _mopsa_assert_valid_ptr(void *p);

/*$
 * requires: valid_bytes_or_fail(p, n);
 */
void _mopsa_assert_valid_bytes(void *p, _mopsa_size_t n);

/*$

 * requires: valid_string_or_fail(s);
 */
void _mopsa_assert_valid_string(char *s);

/*$
 * requires: valid_substring(s,n);
 */
void _mopsa_assert_valid_substring(char *s, _mopsa_size_t n);


/*$
 * requires: valid_ptr_range_or_fail(s, i, j);
 * assigns: s[i, j];
 */
void _mopsa_memrand(char *s, _mopsa_size_t i, _mopsa_size_t j);


/*$
 * requires: valid_ptr_range_or_fail(s, 0, size(s) - offset(s) - 1);
 * assigns: s[0, (size(s) - offset(s)) );
 * ensures: valid_primed_string(s);
 */
void _mopsa_strrand(char *s);

/*$
 * case "non-empty" {
 *   assumes: n >= 1;
 *   requires: valid_ptr_range_or_fail(s, 0, n - 1);
 *   assigns: s[0, n);
 *   ensures: valid_primed_substring(s,n);
 * }
 *
 * case "empty" {
 *   assumes: n == 0;
 * }
 */
void _mopsa_strnrand(char *s, _mopsa_size_t n);


/*$
 * requires: alive_resource(stream, File);
 */
void _mopsa_assert_valid_stream(void* stream);

extern void *_mopsa_find_file_resource(int fd);

/*$
 * local:    void* f = _mopsa_find_file_resource(fd);
 * requires: alive_resource(f, FileRes);
 */
void _mopsa_assert_valid_file_descriptor(int fd);

/*$
 * requires: valid_ptr_range_or_fail(s, i, j);
 * assigns: s[i, j];
 * ensures: forall _mopsa_size_t k in [i,j]: (s[k])' == c;
 */
void _mopsa_memset(char *s, char c, _mopsa_size_t i, _mopsa_size_t j);

/*$
 * requires: valid_ptr_range_or_fail(dst, i, j);
 * requires: valid_ptr_range_or_fail(src, i, j);
 * assigns: dst[i, j];
 * ensures: forall _mopsa_size_t k in [i,j]: (dst[k])' == src[k];
 */
void _mopsa_memcpy(char *dst, char *src, _mopsa_size_t i, _mopsa_size_t j);

/*$
 * requires: valid_wide_string_or_fail(s);
 */
void _mopsa_assert_valid_wide_string(wchar_t *s);


/*$
 * case "non-empty" {
 *   assumes: n >= 1;
 *   requires: valid_wchars_or_fail(s, n);
 *   assigns: s[0, n);
 *   ensures: valid_primed_wide_substring(s,n);
 * }
 *
 * case "empty" {
 *   assumes: n == 0;
 * }
 */
void _mopsa_wcsnrand(wchar_t *s, _mopsa_size_t n);

/*$
 * ensures: valid_float(return);
 */
double _mopsa_valid_double();

/*$
 * ensures: valid_float(return);
 */
float _mopsa_valid_float();
