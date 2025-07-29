/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2017-2019 The MOPSA Project.                               */
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

/*
  libc stub
  based on header from glibc-2.29-r7
*/
#include <stddef.h>
#include <strings.h>
#include <string.h>
#include "mopsa_libc_utils.h"

/*$
 * local: int r = memcmp(__s1, __s2, __n);
 * ensures: return == r;
 */
int bcmp (const void *__s1, const void *__s2, size_t __n);

/*$
 * local: void* r = memmove(__dest, __src, __n);
 */
void bcopy (const void *__src, void *__dest, size_t __n);

/*$
 * requires: valid_bytes(__s, __n);
 * assigns: ((unsigned char*)__s)[0, __n);
 * ensures: forall size_t i in [0, __n): (((unsigned char*)__s)[i])' == 0;
 */
void bzero (void *__s, size_t __n);

/*$
 * local: char* r = strchr(__s, __c);
 * ensures: return == r;
 */
char *index (const char *__s, int __c);

/*$
 * local: char* r = strrchr(__s, __c);
 * ensures: return == r;
 */
char *rindex (const char *__s, int __c);

/*$
 * ensures: return in [0, (sizeof_type(int) * 8)];
 */
int ffs (int __i);

/*$
 * ensures: return in [0, (sizeof_type(long) * 8)];
 */
int ffsl (long int __l);

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 */
int strcasecmp (const char *__s1, const char *__s2);

/*$
 * requires: valid_ptr(__s1);
 * requires: valid_ptr(__s2);
 * requires: size(__s1) < offset(__s1) + __n implies valid_string(__s1);
 * requires: size(__s2) < offset(__s2) + __n implies valid_string(__s2);
 */
int strncasecmp (const char *__s1, const char *__s2, size_t __n);

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 */
int strcasecmp_l (const char *__s1, const char *__s2, locale_t __loc);

/*$
 * requires: valid_ptr(__s1);
 * requires: valid_ptr(__s2);
 * requires: size(__s1) < offset(__s1) + __n implies valid_string(__s1);
 * requires: size(__s2) < offset(__s2) + __n implies valid_string(__s2);
 */
int strncasecmp_l (const char *__s1, const char *__s2,
                   size_t __n, locale_t __loc);
