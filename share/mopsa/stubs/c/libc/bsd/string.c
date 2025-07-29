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
  utility functions from BSD system (libbsd)
*/
#include <stddef.h>
#include <bsd/string.h>
#include "mopsa_libc_utils.h"

/*$
 * requires: valid_bytes(__dst, __size);
 * requires: __size > 0;
 * local: size_t src_len = strlen(__src);
 * ensures: return == src_len;
 *
 * case "small" {
 *   assumes: __size > src_len;
 *   assigns: __dst[0, src_len];
 *   ensures: forall size_t i in [0, src_len]: (__dst[i])' == __src[i];
 * }
 *
 * case "large" {
 *   assumes: __size <= src_len;
 *   assigns: __dst[0, __size);
 *   ensures: forall size_t i in [0, __size-1): (__dst[i])' == __src[i];
 *   ensures: (__dst[__size-1])' == 0;
 * }
 */
size_t strlcpy(char *__dst, const char *__src, size_t __size);

/*$
 * requires: valid_bytes(__dst, __size);
 * requires: __size > 0;
 * local: size_t src_len = strlen(__src);
 * local: size_t dst_len = strlen(__dst);
 * ensures: return == src_len + dst_len;
 *
 * case "small" {
 *   assumes: __size > src_len + dst_len;
 *   assigns: __dst[0, __size);
 *   ensures: forall size_t i in [0, dst_len): (__dst[i])' == __dst[i];
 *   ensures: forall size_t i in [0, src_len]: (__dst[dst_len + i])' == __src[i];
 * }
 *
 * case "large" {
 *   assumes: __size > dst_len and __size <= src_len + dst_len;
 *   assigns: __dst[0, __size);
 *   ensures: forall size_t i in [0, dst_len): (__dst[i])' == __dst[i];
 *   ensures: forall size_t i in [0, __size-dst_len-1): (__dst[dst_len + i])' == __src[i];
 *   ensures: (__dst[__size-1])' == 0;
 * }
 *
 * case "too_large" {
 *   assumes: __size <= dst_len;
 * }
 */
size_t strlcat(char *__dst, const char *__src, size_t __size);

/*$
 * local: size_t len1 = strlen(__str);
 * local: size_t len2 = strlen(__find);
 *
 * case "small" {
 *   assumes: len1 < __str_len;
 *   ensures: return == NULL or
 *            (len1 >= len2 and in_bytes(return, __str, len1 - len2));
 * }
 *
 * case "large" {
 *   assumes: len1 >= __str_len;
 *   ensures: return == NULL or
 *            (__str_len >= len2 and in_bytes(return, __str, __str_len - len2));
 * }
 */
char *strnstr(const char *__str, const char *__find, size_t __str_len);
//
/*$
 * requires: valid_bytes(__str, 12);
 * assigns: __str[0, 11];
 * ensures: (__str[11])' == 0;
 */
void strmode(mode_t mode, char *__str);
