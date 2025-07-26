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
#include <iconv.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * requires: valid_string(__tocode);
 * requires: valid_string(__fromcode);
 *
 * case "success" {
 *   local: iconv_t r = new IConv;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == cast(iconv_t, -1);
 * }
 */
iconv_t iconv_open (const char *__tocode, const char *__fromcode);

/*$
 * requires: alive_resource(__cd, IConv);
 * requires: __inbuf != NULL implies valid_ptr(__inbuf);
 * requires: __outbuf != NULL implies valid_ptr(__outbuf);
 * assigns: _errno;
 *
 * case "inoutbuf" {
 *   assumes: __inbuf != NULL and *__inbuf != NULL;
 *   assumes: __outbuf != NULL;
 *   requires: valid_bytes(*__inbuf, *__inbytesleft);
 *   requires: valid_bytes(*__outbuf, *__outbytesleft);
 *   assigns: (*__outbuf)[0, *__outbytesleft);
 *   assigns: *__inbuf;
 *   assigns: *__outbuf;
 *   assigns: *__inbytesleft;
 *   assigns: *__outbytesleft;
 *   ensures: exists size_t i in [0,*__inbytesleft]: 
 *       ((*__inbytesleft)' == *__inbytesleft - i and
 *        (*__inbuf)' == *__inbuf + i);
 *   ensures: exists size_t i in [0,*__outbytesleft]: 
 *       ((*__outbytesleft)' == *__outbytesleft - i and
 *        (*__outbuf)' == *__outbuf + i);
 * }
 *
 * case "outbuf" {
 *   assumes: __inbuf != NULL implies *__inbuf == NULL;
 *   assumes: __outbuf != NULL and *__outbuf != NULL;
 *   requires: valid_bytes(*__outbuf, *__outbytesleft);
 *   assigns: (*__outbuf)[0, *__outbytesleft);
 *   assigns: *__outbuf;
 *   assigns: *__outbytesleft;
 *   ensures: exists size_t i in [0,*__outbytesleft]: 
 *       ((*__outbytesleft)' == *__outbytesleft - i and
 *        (*__outbuf)' == *__outbuf + i);
 * }
 *
 * case "none" {
 *   assumes: __inbuf != NULL implies *__inbuf == NULL;
 *   assumes: __outbuf != NULL implies *__outbuf == NULL;
 * }
 */
size_t iconv (iconv_t __cd, char **__restrict __inbuf,
              size_t *__restrict __inbytesleft,
              char **__restrict __outbuf,
              size_t *__restrict __outbytesleft);

/*$
 * requires: __cd in IConv;
 * free: __cd;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int iconv_close (iconv_t __cd);
