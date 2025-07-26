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
  based on header from glibc-2.27-r6
*/
#include <stddef.h>
#include <locale.h>
#include <errno.h>
#include "mopsa_libc_utils.h"


#define LOCAL_BUF_SIZE 100

char * _local_buf;


/*$!
 * assigns: _local_buf;
 * local: char *addr = _mopsa_new_readonly_string_max(LOCAL_BUF_SIZE);
 * ensures: _local_buf' == addr;
 */



/*$
 * requires: null_or_valid_string_or_fail(__locale);
 *
 * case "success" {
 *   local: char *addr = _mopsa_new_readonly_string_max(LOCAL_BUF_SIZE);
 *   assigns: _local_buf;
 *   ensures: _local_buf' == addr;
 *   ensures: return == addr;
 *   free:    _local_buf;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *setlocale (int __category, const char *__locale);


struct lconv * _lconv_buf = NULL;

/*$
 * local: struct lconv * addr = new ReadOnlyMemory;
 *
 * assigns: _lconv_buf;
 *
 * ensures: size(addr) == sizeof_type(struct lconv);
 * ensures: return == addr;
 * ensures: _lconv_buf' == addr;
 *
 * case "first-allocation" {
 *   assumes: _lconv_buf == NULL;
 * }
 *
 * case "invalidate-previous-lconv" {
 *   assumes: _lconv_buf != NULL;
 *   free: _lconv_buf;
 * }
 */
struct lconv *localeconv (void);

static locale_t  _mopsa_last_locale;

/*$!
 * local: locale_t r = new Locale;
 * assigns: _mopsa_last_locale;
 * ensures: _mopsa_last_locale' == r;
 */

/*$
 * requires: valid_string_or_fail(__locale);
 * requires: if __base != 0 then alive_resource(__base, Locale) end;
 * local: locale_t r = new Locale;
 * assigns: _errno;
 * ensures: return == r;
 *
 * case "free" {
 *   assumes: alive_resource(__base, Locale);
 *   free: __base;
 * }
 *
 * case "nofree" {
 *   assumes: __base == 0;
 * }
 */
locale_t newlocale (int __category_mask, const char *__locale, locale_t __base);

/*$
 * requires: alive_resource(__dataset, Locale);
 * local: locale_t r = new Locale;
 * assigns: _errno;
 * ensures: return == r or return == 0;
 * // TODO: LC_GLOBAL_LOCALE
 */
locale_t duplocale (locale_t __dataset);

/*$
 * requires: __dataset in Locale;
 * free: __dataset;
 */
void freelocale (locale_t __dataset);

/*$
 * requires: alive_resource(__dataset, Locale) or __dataset == 0;
 * assigns: _mopsa_last_locale;
 * ensures: _mopsa_last_locale' == __dataset;
 * ensures: return == _mopsa_last_locale;
 * // TODO: LC_GLOBAL_LOCALE
 */
locale_t uselocale (locale_t __dataset);

