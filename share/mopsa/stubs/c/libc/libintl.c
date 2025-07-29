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
 * Stubs for libintl.h
 * Based on header from glibc-2.27-r6
 */

#include <libintl.h>
#include <stddef.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/* If true, gettext returns its argument string unchanged.
   This makes it possible to check printf formats that use gettext.
 */
#define IDENTITY_GETTEXT 1


/*$
 * requires: null_or_valid_string_or_fail(__domainname);
 * local: char* r = _mopsa_new_readonly_string();
 * assigns: _errno;
 * ensures: return == NULL or return == r;
 */
char *textdomain (const char *__domainname);

/*$
 * requires: valid_string_or_fail(__domainname);
 * requires: null_or_valid_string_or_fail(__dirname);
 * local: char* r = _mopsa_new_readonly_string();
 * assigns: _errno;
 * ensures: return == NULL or return == r;
 */
char *bindtextdomain (const char *__domainname, const char *__dirname);

/*$
 * requires: valid_string_or_fail(__domainname);
 * requires: null_or_valid_string_or_fail(__codeset);
 * local: char* r = _mopsa_new_readonly_string();
 * assigns: _errno;
 * ensures: return == NULL or return == r;
 */
char *bind_textdomain_codeset (const char *__domainname, const char *__codeset);



#define GETTEXT_BUF_SIZE 100
static char _gettext_buf[GETTEXT_BUF_SIZE];

#if IDENTITY_GETTEXT
/*$
 * requires: valid_string_or_fail(__msgid);
 * ensures: return == __msgid;
 */
char *gettext (const char *__msgid);
#else
/*$
 * requires: valid_string_or_fail(__msgid);
 *
 * case "translation found" {
 *   assigns: _gettext_buf[0, GETTEXT_BUF_SIZE);
 *   ensures: valid_primed_string(_gettext_buf);
 *   ensures: return == _gettext_buf;
 * }
 *
 * case "translation not found" {
 *   ensures: return == _gettext_buf;
 * }
 */
char *gettext (const char *__msgid);
#endif


/*$
 * requires: valid_string_or_fail(__domainname);
 * local: char* r = gettext(__msgid);
 * ensures: return == r;
 */
char *dgettext (const char *__domainname, const char *__msgid);

/*$
 * requires: valid_string_or_fail(__domainname);
 * local: char* r = gettext(__msgid);
 * ensures: return == r;
 */
char *dcgettext (const char *__domainname, const char *__msgid,
		 int __category);

/*$
 * local: char* r1 = gettext(__msgid1);
 * local: char* r2 = gettext(__msgid2);
 * ensures: return == r1 or return == r2;
 */
char *ngettext (const char *__msgid1, const char *__msgid2,
		unsigned long int __n);

/*$
 * requires: valid_string_or_fail(__domainname);
 * local: char* r = ngettext(__msgid1, __msgid2, __n);
 * ensures: return == r;
 */
char *dngettext (const char *__domainname, const char *__msgid1,
		 const char *__msgid2, unsigned long int __n);

/*$
 * requires: valid_string_or_fail(__domainname);
 * local: char* r = ngettext(__msgid1, __msgid2, __n);
 * ensures: return == r;
 */
char *dcngettext (const char *__domainname, const char *__msgid1,
		  const char *__msgid2, unsigned long int __n, int __category);
