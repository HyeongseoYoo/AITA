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
#include <utmp.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
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
int login_tty (int __fd);

/*$
 * requires: valid_ptr(__entry);
 */
void login (const struct utmp *__entry);

/*$
 * requires: valid_string(__ut_line);
 *
 * case "success" {
 *   ensures: return == 1;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == 0;
 * }
 */
int logout (const char *__ut_line);

/*$
 * requires: valid_string(__ut_line);
 * requires: valid_string(__ut_name);
 * requires: valid_string(__ut_host);
 */
void logwtmp (const char *__ut_line, const char *__ut_name,
              const char *__ut_host);

/*$
 * requires: valid_string(__wtmp_file);
 * requires: valid_ptr(__utmp);
 */
void updwtmp (const char *__wtmp_file, const struct utmp *__utmp);

/*$
 * requires: valid_string(__file);
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
int utmpname (const char *__file);

static struct utmp _mopsa_utmp_buf;

/*$
 * assigns: _mopsa_utmp_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmp_buf;
 */
struct utmp *getutent (void);

/*$
 * assigns: _errno;
 */
void setutent (void);

/*$
 * assigns: _errno;
 */
void endutent (void);

/*$
 * requires: valid_ptr(__id);
 * assigns: _mopsa_utmp_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmp_buf;
 */
struct utmp *getutid (const struct utmp *__id);

/*$
 * requires: valid_ptr(__line);
 * assigns: _mopsa_utmp_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmp_buf;
 */
struct utmp *getutline (const struct utmp *__line);

/*$
 * requires: valid_ptr(__utmp_ptr);
 * assigns: _errno;
 * ensures: return == NULL or return == __utmp_ptr;
 */
struct utmp *pututline (const struct utmp *__utmp_ptr);

/*$
 * requires: valid_ptr(__buffer);
 * requires: valid_ptr(__result);
 * assigns: *__buffer;
 * assigns: *__result;
 *
 * case "success" {
 *   ensures: (*__result)' == __buffer;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getutent_r (struct utmp *__buffer, struct utmp **__result);

/*$
 * requires: valid_ptr(__id);
 * requires: valid_ptr(__buffer);
 * requires: valid_ptr(__result);
 * assigns: *__buffer;
 * assigns: *__result;
 *
 * case "success" {
 *   ensures: (*__result)' == __buffer;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getutid_r (const struct utmp *__id, struct utmp *__buffer,
               struct utmp **__result);

/*$
 * requires: valid_ptr(__line);
 * requires: valid_ptr(__buffer);
 * requires: valid_ptr(__result);
 * assigns: *__buffer;
 * assigns: *__result;
 *
 * case "success" {
 *   ensures: (*__result)' == __buffer;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getutline_r (const struct utmp *__line,
                 struct utmp *__buffer, struct utmp **__result);
