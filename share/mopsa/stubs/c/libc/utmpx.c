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
#include <utmpx.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * assigns: _errno;
 */
void setutxent (void);

/*$
 * assigns: _errno;
 */
void endutxent (void);

static struct utmpx _mopsa_utmpx_buf;

/*$
 * assigns: _mopsa_utmpx_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmpx_buf;
 */
struct utmpx *getutxent (void);

/*$
 * requires: valid_ptr(__id);
 * assigns: _mopsa_utmpx_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmpx_buf;
 */
struct utmpx *getutxid (const struct utmpx *__id);

/*$
 * requires: valid_ptr(__line);
 * assigns: _mopsa_utmpx_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmpx_buf;
 */
struct utmpx *getutxline (const struct utmpx *__line);

/*$
 * requires: valid_ptr(__utmpx);
 * assigns: _mopsa_utmpx_buf;
 * assigns: _errno;
 * ensures: return == NULL or return == &_mopsa_utmpx_buf;
 */
struct utmpx *pututxline (const struct utmpx *__utmpx);

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
int utmpxname (const char *__file);

/*$
 * requires: valid_string(__wtmpx_file);
 * requires: valid_ptr(__utmpx);
 */
void updwtmpx (const char *__wtmpx_file,
               const struct utmpx *__utmpx);

/*$
 * requires: valid_ptr(__utmpx);
 * requires: valid_ptr(__utmp);
 * assigns: *__utmp;
 */
void getutmp (const struct utmpx *__utmpx, struct utmp *__utmp);

/*$
 * requires: valid_ptr(__utmpx);
 * requires: valid_ptr(__utmp);
 * assigns: *__utmpx;
 */
void getutmpx (const struct utmp *__utmp, struct utmpx *__utmpx);
