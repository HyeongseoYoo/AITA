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
#include <grp.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * assigns: _errno;
 */
void setgrent (void);

/*$
 * assigns: _errno;
 */
void endgrent (void);

struct group _mopsa_group_buf;

/*$
 * assigns: _mopsa_group_buf;
 * local: char* name = _mopsa_new_valid_string();
 * local: char* passwd = _mopsa_new_valid_string();
 * local: char** mem = _mopsa_new_valid_string_array();
 * ensures: (_mopsa_group_buf.gr_name)' == name;
 * ensures: (_mopsa_group_buf.gr_passwd)' == passwd;
 * ensures: (_mopsa_group_buf.gr_mem)' == mem;
 * ensures: return == &_mopsa_group_buf;
 */
static struct group* _mopsa_group();

/*$
 * assigns: _errno;
 * local: struct group* r = _mopsa_group();
 * ensures: return == NULL or return == r;
 */
struct group *getgrent (void);

/*$
 * requires: alive_resource(__stream, File);
 * assigns: _errno;
 * local: struct group* r = _mopsa_group();
 * ensures: return == NULL or return == r;
 */
struct group *fgetgrent (FILE *__stream);

/*$
 * requires: alive_resource(__f, File);
 * requires: valid_ptr(__p);
 * requires: valid_string(__p -> gr_name);
 * requires: valid_ptr(__p -> gr_name);
 * assigns: _errno;
 */
int putgrent (const struct group *__restrict __p,
              FILE *__restrict __f);

/*$
 * assigns: _errno;
 * local: struct group* r = _mopsa_group();
 * ensures: return == NULL or return == r;
 */
struct group *getgrgid (__gid_t __gid);

/*$
 * requires: valid_string(__name);
 * assigns: _errno;
 * local: struct group* r = _mopsa_group();
 * ensures: return == NULL or return == r;
 */
struct group *getgrnam (const char *__name);

// unsupported
int getgrent_r (struct group *__restrict __resultbuf,
                char *__restrict __buffer, size_t __buflen,
                struct group **__restrict __result);

// unsupported
int getgrgid_r (__gid_t __gid, struct group *__restrict __resultbuf,
                char *__restrict __buffer, size_t __buflen,
                struct group **__restrict __result);

// unsupported
int getgrnam_r (const char *__restrict __name,
                struct group *__restrict __resultbuf,
                char *__restrict __buffer, size_t __buflen,
                struct group **__restrict __result);

// unsupported
int fgetgrent_r (FILE *__restrict __stream,
                 struct group *__restrict __resultbuf,
                 char *__restrict __buffer, size_t __buflen,
                 struct group **__restrict __result);

/*$
 * requires: if (__n >= 1) then valid_ptr_range(__groups, 0, __n - 1) end;
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
int setgroups (size_t __n, const __gid_t *__groups);

/*$
 * requires: valid_ptr(__ngroups);
 * requires: *__groups >= 0 implies valid_ptr_range(__groups, 0, *__groups - 1);
 * assigns: __groups[0, *__ngroups);
 * assigns: *__ngroups;
 * ensures: (*__ngroups)' >= 0;
 * ensures: return >= -1;
 */
int getgrouplist (const char *user, __gid_t __group,
			 __gid_t *__groups, int *__ngroups);
/*$
 * requires: valid_string(user);
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
int initgroups (const char *user, __gid_t __group);
