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

#include <stddef.h>
#include <sys/types.h>
#include <pwd.h>
#include <errno.h>
#include "mopsa_libc_utils.h"


/*$
 * local: struct passwd* r = new Pwd;
 * local: char* pw_name = _mopsa_new_readonly_string();
 * local: char* pw_passwd =  _mopsa_new_readonly_string();
 * local: char* pw_gecos =  _mopsa_new_readonly_string();
 * local: char* pw_dir = _mopsa_new_readonly_string();
 * local: char* pw_shell =  _mopsa_new_readonly_string();
 * ensures: size(r) == sizeof_type(struct passwd);
 * ensures: r->pw_name == pw_name;
 * ensures: r->pw_passwd == pw_passwd;
 * ensures: r->pw_gecos == pw_gecos;
 * ensures: r->pw_dir == pw_dir;
 * ensures: r->pw_shell == pw_shell;
 * ensures: return == r;
 */
struct passwd* _mopsa_alloc_pw();

/*$
 */
void setpwent (void);

/*$
 */
void endpwent (void);


/*$
 * case "success" {
 *   local: struct passwd* r = _mopsa_alloc_pw();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
struct passwd *getpwent (void);

/*$
 * requires: alive_resource(__stream, File);
 *
 * case "success" {
 *   local: struct passwd* r = _mopsa_alloc_pw();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
struct passwd *fgetpwent (FILE *__stream);

/*$
 * requires: alive_resource(__f, File);
 * requires: valid_ptr(__p);
 * requires: valid_string(__p->pw_name);
 * requires: valid_string(__p->pw_passwd);
 * requires: valid_string(__p->pw_gecos);
 * requires: valid_string(__p->pw_dir);
 * requires: valid_string(__p->pw_shell);
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
int putpwent (const struct passwd *__restrict __p, FILE *__restrict __f);

/*$
 * case "success" {
 *   local: struct passwd* r = _mopsa_alloc_pw();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
struct passwd *getpwuid (__uid_t __uid);

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local: struct passwd* r = _mopsa_alloc_pw();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
struct passwd *getpwnam (const char *__name);
