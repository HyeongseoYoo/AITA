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
#include <dirent.h>
#include <errno.h>
#include "mopsa_libc_utils.h"


struct __dirstream { int __fileno; };


/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local: DIR* r = new Dir;
 *   local: int fd = _mopsa_register_file_resource(r);
 *   ensures: bytes(r) == sizeof_type(DIR);
 *   ensures: r->__fileno == fd;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
DIR *opendir (const char *__name);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   local: DIR* r = new Dir;
 *   ensures: bytes(r) == sizeof_type(DIR);
 *   ensures: r->__fileno == __fd;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
DIR *fdopendir (int __fd);

/*$
 * requires: __dirp in Dir;
 *
 * case "success" {
 *   free: __dirp;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int closedir (DIR *__dirp);

static struct dirent _mopsa_dirent_buf;

/*$
 * requires: alive_resource(__dirp, Dir);
 * assigns:  _mopsa_dirent_buf;
 * ensures: valid_primed_substring(_mopsa_dirent_buf.d_name, 256);
 * ensures: return == NULL or return == &_mopsa_dirent_buf;
 */
struct dirent *readdir (DIR *__dirp);

// deprecated, un supported
int readdir_r (DIR *__restrict __dirp,
               struct dirent *__restrict __entry,
               struct dirent **__restrict __result);

/*$
 * requires: alive_resource(__dirp, Dir);
 */
void rewinddir (DIR *__dirp);

/*$
 * requires: alive_resource(__dirp, Dir);
 */
void seekdir (DIR *__dirp, long int __pos);

/*$
 * requires: alive_resource(__dirp, Dir);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
long int telldir (DIR *__dirp);

/*$
 * requires: alive_resource(__dirp, Dir);
 * ensures: return == __dirp->__fileno;
 */
int dirfd (DIR *__dirp);

// unsupported
int scandir (const char *__restrict __dir,
             struct dirent ***__restrict __namelist,
             int (*__selector) (const struct dirent *),
             int (*__cmp) (const struct dirent **,
                           const struct dirent **));

// unsupported
int scandirat (int __dfd, const char *__restrict __dir,
               struct dirent ***__restrict __namelist,
               int (*__selector) (const struct dirent *),
               int (*__cmp) (const struct dirent **,
                             const struct dirent **));

/*$
 * requires: valid_ptr(__e1);
 * requires: valid_ptr(__e2);
 * requires: valid_ptr(*__e1);
 * requires: valid_ptr(*__e2);
 * requires: valid_string((*__e1)->d_name);
 * requires: valid_string((*__e2)->d_name);
 */
int alphasort (const struct dirent **__e1,
               const struct dirent **__e2);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_ptr(__basep);
 * requires: valid_bytes(__buf, __nbytes);
 * assigns: *__basep;
 * assigns: __basep[0, __nbytes);
 */
__ssize_t getdirentries (int __fd, char *__restrict __buf,
                         size_t __nbytes,
                         __off_t *__restrict __basep);

/*$
 * #alias alphasort;
 */
int versionsort (const struct dirent **__e1,
                 const struct dirent **__e2);
