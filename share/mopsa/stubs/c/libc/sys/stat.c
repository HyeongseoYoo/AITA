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
#include <sys/stat.h>
#include "../mopsa_libc_utils.h"
#include <fcntl.h> // for AT_FDCWD
#include <errno.h>


/*$
 * requires: valid_string(__file);
 * requires: valid_ptr(__buf);
 *
 * case "success" {
 *   assigns: *__buf;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int stat (const char *__restrict __file,
          struct stat *__restrict __buf);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_ptr(__buf);
 *
 * case "success" {
 *   assigns: *__buf;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fstat (int __fd, struct stat *__buf);

/*$
 * requires: valid_string(__file);
 * requires: valid_ptr(__buf);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
 *
 * case "success" {
 *   assigns: *__buf;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fstatat (int __fd, const char *__restrict __file,
             struct stat *__restrict __buf, int __flag);

/*$
 * #alias stat;
 */
int lstat (const char *__restrict __file,
           struct stat *__restrict __buf);

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
int chmod (const char *__file, __mode_t __mode);

/*$
 * #alias chmod;
 */
int lchmod (const char *__file, __mode_t __mode);

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
int fchmod (int __fd, __mode_t __mode);

/*$
 * requires: valid_string(__file);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or  alive_resource(f, FileRes);
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
int fchmodat (int __fd, const char *__file, __mode_t __mode,
              int __flag);

/*$
 * // empty
 */
__mode_t umask (__mode_t __mask);

/*$
 * //empty
 */
__mode_t getumask (void);

/*$
 * requires: valid_string(__path);
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
int mkdir (const char *__path, __mode_t __mode);

/*$
 * requires: valid_string(__path);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int mkdirat (int __fd, const char *__path, __mode_t __mode);

/*$
 * requires: valid_string(__path);
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
int mknod (const char *__path, __mode_t __mode, __dev_t __dev);

/*$
 * requires: valid_string(__path);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int mknodat (int __fd, const char *__path, __mode_t __mode,
             __dev_t __dev);

/*$
 * requires: valid_string(__path);
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
int mkfifo (const char *__path, __mode_t __mode);

/*$
 * requires: valid_string(__path);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int mkfifoat (int __fd, const char *__path, __mode_t __mode);

/*$
 * requires: valid_string(__path);
 * requires: __times != NULL implies valid_ptr_range(__times, 0, 1);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int utimensat (int __fd, const char *__path,
               const struct timespec __times[2],
               int __flags);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
  * requires: __times != NULL implies valid_ptr_range(__times, 0, 1);
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
int futimens (int __fd, const struct timespec __times[2]);










