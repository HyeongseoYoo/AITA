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
#include <sys/time.h>
#include "../mopsa_libc_utils.h"
#include <fcntl.h> // for AT_FDCWD
#include <errno.h>


/*
  Starting from glibc 2.31, __timezone_ptr_t is replaced by void*
*/
#if __GLIBC_MINOR__ >= 31
#define __timezone_ptr_t void*
#endif

#define valid_primed_tv(tv)             \
    primed(tv.tv_sec) in [0, 2000000000] and \
    primed(tv.tv_usec) in [0, 1000000]


/*$
 * requires: __tv != NULL implies valid_ptr(__tv);
 * requires: __tz != NULL implies valid_ptr(__tz);
 *
 * case "tv-tz" {
 *   assumes: valid_ptr(__tv);
 *   assumes: valid_ptr(__tz);
 *   assigns: *__tv;
 *   assigns: *__tz;
 *   ensures: valid_primed_tv(*__tv);
 *   ensures: return == 0;
 * }
 *
 * case "tv" {
 *   assumes: valid_ptr(__tv);
 *   assumes: __tz == NULL;
 *   assigns: *__tv;
 *   ensures: valid_primed_tv(*__tv);
 *   ensures: return == 0;
 * }
 *
 * case "tz" {
 *   assumes: __tv == NULL;
 *   assumes: valid_ptr(__tz);
 *   assigns: *__tz;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int gettimeofday (struct timeval *__restrict __tv, __timezone_ptr_t __tz);

/*$
 * requires: __tv != NULL implies valid_ptr(__tv);
 * requires: __tz != NULL implies valid_ptr(__tz);
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
int settimeofday (const struct timeval *__tv,
                  const struct timezone *__tz);

/*$
 * requires: valid_ptr(__delta);
 * requires: __olddelta != NULL implies valid_ptr(__olddelta);
 * 
 * case "olddelta" {
 *   assumes: __olddelta != NULL;
 *   assigns: *__olddelta;
 *   ensures: valid_primed_tv(*__olddelta);
 *   ensures: return == 0;
 * }
 *
 * case "null" {
 *   assumes: __olddelta == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int adjtime (const struct timeval *__delta,
             struct timeval *__olddelta);

/*$
 * requires: valid_ptr(__value);
 *
 * case "success" {
 *   assigns: *__value;
 *   ensures: valid_primed_tv(__value->it_interval);
 *   ensures: valid_primed_tv(__value->it_value);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getitimer (__itimer_which_t __which,
               struct itimerval *__value);

/*$
 * requires: valid_ptr(__new);
 * requires: __old != NULL implies valid_ptr(__old);
 *
 * case "old" {
 *   assumes: __old != NULL;
 *   assigns: *__old;
 *   ensures: valid_primed_tv(__old->it_interval);
 *   ensures: valid_primed_tv(__old->it_value);
 *   ensures: return == 0;
 * }
 *
 * case "noold" {
 *   assumes: __old == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setitimer (__itimer_which_t __which,
               const struct itimerval *__restrict __new,
               struct itimerval *__restrict __old);

/*$
 * requires: valid_string(__file);
 * requires: __tvp != NULL implies valid_ptr_range(__tvp, 0, 1);
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
int utimes (const char *__file, const struct timeval __tvp[2]);

/*$
 * #alias utimes;
 */
int lutimes (const char *__file, const struct timeval __tvp[2]);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __tvp != NULL implies valid_ptr_range(__tvp, 0, 1);
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
int futimes (int __fd, const struct timeval __tvp[2]);

/*$
 * requires: valid_string(__file);
 * requires: __tvp != NULL implies valid_ptr_range(__tvp, 0, 1);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or f in FileRes;
 *
 * case "AT_FDCWD" {
 *   assumes: __fd == AT_FDCWD;
 *   ensures: return == 0;
 * }
 *
 * case "no-AT_FDCWD" {
 *   assumes: f in FileRes;
 *   requires: alive_resource(f, FileRes);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int futimesat (int __fd, const char *__file,
               const struct timeval __tvp[2]);
