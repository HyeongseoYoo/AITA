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
#include <sys/select.h>
#include <errno.h>
#include "../mopsa_libc_utils.h"

/*$
 * requires: __nfds >= 1;
 * requires: null_or_valid_ptr(__readfds);
 * requires: null_or_valid_ptr(__writefds);
 * requires: null_or_valid_ptr(__exceptfds);
 * requires: null_or_valid_ptr(__timeout);
 * assigns: _errno;
 * ensures: return >= -1 and return <- __nfds;
 * unsound: "select does not check that file-descriptors are valid";
 *
 * case "read" {
 *   assumes: __readfds != NULL;
 *   assigns: *__readfds;
 * }
 *
 * case "write" {
 *   assumes: __writefds != NULL;
 *   assigns: *__writefds;
 * }
 *
 * case "except" {
 *   assumes: __exceptfds != NULL;
 *   assigns: *__exceptfds;
 * }
 *
 * case "timeout" {
 *   assumes: __timeout != NULL;
 *   assigns: *__timeout;
 *   ensures: (__timeout->tv_sec)' >= 0 and  (__timeout->tv_sec)' <= __timeout->tv_sec;
 *   ensures: (__timeout->tv_usec)' >= 0 and  (__timeout->tv_usec)' < 10000000;
 * }
 *
 * case "NULL" {
 * }
 */
int select (int __nfds, fd_set *__restrict __readfds,
            fd_set *__restrict __writefds,
            fd_set *__restrict __exceptfds,
            struct timeval *__restrict __timeout);

/*$
 * requires: null_or_valid_ptr(__timeout);
 * requires: null_or_valid_ptr(__sigmask);
 * local: int r = select(__nfds,__readfds,__writefds,__exceptfds,NULL);
 */
int pselect (int __nfds, fd_set *__restrict __readfds,
             fd_set *__restrict __writefds,
             fd_set *__restrict __exceptfds,
             const struct timespec *__restrict __timeout,
             const __sigset_t *__restrict __sigmask);
