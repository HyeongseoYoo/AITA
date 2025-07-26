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
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <errno.h>
#include "../mopsa_libc_utils.h"

/*$
 * requires: __stat_loc != NULL implies valid_ptr(__stat_loc);
 *
 * case "stat_loc" {
 *   assumes: __stat_loc != NULL;
 *   assigns: *__stat_loc;
 *   ensures: return >= 0;
 * }
 *
 * case "no-stat_loc" {
 *   assumes: __stat_loc == NULL;
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t wait (int *__stat_loc);

/*$
 * local: __pid_t r = wait(__stat_loc);
 * ensures: return == r;
 */
__pid_t waitpid (__pid_t __pid, int *__stat_loc, int __options);

/*$
 * requires: valid_ptr(__infop);
 * assigns: *__infop;
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
int waitid (idtype_t __idtype, __id_t __id, siginfo_t *__infop,
            int __options);

#define valid_primed_rusage(r) \
          primed(r->ru_utime.tv_sec)  in [0, 2000000000] \
      and primed(r->ru_utime.tv_usec) in [0, 1000000] \
      and primed(r->ru_stime.tv_sec)  in [0, 2000000000] \
      and primed(r->ru_stime.tv_usec) in [0, 1000000] \
      and primed(r->ru_maxrss) >= 0 \
      and primed(r->ru_minflt) >= 0 \
      and primed(r->ru_majflt) >= 0 \
      and primed(r->ru_inblock) >= 0 \
      and primed(r->ru_oublock) >= 0 \
      and primed(r->ru_nvcsw) >= 0 \
      and primed(r->ru_nivcsw) >= 0


/*$
 * requires: __stat_loc != NULL implies valid_ptr(__stat_loc);
 * requires: __usage != NULL implies valid_ptr(__usage);
 *
 * case "usage-stat_loc" {
 *   assumes: __usage != NULL;
 *   assumes: __stat_loc != NULL;
 *   assigns: *__usage;
 *   assigns: *__stat_loc;
 *   ensures: valid_primed_rusage(__usage);
 *   ensures: return >= 0;
 * }
 *
 * case "usage" {
 *   assumes: __usage != NULL;
 *   assumes: __stat_loc == NULL;
 *   assigns: *__usage;
 *   ensures: valid_primed_rusage(__usage);
 *   ensures: return >= 0;
 * }
 *
 * case "stat_loc" {
 *   assumes: __usage == NULL;
 *   assumes: __stat_loc != NULL;
 *   assigns: *__stat_loc;
 *   ensures: return >= 0;
 * }
 *
 * case "none" {
 *   assumes: __usage == NULL;
 *   assumes: __stat_loc == NULL;
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t wait3 (int *__stat_loc, int __options,
               struct rusage * __usage);

/*$
 * local: __pid_t r = wait3(__stat_loc, __options, __usage);
 * ensures: return == r;
 */
__pid_t wait4 (__pid_t __pid, int *__stat_loc, int __options,
               struct rusage *__usage);
