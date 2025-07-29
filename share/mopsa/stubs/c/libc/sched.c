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
#include <sched.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * requires: valid_ptr(__param);
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
int sched_setparam (__pid_t __pid, const struct sched_param *__param);

/*$
 * requires: valid_ptr(__param);
 * assigns: *__param;
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
int sched_getparam (__pid_t __pid, struct sched_param *__param);

/*$
 * requires: valid_ptr(__param);
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
int sched_setscheduler (__pid_t __pid, int __policy,
                        const struct sched_param *__param);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sched_getscheduler (__pid_t __pid);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sched_yield (void);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sched_get_priority_max (int __algorithm);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sched_get_priority_min (int __algorithm);

/*$
 * requires: valid_ptr(__t);
 * assigns: *__t;
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
int sched_rr_get_interval (__pid_t __pid, struct timespec *__t);

/*$
 * requires: valid_bytes(__cpuset, __cpusetsize);
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
int sched_setaffinity (__pid_t __pid, size_t __cpusetsize,
                       const cpu_set_t *__cpuset);

/*$
 * requires: valid_bytes(__cpuset, __cpusetsize);
 * assigns: ((unsigned char*)__cpuset)[0, __cpusetsize);
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
int sched_getaffinity (__pid_t __pid, size_t __cpusetsize,
                       cpu_set_t *__cpuset);


// from cpu-set.h

static size_t _mopsa_cpu_alloc_size(size_t __setsize) {
  return __CPU_ALLOC_SIZE(__setsize);
}

/*$
 * local: size_t sz = _mopsa_cpu_alloc_size(__setsize);
 * requires: valid_bytes(__setp, sz);
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
int __sched_cpucount (size_t __setsize, const cpu_set_t *__setp);

/*$
 * local: size_t sz = _mopsa_cpu_alloc_size(__count);
 * local: cpu_set_t* r = new Memory;
 * ensures: size(r) == sz;
 * ensures: return == NULL or return == r;
 */
cpu_set_t *__sched_cpualloc (size_t __count);

/*$
 * requires: __set in Memory;
 * free: __set;
 */
void __sched_cpufree (cpu_set_t *__set);
