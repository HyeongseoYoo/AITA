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
  based on header from glibc-2.27-r6
*/
#include <stddef.h>
#include <sys/sysinfo.h>
#include <errno.h>
#include "../mopsa_libc_utils.h"

// TODO: fill-in __info and return reasonable values

/*$
 * requires: valid_ptr(__info);
 * assigns: *__info;
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
int sysinfo (struct sysinfo *__info);

/*$
 * ensures: return >= 0;
 */
int get_nprocs_conf (void);

/*$
 * ensures: return >= 0;
 */
int get_nprocs (void);

/*$
 * ensures: return >= 0;
 */
long int get_phys_pages (void);

/*$
 * ensures: return >= 0;
 */
long int get_avphys_pages (void);
