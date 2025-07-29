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

  We have no multi-thread support in MOPSA.

  Nevertheless, we currenty implement pthread_once to ensure that the
  init_routine function is correctly called in sequential analyses.
*/
#include <stddef.h>
#include <pthread.h>
#include "mopsa_libc_utils.h"

/*
 * Ensure that init_rountine is called.
 */
int pthread_once(pthread_once_t *once_control, void (*init_routine) (void))
{
  if (!(*once_control))  {
    (*init_routine)();
    *once_control = 1;
  }
  return 0;
}
