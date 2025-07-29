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
#include <errno.h>
#include "mopsa_libc_utils.h"

int _errno;

/*$
 * ensures: return == &_errno;
 */
int *__errno_location (void);


/*$!
 * local: char* addr = _mopsa_new_readonly_string();
 * assigns: program_invocation_name;
 * ensures: program_invocation_name' == addr;
 */
char *program_invocation_name;

/*$!
 * local: char* addr = _mopsa_new_readonly_string();
 * assigns: program_invocation_short_name;
 * ensures: program_invocation_short_name' == addr;
 */
char *program_invocation_short_name;

