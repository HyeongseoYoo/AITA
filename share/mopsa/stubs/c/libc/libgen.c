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
#include <libgen.h>
#include <string.h> // for strlen
#include <limits.h> // for PATH_MAX
#include "mopsa_libc_utils.h"

static char _mopsa_dir_buf[PATH_MAX];

/*$
 * requires: valid_string(__path);
 * assigns: _mopsa_dir_buf;
 * local: size_t len = strlen(__path);
 * assigns: __path[0,len);
 * ensures: valid_primed_substring(_mopsa_dir_buf, PATH_MAX);
 * ensures: valid_primed_substring(__path, len);
 * ensures: return == _mopsa_dir_buf;
 */
char *dirname (char *__path);

/*$
 * requires: valid_string(__filename);
 * assigns: _mopsa_dir_buf;
 * local: size_t len = strlen(__filename);
 * assigns: __filename[0,len);
 * ensures: valid_primed_substring(_mopsa_dir_buf, PATH_MAX);
 * ensures: valid_primed_substring(__filename, len);
 * ensures: return == _mopsa_dir_buf;
 */
char *basename (char *__filename);
