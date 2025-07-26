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
#include <mntent.h>
#include <stdio.h>
#include "mopsa_libc_utils.h"

/*$
 * local: FILE* r = fopen(__file, __mode);
 * ensures: return == r;
 */
FILE *setmntent (const char *__file, const char *__mode);

static struct mntent _mopsa_mntent_buf;

/*$
 * assigns: _mopsa_mntent_buf;
 * local: char* fsname = _mopsa_new_valid_string();
 * local: char* dir = _mopsa_new_valid_string();
 * local: char* type = _mopsa_new_valid_string();
 * local: char* opts = _mopsa_new_valid_string();
 * ensures: (_mopsa_mntent_buf.mnt_fsname)' == fsname;
 * ensures: (_mopsa_mntent_buf.mnt_dir)' == dir;
 * ensures: (_mopsa_mntent_buf.mnt_type)' == type;
 * ensures: (_mopsa_mntent_buf.mnt_opts)' == opts;
 * ensures: return == &_mopsa_mntent_buf;
 */
static struct mntent* _mopsa_mntent();

/*$
 * requires: alive_resource(__stream, File);
 * local: struct mntent* r = _mopsa_mntent();
 * ensures: return == NULL or return == r;
 */
struct mntent *getmntent (FILE *__stream);

// unsupported GNU extension
struct mntent *getmntent_r (FILE *__restrict __stream,
                            struct mntent *__restrict __result,
                            char *__restrict __buffer,
                            int __bufsize);

/*$
 * requires: alive_resource(__stream, File);
 * requires: valid_ptr(__mnt);
 * requires: valid_string(__mnt->mnt_fsname);
 * requires: valid_string(__mnt->mnt_dir);
 * requires: valid_string(__mnt->mnt_type);
 * requires: valid_string(__mnt->mnt_opts);
 * ensures: return in [0,1];
 */
int addmntent (FILE *__restrict __stream,
               const struct mntent *__restrict __mnt);

/*$
 * requires: alive_resource(__stream, File);
 * local: int r = fclose(__stream);
 * ensures: return == 1;
 */
int endmntent (FILE *__stream);

/*$
 * requires: valid_ptr(__mnt);
 * requires: valid_string(__mnt->mnt_opts);
 * requires: valid_string(__opt);
 * ensures: return == NULL or in_string(return, __mnt->mnt_opts);
 */
char *hasmntopt (const struct mntent *__mnt,
                 const char *__opt);
