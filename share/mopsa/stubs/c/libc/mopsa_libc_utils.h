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
  Useful definitions used throughout the libc stubs.
 */

#ifndef MOPSA_LIBC_UTILS_H
#define MOPSA_LIBC_UTILS_H

extern int _errno;

// Register a new file resource pointer and return the smallest available
// file descriptor
extern int _mopsa_register_file_resource(void *f);


// Register a new file resource pointer at a specific file descriptor
extern int _mopsa_register_file_resource_at(void *f, int fd);


// Find a file resource by its numeric descriptor
extern void *_mopsa_find_file_resource(int fd);


#endif /* MOPSA_LIBC_UTILS_H */
