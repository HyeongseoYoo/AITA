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
 * Stubs for sys/utsname.h
 */
#include <stddef.h>
#include <sys/utsname.h>

/*$
 * requires: __name != NULL;
 * assigns: __name;
 * ensures: valid_primed_substring(__name->sysname, _UTSNAME_SYSNAME_LENGTH);
 * ensures: valid_primed_substring(__name->nodename, _UTSNAME_NODENAME_LENGTH);
 * ensures: valid_primed_substring(__name->release, _UTSNAME_RELEASE_LENGTH);
 * ensures: valid_primed_substring(__name->version, _UTSNAME_VERSION_LENGTH);
 * ensures: valid_primed_substring(__name->machine, _UTSNAME_MACHINE_LENGTH);
 */
int uname(struct utsname *__name);
