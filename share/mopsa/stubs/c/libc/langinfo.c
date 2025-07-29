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
#include <langinfo.h>

/*$
 * local:   char *addr = _mopsa_new_readonly_string();
 * ensures: return == addr;
 */
char *nl_langinfo (nl_item __item);

/*$
 * // FIXME: nl_langinfo_l is undefined if __l is LC_GLOBAL_LOCALE or 
 * //        is not a valid locale object handle.
 * local:   char *addr = _mopsa_new_readonly_string();
 * ensures: return == addr;
 */
char *nl_langinfo_l (nl_item __item, locale_t __l);
