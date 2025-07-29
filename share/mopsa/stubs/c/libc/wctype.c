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
#include <wctype.h>
#include "mopsa_libc_utils.h"

/*$ // empty
 */
int iswalnum (wint_t __wc);

/*$ // empty
 */
int iswalpha (wint_t __wc);

/*$ // empty
 */
int iswcntrl (wint_t __wc);

/*$ // empty
 */
int iswdigit (wint_t __wc);

/*$ // empty
 */
int iswgraph (wint_t __wc);

/*$ // empty
 */
int iswlower (wint_t __wc);

/*$ // empty
 */
int iswprint (wint_t __wc);

/*$ // empty
 */
int iswpunct (wint_t __wc);

/*$ // empty
 */
int iswspace (wint_t __wc);

/*$ // empty
 */
int iswupper (wint_t __wc);

/*$ // empty
 */
int iswxdigit (wint_t __wc);

/*$ // empty
 */
int iswblank (wint_t __wc);

/*$
 * requires: valid_string(__property);
 */
wctype_t wctype (const char *__property);

/*$ // empty
 */
int iswctype (wint_t __wc, wctype_t __desc);

/*$ // empty
 */
wint_t towlower (wint_t __wc);

/*$ // empty
 */
wint_t towupper (wint_t __wc);

/*$
 * requires: valid_string(__property);
 */
wctrans_t wctrans (const char *__property);

/*$ // empty
 */
wint_t towctrans (wint_t __wc, wctrans_t __desc);


/*$ // empty
 */
int iswalnum_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswalpha_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswcntrl_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswdigit_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswgraph_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswlower_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswprint_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswpunct_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswspace_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswupper_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswxdigit_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
int iswblank_l (wint_t __wc, locale_t __locale);

/*$
 * requires: valid_string(__property);
 */
wctype_t wctype_l (const char *__property, locale_t __locale);

/*$ // empty
 */
int iswctype_l (wint_t __wc, wctype_t __desc, locale_t __locale);

/*$ // empty
 */
wint_t towlower_l (wint_t __wc, locale_t __locale);

/*$ // empty
 */
wint_t towupper_l (wint_t __wc, locale_t __locale);

/*$
 * requires: valid_string(__property);
 */
wctrans_t wctrans_l (const char *__property, locale_t __locale);

/*$ // empty
 */
wint_t towctrans_l (wint_t __wc, wctrans_t __desc, locale_t __locale);
