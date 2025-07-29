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
#include <ctype.h>
#include <stdio.h> // for EOF
#include "mopsa_libc_utils.h"

/*$!
 * assigns: _ctype_b_buf[0,383];
 */
static unsigned short int  _ctype_b_buf[384]; // 256+128 locations
static unsigned short int* _ctype_b_pbuf = &(_ctype_b_buf[128]);

/*$!
 * assigns: _ctype_tolower_buf[0,383];
 */
static __int32_t  _ctype_tolower_buf[384];
static __int32_t* _ctype_tolower_pbuf = &(_ctype_tolower_buf[128]);

/*$!
 * assigns: _ctype_toupper_buf[0,383];
 */
static __int32_t  _ctype_toupper_buf[384];
static __int32_t* _ctype_toupper_pbuf = &(_ctype_toupper_buf[128]);

/*$ 
 * ensures: return == &_ctype_b_pbuf;
 */
const unsigned short int **__ctype_b_loc (void);

/*$
 * ensures: return == &_ctype_tolower_pbuf;
 */
const __int32_t **__ctype_tolower_loc (void);
     
/*$
 * ensures: return == &_ctype_toupper_pbuf;
 */
const __int32_t **__ctype_toupper_loc (void);


#ifdef __NO_CTYPE

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isalnum (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isalpha (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int iscntrl (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isdigit (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int islower (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isgraph (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isprint (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int ispunct (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isspace (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isupper (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isxdigit (int __c); 

#endif

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0, 255];
 */
int tolower (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int toupper (int __c); 
  
#ifdef __NO_CTYPE

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isblank (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isascii (int __c); 

/*$
 * ensures: return in [0, 127];
 */
int toascii (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int _toupper (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int _tolower (int __c); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isalnum_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isalpha_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int iscntrl_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isdigit_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int islower_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isgraph_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isprint_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int ispunct_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isspace_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isupper_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isxdigit_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 */
int isblank_l (int __c, locale_t __loc); 

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int __tolower_l (int __c, locale_t __l); 

#endif

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int tolower_l (int __c, locale_t __l); 

#ifdef __NO_CTYPE

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int __toupper_l (int __c, locale_t __l); 

#endif

/*$
 * requires: __c == EOF or __c in [0,255];
 * ensures: return == EOF or return in [0,255];
 */
int toupper_l (int __c, locale_t __l); 

