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
#include <stdarg.h>
#include <wchar.h>
#include <limits.h>
#include <stdio.h> // for _alloc_FILE
#include <string.h>
#include <errno.h>
#include <time.h> // for struct tm needed by wcsftime
#include "mopsa_libc_utils.h"

static wint_t _weof = ((wint_t)-1);


#define no_overlap_wide(s1, n1, s2, n2)              \
    n1 <=0 or n2 <= 0 or                             \
    base(s1) != base(s2) or                          \
    (unsigned char*)s1 >= (unsigned char*)s2 + n2 or \
    (unsigned char*)s2 >= (unsigned char*)s1 + n1


/*$
 * local: size_t src_len = wcslen(__src);
 * requires: no_overlap_wide(__src, src_len + 1,__dest, src_len + 1);
 * requires: valid_wchars(__dest, src_len + 1);
 * assigns: __dest[0, src_len];
 * ensures: forall size_t i in [0, src_len]: (__dest[i])' == __src[i];
 * ensures: return == __dest;
 */
wchar_t *wcscpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src);

/*$
 * requires: valid_wchars(__src, __n) or valid_wide_string(__src);
 * requires: valid_wchars(__dest, __n);
 * local: size_t src_nlen = wcsnlen(__src, __n);
 * requires: no_overlap_wide(__src, src_nlen, __dest, src_nlen);
 * assigns: __dest[0, __n);
 * ensures: forall size_t i in [0, src_nlen): (__dest[i])' == __src[i];
 * ensures: forall size_t i in [src_nlen, __n): (__dest[i])' == 0;
 * ensures: return == __dest;
 */
wchar_t *wcsncpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src, size_t __n);

/*$
 * local: size_t src_len = wcslen(__src);
 * local: size_t dest_len = wcslen(__dest);
 * requires: no_overlap_wide(__src, src_len + 1, __dest, dest_len + src_len + 1);
 * requires: valid_wchars(__dest, dest_len + src_len);
 * assigns: __dest[0, dest_len + src_len];
 * ensures: forall size_t i in [0, dest_len): (__dest[i])' == __dest[i];
 * ensures: forall size_t i in [0, src_len]: (__dest[dest_len + i])' == __src[i];
 * ensures: return == __dest;
 */
wchar_t *wcscat (wchar_t *__restrict __dest, const wchar_t *__restrict __src);

/*$
 * local: size_t src_nlen = wcsnlen(__src, __n);
 * local: size_t dest_len = wcslen(__dest);
 * requires: no_overlap_wide(__src, src_nlen + 1, __dest, dest_len + src_nlen + 1);
 * requires: valid_wchars(__dest, dest_len + src_nlen + 1);
 * assigns: __dest[0, dest_len + src_nlen];
 * ensures: forall size_t i in [0, dest_len): (__dest[i])' == __dest[i];
 * ensures: forall size_t i in [0, src_nlen): (__dest[dest_len + i])' == __src[i];
 * ensures: (__dest[dest_len + src_nlen])' == 0;
 * ensures: return == __dest;
 */
wchar_t *wcsncat (wchar_t *__restrict __dest, const wchar_t *__restrict __src, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 *
 */
int wcscmp (const wchar_t *__s1, const wchar_t *__s2);

/*$
 * requires: valid_ptr(__s1);
 * requires: valid_ptr(__s2);
 * requires: valid_wchars(__s1, __n) or valid_wide_string(__s1);
 * requires: valid_wchars(__s2, __n) or valid_wide_string(__s2);
 */
int wcsncmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 *
 */
int wcscasecmp (const wchar_t *__s1, const wchar_t *__s2);

/*$
 * requires: valid_wchars(__s1, __n) or valid_wide_string(__s1);
 * requires: valid_wchars(__s2, __n) or valid_wide_string(__s2);
 */
int wcsncasecmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 *
 */
int wcscasecmp_l (const wchar_t *__s1, const wchar_t *__s2, locale_t __loc);

/*$
 * requires: valid_wchars(__s1, __n) or valid_wide_string(__s1);
 * requires: valid_wchars(__s2, __n) or valid_wide_string(__s2);
 */
int wcsncasecmp_l (const wchar_t *__s1, const wchar_t *__s2, size_t __n, locale_t __loc);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 * assigns:  _errno;
 */
int wcscoll (const wchar_t *__s1, const wchar_t *__s2);

/*$
 * requires: valid_wide_string(__s2);
 * requires: valid_wchars(__s1, __n);
 * requires: no_overlap_wide(__s1, __n, __s2, __n);
 * assigns: __s1[0, __n);
 * ensures: return >= 0 and (return < __n implies (__s1[return])' == 0);
 */
size_t wcsxfrm (wchar_t *__restrict __s1, const wchar_t *__restrict __s2, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 * assigns:  _errno;
 */
int wcscoll_l (const wchar_t *__s1, const wchar_t *__s2, locale_t __loc);

/*$
 * requires: valid_wide_string(__s2);
 * requires: valid_wchars(__s1, __n);
 * requires: no_overlap_wide(__s1, __n, __s2, __n);
 * assigns: __s1[0, __n);
 * ensures: return >= 0 and (return < __n implies (__s1[return])' == 0);
 */
size_t wcsxfrm_l (wchar_t *__s1, const wchar_t *__s2, size_t __n, locale_t __loc);

/*$
 * local: size_t len = wcslen(__s);
 *
 * case "success" {
 *   local: wchar_t* r = new Memory;
 *   ensures: return == r;
 *   ensures: size(return) == len + 1;
 *   ensures: forall size_t i in [0, len]: r[i] == __s[i];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
wchar_t *wcsdup (const wchar_t *__s);

/*$
 * local: size_t len = wcslen(__wcs);
 *
 * case "found" {
 *   ensures:  exists size_t i in [0, len): ( 
 *             __wcs[i] == __wc and 
 *             (forall size_t j in [0, i): __wcs[j] != __wc) and
 *             return == __wcs + i 
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, len): __wcs[i] != __wc;
 *   ensures: return == NULL;
 * }
 */
wchar_t *wcschr (const wchar_t *__wcs, wchar_t __wc);

/*$
 * local: size_t len = wcslen(__wcs);
 *
 * case "found" {
 *   ensures:  exists size_t i in [0, len): ( 
 *             __wcs[i] == __wc and 
 *             (forall size_t j in [i + 1, len): __wcs[j] != __wc) and
 *             return == __wcs + i 
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, len): __wcs[i] != __wc;
 *   ensures: return == NULL;
 * }
 */
wchar_t *wcsrchr (const wchar_t *__wcs, wchar_t __wc);

//#ifdef __USE_GNU

/*$
 * local: size_t len = wcslen(__wcs);
 *
 * case "found" {
 *   ensures:  exists size_t i in [0, len): ( 
 *             __wcs[i] == __wc and 
 *             (forall size_t j in [0, i): __wcs[j] != __wc) and
 *             return == __wcs + i 
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, len): __wcs[i] != __wc;
 *   ensures: return == __wcs + len;
 * }
 */
wchar_t *wcschrnul (const wchar_t *__wcs, wchar_t __wc);

//#endif

/*$
 * requires: valid_wide_string(__reject);
 * local: size_t len = wcslen(__wcs);
 * ensures: return in [0, len];
 */
size_t wcscspn (const wchar_t *__wcs, const wchar_t *__reject);

/*$
 * requires: valid_wide_string(__accept);
 * local: size_t len = wcslen(__wcs);
 * ensures: return in [0, len];
 */
size_t wcsspn (const wchar_t *__wcs, const wchar_t *__accept);

/*$
 * requires: valid_wide_string(__wcs);
 * requires: valid_wide_string(__accept);
 * ensures: return == NULL or in_wide_string(return, __wcs);
 */
wchar_t *wcspbrk (const wchar_t *__wcs, const wchar_t *__accept);

/*$
 * local: size_t len1 = wcslen(__haystack);
 * local: size_t len2 = wcslen(__needle);
 * ensures: return == NULL or 
 *          (len1 >= len2 and in_wchars(return, __haystack, len1 - len2));
 */
wchar_t *wcsstr (const wchar_t *__haystack, const wchar_t *__needle);


/*$
 * requires: valid_wide_string(__delim);
 * requires: valid_ptr(__ptr);
 *
 * case "first" {
 *   assumes: __s != NULL;
 *   local: size_t len = wcslen(__s);
 *   assigns: __s[0, len);
 *   assigns: *__ptr;
 *   ensures: exists size_t i in [0, len): return == __s + i;
 *   ensures: exists size_t i in [0, len): (*__ptr)' == __s + i;
 * }
 *
 * case "next" {
 *   assumes: __s == NULL;
 *   assumes: (*__ptr) != NULL;
 *   local: size_t len = wcslen(*__ptr);
 *   assigns: (*__ptr)[0, len);
 *   assigns: *__ptr;
 *   ensures: return == NULL or exists size_t i in [0, len]: return == (*__ptr) + i;
 *   ensures: (*__ptr)' == NULL or exists size_t i in [0, len]: (*__ptr)' == (*__ptr) + i;
 *  }
 *
 * case "end" {
 *   assumes: __s == NULL;
 *   assumes: (*__ptr) == NULL;
 *   ensures: return == NULL;
 *  }
 */
wchar_t *wcstok (wchar_t *__restrict __s,
                 const wchar_t *__restrict __delim,
                 wchar_t **__restrict __ptr);

/*$
 * requires: valid_wide_string(__s);
 * ensures:  exists size_t i in [0, ((bytes(__s) - offset(__s)) / sizeof_type(wchar_t))): ( 
 *             __s[i] == 0 and 
 *             (forall size_t j in [0, i): __s[j] != 0) and
 *             return == i 
 *          );
 */
size_t wcslen (const wchar_t *__s);

/*$
 * #alias wcsstr;
 */
wchar_t *wcswcs (const wchar_t *__haystack, const wchar_t *__needle);

/*$
 * requires: valid_wchars(__s, __maxlen) or valid_wide_string(__s);
 *
 * case "string-smaller-than-maxlen" {
 *   assumes: valid_wide_substring(__s, __maxlen);
 *   local: size_t len = wcslen(__s);
 *   ensures: return == len;
 * }
 *
 * case "string-bigger-than-maxlen" {
 *   assumes: forall size_t i in [0, __maxlen): __s[i] != 0;
 *   ensures: return == __maxlen;
 * }
 */
size_t wcsnlen (const wchar_t *__s, size_t __maxlen);

/*$
 * requires: valid_wchars(__s, __n);
 *
 * case "found" {
 *   assumes: exists size_t i in [0, __n): __s[i] == __c;
 *   ensures: exists size_t i in [0, __n): (
 *              __s[i] == __c and
 *              (forall size_t j in [0, i): __s[i] != __c) and
 *              return == __s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, __n): ((unsigned char*)__s)[i] != __c;
 *   ensures: return == NULL;
 * }
 */
wchar_t *wmemchr (const wchar_t *__s, wchar_t __c, size_t __n);

/*$
 * requires: valid_wchars(__s1, __n);
 * requires: valid_wchars(__s2, __n);
 *
 * case "equal" {
 *   assumes: forall size_t i in [0, __n): __s1[i] == __s2[i];
 *   ensures: return == 0;
 * }
 *
 * case "notequal" {
 *   assumes: exists size_t i in [0, __n): __s1[i] != __s2[i];
 *   ensures: exists size_t i in [0, __n): (
 *             __s1[i] != __s2[i] and
 *             (forall size_t j in [0, i): __s1[j] == __s2[j]) and
 *             (__s1[i] > __s2[i] implies return > 0) and
 *             (__s1[i] < __s2[i] implies return < 0)
 *            );
 * }
 */
int wmemcmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n);

/*$
 * requires: no_overlap_wide(__s1, __n, __s2, __n);
 * requires: valid_wchars(__s1, __n);
 * requires: valid_wchars(__s2, __n);
 * assigns: __s1[0, __n);
 * ensures: forall size_t i in [0, __n): (__s1[i])' == __s2[i];
 * ensures: return == __s1;
 */
wchar_t *wmemcpy (wchar_t *__restrict __s1, const wchar_t *__restrict __s2, size_t __n);

/*$
 * requires: valid_wchars(__s1, __n);
 * requires: valid_wchars(__s2, __n);
 * assigns: __s1[0, __n);
 * ensures: forall size_t i in [0, __n): (__s1[i])' == __s2[i];
 * ensures: return == __s1;
 */
wchar_t *wmemmove (wchar_t *__s1, const wchar_t *__s2, size_t __n);

/*$
 * requires: valid_wchars(__s, __n);
 * assigns: __s[0, __n);
 * ensures: forall size_t i in [0, __n): (__s[i])' == __c;
 * ensures: return == __s;
 */
wchar_t *wmemset (wchar_t *__s, wchar_t __c, size_t __n);

/*$
 * // empty
 */
wint_t btowc (int __c);

/*$
 * // empty
 */
int wctob (wint_t __c);

/*$
 * requires: null_or_valid_ptr(__ps);
 */
int mbsinit (const mbstate_t *__ps);

/*$
 * requires: null_or_valid_bytes(__s, __n);
 * requires: __s != NULL implies null_or_valid_ptr(__pwc);
 * requires: null_or_valid_ptr(__p);
 * assigns: _errno;
 * ensures: return in [0, __n] or return == -1 or return == -2;
 *
 * case "s-pwc-p" {
 *   assumes: __s != NULL;
 *   assumes: __pwc != NULL;
 *   assumes: __p != NULL;
 *   assigns: *__pwc;
 *   assigns: *__p;
 * }
 *
 * case "s-p" {
 *   assumes: __s != NULL;
 *   assumes: __pwc == NULL;
 *   assumes: __p != NULL;
 *   assigns: *__p;
 * }
 *
 * case "p" {
 *   assumes: __s == NULL;
 *   assumes: __p != NULL;
 *   assigns: *__p;
 * }
 *
 * case "s-pwc" {
 *   assumes: __s != NULL;
 *   assumes: __pwc != NULL;
 *   assumes: __p == NULL;
 *   assigns: *__pwc;
 * }
 *
 * case "s" {
 *   assumes: __s != NULL;
 *   assumes: __pwc == NULL;
 *   assumes: __p == NULL;
 * }
 *
 * case "null" {
 *   assumes: __s == NULL;
 *   assumes: __p == NULL;
 * }
 *
 */
size_t mbrtowc (wchar_t *__restrict __pwc,
                const char *__restrict __s, size_t __n,
                mbstate_t *__restrict __p) ;

/*$
 * requires: null_or_valid_bytes(__s, MB_LEN_MAX);
 * requires: null_or_valid_ptr(__ps);
 * assigns: _errno;
 * ensures: return in [0, MB_LEN_MAX] or return == -1;
 *
 * case "s-ps" {
 *   assumes: __s != NULL;
 *   assumes: __ps != NULL;
 *   assigns: __s[0, MB_LEN_MAX);
 *   assigns: *__ps;
 * }
 *
 * case "ps" {
 *   assumes: __s == NULL;
 *   assumes: __ps != NULL;
 *   assigns: *__ps;
 * }
 *
 * case "s" {
 *   assumes: __s != NULL;
 *   assumes: __ps == NULL;
 *   assigns: __s[0, MB_LEN_MAX);
 * }
 *
 * case "null" {
 *   assumes: __s == NULL;
 *   assumes: __ps == NULL;
 * }
 *
 */
size_t wcrtomb (char *__restrict __s, wchar_t __wc, mbstate_t *__restrict __ps);

/*$
 * requires: valid_bytes(__s, __n);
 * requires: null_or_valid_ptr(__ps);
 * assigns: _errno;
 * ensures: return in [0, __n] or return == -1 or return == -2;
 *
 * case "ps" {
 *   assumes: __ps != NULL;
 *   assigns: *__ps;
 * }
 *
 * case "null" {
 *   assumes: __ps == NULL;
 * }
 */
size_t mbrlen (const char *__restrict __s, size_t __n, mbstate_t *__restrict __ps);

/*$
 * #alias mbrlen;
 */
size_t __mbrlen (const char *__restrict __s, size_t __n, mbstate_t *__restrict __ps);


/*$
 * requires: null_or_valid_wchars(__dst, __len);
 * requires: null_or_valid_ptr(__ps);
 * local: size_t src_len = strlen(*__src);
 * assigns: _errno;
 * assigns: *__src;
 * ensures: (*__src)' == NULL or exists size_t i in [0, src_len]: (*__src)' == (*__src) + i;
 * ensures: return in [0, __len] or return == -1;
 *
 * case "dst-ps" {
 *    assumes: __dst != NULL;
 *    assumes: __ps != NULL;
 *    assigns: __dst[0, __len);
 *    assigns: *__ps;
 * }
 *
 * case "ps" {
 *    assumes: __dst == NULL;
 *    assumes: __ps != NULL;
 *    assigns: *__ps;
 * }
 *
 * case "dst" {
 *    assumes: __dst != NULL;
 *    assumes: __ps == NULL;
 *    assigns: __dst[0, __len);
 * }
 *
 * case "null" {
 *    assumes: __dst == NULL;
 *    assumes: __ps == NULL;
 * }
 */
size_t mbsrtowcs (wchar_t *__restrict __dst,
                  const char **__restrict __src, size_t __len,
                  mbstate_t *__restrict __ps);

/*$
 * requires: null_or_valid_bytes(__dst, __len);
 * requires: null_or_valid_ptr(__ps);
 * local: size_t src_len = wcslen(*__src);
 * assigns: _errno;
 * assigns: *__src;
 * ensures: (*__src)' == NULL or exists size_t i in [0, src_len]: (*__src)' == (*__src) + i;
 * ensures: return in [0, __len] or return == -1;
 *
 * case "dst-ps" {
 *    assumes: __dst != NULL;
 *    assumes: __ps != NULL;
 *    assigns: __dst[0, __len);
 *    assigns: *__ps;
 * }
 *
 * case "ps" {
 *    assumes: __dst == NULL;
 *    assumes: __ps != NULL;
 *    assigns: *__ps;
 * }
 *
 * case "dst" {
 *    assumes: __dst != NULL;
 *    assumes: __ps == NULL;
 *    assigns: __dst[0, __len);
 * }
 *
 * case "null" {
 *    assumes: __dst == NULL;
 *    assumes: __ps == NULL;
 * }
 */
size_t wcsrtombs (char *__restrict __dst,
                  const wchar_t **__restrict __src, size_t __len,
                  mbstate_t *__restrict __ps);

/*$
 * requires: valid_bytes(*__src,__nmc) or valid_string(*__src);
 * requires: null_or_valid_wchars(__dst, __len);
 * requires: null_or_valid_ptr(__ps);
 * local: size_t src_len = strnlen(*__src, __nmc);
 * assigns: _errno;
 * assigns: *__src;
 * ensures: (*__src)' == NULL or exists size_t i in [0, src_len]: (*__src)' == (*__src) + i;
 * ensures: return in [0, __len] or return == -1;
 *
 * case "dst-ps" {
 *    assumes: __dst != NULL;
 *    assumes: __ps != NULL;
 *    assigns: __dst[0, __len);
 *    assigns: *__ps;
 * }
 *
 * case "ps" {
 *    assumes: __dst == NULL;
 *    assumes: __ps != NULL;
 *    assigns: *__ps;
 * }
 *
 * case "dst" {
 *    assumes: __dst != NULL;
 *    assumes: __ps == NULL;
 *    assigns: __dst[0, __len);
 * }
 *
 * case "null" {
 *    assumes: __dst == NULL;
 *    assumes: __ps == NULL;
 * }
 */
size_t mbsnrtowcs (wchar_t *__restrict __dst,
                   const char **__restrict __src, size_t __nmc,
                   size_t __len, mbstate_t *__restrict __ps);

/*$
 * requires:  valid_wchars(*__src,__nwc) or valid_wide_string(*__src);
 * requires: null_or_valid_bytes(__dst, __len);
 * requires: null_or_valid_ptr(__ps);
 * local: size_t src_len = wcsnlen(*__src, __nwc);
 * assigns: _errno;
 * assigns: *__src;
 * ensures: (*__src)' == NULL or exists size_t i in [0, src_len]: (*__src)' == (*__src) + i;
 * ensures: return in [0, __len] or return == -1;
 *
 * case "dst-ps" {
 *    assumes: __dst != NULL;
 *    assumes: __ps != NULL;
 *    assigns: __dst[0, __len);
 *    assigns: *__ps;
 * }
 *
 * case "ps" {
 *    assumes: __dst == NULL;
 *    assumes: __ps != NULL;
 *    assigns: *__ps;
 * }
 *
 * case "dst" {
 *    assumes: __dst != NULL;
 *    assumes: __ps == NULL;
 *    assigns: __dst[0, __len);
 * }
 *
 * case "null" {
 *    assumes: __dst == NULL;
 *    assumes: __ps == NULL;
 * }
 */
size_t wcsnrtombs (char *__restrict __dst,
                   const wchar_t **__restrict __src,
                   size_t __nwc, size_t __len,
                   mbstate_t *__restrict __ps);

/*$
 * ensures: return >= -1;
 */
int wcwidth (wchar_t __c);


/*$
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
double wcstod (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr);

/*$
 * requires: valid_wide_substring(__s, __n) or valid_wchars(__s, __n);
 * ensures: return >= -1;
 */
int wcswidth (const wchar_t *__s, size_t __n);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
float wcstof (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
long double wcstold (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * requires: __base == 0 or __base in [2, 36];
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
long int wcstol (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr, int __base);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * requires: __base == 0 or __base in [2, 36];
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
unsigned long int wcstoul (const wchar_t *__restrict __nptr,
                           wchar_t **__restrict __endptr, int __base);
     
/*$
 * requires: null_or_valid_ptr(__endptr);
 * requires: __base == 0 or __base in [2, 36];
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
long long int wcstoll (const wchar_t *__restrict __nptr,
                       wchar_t **__restrict __endptr, int __base);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * requires: __base == 0 or __base in [2, 36];
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_wide_string(__nptr);
 *  }
 */
unsigned long long int wcstoull (const wchar_t *__restrict __nptr,
                                 wchar_t **__restrict __endptr,
                                 int __base);

/*$
 * #alias wcstoll;
 */
long long int wcstoq (const wchar_t *__restrict __nptr,
                      wchar_t **__restrict __endptr, int __base);

/*$
 * #alias wcstoull;
 */
unsigned long long int wcstouq (const wchar_t *__restrict __nptr,
                                wchar_t **__restrict __endptr,
                                int __base);

/*$
 * local: long r = wcstol(__nptr, __endptr, __base);
 * ensures: return == r;
 */
long int wcstol_l (const wchar_t *__restrict __nptr,
                   wchar_t **__restrict __endptr, int __base,
                   locale_t __loc);

/*$
 * local: unsigned long int r = wcstoul(__nptr, __endptr, __base);
 * ensures: return == r;
 */
unsigned long int wcstoul_l (const wchar_t *__restrict __nptr,
                             wchar_t **__restrict __endptr,
                             int __base, locale_t __loc);

/*$
 * local: long long int r = wcstoll(__nptr, __endptr, __base);
 * ensures: return == r;
 */
long long int wcstoll_l (const wchar_t *__restrict __nptr,
                         wchar_t **__restrict __endptr,
                         int __base, locale_t __loc);

/*$
 * local: unsigned long long int r = wcstoull(__nptr, __endptr, __base);
 * ensures: return == r;
 */
unsigned long long int wcstoull_l (const wchar_t *__restrict __nptr,
                                   wchar_t **__restrict __endptr,
                                   int __base, locale_t __loc);

/*$
 * local: double r = wcstod(__nptr, __endptr);
 * ensures: return == r;
 */
double wcstod_l (const wchar_t *__restrict __nptr,
                 wchar_t **__restrict __endptr, locale_t __loc);

/*$
 * local: float r = wcstof(__nptr, __endptr);
 * ensures: return == r;
 */
float wcstof_l (const wchar_t *__restrict __nptr,
                wchar_t **__restrict __endptr, locale_t __loc);

/*$
 * local: long double r = wcstold(__nptr, __endptr);
 * ensures: return == r;
 */
long double wcstold_l (const wchar_t *__restrict __nptr,
                       wchar_t **__restrict __endptr,
                       locale_t __loc);

/*$
 * local: size_t src_len = wcslen(__src);
 * requires: no_overlap_wide(__src, src_len + 1, __dest, src_len + 1);
 * requires: valid_wchars(__dest, src_len + 1);
 * assigns: __dest[0, src_len];
 * ensures: forall size_t i in [0, src_len]: (__dest[i])' == __src[i];
 * ensures: return == __dest + src_len;
 */
wchar_t *wcpcpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src);

/*$
 * requires: valid_wchars(__src, __n) or valid_wide_string(__src);
 * requires: valid_wchars(__dest, __n);
 * local: size_t src_nlen = wcsnlen(__src, __n);
 * requires: no_overlap_wide(__src, src_nlen, __dest, src_nlen);
 * assigns: __dest[0, __n);
 * ensures: forall size_t i in [0, src_nlen): (__dest[i])' == __src[i];
 * ensures: forall size_t i in [src_nlen, __n): (__dest[i])' == 0;
 * ensures: return == __dest + __n - 1;
 */
wchar_t *wcpncpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src, size_t __n);

/*$
 * // TODO: *__bufloc and __sizeloc become volatile
 *
 * case "success" {
 *   local: FILE* r = _alloc_FILE();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *open_wmemstream (wchar_t **__bufloc, size_t *__sizeloc);

/*$
 * requires: alive_resource(__fp, File);
 */
int fwide (FILE *__fp, int __mode);

/* built-in */
int fwprintf (FILE *__restrict __stream, const wchar_t *__restrict __format, ...);

/* built-in */
int wprintf (const wchar_t *__restrict __format, ...);

/* built-in */
int swprintf (wchar_t *__restrict __s, size_t __n, const wchar_t *__restrict __format, ...);

/*$
 * requires: alive_resource(__stream, File);
 * assigns: _errno;
 */
wint_t fgetwc (FILE *__stream);

/*$
 * #alias fgetwc;
 */
wint_t getwc (FILE *__stream);

/*$
 * assigns: _errno;
 */
wint_t getwchar (void);


/*$
 * requires: alive_resource(__stream, File);
 * assigns: _errno;
 * ensures: (return == __wc) or (return == _weof);
 */
wint_t fputwc (wchar_t __wc, FILE *__stream);

/*$
 * #alias fputwc;
 */
wint_t putwc (wchar_t __wc, FILE *__stream);

/*$
 * assigns: _errno;
 * ensures: (return == __wc) or (return == _weof);
 */
wint_t putwchar (wchar_t __wc);

/*$
 * requires: alive_resource(__stream, File);
 * requires: __n >= 0;
 * requires: valid_wchars(__ws, __n);
 * assigns:  __ws[0, __n);
 * assigns: _errno;
 * ensures:  valid_primed_wide_substring(__ws, __n);
 * ensures:  (return == __ws) or (return == NULL);
 */
wchar_t *fgetws (wchar_t *__restrict __ws, int __n, FILE *__restrict __stream);

/*$
 * requires: valid_wide_string(__ws);
 * requires: alive_resource(__stream, File);
 * assigns: _errno;
 * ensures: return >= -1;
 */
int fputws (const wchar_t *__restrict __ws, FILE *__restrict __stream);

/*$
 * requires: alive_resource(__stream, File);
 * assigns: _errno;
 * ensures: (return == __wc) or (return == _weof);
 */
wint_t ungetwc (wint_t __wc, FILE *__stream);

/*$
 * #alias getwc;
 */
wint_t getwc_unlocked (__FILE *__stream);

/*$
 * #alias getwchar;
 */
wint_t getwchar_unlocked (void);

/*$
 * #alias fgetwc;
 */
wint_t fgetwc_unlocked (__FILE *__stream);

/*$
 * #alias fputwc;
 */
wint_t fputwc_unlocked (wchar_t __wc, __FILE *__stream);

/*$
 * #alias putwc;
 */
wint_t putwc_unlocked (wchar_t __wc, __FILE *__stream);

/*$
 * #alias putwchar;
 */
wint_t putwchar_unlocked (wchar_t __wc);

/*$
 * #alias fgetws;
 */
wchar_t *fgetws_unlocked (wchar_t *__restrict __ws, int __n,
                          __FILE *__restrict __stream);

/*$
 * #alias fputws;
 */
int fputws_unlocked (const wchar_t *__restrict __ws,
                     __FILE *__restrict __stream);

/*$
 * requires: valid_wchars(__s, __maxsize);
 * requires: valid_wide_string(__format);
 * requires: valid_ptr(__tp);
 *
 * case "success" {
 *   assigns: __s[0,__maxsize);
 *   ensures: valid_primed_wide_substring(__s, __maxsize);
 *   ensures: return >= 0 and return < __maxsize;
 * }
 *
 * case "failure" {
 *   ensures: return == 0;
 * }
 */
size_t wcsftime (wchar_t *__restrict __s, size_t __maxsize,
                 const wchar_t *__restrict __format,
                 const struct tm *__restrict __tp);

/*$
 * local: size_t r = wcsftime(__s, __maxsize, __format, __tp);
 * ensures: return == r;
 */
size_t wcsftime_l (wchar_t *__restrict __s, size_t __maxsize,
                   const wchar_t *__restrict __format,
                   const struct tm *__restrict __tp,
                   locale_t __loc);


/// TODO: format checking not supported due to va_arg

/*$
 * requires: alive_resource(__s, File);
 * requires: valid_wide_string(__format);
 * unsound: "vfwprintf format is not checked";
 */
int vfwprintf (__FILE *__restrict __s,
               const wchar_t *__restrict __format,
               __gnuc_va_list __arg);

/*$
 * requires: valid_wide_string(__format);
 * unsound: "vwprintf format is not checked";
 */
int vwprintf (const wchar_t *__restrict __format,
              __gnuc_va_list __arg);

/*$
 * requires: valid_wchars(__s, __n);
 * requires: valid_wide_string(__format);
 * assigns: __s[0,__n);
 * ensures: valid_primed_wide_substring(__s, __n);
 * unsound: "vswprintf format is not checked";
 */
int vswprintf (wchar_t *__restrict __s, size_t __n,
               const wchar_t *__restrict __format,
               __gnuc_va_list __arg);

/*$
 * requires: alive_resource(__s, File);
 * requires: valid_wide_string(__format);
 * unsound: "vfwscanf format and arguments not handled";
 */
int vfwscanf (__FILE *__restrict __s,
              const wchar_t *__restrict __format,
              __gnuc_va_list __arg);

/*$
 * requires: valid_wide_string(__format);
 * unsound: "vwscanf format and arguments not handled";
 */
int vwscanf (const wchar_t *__restrict __format,
             __gnuc_va_list __arg);

/*$
 * requires: valid_wide_string(__s);
 * requires: valid_wide_string(__format);
 * unsound: "vswscanf format and arguments not handled";
 */
int vswscanf (const wchar_t *__restrict __s,
              const wchar_t *__restrict __format,
              __gnuc_va_list __arg);
