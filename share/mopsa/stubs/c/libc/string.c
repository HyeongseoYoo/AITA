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
#include <string.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$=
 * predicate no_overlap(s1, n1, s2, n2):
 *   if (n1 > 0 and n2 > 0 and base(s1) == base(s2)) then
 *     (unsigned char*)s1 >= (unsigned char*)s2 + n2 or
 *     (unsigned char*)s2 >= (unsigned char*)s1 + n1
 *   end;
 */

/*$
 * requires: no_overlap(__src, __len, __dest, __len);
 * requires: valid_bytes_or_fail(__src, __len);
 * requires: valid_bytes_or_fail(__dest, __len);
 * assigns: ((unsigned char*)__dest)[0, __len);
 * ensures: forall size_t i in [0, __len): (((unsigned char*)__dest)[i])' == ((unsigned char*)__src)[i];
 * ensures: return == __dest;
 */
void *memcpy (void *__restrict __dest, const void *__restrict __src,
              size_t __len);


/*$
 * #alias memcpy;
 */
void *__builtin_memcpy (void *__restrict __dest, const void *__restrict __src,
                        size_t __len);

/*$
 * requires: valid_bytes_or_fail(__src, __len);
 * requires: valid_bytes_or_fail(__dest, __len);
 * assigns: ((unsigned char*)__dest)[0, __len);
 * ensures: forall size_t i in [0, __len): (((unsigned char*)__dest)[i])' == ((unsigned char*)__src)[i];
 * ensures: return == __dest;
 */
void *memmove(void *__dest, const void *__src, size_t __len);


/*$
 * requires: no_overlap(__src, __n, __dest, __n);
 * requires: valid_bytes_or_fail(__src, __n);
 * requires: valid_bytes_or_fail(__dest, __n);
 * assigns: ((unsigned char*)__dest)[0, __n);
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, __n): ((unsigned char*)__src)[i] != __c;
 *   ensures: forall size_t i in [0, __n): (((unsigned char*)__dest)[i])' == ((unsigned char*)__src)[i];
 *   ensures: return == NULL;
 * }
 *
 * case "found" {
 *   assumes: exists size_t i in [0, __n): ((unsigned char*)__src)[i] == __c;
 *   ensures: exists size_t i in [0, __n): (
 *              ((unsigned char*)__src)[i] == __c and
 *              (((unsigned char*)__dest)[i])' == __c and
 *              return == (unsigned char*)__src + i + 1 and
 *              (forall size_t j in [0, i): (
 *                ((unsigned char*)__src)[j] != __c and
 *                (((unsigned char*)__dest)[j])' == ((unsigned char*)__src)[j]
 *              )) and
 *              (forall size_t j in [i, __n): (((unsigned char*)__dest)[j])' == ((unsigned char*)__dest)[j])
 *            );
 * }
 */
void *memccpy (void *__restrict __dest, const void *__restrict __src,
		      int __c, size_t __n);

/*$
 * requires: valid_bytes_or_fail(__dest, __len);
 * assigns: ((unsigned char*)__dest)[0, __len);
 * ensures: forall size_t i in [0, __len): (((unsigned char*)__dest)[i])' == __ch;
 * ensures: return == __dest;
 */
void *memset(void *__dest, int __ch, size_t __len);


/*$
 * requires: valid_bytes_or_fail(__s1, __n);
 * requires: valid_bytes_or_fail(__s2, __n);
 *
 * case "equal" {
 *   assumes: forall size_t i in [0, __n): ((unsigned char*)__s1)[i] == ((unsigned char*)__s2)[i];
 *   ensures: return == 0;
 * }
 *
 * case "notequal" {
 *   assumes: exists size_t i in [0, __n): ((unsigned char*)__s1)[i] != ((unsigned char*)__s2)[i];
 *   ensures: exists size_t i in [0, __n): (
 *             ((unsigned char*)__s1)[i] != ((unsigned char*)__s2)[i] and
 *             (forall size_t j in [0, i): ((unsigned char*)__s1)[j] == ((unsigned char*)__s2)[j]) and
 *             (((unsigned char*)__s1)[i] > ((unsigned char*)__s2)[i] implies return > 0) and
 *             (((unsigned char*)__s1)[i] < ((unsigned char*)__s2)[i] implies return < 0)
 *            );
 * }
 */
int memcmp (const void *__s1, const void *__s2, size_t __n);

/*$
 * requires: valid_bytes_or_fail(__s, __n);
 *
 * case "found" {
 *   assumes: exists size_t i in [0, __n): ((unsigned char*)__s)[i] == (unsigned char)__c;
 *   ensures: exists size_t i in [0, __n): (
 *              ((unsigned char*)__s)[i] == (unsigned char)__c and
 *              (forall size_t j in [0, i): ((unsigned char*)__s)[i] != (unsigned char)__c) and
 *              return == (unsigned char*)__s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, __n): ((unsigned char*)__s)[i] != (unsigned char)__c;
 *   ensures: return == NULL;
 * }
 */
void *memchr (const void *__s, int __c, size_t __n);


/*$
 * requires: valid_ptr_or_fail(__s);
 * requires: exists size_t i in [0, (bytes(__s) - offset(__s))): ((unsigned char*)__s)[i] == (unsigned char)__c;
 * ensures:  exists size_t i in [0, (bytes(__s) - offset(__s))): (
 *             ((unsigned char*)__s)[i] == (unsigned char)__c and
 *             (forall size_t j in [0, i): ((unsigned char*)__s)[i] != (unsigned char)__c) and
 *             return == (unsigned char*)__s + i
 *           );
 */
void *rawmemchr (const void *__s, int __c);


/*$
 * #alias rawmemchr;
 */
void *__rawmemchr (const void *__s, int __c);


/*$
 * requires: valid_bytes_or_fail(__s, __n);
 *
 * case "found" {
 *   assumes: exists size_t i in [0, __n): ((unsigned char*)__s)[i] == (unsigned char)__c;
 *   ensures: exists size_t i in [0, __n): (
 *              ((unsigned char*)__s)[i] == (unsigned char)__c and
 *              (forall size_t j in [i+1, __n): ((unsigned char*)__s)[i] != (unsigned char)__c) and
 *              return == (unsigned char*)__s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, __n): ((unsigned char*)__s)[i] != (unsigned char)__c;
 *   ensures: return == NULL;
 * }
 */
void *memrchr (const void *__s, int __c, size_t __n);



/*$
 * requires: valid_string_or_fail(__s);
 * ensures:  return in [0, size(__s) - offset(__s));
 * ensures:  __s[return] == 0;
 * ensures:  forall size_t j in [0, return): __s[j] != 0;
 */
size_t strlen (const char *__s);

/*$
 * #alias strlen;
 */
size_t __builtin_strlen (const char *__s);


/*$
 * requires: valid_bytes_or_fail(__string, __maxlen) or valid_string_or_fail(__string);
 *
 * case "string-smaller-than-maxlen" {
 *   assumes: valid_substring(__string, __maxlen);
 *   local: size_t len = strlen(__string);
 *   ensures: return == len;
 * }
 *
 * case "string-bigger-than-maxlen" {
 *   assumes: forall size_t i in [0, __maxlen): __string[i] != 0;
 *   ensures: return == __maxlen;
 * }
 */
size_t strnlen (const char *__string, size_t __maxlen);


/*$
 * local: size_t src_len = strlen(__src);
 * requires: no_overlap(__src, src_len + 1, __dest, src_len + 1);
 * requires: valid_bytes_or_fail(__dest, src_len + 1);
 * assigns: __dest[0, src_len];
 * ensures: forall size_t i in [0, src_len]: (__dest[i])' == __src[i];
 * ensures: return == __dest;
 */
char *strcpy (char *__restrict __dest, const char *__restrict __src);


/*$
 * requires: valid_bytes_or_fail(__src, __len) or valid_string_or_fail(__src);
 * requires: valid_bytes_or_fail(__dest, __len);
 * local: size_t src_len = strnlen(__src, __len);
 * requires: no_overlap(__src, src_len, __dest, src_len);
 * assigns: __dest[0, __len);
 * ensures: forall size_t i in [0, src_len): (__dest[i])' == __src[i];
 * ensures: forall size_t i in [src_len, __len): (__dest[i])' == 0;
 * ensures: return == __dest;
 */
char *strncpy (char *__restrict __dest,
               const char *__restrict __src, size_t __len);

/*$
 * local: size_t src_len = strlen(__src);
 * local: size_t dest_len = strlen(__dest);
 * requires: no_overlap(__src, src_len + 1, __dest, dest_len + src_len + 1);
 * requires: valid_bytes_or_fail(__dest, dest_len + src_len + 1);
 * assigns: __dest[0, dest_len + src_len];
 * ensures: forall size_t i in [0, dest_len): (__dest[i])' == __dest[i];
 * ensures: forall size_t i in [0, src_len]: (__dest[dest_len + i])' == __src[i];
 * ensures: return == __dest;
 */
char *strcat (char *__restrict __dest, const char *__restrict __src);

/*$
 * local: size_t src_nlen = strnlen(__src, __len);
 * local: size_t dest_len = strlen(__dest);
 * requires: no_overlap(__src, src_nlen + 1, __dest, dest_len + src_nlen + 1);
 * requires: valid_bytes_or_fail(__dest, dest_len + src_nlen + 1);
 * assigns: __dest[0, dest_len + src_nlen];
 * ensures: forall size_t i in [0, dest_len): (__dest[i])' == __dest[i];
 * ensures: forall size_t i in [0, src_nlen): (__dest[dest_len + i])' == __src[i];
 * ensures: (__dest[dest_len + src_nlen])' == 0;
 * ensures: return == __dest;
 */
char *strncat (char *__restrict __dest, const char *__restrict __src,
               size_t __len);

/*$
 * requires: valid_string_or_fail(__s1);
 * requires: valid_string_or_fail(__s2);
 */
int strcmp (const char *__s1, const char *__s2);

/*$
 * #alias strcmp;
 */
int __builtin_strcmp (const char *__s1, const char *__s2);


/*$
 * requires: valid_ptr_or_fail(__s1);
 * requires: valid_ptr_or_fail(__s2);
 * requires: valid_bytes_or_fail(__s1, __n) or valid_string_or_fail(__s1);
 * requires: valid_bytes_or_fail(__s2, __n) or valid_string_or_fail(__s2);
 */
int strncmp (const char *__s1, const char *__s2, size_t __n);

/*$
 * #alias strncmp;
 */
int __builtin_strncmp (const char *__s1, const char *__s2, size_t __n);


/*$
 * requires: valid_string_or_fail(__s1);
 * requires: valid_string_or_fail(__s2);
 */
int strcoll (const char *__s1, const char *__s2);

/*$
 * requires: valid_string_or_fail(__src);
 * requires: valid_bytes_or_fail(__dest, __n);
 * requires: no_overlap(__src, __n, __dest, __n);
 * assigns: __dest[0, __n);
 * ensures: return >= 0 and (return < __n implies (__dest[return])' == 0);
 */
size_t strxfrm (char *__restrict __dest,
                const char *__restrict __src, size_t __n);

/*$
 * requires: valid_string_or_fail(__s1);
 * requires: valid_string_or_fail(__s2);
 */
int strcoll_l (const char *__s1, const char *__s2, locale_t __l);

/*$
 * requires: valid_string_or_fail(__src);
 * requires: valid_bytes_or_fail(__dest, __n);
 * requires: no_overlap(__src, __n, __dest, __n);
 * assigns: __dest[0, __n);
 * ensures: return >= 0 and (return < __n implies (__dest[return])' == 0);
 */
size_t strxfrm_l (char *__dest, const char *__src, size_t __n, locale_t __l);


/*$
 * local: size_t len = strlen(__s);
 *
 * case "success" {
 *   local: char* r = new Memory;
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
char *strdup (const char *__s);

/*$
 * #alias strdup;
 */
char *__strdup (const char *__s);


/*$
 * local: size_t len = strnlen(__s, __n);
 *
 * case "success" {
 *   requires: len < cast(size_t, -1); // FIXME: SIZE_MAX is defined in <limits.h> as (size_t)-1,
 *                                     // however the stubs parser can't yet parse casts to typedefs
 *                                     // due to conflict with parenthezied variables
 *   local: char* r = new Memory;
 *   ensures: size(r) == len + 1;
 *   ensures: forall size_t i in [0, len): r[i] == __s[i];
 *   ensures: r[len] == 0;
 *   ensures: return == r;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */ 
char *strndup (const char *__s, size_t __n);

 

/*$
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   ensures:  exists size_t i in [0, len): (
 *             __s[i] == __c and
 *             (forall size_t j in [0, i): __s[j] != __c) and
 *             return == __s + i
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, len): __s[i] != __c;
 *   ensures: return == NULL;
 * }
 */
char *strchr (const char *__s, int __c);

/*$
 * #alias strchr;
 */
char *__builtin_strchr (const char *__s, int __c);


/*$
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   ensures:  exists size_t i in [0, len): (
 *             __s[i] == __c and
 *             (forall size_t j in [i + 1, len): __s[j] != __c) and
 *             return == __s + i
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, len): __s[i] != __c;
 *   ensures: return == NULL;
 * }
 */
char *strrchr (const char *__s, int __c);


//#ifdef __USE_GNU

/*$
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   ensures:  exists size_t i in [0, len): (
 *             __s[i] == __c and
 *             (forall size_t j in [0, i): __s[j] != __c) and
 *             return == __s + i
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall size_t i in [0, len): __s[i] != __c;
 *   ensures: return == __s + len;
 * }
 */
char *strchrnul (const char *__s, int __c);

//#endif

/*$
 * requires: valid_string_or_fail(__reject);
 * local: size_t len = strlen(__s);
 * ensures: return in [0, len];
 */
size_t strcspn (const char *__s, const char *__reject);

/*$
 * requires: valid_string_or_fail(__accept);
 * local: size_t len = strlen(__s);
 * ensures: return in [0, len];
 */
size_t strspn (const char *__s, const char *__accept);

/*$
 * requires: valid_string_or_fail(__s);
 * requires: valid_string_or_fail(__accept);
 * ensures: return == NULL or in_string(return, __s);
 */
char *strpbrk (const char *__s, const char *__accept);

/*$
 * local: size_t len1 = strlen(__haystack);
 * local: size_t len2 = strlen(__needle);
 * ensures: return == NULL or 
 *          (len1 >= len2 and in_bytes(return, __haystack, len1 - len2));
 */
char *strstr (const char *__haystack, const char *__needle);


static char* _strtok_buf = NULL;

/*$
 * local: char* r = __strtok_r(__s, __delim, &_strtok_buf);
 * ensures: return == r;
 */
char *strtok (char *__restrict __s, const char *__restrict __delim);


/*$
 * requires: valid_string_or_fail(__delim);
 * requires: valid_ptr_or_fail(__save_ptr);
 *
 * case "first" {
 *   assumes: __s != NULL;
 *   local: size_t len = strlen(__s);
 *   assigns: __s[0, len);
 *   assigns: *__save_ptr;
 *   ensures: exists size_t i in [0, len): return == __s + i;
 *   ensures: exists size_t i in [0, len): (*__save_ptr)' == __s + i;
 * }
 *
 * case "next" {
 *   assumes: __s == NULL;
 *   assumes: (*__save_ptr) != NULL;
 *   local: size_t len = strlen(*__save_ptr);
 *   assigns: (*__save_ptr)[0, len);
 *   assigns: *__save_ptr;
 *   ensures: return == NULL or exists size_t i in [0, len): return == (*__save_ptr) + i;
 *   ensures: (*__save_ptr)' == NULL or exists size_t i in [0, len): (*__save_ptr)' == (*__save_ptr) + i;
 *  }
 *
 * case "end" {
 *   assumes: __s == NULL;
 *   assumes: *__save_ptr == NULL;
 *   ensures: return == NULL;
 *  }
 */
char *__strtok_r (char *__restrict __s,
                  const char *__restrict __delim,
                  char **__restrict __save_ptr);

//#ifdef __USE_POSIX

/*$
 * #alias __strtok_r;
 */
char *strtok_r (char *__restrict __s, const char *__restrict __delim,
                char **__restrict __save_ptr);

//#endif

//#ifdef __USE_GNU

/*$
 * local: size_t len1 = strlen(__haystack);
 * local: size_t len2 = strlen(__needle);
 * ensures: return == NULL or 
 *          (len1 >= len2 and in_bytes(return, __haystack, len1 - len2));
 */
char *strcasestr (const char *__haystack, const char *__needle);

//#endif

//#ifdef __USE_GNU

/*$
 * requires: valid_bytes_or_fail(__haystack, __haystacklen);
 * requires: valid_bytes_or_fail(__needle, __needlelen);
 * ensures: return == NULL or 
 *          (__haystacklen >= __needlelen and 
 *            in_bytes(return, __haystack, __haystacklen - __needlelen));
 */
void *memmem (const void *__haystack, size_t __haystacklen,
              const void *__needle, size_t __needlelen);

/*$
 * local: void* r = memcpy(__dest, __src, __n);
 * ensures: return == (unsigned char*)__dest + __n;
 */
void *mempcpy (void *__restrict __dest,
               const void *__restrict __src, size_t __n);

//#endif

/*$
 * local: char* s = _mopsa_new_readonly_string();
 * assigns: _errno;
 * ensures: return == s;
 */
char *strerror (int __errnum);

#if defined __USE_XOPEN2K && !defined __USE_GNU

/*$
 * requires: valid_bytes_or_fail(__buf, __buflen);
 * assigns: __buf[0, __buflen);
 *  assigns: _errno;
 * ensures: valid_primed_substring(__buf, __buflen);
 */
int strerorr_r (int __errnum, char *__buf, size_t __buflen);

#else

/*$
 * assigns: _errno;
 *
 * case "buf" {
 *   requires: valid_bytes_or_fail(__buf, __buflen);
 *   assigns: __buf[0, __buflen);
 *   ensures: valid_primed_substring(__buf, __buflen);
 *   ensures: return == __buf;
 * }
 *
 * case "nobuf" {
 *   local: char* s = _mopsa_new_readonly_string();
 *   ensures: return == s;
 * }
 */
char *strerror_r (int __errnum, char *__buf, size_t __buflen);

#endif

//#ifdef __USE_XOPEN2K8

/*$
 * local: char* s = _mopsa_new_readonly_string();
 * assigns: _errno;
 * ensures: return == s;
 */
char *strerror_l (int __errnum, locale_t __l);

//#endif

//#ifdef __USE_MISC

/*$
 * requires: valid_bytes_or_fail(__s, __n);
 * assigns: ((unsigned char*)__s)[0, __n);
 * ensures: forall size_t i in [0, __n): (((unsigned char*)__s)[i])' == 0;
 */
void explicit_bzero (void *__s, size_t __n);

/*$
 * requires: valid_ptr_or_fail(__stringp);
 *
 * case "nop" {
 *   assumes: *__stringp == NULL;
 *   ensures: return == NULL;
 * }
 *
 * case "op" {
 *   assumes: *__stringp != NULL;
 *   requires: valid_string_or_fail(__delim);
 *   local: size_t l = strlen(*__stringp);
 *   assigns: *__stringp;
 *   assigns: (*__stringp)[0, l);
 *   ensures:
 *     (*__stringp)' == NULL
 *     or exists size_t i in [0, l): (
 *          ((*__stringp)[i])' == 0
 *          and (*__stringp)' == *__stringp + i
 *        );
 *   ensures: return == *__stringp;
 * }
 */
char *strsep (char **__restrict __stringp,
              const char *__restrict __delim);

//#endif

//#ifdef __USE_XOPEN2K8

/*$
 * case "success" {
 *   local: char* r = _mopsa_new_readonly_string();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *strsignal (int __sig);

/*$
 * local: size_t src_len = strlen(__src);
 * requires: no_overlap(__src, src_len + 1, __dest, src_len + 1);
 * requires: valid_bytes_or_fail(__dest, src_len + 1);
 * assigns: __dest[0, src_len];
 * ensures: forall size_t i in [0, src_len]: (__dest[i])' == __src[i];
 * ensures: return == __dest + src_len;
 */
char *stpcpy (char *__restrict __dest, const char *__restrict __src);

/*$
 * requires: valid_bytes_or_fail(__src, __n) or valid_string_or_fail(__src);
 * requires: valid_bytes_or_fail(__dest, __n);
 * assigns: __dest[0, __n);
 * local: size_t src_len = strnlen(__src, __n);
 * ensures: forall size_t i in [0, src_len): (__dest[i])' == __src[i];
 * ensures: forall size_t i in [src_len, __n): (__dest[i])' == 0;
 * ensures: return == __dest + src_len;
 */
char *stpncpy (char *__restrict __dest,
               const char *__restrict __src, size_t __n);

//#endif

//#ifdef __USE_GNU

/*$
 * requires: valid_string_or_fail(__s1);
 * requires: valid_string_or_fail(__s2);
 */
int strverscmp (const char *__s1, const char *__s2);

/*$
 * local: size_t len = strlen(__string);
 * assigns: __string[0, len);
 * ensures: return == __string;
 */
char *strfry (char *__string);

/*$
 * requires: valid_bytes_or_fail(__s, __n);
 * assigns: ((unsigned char*)__s)[0, __n);
 * ensures: return == __s;
 */
void *memfrob (void *__s, size_t __n);

/*$
 * #alias memset;
 */
void *__builtin_memset(void *__dest, int __ch, size_t __len);

/*$
 * #alias memmove;
 */
void *__builtin_memmove(void *__dest, const void *__src, size_t __len);

//#endif
