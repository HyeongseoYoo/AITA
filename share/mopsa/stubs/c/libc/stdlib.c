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
#include <stdlib.h>
#include <limits.h>
#include <inttypes.h>
#include <locale.h>
#include <errno.h>
#include "mopsa_libc_utils.h"


/* Stubs */


/*$
 * ensures: return in [1, MB_LEN_MAX];
 */
size_t __ctype_get_mb_cur_max (void);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 */
double atof (const char *__nptr);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 */
int atoi (const char *__nptr);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 */
long int atol (const char *__nptr);

//#ifdef __USE_ISOC99

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 */
long long int atoll (const char *__nptr);

//#endif

/*$
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_string(__nptr);
 * }
 */
double strtod (const char *__restrict __nptr,
               char **__restrict __endptr);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   requires: valid_string(__nptr);
 *   assumes: __endptr == NULL;
 * }
 */
double strtod_l (const char *__restrict __nptr,
		 char **__restrict __endptr, locale_t __loc);


//#ifdef	__USE_ISOC99

/*$
 * requires: valid_string(__nptr);
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 * }
 */
float strtof (const char *__restrict __nptr,
              char **__restrict __endptr);

/*$
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_string(__nptr);
 * }
 */
long double strtold (const char *__restrict __nptr,
                     char **__restrict __endptr);

//#endif

/*$
 * requires: __base == 0 or __base in [2, 36];
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_string(__nptr);
 * }
 */
long int strtol (const char *__restrict __nptr,
                 char **__restrict __endptr, int __base);


/*$
 * requires: __base == 0 or __base in [2, 36];
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_string(__nptr);
 * }
 */
unsigned long int strtoul (const char *__restrict __nptr,
                           char **__restrict __endptr, int __base);

//#ifdef __USE_MISC

/*$
 * #alias strtoll;
 */
long long int strtoq (const char *__restrict __nptr,
                      char **__restrict __endptr, int __base);

/*$
 * #alias strtoull;
 */
unsigned long long int strtouq (const char *__restrict __nptr,
                                char **__restrict __endptr, int __base);

//#endif

//#ifdef __USE_ISOC99

/*$
 * requires: __base == 0 or __base in [2, 36];
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_string(__nptr);
 * }
 */
long long int strtoll (const char *__restrict __nptr,
                       char **__restrict __endptr, int __base);

/*$
 * requires: __base == 0 or __base in [2, 36];
 * requires: null_or_valid_ptr(__endptr);
 * assigns: _errno;
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   local: size_t len = strlen(__nptr);
 *   assigns: *__endptr;
 *   ensures: exists size_t i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   requires: valid_string(__nptr);
 * }
 */
unsigned long long int strtoull (const char *__restrict __nptr,
                                 char **__restrict __endptr, int __base);

//#endif


//#ifdef __USE_GNU

/*$
 * local: long int r = strtol(__nptr,__endptr,__base);
 */
long int strtol_l (const char *__restrict __nptr,
                   char **__restrict __endptr, int __base,
                   locale_t __loc);

/*$
 * local: unsigned long int r = strtoul(__nptr,__endptr,__base);
 */
unsigned long int strtoul_l (const char *__restrict __nptr,
                             char **__restrict __endptr,
                             int __base, locale_t __loc);

/*$
 * local: long long int r = strtoll(__nptr,__endptr,__base);
 */
long long int strtoll_l (const char *__restrict __nptr,
                         char **__restrict __endptr, int __base,
                         locale_t __loc);

/*$
 * local: unsigned long long int r = strtoull(__nptr,__endptr,__base);
 */
unsigned long long int strtoull_l (const char *__restrict __nptr,
                                   char **__restrict __endptr,
                                   int __base, locale_t __loc);

/*$
 * local: double r = strtod(__nptr,__endptr);
 */
double strtod_l (const char *__restrict __nptr,
                 char **__restrict __endptr, locale_t __loc);

/*$
 * local: float r = strtof(__nptr,__endptr);
 */
float strtof_l (const char *__restrict __nptr,
                char **__restrict __endptr, locale_t __loc);

/*$
 * local: long double r = strtold(__nptr,__endptr);
 */
long double strtold_l (const char *__restrict __nptr,
                       char **__restrict __endptr,
                       locale_t __loc);

//#endif


// omitted: __USE_GNU functions


//#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

static char _l64a_buf[7];

/*$
 * requires: __n >= 0;
 * assigns: _l64a_buf[7];
 * ensures: valid_primed_string(_l64a_buf);
 * ensures:  return == _l64a_buf;
 */
char *l64a (long int __n);

/*$
 * requires: valid_bytes(__s, 6) or valid_substring(__s, 6);
 */
long int a64l (const char *__s);

//#endif

//#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * ensures: return in [0, RAND_MAX];
 */
long int random (void);

/*$
 * // empty contract
 */
void srandom (unsigned int __seed);

static char _rand__state[128];
static char* _rand__statebuf = _rand__state;
static size_t _rand__statelen = sizeof(_rand__state);

/*$
 * requires: __statelen >= 8;
 * requires: valid_bytes(__statebuf, __statelen);
 * assigns:  _rand__statebuf;
 * assigns:  _rand__statelen;
 * assigns:  __statebuf[0, __statelen - 1];
 * ensures:  _rand__statebuf' == __statebuf and _rand__statelen' == __statelen;
 * ensures:  return == _rand__statebuf;
 */
char *initstate (unsigned int __seed, char *__statebuf, size_t __statelen);

/*$
 * requires: valid_bytes(__statebuf, _rand__statelen);
 * assigns:  _rand__statebuf;
 * ensures:  _rand__statebuf' == __statebuf;
 *
 * case "success" {
 *   ensures:  return == _rand__statebuf;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *setstate (char *__statebuf);

//#endif

//#ifdef __USE_MISC

/*$
 * assigns: *__buf;
 * assigns: *__result;
 * ensures: (*__result)' in [0, RAND_MAX]; 
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   ensures: return == -1;
 *   assigns: _errno;
 * }
 */
int random_r (struct random_data *__restrict __buf,
              int32_t *__restrict __result);

/*$
 * assigns: *__buf;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   ensures: return == -1;
 *   assigns: _errno;
 * }
 */
int srandom_r (unsigned int __seed, struct random_data *__buf);

/*$
 * requires: __statelen >= 8;
 * requires: valid_bytes(__statebuf, __statelen);
 * assigns:  __statebuf[0, __statelen);
 * assigns:  *__buf;
 * ensures:  (__buf->rand_deg)' == __statelen; // keep length information around
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   ensures: return == -1;
 *   assigns: _errno;
 * }
 */
int initstate_r (unsigned int __seed, char *__restrict __statebuf,
                 size_t __statelen,
                 struct random_data *__restrict __buf);

/*$
 * requires: valid_bytes(__statebuf, __buf->rand_deg);
 * assigns:  __statebuf[0, __buf->rand_deg);
 * assigns:  *__buf;
 * ensures:  (__buf->rand_deg)' == __buf->rand_deg; // keep length information around
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   ensures: return == -1;
 *   assigns: _errno;
 * }
 */
int setstate_r (char *__restrict __statebuf,
                struct random_data *__restrict __buf);

//#endif

/*$
 * ensures: return in [0, RAND_MAX];
 */
int rand (void);

/*$
 * // empty contract
 */
void srand (unsigned int __seed);

//#ifdef __USE_POSIX199506

/*$
 * assigns: *__seed;
 * ensures: return in [0, RAND_MAX];
 */
int rand_r (unsigned int *__seed);

//#endif

//#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * ensures: valid_float(return) and return in [0., 1.];
 */
double drand48 (void);

/*$
 * assigns: __xsubi[0, 2];
 * ensures: valid_float(return) and return in [0., 1.];
 */
double erand48 (unsigned short int __xsubi[3]);

/*$
 * ensures: return in [0, 2147483647];
 */
long int lrand48 (void);

/*$
 * assigns: __xsubi[0, 2];
 * ensures: return in [0, 2147483647];
 */
long int nrand48 (unsigned short int __xsubi[3]);

/*$
 * ensures: return in [-2147483648, 2147483647];
 */
long int mrand48 (void);

/*$
 * assigns: __xsubi[0, 2];
 * ensures: return in [-2147483648, 2147483647];
 */
long int jrand48 (unsigned short int __xsubi[3]);

/*$
 * // empty contract
 */
void srand48 (long int __seedval);

static unsigned short int _seed48_buf[3];

/*$
 * requires: valid_ptr_range(__seed16v, 0, 2);
 * assigns:  _seed48_buf;
 * ensures:  return == &_seed48_buf[0];
 */
unsigned short int *seed48 (unsigned short int __seed16v[3]);

/*$
 * requires: valid_ptr_range(__param, 0, 6);
 */
void lcong48 (unsigned short int __param[7]);

//#endif

//#ifdef __USE_MISC

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * ensures: valid_float((*__result)') and (*__result)' in [0., 1.];
 * ensures: return == 0;
 */
int drand48_r (struct drand48_data *__restrict __buffer,
               double *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * assigns: __xsubi[0, 2];
 * ensures: valid_float((*__result)') and (*__result)' in [0., 1.];
 * ensures: return == 0;
 */
int erand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               double *__restrict __result);
/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * ensures: (*__result)' in [0, 2147483647];
 * ensures: return == 0;
 */
int lrand48_r (struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * assigns: __xsubi[0, 2];
 * ensures: (*__result)' in [0, 2147483647];
 * ensures: return == 0;
 */
int nrand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * ensures: (*__result)' in [-2147483648, 2147483647];
 * ensures: return == 0;
 */
int mrand48_r (struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * assigns: __xsubi[0, 2];
 * ensures: (*__result)' in [-2147483648, 2147483647];
 * ensures: return == 0;
 */
int jrand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__buffer;
 * ensures: return == 0;
 */
int srand48_r (long int __seedval, struct drand48_data *__buffer);

/*$
 * requires: valid_ptr_range(__seed16v, 0, 2);
 * assigns:  *__buffer;
 * ensures:  return == 0;
 */
int seed48_r (unsigned short int __seed16v[3],
              struct drand48_data *__buffer);

/*$
 * requires: valid_ptr_range(__param, 0, 6);
 * assigns:  *__buffer;
 * ensures:  return == 0;
 */
int lcong48_r (unsigned short int __param[7],
               struct drand48_data *__buffer);

//#endif


/*$
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __size; // __size==0 also possible here
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 *
 * case "empty" {
 *   assumes: __size == 0;
 *   ensures: return == NULL;
 * }
 */
void *malloc (size_t __size);

/*$
 * //TODO: check overflow in __nmemb * __size
 *
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __nmemb * __size;
 *   ensures: forall size_t i in [0, (__nmemb * __size) ): ((unsigned char*)r)[i] == 0;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 *
 * case "empty" {
 *   assumes: __nmemb == 0 or __size == 0;
 *   ensures: return == NULL;
 * }
 */
void *calloc (size_t __nmemb, size_t __size);

/*$
 * case "nop" {
 *   assumes:  __ptr == NULL;
 *   assumes:  __size == 0;
 *   ensures:  return == NULL;
 * }
 *
 * case "alloc" {
 *   assumes:  __ptr == NULL;
 *   local:    void* r = new Memory;
 *   ensures:  size(r) == __size; // __size==0 also possible here
 *   ensures:  return == r;
 * }
 *
 * case "free" {
 *   assumes:  __ptr != NULL;
 *   assumes:  __size == 0;
 *   requires: __ptr in Memory;
 *   free:     __ptr;
 *   ensures:  return == NULL;
 * }
 *
 * case "resize" {
 *   assumes:  __ptr != NULL;
 *   assumes:  __size > 0;
 *   requires: __ptr in Memory;
 *   local:    void* r = new Memory;
 *   ensures:  size(r) == __size;
 *   ensures:  size(__ptr) >= __size implies 
 *             forall size_t i in [0, __size): ((unsigned char*)r)[i] == ((unsigned char*)__ptr)[i];
 *   ensures:  size(__ptr) <= __size implies 
 *             forall size_t i in [0, size(__ptr)): ((unsigned char*)r)[i] == ((unsigned char*)__ptr)[i];
 *   free:     __ptr;
 *   ensures:  return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
void *realloc (void *__ptr, size_t __size);

// omitted: __USE_GNU reallocarray

/*$
 * case "nop" {
 *   assumes:  __ptr == NULL;
 * }
 *
 * case "free" {
 *   assumes:  __ptr != NULL;
 *   requires: __ptr in Memory;
 *   requires: offset(__ptr) == 0;
 *   free:     __ptr;
 * }
 */
void free (void *__ptr);

//#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K) || defined __USE_MISC

/*$
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __size;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
void *valloc (size_t __size);

//#endif

//#ifdef __USE_XOPEN2K

static const size_t _sizeof_ptr = sizeof(void*);

/*$
 * requires: __alignment % _sizeof_ptr == 0;
 * requires: (__alignment & (__alignment - 1)) == 0; // __alignment is a power of 2
 *
 * case "success" {
 *   local:   void* r = new Memory;
 *   assigns: **__memptr;
 *   ensures: size(r) == __size;
 *   ensures: (*__memptr)' == r;
 *   ensures: return == 0;
 * }
 *
 * case "empty" {
 *   assumes: __size == 0;
 *   assigns: **__memptr;
 *   ensures: (*__memptr)' == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   ensures: return != 0;
 * }
 */
int posix_memalign (void **__memptr, size_t __alignment, size_t __size);

//#endif

//#ifdef __USE_ISOC11

/*$
 * requires: (__alignment & (__alignment - 1)) == 0; // __alignment is a power of 2
 * requires: __size % __alignment == 0;
 *
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __size;
 *   ensures: return == r;
 * }
 *
 * case "empty" {
 *   assumes: __size == 0;
 *   ensures: return == NULL;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
void *aligned_alloc (size_t __alignment, size_t __size);

//#endif

/*$
 * ensures: raise("abort called");
 * ensures: 1 == 0;
 */
void abort (void);

/*$
 * ensures: raise("__builtin_abort called");
 * ensures: 1 == 0;
 */
void __builtin_abort (void);


#define _ATEXIT_MAX 32

void (*_exit_fun_buf[_ATEXIT_MAX])(void);
unsigned int _next_exit_fun_slot = 0;

/*$
 * case "success" {
 *   assumes: _next_exit_fun_slot <= _ATEXIT_MAX;
 *   assigns: _next_exit_fun_slot;
 *   assigns: _exit_fun_buf[_next_exit_fun_slot];
 *   ensures: (_exit_fun_buf[_next_exit_fun_slot])' == __func;
 *   ensures: _next_exit_fun_slot' == _next_exit_fun_slot + 1;
 *   ensures: return == 1;
 * }
 *
 * case "failure" {
 *   assumes: _next_exit_fun_slot > _ATEXIT_MAX;
 *   ensures: return == 0;
 * }
 */
int atexit (void (*__func) (void));

// stub built in mopsa
void exit (int __status);

/*$
 * ensures: 1 == 0;
 */
void _Exit (int __status);

/*$
 * ensures: 1 == 0;
 */
void _exit (int __status);

//#if defined __USE_ISOC11 || defined __USE_ISOCXX11

void (*_quick_exit_fun_buf[_ATEXIT_MAX])(void);
unsigned int _next_quick_exit_fun_slot = 0;

/*$
 * case "success" {
 *   assumes: _next_quick_exit_fun_slot <= _ATEXIT_MAX;
 *   assigns: _next_quick_exit_fun_slot;
 *   assigns: _quick_exit_fun_buf[_next_quick_exit_fun_slot];
 *   ensures: (_quick_exit_fun_buf[_next_quick_exit_fun_slot])' == __func;
 *   ensures: _next_quick_exit_fun_slot' == _next_quick_exit_fun_slot + 1;
 *   ensures: return == 1;
 * }
 *
 * case "failure" {
 *   assumes: _next_quick_exit_fun_slot > _ATEXIT_MAX;
 *   ensures: return == 0;
 * }
 */
int at_quick_exit (void (*__func) (void)) ;

// stub built in mopsa
void quick_exit (int __status);

//#endif

#ifdef	__USE_MISC

int on_exit (void (*__func) (int __status, void *__arg), void *__arg);

#endif


/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local:    char* r = new GetEnvString;
 *   ensures:  size(r) == INT_MAX;
 *   ensures:  return == r;
 *   ensures:  valid_string(return);
 * }
 *
 * case "failure" {
 *   ensures:  return == NULL;
 * }
 */
char *getenv (const char *__name);

//#ifdef __USE_GNU

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local:    char* r = _mopsa_new_readonly_string();
 *   ensures:  return == r;
 * }
 *
 * case "failure" {
 *   ensures:  return == NULL;
 * }
 */
char *secure_getenv (const char *__name);

//#endif

//#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * //TODO: the function takes ownership of the string
 *
 * requires: valid_string(__string);
 *
 * case "success" {
 *   ensures:  return == 0;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return != 0;
 * }
 */
int putenv (char *__string);

//#endif

//#ifdef __USE_XOPEN2K

/*$
 * requires: valid_string(__name);
 * requires: valid_string(__value);
 *
 * case "success" {
 *   ensures:  return == 0;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == -1;
 * }
 */
int setenv (const char *__name, const char *__value, int __replace);

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   ensures:  return == 0;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == -1;
 * }
 */
int unsetenv (const char *__name);

//#endif

//#ifdef __USE_MISC

/*$
 * // empty contract
 */
int clearenv (void);

//#endif

//#if defined __USE_MISC || (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8)

/*$
 * local: size_t len = strlen(__template);
 * requires: len >= 6;
 * requires: forall int i in [0, 5]: __template[len - 6 + i] == 'X';
 * assigns: __template[0, len);
 * assigns: _errno;
 * ensures:  return == __template;
 */
char *mktemp (char *__template);

//#endif

//#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * local: size_t len = strlen(__template);
 * requires: len >= 6;
 * requires: forall int i in [0, 5]: __template[len - 6 + i] == 'X';
 * assigns: __template[0, len);
 *
 * case "success" {
 *   local:   void* f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int mkstemp (char *__template);

//#endif

//#ifdef __USE_MISC

/*$
 * requires: __suffixlen >= 0;
 * local: size_t len = strlen(__template);
 * requires: len >= 6 + __suffixlen;
 * requires: forall int i in [0, 5]: __template[len - 6 - __suffixlen + i] == 'X';
 * assigns: __template[0, len);
 *
 * case "success" {
 *   local:   void* f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int mkstemps (char *__template, int __suffixlen);

//#endif
  
//#ifdef __USE_XOPEN2K8
  
/*$
 * #alias mktemp;
 */
char *mkdtemp (char *__template);

//#endif

//#ifdef __USE_GNU

/*$
 * local: int r = mkstemp(__template);
 * ensures: return == r;
 */
int mkostemp (char *__template, int __flags);

/*$
 * local: int r = mkstemps(__template, __suffixlen);
 * ensures: return == r;
 */
int mkostemps (char *__template, int __suffixlen, int __flags);

//#endif

/*$
 * requires: null_or_valid_string(__command);
 */
int system (const char *__command);


//#ifdef __USE_GNU

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local:    char* r = _mopsa_new_valid_string_max(PATH_MAX);
 *   ensures:  return == r;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == NULL;
 * }
 */
char *canonicalize_file_name (const char *__name);

//#endif

//#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * requires: valid_string(__name);
 * requires: null_or_valid_bytes(__resolved, PATH_MAX);
 *
 * case "alloc" {
 *   assumes:  __resolved == NULL;
 *   local:    char* r = _mopsa_new_valid_string_max(PATH_MAX);
 *   ensures:  return == r;
 * }
 *
 * case "copy" {
 *   assumes:  __resolved != NULL;
 *   assigns:  __resolved[0, PATH_MAX);
 *   ensures:  valid_primed_substring(__resolved, PATH_MAX);
 *   ensures:  return == __resolved;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == NULL;
 * }
 */
char *realpath (const char *__restrict __name,
                char *__restrict __resolved);

//#endif

/*$
 * requires: valid_bytes(__base, __nmemb * __size);
 * requires: valid_bytes(__key, __size);
 * ensures: return == NULL or
 *          exists size_t i in [0,__nmemb): return == ((unsigned char*)__base) + i * __size;
 * unsound: "bsearch does not call the comparison function";
 */
void *bsearch (const void *__key, const void *__base,
               size_t __nmemb, size_t __size, __compar_fn_t __compar);

/*$
 * requires: valid_bytes(__base, __nmemb * __size);
 * unsound: "qsort stub does not update the array argument and does not call the comparison function";
 */
void qsort (void *__base, size_t __nmemb, size_t __size,
            __compar_fn_t __compar);

#ifdef __USE_GNU

/*$
 * requires: valid_bytes(__base, __nmemb * __size);
 * unsound: "qsort_r stub does not update the array argument and does not call the comparison function";
 */
void qsort_r (void *__base, size_t __nmemb, size_t __size,
              __compar_d_fn_t __compar, void *__arg);

#endif


/*$
 * requires: __x > INT_MIN;
 * ensures: if __x >= 0 then return == __x else  return == -__x end;
*/
int abs (int __x);

/*$
 * requires: __x > LONG_MIN;
 * ensures: if __x >= 0 then return == __x else  return == -__x end;
 */
long int labs (long int __x);

/*$
 * requires: __x > LLONG_MIN;
 * ensures: if __x >= 0 then return == __x else  return == -__x end;
 */
long long int llabs (long long int __x);


/*$
 * requires: __denom != 0 and __denom > INT_MIN;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
div_t div (int __numer, int __denom);

/*$
 * requires: __denom != 0 and __denom > LONG_MIN;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
ldiv_t ldiv (long int __numer, long int __denom);

//#ifdef __USE_ISOC99

/*$
 * requires: __denom != 0 and __denom > LLONG_MIN;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
lldiv_t lldiv (long long int __numer, long long int __denom);

//#endif


//#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) || defined __USE_MISC

static char cvt_buf[1024];

/*$
 * assigns: cvt_buf;
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_string(cvt_buf);
 * ensures: return == cvt_buf;
 */
char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign);

/*$
 * assigns: cvt_buf;
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_string(cvt_buf);
 * ensures: return == cvt_buf;
 */
char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign);

/*$
 * requires: valid_bytes(__buf, __ndigit);
 * assigns: __buf[0, __ndigit);
 * ensures: valid_primed_string(__buf);
 * ensures: return == __buf;
 */
char *gcvt (double __value, int __ndigit, char *__buf);

//#endif

//#ifdef __USE_MISC

/*$
 * assigns: cvt_buf;
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_string(cvt_buf);
 * ensures: return == cvt_buf;
 */
char *qecvt (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign);

/*$
 * assigns: cvt_buf;
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_string(cvt_buf);
 * ensures: return == cvt_buf;
 */
char *qfcvt (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign);

/*$
 * assigns: cvt_buf;
 * ensures: valid_primed_string(cvt_buf);
 * ensures: return == cvt_buf;
 */
char *qgcvt (long double __value, int __ndigit, char *__buf);

/*$
 * requires: valid_bytes(__buf, __len);
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_substring(cvt_buf, __len);
 * ensures: return in [-1,0];
 */
int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign, char *__restrict __buf,
            size_t __len);

/*$
 * requires: valid_bytes(__buf, __len);
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_substring(cvt_buf, __len);
 * ensures: return in [-1,0];
 */
int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign, char *__restrict __buf,
            size_t __len);

/*$
 * requires: valid_bytes(__buf, __len);
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_substring(cvt_buf, __len);
 * ensures: return in [-1,0];
 */
int qecvt_r (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign,
             char *__restrict __buf, size_t __len);

/*$
 * requires: valid_bytes(__buf, __len);
 * assigns: *__decpt;
 * assigns: *__sign;
 * ensures: valid_primed_substring(cvt_buf, __len);
 * ensures: return in [-1,0];
 */
int qfcvt_r (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign,
             char *__restrict __buf, size_t __len);

//#endif


/*$
 * requires: null_or_valid_bytes(__s, __n);
 *
 * case "shift" {
 *   assumes:  __s != NULL;
 *   ensures: return in [-1, __n];
 * }
 *
 * case "reset" {
 *   assumes:  __s == NULL;
 * }
 */
int mblen (const char *__s, size_t __n);

/*$
 * requires: null_or_valid_bytes(__s, __n);
 *
 * case "shift" {
 *   assumes:  __s != NULL and __pwc == NULL;
 *   ensures: return in [-1, __n];
 * }
 *
 * case "copy" {
 *   assumes:  __s != NULL and __pwc != NULL;
 *   assigns:  *__pwc;
 *   ensures:  return in [-1, __n];
 * }
 *
 * case "reset" {
 *   assumes:  __s == NULL;
 * }
 */
int mbtowc (wchar_t *__restrict __pwc,
            const char *__restrict __s, size_t __n);

/*$
 * requires: null_or_valid_bytes(__s, MB_LEN_MAX);
 *
 * case "shift" {
 *   assumes: __s != NULL; 
 *   assigns: __s[0, MB_LEN_MAX);
 *   ensures: return in [-1, MB_LEN_MAX];
 *  }
 *
 * case "reset" {
 *   assumes: __s == NULL;
 * }
 */
int wctomb (char *__s, wchar_t __wchar);

/*$
 * requires: null_or_valid_wchars(__dst, __len);
 * requires: valid_string(__src);
 *
 * case "copy" {
 *   assumes: __dst != NULL;
 *   assigns: __dst[0, __len);
 * }
 *
 * case "count" {
 *   assumes: __dst == NULL;
 * }
 */
size_t mbstowcs (wchar_t *__restrict __dst,
                 const char *__restrict __src, size_t __len);

/*$
 * requires: null_or_valid_bytes(__dst, __len);
 * requires: valid_wide_string(__src);
 *
 * case "copy" {
 *   assumes: __dst != NULL;
 *   assigns: __dst[0, __len);
 * }
 *
 * case "count" {
 *   assumes: __dst == NULL;
 * }
 */
size_t wcstombs (char *__restrict __dst,
                 const wchar_t *__restrict __src, size_t __len);

//#ifdef __USE_MISC

/*$
 * requires: valid_string(__response);
 * ensures:  return in [-1, 1];
 */
int rpmatch (const char *__response);

//#endif


//#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

int getsubopt (char **__restrict __optionp,
               char *const *__restrict __tokens,
               char **__restrict __valuep);

//#endif


//#ifdef __USE_XOPEN

/*$
 * requires: valid_bytes(__key, 64);
 */
void setkey (const char *__key);

//#endif


//#ifdef __USE_XOPEN2KXSI

/*$
 * case "success" {
 *   local:   void* f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int posix_openpt (int __oflag);

//#endif

//#ifdef __USE_XOPEN_EXTENDED

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int grantpt (int __fd);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int unlockpt (int __fd);

static char _ptsname_buf[1024]; // TODO: fix size

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   assigns: _ptsname_buf[0, (size(_ptsname_buf) - offset(_ptsname_buf)));
 *   ensures: valid_primed_string(_ptsname_buf);
 *   ensures: return == (char*)_ptsname_buf;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *ptsname (int __fd) ;

//#endif

//#ifdef __USE_GNU

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__buf, __buflen);
 *
 * case "success" {
 *   assigns: __buf[0, __buflen);
 *   ensures: valid_primed_substring(__buf, __buflen);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return != 0;
 * }
 */
int ptsname_r (int __fd, char *__buf, size_t __buflen);

/*$
 * case "success" {
 *   local:   void* f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getpt (void);

//#endif

//#ifdef __USE_MISC

/*$
 * requires: valid_bytes(__loadavg, __nelem * sizeof_type(double));
 * assigns:  __loadavg[0, __nelem);
 * ensures:  return in [-1, __nelem];
 */
int getloadavg (double __loadavg[], int __nelem);

//#endif
