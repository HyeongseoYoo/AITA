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
#include <time.h>
#include <errno.h>
#include <signal.h> // for struct sigevent needed by time_create
#include "mopsa_libc_utils.h"


char *tzname[2];
int daylight;
long int timezone;


/*$
 * assigns: tzname[0,1];
 * assigns: daylight;
 * assigns: timezone;
 * local: char* tz0 =_mopsa_new_readonly_string();
 * local: char* tz1 =_mopsa_new_readonly_string();
 * ensures: (tzname[0])' == tz0;
 * ensures: (tzname[1])' == tz1;
 */
static int _mopsa_tzset();

/*$!
 * local: int dummy = _mopsa_tzset();
 */



/*$
 * ensures: return >= -1;
 */
clock_t clock (void);

/*$
 * requires: null_or_valid_ptr(__timer);
 * assigns: _errno;
 * ensures: return >= -1;
 *
 * case "null" {
 *   assumes: __timer == NULL;
 * }
 *
 * case "nonnull" {
 *   assumes: __timer != NULL;
 *   assigns: *__timer;
 *   ensures: return == (*__timer)';
 * }
 */
time_t time (time_t *__timer);

/*$
 * ensures: valid_float(return);
 */
double difftime (time_t __time1, time_t __time0);

#define valid_primed_tm(tm)                  \
    primed(tm.tm_sec)  in [0,60] and         \
    primed(tm.tm_min)  in [0,59] and         \
    primed(tm.tm_hour) in [0,23] and         \
    primed(tm.tm_mday) in [1,31] and         \
    primed(tm.tm_mon)  in [0,11] and         \
    primed(tm.tm_wday) in [0,6]  and         \
    primed(tm.tm_yday) in [0,365] and        \
    primed(tm.tm_isdst) >= 0

/*$
 * requires: valid_ptr(__tp);
 * assigns: *__tp;
 * local: int dummy = _mopsa_tzset();
 * ensures: valid_primed_tm(*__tp);
 * ensures: return >= -1;
 */
time_t mktime (struct tm *__tp);

/*$
 * requires: valid_string(__format);
 * requires: valid_bytes(__s, __maxsize);
 * assigns: __s[0, __maxsize);
 * ensures: valid_primed_substring(__s, __maxsize);
 * ensures: return >= 0;
 * // TODO: return == strlen(__s), case where return == 0 and __s is undefined
 */
size_t strftime (char *__restrict __s, size_t __maxsize,
                 const char *__restrict __format,
                 const struct tm *__restrict __tp);

/*$
 * requires: valid_string(__s);
 * requires: valid_string(__fmt);
 * assigns: *__tp;
 * ensures: valid_primed_tm(*__tp);
 * ensures: return == NULL or in_string(return, __s);
 */
char *strptime (const char *__restrict __s,
                const char *__restrict __fmt, struct tm *__tp);

/*$
 * local: size_t r = strftime(__s, __maxsize, __format, __tp);
 * ensures: return == r;
 */
size_t strftime_l (char *__restrict __s, size_t __maxsize,
                   const char *__restrict __format,
                   const struct tm *__restrict __tp,
                   locale_t __loc);

/*$
 * local: char* r = strptime(__s, __fmt, __tp);
 * ensures: return == r;
 */
char *strptime_l (const char *__restrict __s,
                  const char *__restrict __fmt, struct tm *__tp,
                  locale_t __loc);

static struct tm _mopsa_tm_buf;

/*$
 * requires: valid_ptr(__timer);
 * assigns: _mopsa_tm_buf;
 * ensures: valid_primed_tm(_mopsa_tm_buf);
 * ensures: return == NULL or return == &_mopsa_tm_buf;
 */
struct tm *gmtime (const time_t *__timer);

/*$
 * requires: valid_ptr(__timer);
 * local: int dummy = _mopsa_tzset();
 * assigns: _mopsa_tm_buf;
 * ensures: valid_primed_tm(_mopsa_tm_buf);
 * ensures: return == NULL or return == &_mopsa_tm_buf;
 */
struct tm *localtime (const time_t *__timer);

/*$
 * requires: valid_ptr(__timer);
 * requires: valid_ptr(__tp);
 * assigns: *__tp;
 * ensures: valid_primed_tm(*__tp);
 * ensures: return == __tp;
 * // TODO: can return NULL ?
 */
struct tm *gmtime_r (const time_t *__restrict __timer,
                     struct tm *__restrict __tp);

/*$
 * requires: valid_ptr(__timer);
 * requires: valid_ptr(__tp);
 * local: int dummy = _mopsa_tzset();
 * assigns: *__tp;
 * ensures: valid_primed_tm(*__tp);
 * ensures: return == __tp;
 * // TODO: can return NULL ?
 */
struct tm *localtime_r (const time_t *__restrict __timer,
                        struct tm *__restrict __tp);

static char _mopsa_asctime_buf[26];

/*$
 * requires: valid_ptr(__tp);
 * assigns: _mopsa_asctime_buf;
 * ensures: valid_primed_substring(_mopsa_asctime_buf, 26);
 * ensures: return == _mopsa_asctime_buf;
 */
char *asctime (const struct tm *__tp);

/*$
 * requires: valid_ptr(__timer);
 * assigns: _mopsa_asctime_buf;
 * ensures: valid_primed_substring(_mopsa_asctime_buf, 26);
 * ensures: return == _mopsa_asctime_buf;
 */
char *ctime (const time_t *__timer);

/*$
 * requires: valid_ptr(__tp);
 * requires: valid_bytes(__buf, 26);
 * assigns: __buf[0,26);
 * ensures: valid_primed_substring(__buf, 26);
 * ensures: return == __buf;
 */
char *asctime_r (const struct tm *__restrict __tp,
                 char *__restrict __buf);

/*$
 * requires: valid_ptr(__timer);
 * requires: valid_bytes(__buf, 26);
 * assigns: __buf[0,26);
 * ensures: valid_primed_substring(__buf, 26);
 * ensures: return == __buf;
 */
char *ctime_r (const time_t *__restrict __timer,
               char *__restrict __buf);

/*$
 * local: int dummy = _mopsa_tzset();
 */
void tzset (void);

/*$
 * requires: valid_ptr(__when);
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
int stime (const time_t *__when);

/*$
 * requires: valid_ptr(__tp);
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
time_t timegm (struct tm *__tp);

/*$
 * #alias timegm;
 */
time_t timelocal (struct tm *__tp);

/*$
 * ensures: return in [365,366];
 */
int dysize (int __year);

/*$
 * requires: valid_ptr(__requested_time);
 * requires: null_or_valid_ptr(__remaining);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure-remaining" {
 *   assumes: __remaining != NULL;
 *   assigns: *__remaining;
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 *
 * case "failure-noremaining" {
 *   assumes: __remaining == NULL;
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int nanosleep (const struct timespec *__requested_time,
               struct timespec *__remaining);

/*$
 * requires: null_or_valid_ptr(__res);
 *
 * case "nonnull" {
 *   assumes: __res != NULL;
 *   assigns: *__res;
 *   ensures: return == 0;
 * }
 *
 * case "null" {
 *   assumes: __res == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int clock_getres (clockid_t __clock_id, struct timespec *__res);

/*$
 * requires: null_or_valid_ptr(__tp);
 *
 * case "nonnull" {
 *   assumes: __tp != NULL;
 *   assigns: *__tp;
 *   ensures: return == 0;
 * }
 *
 * case "null" {
 *   assumes: __tp == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int clock_gettime (clockid_t __clock_id, struct timespec *__tp);

/*$
 * requires: valid_ptr(__tp);
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
int clock_settime (clockid_t __clock_id, const struct timespec *__tp);


/*$
 * requires: valid_ptr(__req);
 * requires: null_or_valid_ptr(__rem);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure-remaining" {
 *   assumes: __rem != NULL;
 *   assigns: *__rem;
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 *
 * case "failure-noremaining" {
 *   assumes: __rem == NULL;
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int clock_nanosleep (clockid_t __clock_id, int __flags,
                     const struct timespec *__req,
                     struct timespec *__rem);

/*$
 * requires: valid_ptr(__clock_id);
 * assigns: *__clock_id;
 */
int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id);

/*$
 * requires: null_or_valid_ptr(__evp);
 * requires: valid_ptr(__timerid);
 *
 * case "success" {
 *   local: timer_t r = new Timer;
 *   assigns: *__timerid;
 *   ensures: (*__timerid)' == r;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int timer_create (clockid_t __clock_id,
                  struct sigevent *__restrict __evp,
                  timer_t *__restrict __timerid);

/*$
 * requires: __timerid in Timer;
 * free: __timerid;
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
int timer_delete (timer_t __timerid);

/*$
 * requires: alive_resource(__timerid, Timer);
 * requires: valid_ptr(__value);
 * requires: null_or_valid_ptr(__ovalue);
 *
 * case "success-nonnull" {
 *   assumes: __ovalue != NULL;
 *   assigns: *__ovalue;
 *   ensures: return == 0;
 * }
 *
 * case "success-null" {
 *   assumes: __ovalue == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int timer_settime (timer_t __timerid, int __flags,
                   const struct itimerspec *__restrict __value,
                   struct itimerspec *__restrict __ovalue);

/*$
 * requires: alive_resource(__timerid, Timer);
 * requires: valid_ptr(__value);
 *
 * case "success" {
 *   assigns: *__value;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int timer_gettime (timer_t __timerid, struct itimerspec *__value);

/*$
 * requires: alive_resource(__timerid, Timer);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int timer_getoverrun (timer_t __timerid);

/*$
 * requires: valid_ptr(__ts);
 * assigns: *__ts;
 *
 * case "success" {
 *   ensures: return == __base;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int timespec_get (struct timespec *__ts, int __base);

int getdate_err;

/*$
 * requires: valid_string(__string);
 *
 * case "success" {
 *   assigns: _mopsa_tm_buf;
 *   ensures: valid_primed_tm(_mopsa_tm_buf);
 *   ensures: return == &_mopsa_tm_buf;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   assigns: getdate_err;
 *   ensures: return == NULL;
 * }
 */
struct tm *getdate (const char *__string);

/*$
 * requires: valid_string(__string);
 * requires: valid_ptr(__resbufp);
 * assigns: *__resbufp;
 * ensures: valid_primed_tm(*__resbufp);
 */
int getdate_r (const char *__restrict __string,
               struct tm *__restrict __resbufp);
