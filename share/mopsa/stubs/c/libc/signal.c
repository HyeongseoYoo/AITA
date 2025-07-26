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
#include <signal.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * #alias signal;
 */
__sighandler_t __sysv_signal (int __sig, __sighandler_t __handler);

/*$
 * #alias signal;
 */
__sighandler_t sysv_signal (int __sig, __sighandler_t __handler);

/*$
 * assigns: _errno;
 * ensures: return == NULL;
 * unsound: "signal ignores its callback handler";
 */
__sighandler_t signal (int __sig, __sighandler_t __handler);

/*$
 * #alias signal;
 */
__sighandler_t bsd_signal (int __sig, __sighandler_t __handler);

/*$
 * unsound: "the effect of kill is ignored";
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
int kill (__pid_t __pid, int __sig);

/*$
 * #alias kill;
 */
int killpg (__pid_t __pgrp, int __sig);

/*$
 * unsound: "the effect of raise is ignored";
 */
int raise (int __sig);

/*$
 * warn: "ssignal is not correctly implemented under Linux";
 */
__sighandler_t ssignal (int __sig, __sighandler_t __handler);

/*$
 * warn: "gsignal is not correctly implemented under Linux";
 */
int gsignal (int __sig);

/*$
 * requires: valid_string(__s);
 */
void psignal (int __sig, const char *__s);

/*$
 * requires: valid_string(__s);
 * requires: valid_ptr(__pinfo);
 */
void psiginfo (const siginfo_t *__pinfo, const char *__s);

/*$
 * warn: "sigpause is deprecated";
 * assigns: _errno;
 * ensures: return == -1;
 */
int sigpause (int __sig);

/*$
 * // empty
 */
int sigblock (int __mask);

/*$
 * // empty
 */
int sigsetmask (int __mask);

/*$
 * // empty
 */
int siggetmask (void);

/*$
 * requires: valid_ptr(__set);
 * assigns: *__set;
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
int sigemptyset (sigset_t *__set);

/*$
 * requires: valid_ptr(__set);
 * assigns: *__set;
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
int sigfillset (sigset_t *__set);

/*$
 * requires: valid_ptr(__set);
 * assigns: *__set;
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
int sigaddset (sigset_t *__set, int __signo);

/*$
 * requires: valid_ptr(__set);
 * assigns: *__set;
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
int sigdelset (sigset_t *__set, int __signo);

/*$
 * requires: valid_ptr(__set);
 *
 * case "success" {
 *   ensures: return in [0,1];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigismember (const sigset_t *__set, int __signo);

/*$
 * requires: valid_ptr(__set);
 *
 * case "success" {
 *   ensures: return in [0,1];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigisemptyset (const sigset_t *__set);

/*$
 * requires: valid_ptr(__set);
 * requires: valid_ptr(__left);
 * requires: valid_ptr(__right);
 * assigns: *__set;
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
int sigandset (sigset_t *__set, const sigset_t *__left,
               const sigset_t *__right);

/*$
 * requires: valid_ptr(__set);
 * requires: valid_ptr(__left);
 * requires: valid_ptr(__right);
 * assigns: *__set;
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
int sigorset (sigset_t *__set, const sigset_t *__left,
              const sigset_t *__right);

/*$
 * requires: __set != NULL implies valid_ptr(__set);
 * requires: __oset != NULL implies valid_ptr(__oset);
 *
 * case "oset" {
 *   assumes: __oset != NULL;
 *   assigns: *__oset;
 *   ensures: return == 0;
 * }
 *
 * case "nooset" {
 *   assumes: __oset == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigprocmask (int __how, const sigset_t *__restrict __set,
                 sigset_t *__restrict __oset);

/*$
 * requires: valid_ptr(__set);
 * assigns: _errno;
 * ensures: return == -1;
 */
int sigsuspend (const sigset_t *__set);

/*$
 * requires: __act != NULL implies valid_ptr(__act);
 * requires: __oact != NULL implies valid_ptr(__oact);
 * unsound: "sigaction ignores its callback handler";
 *
 * case "oact" {
 *   assumes: __oact != NULL;
 *   assigns: *__oact;
 *   ensures: return == 0;
 * }
 *
 * case "noact" {
 *   assumes: __oact == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigaction (int __sig, const struct sigaction *__restrict __act,
               struct sigaction *__restrict __oact);

/*$
 * requires: valid_ptr(__set);
 * assigns: *__set;
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
int sigpending (sigset_t *__set);

/*$
 * requires: valid_ptr(__set);
 * requires: valid_ptr(__sig);
 * assigns: *__sig;
 * ensures: return >= 0;
 */
int sigwait (const sigset_t *__restrict __set, int *__restrict __sig);

/*$
 * requires: valid_ptr(__set);
 * requires: __info != NULL implies valid_ptr(__info);
 *
 * case "info" {
 *   assumes: __info != NULL;
 *   assigns: *__info;
 *   ensures: return >= 0;
 * }
 *
 * case "noinfo" {
 *   assumes: __info == NULL;
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigwaitinfo (const sigset_t *__restrict __set,
                 siginfo_t *__restrict __info);

/*$
 * requires: valid_ptr(__timeout);
 * local: int r = sigwaitinfo(__set,__info);
 * ensures: return == r;
 */
int sigtimedwait (const sigset_t *__restrict __set,
                  siginfo_t *__restrict __info,
                  const struct timespec *__restrict __timeout);

/*$
 * unsound: "the effect of sigqueue is ignored";
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
int sigqueue (__pid_t __pid, int __sig, const union sigval __val);


const char *const _sys_siglist[_NSIG];
const char *const sys_siglist[_NSIG];

/*$!
 * assigns: _sys_siglist[0,_NSIG);
 * assigns: sys_siglist[0,_NSIG);
 * local: char* s = _mopsa_new_readonly_string();
 * ensures: forall int i in [0,_NSIG): (_sys_siglist[i])' == s;
 * ensures: forall int i in [0,_NSIG): (sys_siglist[i])' == s;
 * // TODO: allocate different strings for each array element
 */

/*$
 * ensures: 1 == 0;
 */
int sigreturn (struct sigcontext *__scp);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int siginterrupt (int __sig, int __interrupt);

/*$
 * requires: __oss != NULL implies valid_ptr(__oss);
 * requires: __ss != NULL implies (valid_ptr(__ss) and valid_bytes(__ss->ss_sp, __ss->ss_size));
 *
 * case "oss" {
 *   assumes: __oss != NULL;
 *   assigns: *__oss;
 *   ensures: return == 0;
 * }
 *
 * case "nooss" {
 *   assumes: __oss == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigaltstack (const stack_t *__restrict __ss,
                 stack_t *__restrict __oss);

/*$
 * requires: __oss != NULL implies valid_ptr(__oss);
 * requires: __ss != NULL implies valid_ptr(__ss);
 * warn: "sigstack is deprecated and dangerous";
 *
 * case "oss" {
 *   assumes: __oss != NULL;
 *   assigns: *__oss;
 *   ensures: return == 0;
 * }
 *
 * case "nooss" {
 *   assumes: __oss == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigstack (struct sigstack *__ss, struct sigstack *__oss);

/*$
 * case "success" {
 *   ensures: return in [0,1];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sighold (int __sig);

/*$
 * case "success" {
 *   ensures: return in [0,1];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigrelse (int __sig);

/*$
 * case "success" {
 *   ensures: return in [0,1];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sigignore (int __sig);

/*$
 * unsound: "sigaction ignores its callback handler";
 * ensures: return == NULL;
 */
__sighandler_t sigset (int __sig, __sighandler_t __disp);

/*$
 * // based on Linux RT signal ranges
 * ensures: return == 32;
 */
int __libc_current_sigrtmin (void);

/*$
 * // based on Linux RT signal ranges
 * ensures: return == 64;
 */
int __libc_current_sigrtmax (void);


/*$
 * local: int r = sigprocmask(__how, __newmask, __oldmask);
 * ensures: return == r;
 */
int pthread_sigmask (int __how,
                     const __sigset_t *__restrict __newmask,
                     __sigset_t *__restrict __oldmask);

/*$
* unsound: "the effect of pthread_kill is ignored";
 */
int pthread_kill (pthread_t __threadid, int __signo);
