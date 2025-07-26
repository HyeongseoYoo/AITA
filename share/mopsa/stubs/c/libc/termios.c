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
#include <termios.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/* Stubs for functions provided by termios.h */

/*$
 * requires: valid_ptr(__termios_p);
 * ensures: return == __termios_p->c_ospeed;
 */
speed_t cfgetospeed (const struct termios *__termios_p);

/*$
 * requires: valid_ptr(__termios_p);
 * ensures: return == __termios_p->c_ispeed;
 */
speed_t cfgetispeed (const struct termios *__termios_p);

/*$
 * requires: valid_ptr(__termios_p);
 * assigns: __termios_p->c_ospeed;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int cfsetospeed (struct termios *__termios_p, speed_t __speed);

/*$
 * requires: valid_ptr(__termios_p);
 * assigns: __termios_p->c_ispeed;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int cfsetispeed (struct termios *__termios_p, speed_t __speed);

//#ifdef __USE_MISC

/*$
 * requires: valid_ptr(__termios_p);
 * assigns: __termios_p->c_ispeed;
 * assigns: __termios_p->c_ospeed;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int cfsetspeed (struct termios *__termios_p, speed_t __speed);

//#endif

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * assigns: __termios_p;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int tcgetattr (int __fd, struct termios *__termios_p);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_ptr(__termios_p);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int tcsetattr (int __fd, int __optional_actions,
               const struct termios *__termios_p);

//#ifdef __USE_MISC

/*$
 * assigns: __termios_p;
 */
void cfmakeraw (struct termios *__termios_p);

//#endif

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int tcsendbreak (int __fd, int __duration);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int tcdrain (int __fd);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int tcflush (int __fd, int __queue_selector);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
int tcflow (int __fd, int __action);

//#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }  
 */
__pid_t tcgetsid (int __fd);

//#endif
