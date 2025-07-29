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
#include <arpa/inet.h>
#include <errno.h>
#include "../mopsa_libc_utils.h"

/*$
 * requires: valid_string(__cp);
 */
in_addr_t inet_addr (const char *__cp);

/*$
 * // empty
 */
in_addr_t inet_lnaof (struct in_addr __in);

/*$
 * // empty
 */
struct in_addr inet_makeaddr (in_addr_t __net, in_addr_t __host);

/*$
 * // empty
 */
in_addr_t inet_netof (struct in_addr __in);

/*$
 * requires: valid_string(__cp);
 */
in_addr_t inet_network (const char *__cp);

static char _mopsa_inet_buf[16];

/*$
 * assigns: _mopsa_inet_buf;
 * ensures: valid_primed_substring(_mopsa_inet_buf, 16);
 * ensures: return == _mopsa_inet_buf;
 */
char *inet_ntoa (struct in_addr __in);

/*$
 * requires: valid_string(__cp);
 * requires: __af == AF_INET or __af == AF_INET6;
 *
 * case "ipv4" {
 *   assumes: __af == AF_INET;
 *   requires: valid_bytes(__buf, 4);
 *   assigns: ((unsigned char*)__buf)[0,4);
 * }
 *
 * case "ipv6" {
 *   assumes: __af == AF_INET6;
 *   requires: valid_bytes(__buf, 16);
 *   assigns: ((unsigned char*)__buf)[0,16);
 * }
 */
int inet_pton (int __af, const char *__restrict __cp,
               void *__restrict __buf);

/*$
 * requires: valid_ptr(__cp);
 * requires: valid_bytes(__buf, __len);
 *
 * case "success" {
 *   assigns: __buf[0,__len);
 *   ensures: valid_primed_substring(__buf, __len);
 *   ensures: return == __buf;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
const char *inet_ntop (int __af, const void *__restrict __cp,
                       char *__restrict __buf, socklen_t __len);

/*$
 * requires: valid_string(__cp);
 * requires: valid_ptr(__inp);
 * assigns: *__inp;
 * ensures: return in [0,1];
 */
int inet_aton (const char *__cp, struct in_addr *__inp);

/*$
 * requires: valid_bytes(__buf, __len);
 *
 * case "success" {
 *   assigns: __buf[0,__len);
 *   ensures: valid_primed_substring(__buf, __len);
 *   ensures: return == __buf;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *inet_neta (in_addr_t __net, char *__buf, size_t __len);

/*$
 * requires: valid_bytes(__buf, __len);
 * requires: valid_bytes(__cp, (__bits + 7) / 8);
 *
 * case "success" {
 *   assigns: __buf[0,__len);
 *   ensures: valid_primed_substring(__buf, __len);
 *   ensures: return == __buf;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *inet_net_ntop (int __af, const void *__cp, int __bits,
                     char *__buf, size_t __len);

/*$
 * requires: valid_string(__cp);
 * requires: valid_bytes(__buf, __len);
 *
 * case "success" {
 *   assigns: __buf[0,__len);
 *   ensures: return in [0, 8 * __len];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int inet_net_pton (int __af, const char *__cp,
                   void *__buf, size_t __len);

// undocumented
unsigned int inet_nsap_addr (const char *__cp,
                             unsigned char *__buf, int __len);

// undocumented
char *inet_nsap_ntoa (int __len, const unsigned char *__cp,
                      char *__buf);
