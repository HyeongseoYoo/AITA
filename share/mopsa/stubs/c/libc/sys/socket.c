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

/* Stubs for <sys/socket.h> */
#include <stddef.h>
#include <sys/socket.h>
#include <errno.h>
#include "../mopsa_libc_utils.h"

/*
 * __SOCKADDR_ARG might be either a pointer or a (transparent) union
 * over __SOCKADDR_ALLTYPES list of pointers.
 * Try to cover both cases.
 */
#ifdef __SOCKADDR_ALLTYPES
#define __addr_ptr ((char*)__addr.__sockaddr__)
#else
#define __addr_ptr ((char*)__addr)
#endif


/*
 * TODO: sendmsg, sendmmsg, recvmsg, recvmmsg
 */

/*$
 * case "safe" {
 *   local:   void *f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int socket (int __domain, int __type, int __protocol);


/*$
 * case "safe" {
 *   local:   void *f0 = new FileRes;
 *   local:   void *f1 = new FileRes;
 *   local:   int fd0 = _mopsa_register_file_resource(f0);
 *   local:   int fd1 = _mopsa_register_file_resource(f1);
 *   assigns: __fds[0,1];
 *   ensures: (__fds[0])' == fd0 and (__fds[1])' == fd1;
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int socketpair (int __domain, int __type, int __protocol, int __fds[2]);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__addr_ptr, __len);
 *
 * case "safe" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int connect (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__addr_ptr, __len);
 *
 * case "safe" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int bind (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * assigns: _errno;
 * ensures: return in [-1, 0];
 */
int listen (int __fd, int __n);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __addr_ptr != NULL implies valid_ptr(__addr_len);
 * requires: __addr_ptr != NULL implies null_or_valid_bytes(__addr_ptr, *__addr_len);
 *
 * case "valid-addr" {
 *   assumes: __addr_ptr != NULL;
 *   local:   void *f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   assigns: __addr_ptr[0, *__addr_len);
 *   ensures: return == fd;
 * }
 *
 * case "null-addr" {
 *   assumes: __addr_ptr == NULL;
 *   local:   void *f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int accept (int __fd, __SOCKADDR_ARG __addr, socklen_t *__restrict __addr_len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__buf, __n);
 *
 * case "safe" {
 *   assigns: ((char*)__buf)[0, __n);
 *   ensures: return in [0, __n];
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t recv (int __fd, void *__buf, size_t __n, int __flags);

/*$
 * local: ssize_t r = recv(__fd, __buf, __n, __flags);
 * requires: __addr_ptr != NULL implies valid_ptr(__addr_len);
 * requires: null_or_valid_bytes(__addr_ptr, *__addr_len);
 * ensures: return == r;
 *
 * case "valid-addr" {
 *   assumes: __addr_ptr != NULL;
 *   assigns: __addr_ptr[0, *__addr_len);
 *   assigns: *__addr_len;
 *   ensures: (*__addr_len)' >= 0;
 * }
 *
 * case "null-addr" {
 *   assumes: __addr_ptr == NULL;
 * }
 */
ssize_t recvfrom (int __fd, void *__buf, size_t __n, int __flags,
                  __SOCKADDR_ARG __addr, socklen_t *__addr_len);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__buf, __n);
 *
 * case "safe" {
 *   ensures: return == __n;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t send (int __fd, const void *__buf, size_t __n, int __flags);


/*$
 * local: ssize_t r = send(__fd, __buf, __n, __flags);
 * requires: null_or_valid_bytes(__addr_ptr, __addr_len);
 * ensures: return == r;
 */
ssize_t sendto (int __fd, const void *__buf, size_t __n, int __flags,
                __CONST_SOCKADDR_ARG __addr, socklen_t __addr_len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__addr_ptr, *__len);
 * assigns: __addr_ptr[0, *__len);
 * assigns: *__len;
 * ensures: (*__len)' >= 0;
 *
 * case "OK" {
 *   ensures: return == 0;
 * }
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getsockname (int __fd, __SOCKADDR_ARG __addr, socklen_t *__restrict __len);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__addr_ptr, *__len);
 * assigns: __addr_ptr[0, *__len);
 * assigns: *__len;
 * ensures: (*__len)' >= 0;
 *
 * case "OK" {
 *   ensures: return == 0;
 * }
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getpeername (int __fd, __SOCKADDR_ARG __addr, socklen_t *__restrict __len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: null_or_valid_bytes(__optval, __optlen);
 *
 * case "OK" {
 *   ensures: return == 0;
 * }
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setsockopt (int __fd, int __level, int __optname,
                const void *__optval, socklen_t __optlen);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __optval != NULL implies valid_ptr(__optlen);
 * requires: null_or_valid_bytes(__optval, *__optlen);
 *
 * case "val" {
 *   assumes: __optval != NULL;
 *   assigns: ((char*)__optval)[0, *__optlen);
 *   assigns: *__optlen;
 *   ensures: return == 0;
 * }
 *
 * case "null" {
 *   assumes: __optval ==  NULL;
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getsockopt (int __fd, int __level, int __optname,
                void *__optval, socklen_t* __optlen);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
*
 * case "safe" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int shutdown (int __fd, int __how);
