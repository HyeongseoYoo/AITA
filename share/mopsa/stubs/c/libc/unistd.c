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
#include <unistd.h>
#include <limits.h>
#include "mopsa_libc_utils.h"
#include <fcntl.h> // for AT_FDCWD
#include <errno.h>


/*$
 * requires: valid_string(__name);
 * requires: __type in [0, __type];
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
int access (const char *__name, int __type);



/*$
 * local: int r = access(__name, __type);
 * ensures: return == r;
 */
int euidaccess (const char *__name, int __type);

/*$
 * local: int r = access(__name, __type);
 * ensures: return == r;
 */
int eaccess (const char *__name, int __type);


/*$
 * requires: valid_string(__file);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int faccessat (int __fd, const char *__file, int __type, int __flag);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __whence in [0, __whence];
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
__off_t lseek (int __fd, __off_t __offset, int __whence);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   free:    f;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int close (int __fd);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__buf, __nbytes);
 *
 * case "success" {
 *   assigns: ((char*)__buf)[0, __nbytes);
 *   ensures: return in [0, __nbytes];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t read (int __fd, void *__buf, size_t __nbytes);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__buf, __n);
 *
 * case "success" {
 *   ensures: return in [0, __n];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t write (int __fd, const void *__buf, size_t __n);

/*$
 * local:   ssize_t r = read(__fd, __buf, __nbytes);
 * ensures: return == r;
 */
ssize_t pread (int __fd, void *__buf, size_t __nbytes,
               __off_t __offset);

/*$
 * local:   ssize_t r = write(__fd, __buf, __n);
 * ensures: return == r;
 */
ssize_t pwrite (int __fd, const void *__buf, size_t __n,
                __off_t __offset);

/*$
 * case "success" {
 *   local:   void *f0 = new FileRes;
 *   local:   void *f1 = new FileRes;
 *   local:   int fd0 = _mopsa_register_file_resource(f0);
 *   local:   int fd1 = _mopsa_register_file_resource(f1);
 *   requires: valid_ptr_range(__pipedes, 0, 1);
 *   assigns: __pipedes[0,1];
 *   ensures: (__pipedes[0])' == fd0;
 *   ensures: (__pipedes[1])' == fd1;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int pipe (int __pipedes[2]);


/*$
 * local:   int r = pipe(__pipedes);
 * ensures: return == r;
 */
int pipe2 (int __pipedes[2], int __flags);

/*$
 * // empty contract
 */
unsigned int alarm (unsigned int __seconds);

/*$
 * ensures: return in [0,__seconds];
 */
unsigned int sleep (unsigned int __seconds);

/*$
 * assigns: _errno;
 */
__useconds_t ualarm (__useconds_t __value, __useconds_t __interval);

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
int usleep (__useconds_t __useconds);


/*$
 * // NOTE: waits until EINTR, so, always return an error!
 * assigns: _errno;
 * ensures: return == -1;
 */
int pause (void);

/*$
 * requires: valid_string(__file);
 * requires: __owner >= 0;
 * requires: __group >= 0;
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
int chown (const char *__file, __uid_t __owner, __gid_t __group);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __owner >= 0;
 * requires: __group >= 0;
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
int fchown (int __fd, __uid_t __owner, __gid_t __group);

/*$
 * requires: __owner >= 0;
 * requires: __group >= 0;
 * local: int r = chown(__file, __owner, __group);
 * ensures: return == r;
 */
int lchown (const char *__file, __uid_t __owner, __gid_t __group);


/*$
 * requires: valid_string(__file);
 * requires: __owner >= 0;
 * requires: __group >= 0;
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int fchownat (int __fd, const char *__file, __uid_t __owner,
              __gid_t __group, int __flag);


/*$
 * requires: valid_string(__path);
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
int chdir (const char *__path);


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
int fchdir (int __fd);

/*$
 * requires: null_or_valid_bytes(__buf, __size);
 *
 * case "noalloc" {
 *   assumes: __buf != NULL;
 *   assigns: __buf[0, __size);
 *   ensures: valid_primed_substring(__buf, __size);
 *   ensures: return == __buf;
 * }
 *
 * case "toosmall" {
 *   assumes: __buf != NULL;
 *   ensures: return == NULL;
 * }
 *
 * case "alloc" {
 *   assumes: __buf == NULL;
 *   local:   char* r = _mopsa_new_valid_string_max(PATH_MAX);
 *   ensures: return == r;
 * }
 *   
 *  case "failure" {
 *    assigns: _errno;
 *    ensures: return == NULL;
 * }
 */
char *getcwd (char *__buf, size_t __size);


/*$
 * case "success" {
 *   local:   char* r = _mopsa_new_valid_string_max(PATH_MAX);
 *   ensures: return == r;
 * }
 *   
 *  case "failure" {
 *    assigns: _errno;
 *    ensures: return == NULL;
 * }
 */
char *get_current_dir_name (void);


/*$
 * requires: valid_bytes(__buf, PATH_MAX);
 *
 * case "success" {
 *   assigns: __buf[0, PATH_MAX - 1];
 *   ensures: valid_primed_substring(__buf, PATH_MAX);
 *   ensures: return == __buf;
 * }
 *
 *  case "failure" {
 *    assigns: _errno;
 *    ensures: return == NULL;
 * }
 */
char *getwd (char *__buf);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   local:   void *f2 = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int dup (int __fd);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * local:    void* f2 = _mopsa_find_file_resource(__fd2);
 * requires: alive_resource(f, FileRes);
 *
 * case "newfd" {
 *   assumes: not f2 in FileRes;
 *   local: void *f3 = new FileRes;
 *   local: int x = _mopsa_register_file_resource_at(f3, __fd2);
 *   ensures: return == __fd2;
 * }
 *
 * case "reopen" {
 *   assumes: f2 in FileRes;
 *   ensures: return == __fd2;
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int dup2 (int __fd, int __fd2);


/*$
 * local:   int r = dup2(__fd, __fd2);
 * ensures: return == r;
 */
int dup3 (int __fd, int __fd2, int __flags);


// TODO: __environ

char **environ;

/*$!
 * assigns: environ;
 * local: char **addr = new Memory;
 * ensures: size(addr) >= 1 and size(addr) <= INT_MAX - 1;
 * ensures: environ' == addr;
 */

/*$
 * requires: valid_string(__path);
 * requires: forall size_t i in [0, size(__argv) - index(__argv)):
 *             valid_string(__argv[i]);
 * requires: forall size_t i in [0, size(__envp) - index(__envp)):
 *             valid_string(__envp[i]);
 *
 * case "success" {
 *   ensures: 1 == 0; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execve (const char *__path, char *const __argv[],
            char *const __envp[]);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: forall size_t i in [0, size(__argv) - index(__argv)):
 *             valid_string(__argv[i]);
 * requires: forall size_t i in [0, size(__envp) - index(__envp)):
 *             valid_string(__envp[i]);
 *
 * case "success" {
 *   ensures: 1 == 0; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int fexecve (int __fd, char *const __argv[], char *const __envp[]);


/*$
 * requires: valid_string(__path);
 * requires: valid_ptr(__argv);
 * requires: valid_ptr(__argv + 1);
 * requires: valid_string(__argv[0]);
 *
 * case "no-args" {
 *   assumes: __argv[1] == NULL;
 * }
 *
 * case "with-args" {
 *   assumes: __argv[1] != NULL;
 *   requires: forall size_t i in [1, size(__argv) - index(__argv)): valid_string(__argv[i]);
 * }
 *
 * case "success" {
 *   ensures: 1 == 0; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execv (const char *__path, char *const __argv[]);

/*$
 * // TODO: variable arguments
 * requires: valid_string(__path);
 * requires: valid_string(__arg);
 *
 * case "success" {
 *   ensures: 1 == 0; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execle (const char *__path, const char *__arg, ...);

/*$
 * // TODO: variable arguments
 * requires: valid_string(__path);
 * requires: valid_string(__arg);
 *
 * case "success" {
 *   ensures: 1 == 0; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execl (const char *__path, const char *__arg, ...);

/*$
 * requires: valid_string(__file);
 * requires: valid_ptr(__argv);
 *
 * assigns: _errno;
 * requires: exists size_t i in [0, size(__argv) - index(__argv)): (
 *    __argv[i] == NULL
 *    and forall size_t j in [0, i): valid_string(__argv[j])
 * );
 */
int execvp (const char *__file, char *const __argv[]);

/*$
 * // TODO: variable arguments
 * requires: valid_string(__file);
 * requires: valid_string(__arg);
 *
 * case "success" {
 *   ensures: 1 == 0; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execlp (const char *__file, const char *__arg, ...);


/*$
 * requires: valid_string(__file);
 * requires: forall size_t i in [0, size(__argv) - index(__argv)):
 *             valid_string(__argv[i]);
 * requires: forall size_t i in [0, size(__envp) - index(__envp)):
 *           valid_string(__envp[i]);
 *
 * case "success" {
 *   ensures: 1 == 0; // does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execvpe (const char *__file, char *const __argv[],
             char *const __envp[]);


/*$
 * case "success" { }
 * 
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int nice (int __inc);


/*$
 * ensures: 1 == 0; // does not terminate
 */
void _exit (int __status);

/*$
 * requires: valid_string(__path);
 *
 * case "success" { }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
long int pathconf (const char *__path, int __name);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" { }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
long int fpathconf (int __fd, int __name);

/*$
 * case "success" { }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
long int sysconf (int __name);


/*$
 * requires: null_or_valid_bytes(__buf, __len);
 *
 * case "copy" {
 *   assumes: __buf != NULL;
 *   assigns: __buf[0, size(__buf) - offset(__buf) - 1];
 *   ensures: valid_primed_string(__buf);
 *   ensures: return >= 1; // TODO: upper bound on return
 * }
 *
 * case "null" {
 *   assumes: __buf == NULL;
 *   ensures: return >= 1;
 * }
 *   
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == 0;
 * }
 */
size_t confstr (int __name, char *__buf, size_t __len);


/*$
 * ensures: return >= 0;
 */
__pid_t getpid (void);

/*$
 * ensures: return >= 0;
 */
__pid_t getppid (void);

/*$
 * ensures: return >= 0;
 */
__pid_t getpgrp (void);

/*$
 * requires: __pid >= 0;
 * ensures:  return >= 0;
 */
__pid_t __getpgid (__pid_t __pid);


/*$
 * requires: __pid >= 0;
 * ensures:  return >= 0;
 */
__pid_t getpgid (__pid_t __pid);


/*$
 * requires: __pid >= 0;
 * requires: __pgid >= 0;
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
int setpgid (__pid_t __pid, __pid_t __pgid);


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
int setpgrp (void);


/*$
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t setsid (void);


/*$
 * requires: __pid >= 0;
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
__pid_t getsid (__pid_t __pid);


/*$
 * ensures: return >= 0;
 */
__uid_t getuid (void);

/*$
 * ensures: return >= 0;
 */
__uid_t geteuid (void);

/*$
 * ensures: return >= 0;
 */
__gid_t getgid (void);

/*$
 * ensures: return >= 0;
 */
__gid_t getegid (void);

/*$
 * requires: __size >= 0;
 *
 * case "success" {
 *   assumes: __size > 0;
 *   assigns: __list[0, __size);
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getgroups (int __size, __gid_t __list[]);


/*$
 * requires: __gid >= 0;
 */
int group_member (__gid_t __gid);


/*$
 * requires: __uid >= 0;
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
int setuid (__uid_t __uid);


/*$
 * requires: __ruid >= 0;
 * requires: __euid >= 0;
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
int setreuid (__uid_t __ruid, __uid_t __euid);


/*$
 * requires: __uid >= 0;
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
int seteuid (__uid_t __uid);


/*$
 * requires: __gid >= 0;
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
int setgid (__gid_t __gid);


/*$
 * requires: __rgid >= 0;
 * requires: __egid >= 0;
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
int setregid (__gid_t __rgid, __gid_t __egid);


/*$
 * requires: __gid >= 0;
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
int setegid (__gid_t __gid);


/*$
 * case "success" {
 *   assigns: *__ruid;
 *   assigns: *__euid;
 *   assigns: *__suid;
 *   ensures: (*__ruid)' >= 0;
 *   ensures: (*__euid)' >= 0;
 *   ensures: (*__suid)' >= 0;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getresuid (__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid);

/*$
 * case "success" {
 *   assigns: *__rgid;
 *   assigns: *__egid;
 *   assigns: *__sgid;
 *   ensures: (*__rgid)' >= 0;
 *   ensures: (*__egid)' >= 0;
 *   ensures: (*__sgid)' >= 0;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getresgid (__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid);

/*$
 * requires: __ruid >= 0;
 * requires: __euid >= 0;
 * requires: __suid >= 0;
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
int setresuid (__uid_t __ruid, __uid_t __euid, __uid_t __suid);

/*$
 * requires: __rgid >= 0;
 * requires: __egid >= 0;
 * requires: __sgid >= 0;
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
int setresgid (__gid_t __rgid, __gid_t __egid, __gid_t __sgid);


/*$
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t fork (void);


/*$
 * #alias fork;
 */
__pid_t vfork (void);


static char _ttyname_buf[PATH_MAX];

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   assigns: _ttyname_buf[0, PATH_MAX - 1];
 *   ensures: valid_primed_substring(_ttyname_buf, PATH_MAX);
 *   ensures: return == (char*)&_ttyname_buf[0];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *ttyname (int __fd);

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
int ttyname_r (int __fd, char *__buf, size_t __buflen);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   ensures: return == 1;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == 0;
 * }
 */
int isatty (int __fd);


/*$
 * ensures: return >= -1;
 */
int ttyslot (void);


/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
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
int link (const char *__from, const char *__to);


/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
 * local:    void* f1 = _mopsa_find_file_resource(__fromfd);
 * local:    void* f2 = _mopsa_find_file_resource(__tofd);
 * requires: __fromfd == AT_FDCWD or alive_resource(f1, FileRes);
 * requires: __tofd == AT_FDCWD or alive_resource(f2, FileRes);
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
int linkat (int __fromfd, const char *__from, int __tofd,
            const char *__to, int __flags);


/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
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
int symlink (const char *__from, const char *__to);

/*$
 * requires: valid_string(__path);
 * requires: valid_bytes(__buf, __len);
 *
 * case "success" {
 *   assigns: __buf[0, __len);
 *   ensures: return in [0,__len];
 *   // no 0 added, but returns the position where to put the 0
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t readlink (const char *__restrict __path,
                  char *__restrict __buf, size_t __len);


/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
 * local:    void* f = _mopsa_find_file_resource(__tofd);
 * requires: __tofd == AT_FDCWD or alive_resource(f, FileRes);
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
int symlinkat (const char *__from, int __tofd,
               const char *__to);

/*$
 * requires: valid_string(__path);
 * requires: valid_bytes(__buf, __len);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
 *
 * case "success" {
 *   assigns: __buf[0, __len);
 *   ensures: return in [0, __len];
 *   // no 0 added, but returns the position where to put the 0
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t readlinkat (int __fd, const char *__restrict __path,
                    char *__restrict __buf, size_t __len);


/*$
 * requires: valid_string(__name);
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
int unlink (const char *__name);


/*$
 * requires: valid_string(__name);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: __fd == AT_FDCWD or alive_resource(f, FileRes);
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
int unlinkat (int __fd, const char *__name, int __flag);


/*$
 * requires: valid_string(__path);
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
int rmdir (const char *__path);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
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
__pid_t tcgetpgrp(int __fd);

/*$
  * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __pgrp_id >= 0;
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
int tcsetpgrp (int __fd, __pid_t __pgrp_id);

static char _getlogin_buf[LOGIN_NAME_MAX];

/*$
 * assigns: _getlogin_buf[0, LOGIN_NAME_MAX - 1];
 * ensures: valid_primed_substring(_getlogin_buf, LOGIN_NAME_MAX);
 * ensures: return == (char*)&_getlogin_buf[0];
 */
char *getlogin (void);


/*$
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
 *   ensures: return == -1;
 * }
 */
int getlogin_r (char *__buf, size_t __buflen);


/*$
 * requires: valid_string(__name);
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
int setlogin (const char *__name);


/*$
 * requires: valid_bytes(__name, __len);
 *
 * case "success" {
 *   // NOTE: 0 added only if the buffer is large enough, which we cannot assume
 *   assigns: __name[0, __len);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int gethostname (char *__name, size_t __len);


/*$
 * requires: valid_bytes(__name, __len);
 * // NOTE: no terminal 0 required
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
int sethostname (const char *__name, size_t __len);

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
int sethostid (long int __id);

/*$
 * requires: valid_bytes(__buf, __buflen);
 *
 * case "success" {
 *   // NOTE: 0 added only if the buffer is large enugh, which we cannot assume
 *   assigns: __buf[0, __buflen);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getdomainname (char *__buf, size_t __buflen);

/*$
 * requires: valid_bytes(__name, __len);
 * // NOTE: no terminal 0 required
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
int setdomainname (const char *__name, size_t __len);

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
int vhangup (void);

/*$
 * warn: "undocumented call";
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
int revoke (const char *__file);

/*$
 * warn: "unimplemented call";
 * assigns: _errno;
 * ensures: return == -1;
 */
int profil (unsigned short int *__sample_buffer, size_t __size,
            size_t __offset, unsigned int __scale);

/*$
 * requires: valid_string(__name);
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
int acct (const char *__name);

// size and allocation policy not specified; using a static PATH buffer
static char _getusershell_buf[PATH_MAX];  

/*$
 * assigns: _getusershell_buf[0, PATH_MAX - 1];
 * ensures: valid_primed_substring(_getusershell_buf, PATH_MAX);
 * ensures: return == (char*)&_getusershell_buf[0];
 */
char *getusershell (void);

/*$
 * // empty contract
 */
void endusershell (void);

/*$
 * // empty contract
 */
void setusershell (void);

/*$
 * warn: "undocumented call";
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
int daemon (int __nochdir, int __noclose);


/*$
 * requires: valid_string(__path);
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
int chroot (const char *__path);


// NOTE: GlibC does not define PASS_MAX as password can be arbitrary long
// for now, we define it
// TODO: get rid of the constant

#ifndef PASS_MAX
#define PASS_MAX 4096
#endif


static char _getpass_buf[PASS_MAX];

/*$
 * warn: "obsolete function, do not call";
 *
 * requires: valid_string(__prompt);
 *
 * case "success" {
 *   assigns: _getpass_buf[0, PASS_MAX - 1];
 *   ensures: valid_primed_substring(_getpass_buf, PASS_MAX);
 *   ensures: return == (char*)&_getpass_buf[0];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *getpass (const char *__prompt);


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
int fsync (int __fd);


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
int syncfs (int __fd);


/*$
 * //empty contract
 */
long int gethostid (void);

/*$
 * //empty contract
 */
void sync (void);


/*$
 * warn: "not portable; use sysconf(SC_PAGESIZE) instead";
 * ensures: return >= 1;
 */
int getpagesize (void);

/*$
 * warn: "not portable; use sysconf(SC_OPEN_MAX) instead";
 * ensures: return >= 1;
 */
int getdtablesize (void);


/*$
 * requires: valid_string(__file);
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
int truncate (const char *__file, __off_t __length);


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
int ftruncate (int __fd, __off_t __length);


/*$
 * unsound: "the effect of brk is ignored";
 * assigns: _errno;
 */
int brk (void *__addr);

/*$
 * unsound: "the effect of sbrk is ignored";
 * assigns: _errno;
 */
void *sbrk (intptr_t __delta);

/*$
 * unsound: "the effect of syscall is ignored";
 * assigns: _errno;
 */
long int syscall (long int __sysno, ...);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: __cmd in [0, 3];
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
int lockf (int __fd, int __cmd, __off_t __len);


/*$
 * local:    void* f1 = _mopsa_find_file_resource(__infd);
 * requires: alive_resource(f1, FileRes);
 * local:    void* f2 = _mopsa_find_file_resource(__outfd);
 * requires: alive_resource(f2, FileRes);
 * requires: null_or_valid_ptr(__pinoff);
 * requires: null_or_valid_ptr(__poutoff);
 *
 * case "pinoff" {
 *   assumes: __pinoff != NULL;
 *   assigns: *__pinoff;
 * }
 *
 * case "poutoff" {
 *   assumes: __poutoff != NULL;
 *   assigns: *__poutoff;
 * }
 *
 * case "success" {
 *   ensures: return in [0, __length];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t copy_file_range (int __infd, __off64_t *__pinoff,
			 int __outfd, __off64_t *__poutoff,
			 size_t __length, unsigned int __flags);

/*$
 * local:    void* f = _mopsa_find_file_resource(__fildes);
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
int fdatasync (int __fildes);


/*$
 * // TODO: allocation policy is not documented, assuming dynamic allocation
 *
 * requires: valid_string(__key);
 * requires: valid_bytes(__salt, 2);
 * 
 * case "succes" {
 *   local: char* r = _mopsa_new_readonly_string_max(size(__key) - offset(__key));
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *crypt (const char *__key, const char *__salt);

/*$
 * requires: __edflag in [0, 1];
 * requires: valid_bytes(__glibc_block, 8); // en/decode 64-bit messages
 * assigns: ((unsigned char*)__glibc_block)[0, 7];
 * assigns: _errno;
 */
void encrypt (char *__glibc_block, int __edflag);

/*$
 * requires: valid_bytes(__from, __n);
 * requires: valid_bytes(__to, __n);
 *
 * case "odd" {
 *   assumes: __n >= 0 and (__n & 1) == 0;
 *   assigns: ((unsigned char*)__to)[0, __n);
 *   ensures: forall ssize_t i in [0, __n): ((unsigned char*)__from)[i] ==  (((unsigned char*)__to)[i ^ 1])';
 * }
 *
 * case "even" {
 *   assumes: __n >= 1 and (__n & 1) == 1;
 *   assigns: ((unsigned char*)__to)[0, __n - 1);
 *   // the last byte is unspecified
 *   ensures: forall ssize_t i in [0, __n - 1): ((unsigned char*)__from)[i] ==  (((unsigned char*)__to)[i ^ 1])';
 * }
 *
 * case "negative" {
 *   assumes: __n < 0;
 * }
 */
void swab (const void *__restrict __from, void *__restrict __to,
           ssize_t __n);


// NOTE: ctermid and cuserid defined in stdio.c
// pthread_atfork should be defined in pthread.c


/*$
 * requires: __length <= 256;
 * requires: valid_bytes(__buffer, __length);
 *
 * case "success" {
 *   assigns: ((unsigned char*)__buffer)[0, __length);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getentropy (void *__buffer, size_t __length);

