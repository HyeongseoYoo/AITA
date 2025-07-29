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
#include <stdarg.h>
#include <stdio.h>
#include <wchar.h>
#include <fcntl.h> // for AT_FDCWD
#include <errno.h>
#include "mopsa_libc_utils.h"

FILE *stdin;
FILE *stdout;
FILE *stderr;

#define STD_BUFFER_SIZE 100
char _std_buffer[STD_BUFFER_SIZE];

/*$
 * local: void *f = new FileRes;
 * local: int fd = _mopsa_register_file_resource(f);
 * local: FILE* file = new File;
 * ensures: bytes(file) == sizeof_type(FILE);
 * ensures: file->_fileno == fd;
 * ensures: exists size_t i in [0, STD_BUFFER_SIZE - 1]: file->_IO_read_ptr == &(_std_buffer[i]);
 * ensures: exists size_t i in [0, STD_BUFFER_SIZE - 1]: file->_IO_read_end == &(_std_buffer[i]);
 * ensures: exists size_t i in [0, STD_BUFFER_SIZE - 1]: file->_IO_read_base == &(_std_buffer[i]);
 * ensures: exists size_t i in [0, STD_BUFFER_SIZE - 1]: file->_IO_write_ptr == &(_std_buffer[i]);
 * ensures: exists size_t i in [0, STD_BUFFER_SIZE - 1]: file->_IO_write_end == &(_std_buffer[i]);
 * ensures: exists size_t i in [0, STD_BUFFER_SIZE - 1]: file->_IO_write_base == &(_std_buffer[i]);
 * ensures: return == file;
 */
FILE *_alloc_std_stream();



/*$!
 * assigns: stdin;
 * assigns: stdout;
 * assigns: stderr;
 * local:   FILE* fstdin = _alloc_std_stream();
 * local:   FILE* fstdout = _alloc_std_stream();
 * local:   FILE* fstderr = _alloc_std_stream();
 * ensures: stdin' == fstdin;
 * ensures: stdout' == fstdout;
 * ensures: stderr' == fstderr;
 */



/*$
 * local: void *f = new FileRes;
 * local: int fd = _mopsa_register_file_resource(f);
 * local: FILE* file = new File;
 * ensures: bytes(file) == sizeof_type(FILE);
 * ensures: file->_fileno == fd;
 * ensures: return == file;
 */
FILE *_alloc_FILE();



/*$
 * local: void *f = new FileRes;
 * local: int ffd = _mopsa_register_file_resource_at(f, fd);
 * local: FILE* file = new File;
 * ensures: bytes(file) == sizeof_type(FILE);
 * ensures: file->_fileno == fd;
 * ensures: return == file;
 */
FILE *_alloc_FILE_at(int fd);


const char *const sys_errlist[128]; // TODO: actual size

/*$
 * requires: valid_string_or_fail(__filename);
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
int remove (const char *__filename);

/*$
 * requires: valid_string_or_fail(__old);
 * requires: valid_string_or_fail(__new);
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
int rename (const char *__old, const char *__new);


/*$
 * requires: valid_string_or_fail(__old);
 * requires: valid_string_or_fail(__new);
 * local:    void* f1 = _mopsa_find_file_resource(__oldfd);
 * local:    void* f2 = _mopsa_find_file_resource(__newfd);
 * requires: __oldfd == AT_FDCWD or alive_resource(f1, FileRes);
 * requires: __newfd == AT_FDCWD or alive_resource(f2, FileRes);
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
int renameat (int __oldfd, const char *__old, int __newfd, const char *__new);

/*$
 * local: int r = renameat(__oldfd, __old, __newfd, __new);
 * ensures: return == r;
 */
extern int renameat2 (int __oldfd, const char *__old, int __newfd,
		      const char *__new, unsigned int __flags);

/*$
 * case "success" {
 *   local:   FILE* f = _alloc_FILE();
 *   ensures: return == f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *tmpfile (void);

static char _tmpnam_buf[L_tmpnam];

/*$
 * warn: "avoid using function tmpnam";
 *
 * requires: null_or_valid_bytes_or_fail(__s, L_tmpnam);
 *
 * case "use_s" {
 *   assumes: __s != NULL;
 *   assigns: __s[0, L_tmpnam);
 *   ensures: valid_primed_substring(__s, L_tmpnam);
 *   ensures: return == __s;
 * }
 * 
 * case "use_static" {
 *   assumes: __s == NULL;
 *   assigns: _tmpnam_buf[0, size(_tmpnam_buf));
 *   ensures: valid_primed_string(_tmpnam_buf);
 *   ensures: return == _tmpnam_buf;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *tmpnam (char *__s);


/*$
 * warn: "avoid using function tmpnam_r";
 *
 * requires: valid_bytes_or_fail(__s, L_tmpnam);
 *
 * case "success" {
 *   assigns: __s[0, L_tmpnam);
 *   ensures: valid_primed_substring(__s, L_tmpnam);
 *   ensures: return == __s;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *tmpnam_r (char *__s);


/*$
 * warn: "tempnam is obsolete, do not call";
 */
char *tempnam (const char *__dir, const char *__pfx);


/*$
 * requires: __stream in File;
 *
 * case "success" {
 *   local:   void *f = _mopsa_find_file_resource(__stream->_fileno);
 *   ensures: return == 0;
 *   free:    __stream;
 *   free:    f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int fclose (FILE *__stream);


/*$
 * case "success" {
 *   assumes:  __stream != NULL;
 *   requires: __stream in File;
 *   ensures: return == 0;
 * }
 *
 * case "flushall" {
 *   assumes: __stream == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int fflush (FILE *__stream);


/*$
 * #alias fflush;
 */
int fflush_unlocked (FILE *__stream);

/*$
 * unsound: "fcloseall effect ignored";
 */
int fcloseall(void);

/*$
 * requires: valid_string_or_fail(__filename);
 * requires: valid_string_or_fail(__modes);
 *
 * case "success" {
 *   local:   FILE* r = _alloc_FILE();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *fopen (const char *__restrict __filename,
             const char *__restrict __modes);

/*$
 * requires: valid_string_or_fail(__filename);
 * requires: valid_string_or_fail(__modes);
 * requires: alive_resource(__stream, File);
 *
 * case "success" {
 *   ensures: return == __stream;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *freopen (const char *__restrict __filename,
               const char *__restrict __modes,
               FILE *__restrict __stream);


/*$
 * requires: valid_string_or_fail(__modes);
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "success" {
 *   local:   FILE* f = _alloc_FILE_at(__fd);
 *   ensures: bytes(f) == sizeof_type(FILE);
 *   ensures: return == f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *fdopen (int __fd, const char *__modes);



// TODO: omitted __USE_GNU fopencookie


/*$
 * // TODO: if __s != NULL, the buffer pointed to by __s becomes volatile
 *
 * requires: valid_string_or_fail(__modes);
 * requires: null_or_valid_bytes_or_fail(__s, __len);
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
FILE *fmemopen (void *__s, size_t __len, const char *__modes);

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
FILE *open_memstream (char **__bufloc, size_t *__sizeloc);


/*$
 * // TODO: __buf becomes volatile
 *
 * requires: alive_resource(__stream, File);
 * requires: null_or_valid_bytes_or_fail(__buf, BUFSIZ);
 */
void setbuf (FILE *__restrict __stream, char *__restrict __buf);

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: alive_resource(__stream, File);
 * requires: null_or_valid_bytes_or_fail(__buf, __n);
 * requires: __modes in [0, 2];
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return != 0;
 * }
 */
int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
             int __modes, size_t __n);


/*$
 * // TODO: __buf becomes volatile
 *
 * requires: alive_resource(__stream, File);
 * requires: null_or_valid_bytes_or_fail(__buf, __size);
 */
void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
                size_t __size);

/*$
 * requires: alive_resource(__stream, File);
 */
void setlinebuf (FILE *__stream);


/* built-in */
int fprintf (FILE *__restrict __stream,
             const char *__restrict __format, ...);

/* built-in */
int dprintf (int __fd, const char *__restrict __format, ...);

/* built-in */
int printf (const char *__restrict __format, ...);

/* built-in */
extern int sprintf (char *__restrict __s,
                    const char *__restrict __format, ...);

/* built-in */
int snprintf (char *__restrict __s, size_t __maxlen,
              const char *__restrict __format, ...);

/* built-in */
int asprintf (char **__restrict __ptr,
              const char *__restrict __fmt, ...);

/*$
 * // called at the end of asprintf's stub, after checking the format
 *
 * case "success" {
 *   local: char* r = _mopsa_new_valid_string();
 *   local: size_t l = strlen(r);
 *   assigns: *strp;
 *   ensures: (*strp)' == r;
 *   ensures: return >= 0 and return == l;
 * }
 *
 * case "failure" {
 *   ensures: return == -1;
 *  }
 */
int _mopsa_asprintf(char** strp);


/* built-in */
int fscanf (FILE *__restrict __stream,
            const char *__restrict __format, ...);

/* built-in */
int scanf (const char *__restrict __format, ...);

/* built-in */
int sscanf (const char *__restrict __s,
            const char *__restrict __format, ...) ;


/*$
 * requires: alive_resource(__stream, File);
 * ensures: return in [0, 255] or return == EOF;
 */
int fgetc (FILE *__stream);


#ifndef getc

/*$
 * #alias fgetc;
 */
int getc (FILE *__stream);

#endif



/*$
 * ensures: return in [0, 255] or return == EOF;
 */
int getchar (void);



/*$
 * #alias fgetc;
 */
int getc_unlocked (FILE *__stream);

/*$
 * #alias getchar;
 */
int getchar_unlocked (void);


/*$
 * #alias fgetc;
 */
int fgetc_unlocked (FILE *__stream);


/*$
 * requires: alive_resource(__stream, File);
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int fputc (int __c, FILE *__stream);


/*
 Before glibc 2.28, putc is defined as a macro for _IO_putc
*/
#if __GLIBC_MINOR__ <= 27

/*$
 * #alias fputc;
 */
int _IO_putc (int __c, _IO_FILE *__stream);

#else

/*$
 * #alias fputc;
 */
int putc (int __c, FILE *__stream);

#endif


/*$
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int putchar (int __c);

/*$
 * #alias fputc;
 */
int fputc_unlocked (int __c, FILE *__stream);

/*$
 * #alias fputc;
 */
int putc_unlocked (int __c, FILE *__stream);

/*$
 * #alias putchar;
 */
int putchar_unlocked (int __c);


/*$
 * requires: alive_resource(__stream, File);
 */
int getw (FILE *__stream);

/*$
 * requires: alive_resource(__stream, File);
 * ensures:  (return == 0) or (return == EOF);
 */
int putw (int __w, FILE *__stream);


/*$
 * requires: alive_resource(__stream, File);
 * requires: __n >= 0;
 * requires: valid_bytes_or_fail(__s, __n);
 * assigns:  __s[0, __n);
 * ensures:  valid_primed_substring(__s, __n);
 * ensures:  (return == __s) or (return == NULL);
 */
char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);

/*$
 * warn: "gets: dangerous function, do not use";
 * assigns: __s[0, (bytes(__s) - offset(__s)));
 * ensures: (return == __s) or (return == NULL);
 */
char *gets (char *__s);

/*$
 * #alias fgets;
 */
char *fgets_unlocked (char *__restrict __s, int __n,
                      FILE *__restrict __stream);


/*$
 * requires: alive_resource(__stream, File);
 * requires: null_or_valid_bytes_or_fail(*__lineptr, *__n);
 * 
 * case "realloc" {
 *   assumes:  *__lineptr != NULL;
 *   local:    char* r = new Memory;
 *   assigns:  *__lineptr;
 *   assigns:  *__n;
 *   free:     *__lineptr;
 *   ensures:  size(r) > 0;
 *   ensures:  size(r) == (*__n)';
 *   ensures:  r[size(r) - 1] == 0;
 *   ensures:  (*__lineptr)' == r;
 *   ensures:  return >= 0;
 * }
 *
 * case "alloc" {
 *   assumes: *__lineptr == NULL;
 *   local:   char* r = new Memory;
 *   assigns: *__lineptr;
 *   assigns: *__n;
 *   ensures: size(r) > 0;
 *   ensures: size(r) == (*__n)';
 *   ensures:  r[size(r) - 1] == 0;
 *   ensures: (*__lineptr)' == r;
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t getdelim (char **__restrict __lineptr,
                  size_t *__restrict __n, int __delimiter,
                  FILE *__restrict __stream);

/*$
 * local:   size_t r  = getdelim(__lineptr, __n, '\n', __stream);
 * ensures: return == r;
 */
ssize_t getline (char **__restrict __lineptr,
                 size_t *__restrict __n,
                 FILE *__restrict __stream);


/*$
 * requires: alive_resource(__stream, File);
 * requires: valid_string_or_fail(__s);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int fputs (const char *__restrict __s, FILE *__restrict __stream);

/*$
 * requires: valid_string_or_fail(__s);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int puts (const char *__s);

/*$
 * requires: alive_resource(__stream, File);
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int ungetc (int __c, FILE *__stream);

/*$
 * requires: alive_resource(__stream, File);
 * requires: valid_bytes_or_fail(__ptr, __size * __n);
 * assigns:  ((char*)__ptr)[0, (__size * __n));
 * ensures:  return in [0, __n];
 */
size_t fread (void *__restrict __ptr, size_t __size,
              size_t __n, FILE *__restrict __stream);

/*$
 * requires: alive_resource(__stream, File);
 * requires: valid_bytes_or_fail(__ptr, __size * __n);
 * ensures:  return in [0, __n];
 */
size_t fwrite (const void *__restrict __ptr, size_t __size,
               size_t __n, FILE *__restrict __stream);


/*$
 * #alias fputs;
 */
int fputs_unlocked (const char *__restrict __s,
                    FILE *__restrict __stream);

/*$
 * #alias fread;
 */
size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                       size_t __n, FILE *__restrict __stream);

/*$
 * #alias fwrite;
 */
size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
                        size_t __n, FILE *__restrict __stream);

/*$
 * requires: alive_resource(__stream, File);
 * requires: __whence in [0, 2];
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
int fseek (FILE *__stream, long int __off, int __whence);

/*$
 * requires: alive_resource(__stream, File);
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
long int ftell (FILE *__stream);

/*$
 * requires: alive_resource(__stream, File);
 */
void rewind (FILE *__stream);

/*$
 * requires: alive_resource(__stream, File);
 * requires: __whence in [0, 2];
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
int fseeko (FILE *__stream, __off_t __off, int __whence);

/*$
 * requires: alive_resource(__stream, File);
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
__off_t ftello (FILE *__stream);


/*$
 * requires: alive_resource(__stream, File);
 *
 * case "success" {
 *   assigns: *__pos;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);

/*$
 * requires: alive_resource(__stream, File);
 * requires: valid_ptr_or_fail(__pos);
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
int fsetpos (FILE *__stream, const fpos_t *__pos);

/*$
 * requires: alive_resource(__stream, File);
 */
void clearerr (FILE *__stream);

/*$
 * requires: alive_resource(__stream, File);
 */
int feof (FILE *__stream);


/*$
 * requires: alive_resource(__stream, File);
 */
int ferror (FILE *__stream);


/*$
 * #alias clearerr;
 */
void clearerr_unlocked (FILE *__stream);

/*$
 * #alias feof;
 */
int feof_unlocked (FILE *__stream);

/*$
 * #alias ferror;
 */
int ferror_unlocked (FILE *__stream);


/*$
 * requires: valid_string_or_fail(__s);
 */
void perror (const char *__s);


/*$
 * requires: alive_resource(__stream, File);
 * ensures:  return == __stream->_fileno;
 */
int fileno (FILE *__stream);


/*$
 * #alias fileno;
 */
int fileno_unlocked (FILE *__stream);


/*$
 * requires: valid_string_or_fail(__command);
 * requires: valid_string_or_fail(__modes);
 *
 * case "success" {
 *   local:   FILE* f = _alloc_FILE();
 *   ensures: return == f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *popen (const char *__command, const char *__modes);

/*$
 * requires: __stream in File;
 * assigns:  _errno;
 * free:     __stream;
 */
int pclose (FILE *__stream);


static char _ctermid_buf[L_ctermid];

/*$
 * requires: null_or_valid_bytes_or_fail(__s, L_ctermid);
 *
 * case "buf" {
 *   assumes: __s != NULL;
 *   assigns: __s[0, L_ctermid);
 *   ensures: valid_primed_substring(__s, L_ctermid);
 *   ensures: return == __s;
 * }
 *
 * case "nobuf" {
 *   assumes: __s == NULL;
 *   assigns: _ctermid_buf[0, L_ctermid);
 *   ensures: valid_primed_substring(_ctermid_buf, L_ctermid);
 *   ensures: return == &_ctermid_buf[0];
 * }
 */
char *ctermid (char *__s);

// L_cuserid is not always defined
#ifndef L_cuserid
#define L_cuserid 10
#endif

static char _cuserid_buf[L_cuserid];

/*$
 * requires: null_or_valid_bytes_or_fail(__s, L_cuserid);
 *
 * case "buf" {
 *   assumes: __s != NULL;
 *   assigns: __s[0, L_cuserid);
 *   ensures: valid_primed_substring(__s, L_cuserid);
 *   ensures: return == __s;
 * }
 *
 * case "nobuf" {
 *   assumes: __s == NULL;
 *   assigns: _cuserid_buf[0, L_cuserid);
 *   ensures: valid_primed_substring(_cuserid_buf, L_cuserid);
 *   ensures: return == &_cuserid_buf[0];
 * }
 */
char *cuserid (char *__s);


// omitted: obstack with __USE_GNU


/*$
 * requires: alive_resource(__stream, File);
 */
void flockfile (FILE *__stream);


/*$
 * requires: alive_resource(__stream, File);
 */
int ftrylockfile (FILE *__stream);

/*$
 * requires: alive_resource(__stream, File);
 */
void funlockfile (FILE *__stream);



/*$
 * // MS-specific, not in libc, but used in Juliet
 *
 * requires: valid_wide_string_or_fail(__filename);
 * requires: valid_wide_string_or_fail(__modes);
 *
 * case "success" {
 *   local:   FILE* r = _alloc_FILE();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *_wfopen (const wchar_t *__restrict __filename,
               const wchar_t *__restrict __modes);


/// TODO: format checking not supported due to va_arg

/*$
 * requires: alive_resource(__s, File);
 * requires: valid_string_or_fail(__format);
 * unsound: "vfprintf format is not checked";
 */
int vfprintf (FILE *__restrict __s, const char *__restrict __format,
              __gnuc_va_list __arg);

/*$
 * requires: valid_string_or_fail(__format);
 * unsound: "vprintf format is not checked";
 */
int vprintf (const char *__restrict __format, __gnuc_va_list __arg);

/*$
 * requires: valid_ptr_or_fail(__s);
 * requires: valid_string_or_fail(__format);
 * warn: "vsprintf is not safe to use";
 * unsound: "vsprintf format is not checked";
 */
int vsprintf (char *__restrict __s, const char *__restrict __format,
              __gnuc_va_list __arg);

/*$
 * requires: valid_bytes_or_fail(__s, __maxlen);
 * requires: valid_string_or_fail(__format);
 * assigns: __s[0,__maxlen);
 * ensures: valid_primed_substring(__s, __maxlen);
 * unsound: "vsnprintf format is not checked";
 */
int vsnprintf (char *__restrict __s, size_t __maxlen,
               const char *__restrict __format, __gnuc_va_list __arg);

/* built-in */
int vasprintf (char **__restrict __ptr, const char *__restrict __f,
               __gnuc_va_list __arg);

/*$
 * requires: valid_ptr_or_fail(__ptr);
 * requires: valid_string_or_fail(fmt);
 * unsound: "vasprintf format is not checked";
 *
 * case "not-constant" {
 *   local: char* r = _mopsa_new_valid_string();
 *   local: size_t l = strlen(r);
 *   assigns: *__ptr;
 *   ensures: (*__ptr)' == r;
 *   ensures: return >= 0 and return == l;
 * }
 *
 * case "failure" {
 *   ensures: return == -1;
 *  }
 */
int _mopsa_general_vasprintf(char **__ptr, const char *fmt);

/*$
 * requires: valid_ptr_or_fail(__ptr);
 * requires: valid_string_or_fail(fmt);
 * local: size_t s = strlen(fmt);
 * local: char* str = new Memory;
 * assigns: *__ptr;
 * ensures: size(str) == s+1;
 * ensures: forall size_t i in [0, s]: str[i] == fmt[i];
 * ensures: (*__ptr)' == str;
 * ensures: return >= 0 and return == s;
 */
int _mopsa_constant_vasprintf(char **__ptr, const char *fmt);
 
/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_string_or_fail(__fmt);
 * unsound: "vdprintf format is not checked";
 */
int vdprintf (int __fd, const char *__restrict __fmt,
              __gnuc_va_list __arg);

/*$
 * requires: alive_resource(__s, File);
 * requires: valid_string_or_fail(__format);
 * unsound: "vfscanf format and arguments not handled";
 */
int vfscanf (FILE *__restrict __s, const char *__restrict __format,
             __gnuc_va_list __arg);

/*$
 * requires: valid_string_or_fail(__format);
 * unsound: "vscanf format and arguments not handled";
 */
int vscanf (const char *__restrict __format, __gnuc_va_list __arg);

/*$
 * requires: valid_string_or_fail(__s);
 * requires: valid_string_or_fail(__format);
 * unsound: "vsscanf format and arguments not handled";
 */
int vsscanf (const char *__restrict __s,
             const char *__restrict __format, __gnuc_va_list __arg);
