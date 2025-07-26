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

/* Stubs for functions in error.h */

#include <error.h>
#include <stddef.h> // to get NULL


/*
  error and error_at_line are handled by builtins to check formats.
  Then, they call the following functions to complete the stub.
 */

/* built-in */
void error (int __status, int __errnum, const char *__format, ...);

/* built-in */
void error_at_line (int __status, int __errnum, const char *__fname,
                    unsigned int __lineno, const char *__format, ...);


/*$
 * requires: error_print_progname != NULL
 *           implies ( error_print_progname != INVALID and
 *                     alive(error_print_progname));
 * assigns: error_message_count;
 *
 * case "exit" {
 *   assumes: __status != 0;
 *   warn: "error called";
 *   ensures: 1 == 0;
 * }
 *
 * case "noexit" {
 *   assumes: __status == 0;
 * }
 */
void _mopsa_error(int __status);

/*$
 * requires: error_print_progname != NULL
 *           implies ( error_print_progname != INVALID and
 *                     alive(error_print_progname));
 * requires: valid_string(__filename);
 * assigns: error_message_count;
 *
 * case "exit" {
 *   assumes: __status != 0;
 *   warn: "error_at_line called";
 *   ensures: 1 == 0;
 * }
 *
 * case "noexit" {
 *   assumes: __status == 0;
 * }
 */
void _mopsa_error_at_line(int __status, const char* __filename);

void (*error_print_progname) (void) = NULL;

unsigned int error_message_count = 0;

int error_one_per_line = 0;
