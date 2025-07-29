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
 * Stubs for getopt.h
 * Based on header from glibc-2.27-r6
 */

#include <getopt.h>
#include <stddef.h> // to get NULL

char *optarg;
int optind = 1;
int opterr;
int optopt;


/*$
 * requires: ___argc > 0;
 * requires: optind >= 0 and optind <= ___argc;
 * requires: valid_string(__shortopts);
 * requires: forall int i in [0, ___argc): valid_string(___argv[i]);
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * ensures: optind' in [1, ___argc];
 * ensures: optarg' != NULL implies exists int i in [0, ___argc): in_string(optarg', ___argv[i]);
 * ensures: return in [-1, 255];
 * case "modify-argv" {
 *   assigns: ___argv[0, ___argc);
 *   ensures: forall int i in [0, ___argc): exists int j in [0, ___argc): (___argv[i])' == ___argv[j];
 * }
 */
int getopt (int ___argc, char *const *___argv, const char *__shortopts);



#define valid_longopt(l)                                                                 \
    valid_ptr(l) and                                                                     \
    exists size_t i in [0, ((bytes(l) - offset(l)) / sizeof_type(struct option))): (     \
      l[i].name == NULL and                                                              \
      forall size_t j in [0, i): (                                                       \
        valid_ptr(l[j].name) and                                                         \
        (exists size_t k in [0, (size(l[j].name) - offset(l[j].name))): l[j].name[k] == 0) and \
        (l[j].flag != NULL implies valid_ptr(l[j].flag)) \
      ) \
    )

/*$
 * ensures:
 *   exists size_t i in [0, ((bytes(l) - offset(l)) / sizeof_type(struct option))): (
 *     l[i].name == NULL and
 *     return == i and
 *     forall size_t j in [0, i): l[j].name != NULL
 *   )
 * ;
 */
size_t _mopsa_len_option (const struct option* l);

/*$
 * case "null" {
 *   assumes: i == 0;
 *   ensures: return == 0;
 * }
 *
 * case "nonnull" {
 *   assumes: i > 0;
 *   ensures: return in [0, i);
 * }
 */
size_t _mopsa_pick(size_t i);

/*$
 * requires: ___argc > 0;
 * requires: optind >= 0 and optind <= ___argc;
 * requires: valid_string(__shortopts);
 * requires: forall int i in [0, ___argc): valid_string(___argv[i]);
 * //requires: valid_longopt(__longopts);
 * requires: null_or_valid_ptr(__longind);
 * local: size_t len = _mopsa_len_option(__longopts);
 * local: size_t r = _mopsa_pick(len);
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * ensures: optind' in [1, ___argc];
 * ensures: optarg' == NULL or exists int i in [0, ___argc): in_string(optarg', ___argv[i]);
 *
 * case "opt-ind" {
 *   assumes: len > 0;
 *   assumes: __longopts[r].flag != NULL;
 *   assumes: __longind != NULL;
 *   assigns: *__longopts[r].flag;
 *   assigns: *__longind;
 * }
 *
 * case "opt" {
 *   assumes: len > 0;
 *   assumes: __longopts[r].flag != NULL;
 *   assumes: __longind == NULL;
 *   assigns: *__longopts[r].flag;
 * }
 *
 * case "ind" {
 *   assumes: len > 0;
 *   assumes: __longopts[r].flag == NULL;
 *   assumes: __longind != NULL;
 *   assigns: *__longind;
 * }
 *
 * case "none" {
 *   assumes: len > 0;
 *   assumes: __longopts[r].flag == NULL;
 *   assumes: __longind == NULL;
 * }
 *
 * case "empty" {
 *   assumes: len == 0;
 * }
 *
 * case "modify-argv" {
 *   assigns: ___argv[0, ___argc);
 *   ensures: forall int i in [0, ___argc): exists int j in [0, ___argc): (___argv[i])' == ___argv[j];
 * }
 */
int getopt_long (int ___argc, char *const *___argv, const char *__shortopts,
                 const struct option *__longopts, int *__longind);

/*$
 * #alias getopt_long;
 */
int getopt_long_only (int ___argc, char *const *___argv, const char *__shortopts,
                      const struct option *__longopts, int *__longind);
