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
#include <regex.h>
#include <string.h>

// POSIX regular expressions


/*$
 * requires: valid_string(__pattern);
 * requires: valid_ptr(__preg);
 * assigns: *__preg;
 * // TODO: allocate resource into __preg, check it in regexec, free it in regfree
 */
int regcomp (regex_t *__restrict __preg,
             const char *__restrict __pattern,
             int __cflags);

/*$
 * requires: valid_ptr(__preg);
 * requires: valid_string(__String);
 * requires: __nmatch > 0 implies valid_ptr_range(__pmatch, 0, __nmatch - 1);
 * assigns: __pmatch[0, __nmatch);
 * local: size_t len = strlen(__String);
 * ensures: forall size_t i in [0, __nmatch): (__pmatch[i].rm_so)' in [-1, len];
 * ensures: forall size_t i in [0, __nmatch): (__pmatch[i].rm_eo)' in [-1, len];
 * ensures: return == 0 or return == REG_NOMATCH;
 */
int regexec (const regex_t *__restrict __preg,
             const char *__restrict __String, size_t __nmatch,
             regmatch_t __pmatch[__restrict_arr],
             int __eflags);

/*$
 * requires: valid_substring(__errbuf, __errbuf_size);
 * requires: valid_ptr(__preg);
 * assigns: __errbuf[0, __errbuf_size);
 * ensures: valid_primed_substring(__errbuf, __errbuf_size);
 */
size_t regerror (int __errcode, const regex_t *__restrict __preg,
                 char *__restrict __errbuf, size_t __errbuf_size);

/*$
 * requires: valid_ptr(__preg);
 */
void regfree (regex_t *__preg);



// GNU regulat expressions


reg_syntax_t re_syntax_options;

static reg_syntax_t _mopsa_re_syntax;

/*$
 * assigns: _mopsa_re_syntax;
 * ensures: _mopsa_re_syntax' == __syntax;
 * ensures: return == _mopsa_re_syntax;
 */
reg_syntax_t re_set_syntax (reg_syntax_t __syntax);

/*$
 * requires: valid_bytes(__pattern, __length);
 * requires: valid_ptr(__buffer);
 * assigns: *__buffer;
 * local: char* msg = _mopsa_new_readonly_string();
 * ensures: return == NULL or return == msg;
 * // TODO resource for __buffer to be freed with regfree
 */
const char *re_compile_pattern (const char *__pattern, size_t __length,
                                struct re_pattern_buffer *__buffer);

/*$
 * requires: valid_ptr(__buffer);
 * assigns: *__buffer;
 * ensures: return == 0 or return == -2;
 */
int re_compile_fastmap (struct re_pattern_buffer *__buffer);

/*$
 * requires: valid_ptr(__buffer);
 * requires: valid_bytes(__String, __length);
 * requires: __start >= 0 and __range >= 0 and __start + __range <= __length;
 * requires: __regs != NULL implies valid_ptr(__regs);
 * assigns: *__buffer;
 * ensures: return >= -2 and return <= __range;
 *
 * case "regs" {
 *   assumes: __regs != NULL;
 *   assigns: *__regs;
 * }
 *
 * case "noregs" {
 *   assumes: __regs == NULL;
 * }
 */
regoff_t re_search (struct re_pattern_buffer *__buffer,
                    const char *__String, regoff_t __length,
                    regoff_t __start, regoff_t __range,
                    struct re_registers *__regs);

/*$
 * requires: valid_ptr(__buffer);
 * requires: valid_bytes(__string1, __length1);
 * requires: valid_bytes(__string2, __length2);
 * requires: __start >= 0 and __range >= 0 and __start + __range <= __stop and __stop <= __length1 + __length2;
 * requires: __regs != NULL implies valid_ptr(__regs);
 * assigns: *__buffer;
 * ensures: return >= -2 and return <= __range;
 *
 * case "regs" {
 *   assumes: __regs != NULL;
 *   assigns: *__regs;
 * }
 *
 * case "noregs" {
 *   assumes: __regs == NULL;
 * }
 */
regoff_t re_search_2 (struct re_pattern_buffer *__buffer,
                      const char *__string1, regoff_t __length1,
                      const char *__string2, regoff_t __length2,
                      regoff_t __start, regoff_t __range,
                      struct re_registers *__regs,
                      regoff_t __stop);

/*$
 * requires: valid_ptr(__buffer);
 * requires: valid_bytes(__String, __length);
 * requires: __start >= 0 and __start <= __length;
 * requires: __regs != NULL implies valid_ptr(__regs);
 * assigns: *__buffer;
 * ensures: return >= -2 and return <= __length;
 *
 * case "regs" {
 *   assumes: __regs != NULL;
 *   assigns: *__regs;
 * }
 *
 * case "noregs" {
 *   assumes: __regs == NULL;
 * }
 */
regoff_t re_match (struct re_pattern_buffer *__buffer,
                   const char *__String, regoff_t __length,
                   regoff_t __start, struct re_registers *__regs);

/*$
 * requires: valid_ptr(__buffer);
 * requires: valid_bytes(__string1, __length1);
 * requires: valid_bytes(__string2, __length2);
 * requires: __start >= 0 and __start <= __stop and __stop <= __length1 + __length2;
 * requires: __regs != NULL implies valid_ptr(__regs);
 * assigns: *__buffer;
 * ensures: return >= -2 and return <= __stop;
 *
 * case "regs" {
 *   assumes: __regs != NULL;
 *   assigns: *__regs;
 * }
 *
 * case "noregs" {
 *   assumes: __regs == NULL;
 * }
 */
regoff_t re_match_2 (struct re_pattern_buffer *__buffer,
                     const char *__string1, regoff_t __length1,
                     const char *__string2, regoff_t __length2,
                     regoff_t __start, struct re_registers *__regs,
                     regoff_t __stop);

/*$
 * requires: valid_ptr(__buffer);
 * requires: valid_ptr(__regs);
 * requires: __num_regs > 0 implies valid_ptr_range(__starts, 0, __num_regs - 1);
 * requires: __num_regs > 0 implies valid_ptr_range(__ends, 0, __num_regs - 1);
 * assigns: *__buffer;
 * assigns: *__regs;
 * assigns: __starts[0, __num_regs);
 * assigns: __ends[0, __num_regs);
 */
void re_set_registers (struct re_pattern_buffer *__buffer,
                       struct re_registers *__regs,
                       unsigned int __num_regs,
                       regoff_t *__starts, regoff_t *__ends);
