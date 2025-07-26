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
#include <netdb.h>


static int _mopsa_herrno;

/*$
 * ensures: return == &_mopsa_herrno;
 */
int *__h_errno_location (void);

/*$
 * requires: valid_string(__str);
 */
void herror (const char *__str);



static struct hostent _mopsa_hostent_buf;

/*$
 * assigns: _mopsa_hostent_buf;
 * local: char* name = _mopsa_new_valid_string();
 * local: char** aliases = _mopsa_new_valid_string_array();
 * local: char** addr_list = _mopsa_new_valid_string_array();
 * ensures: (_mopsa_hostent_buf.h_name)' == name;
 * ensures: (_mopsa_hostent_buf.h_aliases)' == aliases;
 * ensures: (_mopsa_hostent_buf.h_addr_list)' == addr_list;
 * ensures: return == &_mopsa_hostent_buf;
 */
static struct hostent* _mopsa_hostent();

/*$
 * assigns: _mopsa_herrno;
 */
void sethostent (int __stay_open);

/*$
 * assigns: _mopsa_herrno;
 */
void endhostent (void);

/*$
 * assigns: _mopsa_herrno;
 * local: struct hostent* r = _mopsa_hostent();
 * ensures: return == NULL or return == r;
 */
struct hostent *gethostent (void);

/*$
 * requires: valid_bytes(__addr, __len);
 * assigns: _mopsa_herrno;
 * local: struct hostent* r = _mopsa_hostent();
 * ensures: return == NULL or return == r;
 */
struct hostent *gethostbyaddr (const void *__addr, __socklen_t __len,
                               int __type);

/*$
 * requires: valid_string(__name);
 * assigns: _mopsa_herrno;
 * local: struct hostent* r = _mopsa_hostent();
 * ensures: return == NULL or return == r;
 */
struct hostent *gethostbyname (const char *__name);

/*$
 * requires: valid_string(__name);
 * assigns: _mopsa_herrno;
 * local: struct hostent* r = _mopsa_hostent();
 * ensures: return == NULL or return == r;
 */
struct hostent *gethostbyname2 (const char *__name, int __af);

// unsupported GNU extension
int gethostent_r (struct hostent *__restrict __result_buf,
			 char *__restrict __buf, size_t __buflen,
			 struct hostent **__restrict __result,
			 int *__restrict __h_errnop);

// unsupported GNU extension
int gethostbyaddr_r (const void *__restrict __addr, __socklen_t __len,
			    int __type,
			    struct hostent *__restrict __result_buf,
			    char *__restrict __buf, size_t __buflen,
			    struct hostent **__restrict __result,
			    int *__restrict __h_errnop);

// unsupported GNU extension
int gethostbyname_r (const char *__restrict __name,
			    struct hostent *__restrict __result_buf,
			    char *__restrict __buf, size_t __buflen,
			    struct hostent **__restrict __result,
			    int *__restrict __h_errnop);

// unsupported GNU extension
int gethostbyname2_r (const char *__restrict __name, int __af,
			     struct hostent *__restrict __result_buf,
			     char *__restrict __buf, size_t __buflen,
			     struct hostent **__restrict __result,
			     int *__restrict __h_errnop);



static struct netent _mopsa_netent_buf;

/*$
 * assigns: _mopsa_netent_buf;
 * local: char* name = _mopsa_new_valid_string();
 * local: char** aliases = _mopsa_new_valid_string_array();
 * ensures: (_mopsa_netent_buf.n_name)' == name;
 * ensures: (_mopsa_netent_buf.n_aliases)' == aliases;
 * ensures: return == &_mopsa_netent_buf;
 */
static struct netent* _mopsa_netent();

/*$
 * assigns: _mopsa_herrno;
 */
void setnetent (int __stay_open);

/*$
 * assigns: _mopsa_herrno;
 */
void endnetent (void);

/*$
 * assigns: _mopsa_herrno;
 * local: struct netent* r = _mopsa_netent();
 * ensures: return == NULL or return == r;
 */
struct netent *getnetent (void);

/*$
 * assigns: _mopsa_herrno;
 * local: struct netent* r = _mopsa_netent();
 * ensures: return == NULL or return == r;
 */
struct netent *getnetbyaddr (uint32_t __net, int __type);

/*$
 * requires: valid_string(__name);
 * assigns: _mopsa_herrno;
 * local: struct netent* r = _mopsa_netent();
 * ensures: return == NULL or return == r;
 */
struct netent *getnetbyname (const char *__name);

// unsupported GNU extension
int getnetent_r (struct netent *__restrict __result_buf,
			char *__restrict __buf, size_t __buflen,
			struct netent **__restrict __result,
			int *__restrict __h_errnop);

// unsupported GNU extension
int getnetbyaddr_r (uint32_t __net, int __type,
			   struct netent *__restrict __result_buf,
			   char *__restrict __buf, size_t __buflen,
			   struct netent **__restrict __result,
			   int *__restrict __h_errnop);

// unsupported GNU extension
int getnetbyname_r (const char *__restrict __name,
			   struct netent *__restrict __result_buf,
			   char *__restrict __buf, size_t __buflen,
			   struct netent **__restrict __result,
			   int *__restrict __h_errnop);



static struct servent _mopsa_servent_buf;

/*$
 * assigns: _mopsa_servent_buf;
 * local: char* name = _mopsa_new_valid_string();
 * local: char** aliases = _mopsa_new_valid_string_array();
 * local: char* proto = _mopsa_new_valid_string();
 * ensures: (_mopsa_servent_buf.s_name)' == name;
 * ensures: (_mopsa_servent_buf.s_aliases)' == aliases;
 * ensures: (_mopsa_servent_buf.s_proto)' == proto;
 * ensures: return == &_mopsa_servent_buf;
 */
static struct servent* _mopsa_servent();

/*$
 * assigns: _mopsa_herrno;
 */
void setservent (int __stay_open);

/*$
 * assigns: _mopsa_herrno;
 */
void endservent (void);

/*$
 * assigns: _mopsa_herrno;
 * local: struct servent* r = _mopsa_servent();
 * ensures: return == NULL or return == r;
 */
struct servent *getservent (void);

/*$
 * requires: valid_string(__name);
 * requires: valid_string(__proto);
 * assigns: _mopsa_herrno;
 * local: struct servent* r = _mopsa_servent();
 * ensures: return == NULL or return == r;
 */
struct servent *getservbyname (const char *__name, const char *__proto);

/*$
 * requires: valid_string(__proto);
 * assigns: _mopsa_herrno;
 * local: struct servent* r = _mopsa_servent();
 * ensures: return == NULL or return == r;
 */
struct servent *getservbyport (int __port, const char *__proto);

// unsupported GNU extension
int getservent_r (struct servent *__restrict __result_buf,
			 char *__restrict __buf, size_t __buflen,
			 struct servent **__restrict __result);

// unsupported GNU extension
int getservbyname_r (const char *__restrict __name,
			    const char *__restrict __proto,
			    struct servent *__restrict __result_buf,
			    char *__restrict __buf, size_t __buflen,
			    struct servent **__restrict __result);

// unsupported GNU extension
int getservbyport_r (int __port, const char *__restrict __proto,
			    struct servent *__restrict __result_buf,
			    char *__restrict __buf, size_t __buflen,
			    struct servent **__restrict __result);



static struct protoent _mopsa_protoent_buf;

/*$
 * assigns: _mopsa_protoent_buf;
 * local: char* name = _mopsa_new_valid_string();
 * local: char** aliases = _mopsa_new_valid_string_array();
 * ensures: (_mopsa_protoent_buf.p_name)' == name;
 * ensures: (_mopsa_protoent_buf.p_aliases)' == aliases;
 * ensures: return == &_mopsa_protoent_buf;
 */
static struct protoent* _mopsa_protoent();

/*$
 * assigns: _mopsa_herrno;
 */
void setprotoent (int __stay_open);

/*$
 * assigns: _mopsa_herrno;
 */
void endprotoent (void);

/*$
 * assigns: _mopsa_herrno;
 * local: struct protoent* r = _mopsa_protoent();
 * ensures: return == NULL or return == r;
 */
struct protoent *getprotoent (void);

/*$
 * requires: valid_string(__name);
 * assigns: _mopsa_herrno;
 * local: struct protoent* r = _mopsa_protoent();
 * ensures: return == NULL or return == r;
 */
struct protoent *getprotobyname (const char *__name);

/*$
 * assigns: _mopsa_herrno;
 * local: struct protoent* r = _mopsa_protoent();
 * ensures: return == NULL or return == r;
 */
struct protoent *getprotobynumber (int __proto);

// unsupported GNU extension
int getprotoent_r (struct protoent *__restrict __result_buf,
			  char *__restrict __buf, size_t __buflen,
			  struct protoent **__restrict __result);

// unsupported GNU extension
int getprotobyname_r (const char *__restrict __name,
			     struct protoent *__restrict __result_buf,
			     char *__restrict __buf, size_t __buflen,
			     struct protoent **__restrict __result);

// unsupported GNU extension
int getprotobynumber_r (int __proto,
			       struct protoent *__restrict __result_buf,
			       char *__restrict __buf, size_t __buflen,
			       struct protoent **__restrict __result);



/*$
 * requires: __netgroup != NULL implies valid_string(__netgroup);
 * assigns: _mopsa_herrno;
 * ensures: return in [0,1];
 */
int setnetgrent (const char *__netgroup);

/*$
 * assigns: _mopsa_herrno;
 */
void endnetgrent (void);

/*$
 * requires: valid_ptr(__hostp);
 * requires: valid_ptr(__userp);
 * requires: valid_ptr(__domainp);
 * local: char* host = _mopsa_new_valid_string();
 * local: char* user = _mopsa_new_valid_string();
 * local: char* domain = _mopsa_new_valid_string();
 * assigns: _mopsa_herrno;
 * assigns: *__hostp;
 * assigns: *__userp;
 * assigns: *__domainp;
 * ensures: (*__hostp)' == host;
 * ensures: (*__userp)' == user;
 * ensures: (*__domainp)' == domain;
 * ensures: return in [0,1];
 */
int getnetgrent (char **__restrict __hostp,
                 char **__restrict __userp,
                 char **__restrict __domainp);

/*$
 * requires: __netgroup != NULL implies valid_string(__netgroup);
 * requires: __host != NULL implies valid_string(__host);
 * requires: __user_ != NULL implies valid_string(__user_);
 * requires: __domain != NULL implies valid_string(__domain);
 * assigns: _mopsa_herrno;
 * ensures: return in [0,1];
 */
int innetgr (const char *__netgroup, const char *__host,
             const char *__user_, const char *__domain);

// unsupported GNU extension
int getnetgrent_r (char **__restrict __hostp,
                   char **__restrict __userp,
                   char **__restrict __domainp,
                   char *__restrict __buffer, size_t __buflen);

// unsupported
int rcmd (char **__restrict __ahost, unsigned short int __rport,
          const char *__restrict __locuser,
          const char *__restrict __remuser,
          const char *__restrict __cmd, int *__restrict __fd2p);

// unsupported
int rcmd_af (char **__restrict __ahost, unsigned short int __rport,
             const char *__restrict __locuser,
             const char *__restrict __remuser,
             const char *__restrict __cmd, int *__restrict __fd2p,
             sa_family_t __af);

// unsupported
int rexec (char **__restrict __ahost, int __rport,
           const char *__restrict __name,
           const char *__restrict __pass,
           const char *__restrict __cmd, int *__restrict __fd2p);

// unsupported
int rexec_af (char **__restrict __ahost, int __rport,
              const char *__restrict __name,
              const char *__restrict __pass,
              const char *__restrict __cmd, int *__restrict __fd2p,
              sa_family_t __af);

// unsupported
int ruserok (const char *__rhost, int __suser,
             const char *__remuser, const char *__locuser);

// unsupported
int ruserok_af (const char *__rhost, int __suser,
                const char *__remuser, const char *__locuser,
                sa_family_t __af);

// unsupported
int iruserok (uint32_t __raddr, int __suser,
              const char *__remuser, const char *__locuser);

// unsupported
int iruserok_af (const void *__raddr, int __suser,
                 const char *__remuser, const char *__locuser,
                 sa_family_t __af);

// unsupported
int rresvport (int *__alport);

// unsupported
int rresvport_af (int *__alport, sa_family_t __af);


/*$
 * local: struct addrinfo* r = new Memory;
 * local: struct sockaddr* addr = new Memory;
 * ensures: size(r) == sizeof_type(struct addrinfo);
 * ensures: size(addr) == sizeof_type(struct sockaddr);
 * ensures: r->ai_addr == addr;
 * ensures: return == r;
 */
static struct addrinfo* _mopsa_new_addrinfo();


/*$
 * requires: __name != NULL implies valid_string(__name);
 * requires: __service != NULL implies valid_string(__service);
 * requires: __name != NULL or __service != NULL;
 * requires: valid_ptr(__req);
 * requires: valid_ptr(__pai);
 * assigns: *__pai;
 * local: struct addrinfo* a1 = _mopsa_new_addrinfo();
 * local: struct addrinfo* a2 = _mopsa_new_addrinfo();
 * local: struct addrinfo* a3 = _mopsa_new_addrinfo();
 * ensures: a1->ai_next == a2;
 * ensures: a2->ai_next == a3;
 * ensures: a3->ai_next == NULL;
 * ensures: (*__pai)' == a1 or (*__pai)' == a2 or (*__pai)' == a3 or (*__pai)' == NULL;
 * unsound: "getaddrinfo only allocates a linked list of size 3";
 */
int getaddrinfo (const char *__restrict __name,
                 const char *__restrict __service,
                 const struct addrinfo *__restrict __req,
                 struct addrinfo **__restrict __pai);

/*$
 * ensures: null_or_valid_ptr(__ai);
 *
 * case "free" {
 *   assumes: __ai != NULL;
 *   free: __ai;
 *   unsound: "freeaddrinfo does not free all its data";
 * }
 *
 * case "nop" {
 *   assumes: __ai == NULL;
 * }
 */
void freeaddrinfo (struct addrinfo *__ai);

/*$
 * local: char* r = _mopsa_new_readonly_string();
 * ensures: return == r;
 */
const char *gai_strerror (int __ecode);

/*$
 * requires: valid_bytes(__sa, __salen);
 * requires: null_or_valid_bytes(__host, __hostlen);
 * requires: null_or_valid_bytes(__serv, __servlen);
 * requires: (__host != NULL and __hostlen > 0) or (__serv != NULL and __servlen > 0);
 * 
 * case "host-serv" {
 *   assumes: __host != NULL and __hostlen > 0;
 *   assumes: __serv != NULL and __servlen > 0;
 *   assigns: __host[0, __hostlen);
 *   assigns: __serv[0, __servlen);
 *   ensures: valid_primed_substring(__host, __hostlen);
 *   ensures: valid_primed_substring(__serv, __servlen);
 * }
 * 
 * case "host" {
 *   assumes: __host != NULL and __hostlen > 0;
 *   assumes: __serv == NULL or __servlen == 0;
 *   assigns: __host[0, __hostlen);
 *   ensures: valid_primed_substring(__host, __hostlen);
 * }
 * 
 * case "serv" {
 *   assumes: __host == NULL or __hostlen == 0;
 *   assumes: __serv != NULL and __servlen > 0;
 *   assigns: __serv[0, __servlen);
 *   ensures: valid_primed_substring(__serv, __servlen);
 * }
 */
int getnameinfo (const struct sockaddr *__restrict __sa,
                 socklen_t __salen, char *__restrict __host,
                 socklen_t __hostlen, char *__restrict __serv,
                 socklen_t __servlen, int __flags);
