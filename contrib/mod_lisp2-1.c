#define MOD_LISP_VERSION "2.33"
#define HEADER_STR_LEN 500

#define END_HEADER_TOKEN "end"
#define END_TOKEN_LEN 3
/*
 Modification for version 2 by Guido Zayas:
     Ok here it goes. Let me tell you what the changes are:
    -The module decl is 2.0 complain now
    -the module use apr_socket_t instead of the plain fd because the
    ap_socket's function doesn't exist anymore.
    -As consequence of this change I needed to change the functions that
    manipulate socket (OpenSocket, CloseSocket, FlushSocket, etc) 
    -I couldn't find a direct way of flushing the socket. The only way seems
    to be ensuring the buffer size to be 1 or 0 (note by default is 0)
    -The directives for WIN32 are commented out since now it should be more
    platform independent. But note that I haven't tried it under windows.
    -There are no replacement for timeout function other than setting a
    timeout in the socket itself. I'll do this next. Therefore you will see
    that the related functions are commented out (ap_reset_timeout,
    ap_kill_timeout, etc)
    -the BUFF structure does not exists anymore.
    -and last, ap_send_http_header doesn't do anything. I do not of any
    replacement. For what I could read it is enough to make the changes in
    r->header_out.
    
    For the rest, the structure is the same as what you have before. I hope
    not to have change more than the necessary.

  Version 2.33
  Added a couple of new headers like "Log-Notice" and so on. 
  They are named like the corresponding log levels in httpd_log.h. 
  The "default" log level (i.e. the one sent by just "Log") has not changed.
  (contributed by Edi Weitz)

  Version 2.32
  Removed duplicate URL header sent to Lisp.
  moved server-id header before the user http headers for security.
  do not transmit any "end" header that could be sent by an malicious user
  (Thanks to Robert Macomber for the security screening)

  Version 2.31
  Put back the correct handling of replies without known content length.
  Reads only the missing number of bytes in the reply when the browser aborts the connection.

  Version 2.3
  Force Apache to read all the Lisp reply before eventually closing the socket or handling another request.
  This avoids trying to write to a closed socket or having Lisp and Apache out of sync.
  (contributed by Edi Weitz)

  Version 2.2
  Allow more than one Set-Cookie
  Remaned the win32 dll to mod_lisp.dll
  
  Version 2.1
  Added the possibility to add notes in the apache notes table
  Removed the socket reuse for the multi-threaded WIN32 apache
  Better handling of header only replies
  
  Version 2.0 beta 1
  turned mod_lisp from a quick hack to something more clean.
  added a lisp -> apache protocol for the reply 
  added a keep-alive connection between lisp and apache (the connection is not closed each time)
  
  Version 0.92
  corrected POST handling
  
  Version 0.91
  added several values : method, content-length, content-type
  
  Version 0.90
  first release
*/

/* ====================================================================
   mod_lisp is based on the example module from the apache distribution, 
   and various other modules.

   It is distributed under a FreeBSD style license (if you want another license contact me)
   marc.battyani@fractalconcept.com
 
   Copyright 2000 Marc Battyani. All rights reserved. 

   Redistribution and use in source and binary forms, with or without modification, are permitted provided 
   that the following conditions are met: 

   Redistributions of source code must retain the above copyright notice, this list of conditions and the 
   following disclaimer. 

   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the 
   following disclaimer in the documentation and/or other materials provided with the distribution. 

   THIS SOFTWARE IS PROVIDED BY THE MARC BATTYANI ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
   BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
   DISCLAIMED. IN NO EVENT SHALL MARC BATTYANI OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
   ==================================================================== 
*/


/* ====================================================================
 * Copyright (c) 1995-1999 The Apache Group.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * 4. The names "Apache Server" and "Apache Group" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Group and was originally based
 * on public domain software written at the National Center for
 * Supercomputing Applications, University of Illinois, Urbana-Champaign.
 * For more information on the Apache Group and the Apache HTTP server
 * project, please see <http://www.apache.org/>.
 *
 */

/* 
 * Apache lisp module.  Provide demonstrations of how modules do things.
 *
 */
#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_log.h"
#include "http_main.h"
#include "http_protocol.h"
#include "http_request.h"
#include "util_script.h"
#include "http_connection.h"
#include "apr_network_io.h"
#include "apr_date.h"		/* For parseHTTPdate() */
#include "apr_pools.h"
#include "apr_tables.h"

#include "apr_strings.h"


#include <stdio.h>

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Data declarations.                                                       */
/*                                                                          */
/* Here are the static cells and structure declarations private to our      */
/* module.                                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Sample configuration record.  Used for both per-directory and per-server
 * configuration data.
 *
 * It's perfectly reasonable to have two different structures for the two
 * different environments.  The same command handlers will be called for
 * both, though, so the handlers need to be able to tell them apart.  One
 * possibility is for both structures to start with an int which is zero for
 * one and 1 for the other.
 *
 * Note that while the per-directory and per-server configuration records are
 * available to most of the module handlers, they should be treated as
 * READ-ONLY by all except the command and merge handlers.  Sometimes handlers
 * are handed a record that applies to the current location by implication or
 * inheritance, and modifying it will change the rules for other locations.
 */
typedef struct excfg {
  int cmode;                  /* Environment to which record applies (directory,
			       * server, or combination).
			       */
#define CONFIG_MODE_SERVER 1
#define CONFIG_MODE_DIRECTORY 2
#define CONFIG_MODE_COMBO 3     /* Shouldn't ever happen. */
  int local;                  /* Boolean: "Lisp" directive declared here? */
  int congenital;             /* Boolean: did we inherit an "Lisp"? */
  char *loc;                  /* Location to which this record applies. */
  int  DefaultLispServer;     // true if default values;
  char LispServerIP[20];
  long LispServerPort;
  char LispServerId[100];
  apr_socket_t *LispSocket;
  long UnsafeLispSocket;
} excfg;

static apr_pool_t *SocketPool = NULL;

/*
 * To avoid leaking memory from pools other than the per-request one, we
 * allocate a module-private pool
 */
//static apr_pool_t *lisp_pool = NULL;
//static pool *lisp_subpool = NULL;

/*
 * Declare ourselves so the configuration routines can find and know us.
 * We'll fill it in at the end of the module.
 */
module AP_MODULE_DECLARE_DATA lisp_module;

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* The following pseudo-prototype declarations illustrate the parameters    */
/* passed to command handlers for the different types of directive          */
/* syntax.  If an argument was specified in the directive definition        */
/* (look for "command_rec" below), it's available to the command handler    */
/* via the (void *) info field in the cmd_parms argument passed to the      */
/* handler (cmd->info for the examples below).                              */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Command handler for a NO_ARGS directive.
 *
 * static const char *handle_NO_ARGS(cmd_parms *cmd, void *mconfig);
 */

/*
 * Command handler for a RAW_ARGS directive.  The "args" argument is the text
 * of the commandline following the directive itself.
 *
 * static const char *handle_RAW_ARGS(cmd_parms *cmd, void *mconfig,
 *                                    const char *args);
 */

/*
 * Command handler for a FLAG directive.  The single parameter is passed in
 * "bool", which is either zero or not for Off or On respectively.
 *
 * static const char *handle_FLAG(cmd_parms *cmd, void *mconfig, int bool);
 */

/*
 * Command handler for a TAKE1 directive.  The single parameter is passed in
 * "word1".
 *
 * static const char *handle_TAKE1(cmd_parms *cmd, void *mconfig,
 *                                 char *word1);
 */

/*
 * Command handler for a TAKE2 directive.  TAKE2 commands must always have
 * exactly two arguments.
 *
 * static const char *handle_TAKE2(cmd_parms *cmd, void *mconfig,
 *                                 char *word1, char *word2);
 */

/*
 * Command handler for a TAKE3 directive.  Like TAKE2, these must have exactly
 * three arguments, or the parser complains and doesn't bother calling us.
 *
 * static const char *handle_TAKE3(cmd_parms *cmd, void *mconfig,
 *                                 char *word1, char *word2, char *word3);
 */

/*
 * Command handler for a TAKE12 directive.  These can take either one or two
 * arguments.
 * - word2 is a NULL pointer if no second argument was specified.
 *
 * static const char *handle_TAKE12(cmd_parms *cmd, void *mconfig,
 *                                  char *word1, char *word2);
 */

/*
 * Command handler for a TAKE123 directive.  A TAKE123 directive can be given,
 * as might be expected, one, two, or three arguments.
 * - word2 is a NULL pointer if no second argument was specified.
 * - word3 is a NULL pointer if no third argument was specified.
 *
 * static const char *handle_TAKE123(cmd_parms *cmd, void *mconfig,
 *                                   char *word1, char *word2, char *word3);
 */

/*
 * Command handler for a TAKE13 directive.  Either one or three arguments are
 * permitted - no two-parameters-only syntax is allowed.
 * - word2 and word3 are NULL pointers if only one argument was specified.
 *
 * static const char *handle_TAKE13(cmd_parms *cmd, void *mconfig,
 *                                  char *word1, char *word2, char *word3);
 */

/*
 * Command handler for a TAKE23 directive.  At least two and as many as three
 * arguments must be specified.
 * - word3 is a NULL pointer if no third argument was specified.
 *
 * static const char *handle_TAKE23(cmd_parms *cmd, void *mconfig,
 *                                  char *word1, char *word2, char *word3);
 */

/*
 * Command handler for a ITERATE directive.
 * - Handler is called once for each of n arguments given to the directive.
 * - word1 points to each argument in turn.
 *
 * static const char *handle_ITERATE(cmd_parms *cmd, void *mconfig,
 *                                   char *word1);
 */

/*
 * Command handler for a ITERATE2 directive.
 * - Handler is called once for each of the second and subsequent arguments
 *   given to the directive.
 * - word1 is the same for each call for a particular directive instance (the
 *   first argument).
 * - word2 points to each of the second and subsequent arguments in turn.
 *
 * static const char *handle_ITERATE2(cmd_parms *cmd, void *mconfig,
 *                                    char *word1, char *word2);
 */

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* These routines are strictly internal to this module, and support its     */
/* operation.  They are not referenced by any external portion of the       */
/* server.                                                                  */
/*                                                                          */
/*--------------------------------------------------------------------------*/

/*
 * Locate our directory configuration record for the current request.
 */
static excfg *our_dconfig(request_rec *r)
{

  return (excfg *) ap_get_module_config(r->per_dir_config, &lisp_module);
}

#if 0
/*
 * Locate our server configuration record for the specified server.
 */
static excfg *our_sconfig(server_rec *s)
{

  return (excfg *) ap_get_module_config(s->module_config, &lisp_module);
}

/*
 * Likewise for our configuration record for the specified request.
 */
static excfg *our_rconfig(request_rec *r)
{

  return (excfg *) ap_get_module_config(r->request_config, &lisp_module);
}
#endif

/*
 * This routine sets up some module-wide cells if they haven't been already.
 */
#ifdef gag
static void setup_module_cells()
{
  /*
   * If we haven't already allocated our module-private pool, do so now.
   */
  if (lisp_pool == NULL) {
    lisp_pool = ap_make_sub_pool(NULL);
  };
  /*
   * Likewise for the table of routine/environment pairs we visit outside of
   * request context.
   */
  if (static_calls_made == NULL) {
    static_calls_made = ap_make_table(lisp_pool, 16);
  };
}
#endif

/*--------------------------------------------------------------------------*/
/* We prototyped the various syntax for command handlers (routines that     */
/* are called when the configuration parser detects a directive declared    */
/* by our module) earlier.  Now we actually declare a "real" routine that   */
/* will be invoked by the parser when our "real" directive is               */
/* encountered.                                                             */
/*                                                                          */
/* If a command handler encounters a problem processing the directive, it   */
/* signals this fact by returning a non-NULL pointer to a string            */
/* describing the problem.                                                  */
/*                                                                          */
/* The magic return value DECLINE_CMD is used to deal with directives       */
/* that might be declared by multiple modules.  If the command handler      */
/* returns NULL, the directive was processed; if it returns DECLINE_CMD,    */
/* the next module (if any) that declares the directive is given a chance   */
/* at it.  If it returns any other value, it's treated as the text of an    */
/* error message.                                                           */
/*--------------------------------------------------------------------------*/
/* 
 * Command handler for the NO_ARGS "Lisp" directive.  
 */
static const char *cmd_lisp(cmd_parms *cmd, void *mconfig)
{

  excfg *cfg = (excfg *) mconfig;

  /*
   * "Lisp Wuz Here"
   */
  cfg->local = 1;
  return NULL;
}

int OpenLispSocket(excfg *cfg, apr_socket_t **new_sock)
{
  if (cfg->LispSocket && !cfg->UnsafeLispSocket)
    return 0;


  apr_status_t status;

  int ret;
  apr_sockaddr_t addr;
  apr_sockaddr_t *local_sa, *remote_sa;
 
  if ((status = apr_sockaddr_info_get(&remote_sa, cfg->LispServerIP, APR_INET, 
				      cfg->LispServerPort, 0, SocketPool))
      != APR_SUCCESS) {
    char errorbuff[HUGE_STRING_LEN];
    apr_strerror(status, errorbuff, HUGE_STRING_LEN);
    ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: apr_sockaddr_info_get: %s", errorbuff);
    return -1;
  }

  cfg->LispSocket = NULL;
  cfg->UnsafeLispSocket = 0;
  status = apr_socket_create(new_sock, remote_sa->sa.sin.sin_family, SOCK_STREAM,
			     SocketPool);
  if (status != APR_SUCCESS){
    char errorbuff[HUGE_STRING_LEN];
    apr_strerror(status, errorbuff, HUGE_STRING_LEN);
    ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: apr_socket_create: %s", errorbuff);
    return -1;
  }
  /* Tries to connect to Lisp (continues trying while error is EINTR) */
  do 
    {
      status = apr_socket_connect(*new_sock, remote_sa);
      char errorbuff[HUGE_STRING_LEN];
      apr_strerror(status, errorbuff, HUGE_STRING_LEN);
      ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: apr_socket_connect: %s", errorbuff);
    } while (status == APR_EINTR);
  
  /* Check if we connected */
  if (status != APR_SUCCESS) {
    char errorbuff[HUGE_STRING_LEN];
    apr_strerror(status, errorbuff, HUGE_STRING_LEN);
    ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: apr_socket_connect: %s", errorbuff);
    return -1;
  }

  cfg->LispSocket = *new_sock;

  if (apr_socket_timeout_set(cfg->LispSocket, ) != APR_SUCCESS)
    ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: could not set timeout");

  
/*   apr_int32_t on;	 */
/*   if (apr_socket_opt_get(cfg->LispSocket, APR_SO_SNDBUF, &on) == APR_SUCCESS) */
/*     ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: APR_SO_SNDBUF is %d", on); */
/*   else */
/*     ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: APR_SO_SNDBUF could not be retrieved"); */
/*   if (apr_socket_opt_set(cfg->LispSocket, APR_SO_SNDBUF, 1) == APR_SUCCESS) */
/*     ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: APR_SO_SNDBUF set to 1"); */
/*   else */
/*     ap_log_error(APLOG_MARK, APLOG_ERR, 0, NULL, "mod_lisp: APR_SO_SNDBUF could not be setted"); */
/*   ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, NULL, "mod_lisp: socket succesfully opened"); */
  return 0;
}

apr_status_t SendLispString(apr_socket_t* sock, char *Line)
{
  int i, NbChar;
  NbChar = strlen(Line);
  return apr_socket_send(sock, Line, &NbChar);
}

int SendLispHeaderLine(apr_socket_t* sock, char *Name, char *Value)
{
  if (SendLispString(sock, Name) != APR_SUCCESS)
    return -1;
  if (SendLispString(sock, "\n") != APR_SUCCESS)
    return -1;
  if (SendLispString(sock, Value) != APR_SUCCESS) 
    return -1;
  if (SendLispString(sock, "\n") != APR_SUCCESS)
    return -1;
  return 0;
}

void CloseLispSocket(excfg *cfg, apr_socket_t *sock) // socket for WIN32
{
  if (!cfg->LispSocket)
    return;

  apr_socket_close(cfg->LispSocket);

  cfg->LispSocket = NULL;
  cfg->UnsafeLispSocket = 0;
}

int get_line_end(const char* buff, int start, int len)
{
  int i = start;
  while(i < len && buff[i]!= 0 && buff[i] != '\n') 
    i++;
  
  return i;
}

int get_line(char *line, const char* buff, int start, int len)
{
    int end = get_line_end(buff, start, len);
    strncpy(line, buff+start, end);
    line[end-start] = 0;
    return end;
}

int copy_header_part(const char * data, int len, char *header)
{
  int end = 0, start = 0;
  char line[HEADER_STR_LEN];
  while (end < len) {
    end = get_line_end(data, start, len);
    strncpy(line, data+start, end);
    line[end-start] = 0;
    if (!strncmp(data+end-END_TOKEN_LEN, END_HEADER_TOKEN, END_TOKEN_LEN))
      break;
    start = end+1;
  }
  strncpy(header, data, end);
  return end;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Now we declare our content handlers, which are invoked when the server   */
/* encounters a document which our module is supposed to have a chance to   */
/* see.  (See mod_mime's SetHandler and AddHandler directives, and the      */
/* mod_info and mod_status lisps, for more details.)                     */
/*                                                                          */
/* Since content handlers are dumping data directly into the connexion      */
/* (using the r*() routines, such as rputs() and rprintf()) without         */
/* intervention by other parts of the server, they need to make             */
/* sure any accumulated HTTP headers are sent first.  This is done by       */
/* calling send_http_header().  Otherwise, no header will be sent at all,   */
/* and the output sent to the client will actually be HTTP-uncompliant.     */
/*--------------------------------------------------------------------------*/
/* 
 * Sample content handler.  All this does is display the call list that has
 * been built up so far.
 *
 * The return value instructs the caller concerning what happened and what to
 * do next:
 *  OK ("we did our thing")
 *  DECLINED ("this isn't something with which we want to get involved")
 *  HTTP_mumble ("an error status should be reported")
 */

static int lisp_handler(request_rec *r)
{
  int ret;
  apr_status_t status;
  apr_socket_t *sock;
  char header[HEADER_STR_LEN];
  int ContentLength = -1;
  int KeepSocket = 0;

  excfg *dcfg;

  if (strcmp(r->handler, "lisp-handler"))
    return DECLINED;
 

  ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server, "mod_lisp: in lisp-handler");
  dcfg = our_dconfig(r);
  /*
   * We're about to start sending content, so we need to force the HTTP
   * headers to be sent at this point.  Otherwise, no headers will be sent
   * at all.  We can set any we like first, of course.  **NOTE** Here's
   * where you set the "Content-type" header, and you do so by putting it in
   * r->content_type, *not* r->headers_out("Content-type").  If you don't
   * set it, it will be filled in with the server's default type (typically
   * "text/plain").  You *must* also ensure that r->content_type is lower
   * case.
   *
   * We also need to start a timer so the server can know if the connexion
   * is broken.
   */
  /* Set vars and environment in request_rec */

  //    {static NoDebug=0; if (NoDebug == 0) DebugBreak();}


  if (OpenLispSocket(dcfg, &sock)){ 
    ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: fail to open Socket");
    return HTTP_INTERNAL_SERVER_ERROR;
  }
  ap_add_common_vars(r);
  ap_add_cgi_vars(r);

  ret = ap_setup_client_block(r, REQUEST_CHUNKED_DECHUNK);
  if (ret) 
    {
      CloseLispSocket(dcfg, sock);
      ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: ap_setup_client_block failed");
      return ret;
    }
  // ap_reset_timeout(r);

  dcfg->UnsafeLispSocket = 1;

  if (r->subprocess_env) 
    {
      const apr_array_header_t *env_arr = apr_table_elts(r->subprocess_env);
      apr_table_entry_t *elts = (apr_table_entry_t *) env_arr->elts;
      int i;
      
      ret = 0;
      for (i = 0; i < env_arr->nelts; ++i) 
	{
	
	  if (!elts[i].key) continue;
	  if (!strncmp(elts[i].key, "HTTP_", 5)) continue;
	  if (!strcmp(elts[i].key, "REQUEST_URI"))
	    ret = SendLispHeaderLine(sock, "url", elts[i].val);
	  else if (!strcmp(elts[i].key, "CONTENT_TYPE"))
	    ret = SendLispHeaderLine(sock, "content-type", elts[i].val);
	  else if (!strcmp(elts[i].key, "CONTENT_LENGTH"))
	    ret = SendLispHeaderLine(sock, "content-length", elts[i].val);
	  else if (!strcmp(elts[i].key, "REQUEST_METHOD"))
	    ret = SendLispHeaderLine(sock, "method", elts[i].val);
	  else if (!strcmp(elts[i].key, "REMOTE_ADDR"))
	    ret = SendLispHeaderLine(sock, "remote-ip-addr", elts[i].val);
	  else if (!strcmp(elts[i].key, "REMOTE_PORT"))
	    ret = SendLispHeaderLine(sock, "remote-ip-port", elts[i].val);
	  else if (!strcmp(elts[i].key, "SERVER_ADDR"))
	    ret = SendLispHeaderLine(sock, "server-ip-addr", elts[i].val);
	  else if (!strcmp(elts[i].key, "SERVER_PORT"))
	    ret = SendLispHeaderLine(sock, "server-ip-port", elts[i].val);
	  else if (!strcmp(elts[i].key, "SERVER_PROTOCOL"))
	    ret = SendLispHeaderLine(sock, "server-protocol", elts[i].val);
	  else if (!strcmp(elts[i].key, "SSL_SESSION_ID"))
	    ret = SendLispHeaderLine(sock, "ssl-session-id", elts[i].val);
	  if (ret != 0) 
	    {
	      //	      ap_kill_timeout(r);
	      CloseLispSocket(dcfg, sock);
      	      ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: failed to send pair/value string to lisp server");
	      return HTTP_INTERNAL_SERVER_ERROR;
	    }
	  //	  ap_reset_timeout(r);
	}
    }
  
  /* Send this before client headers so ASSOC can be used to grab it
   * without worrying about some joker sending a server-id header of
   * his own. (Robert Macomber)*/
  ret = SendLispHeaderLine(sock, "server-id", dcfg->LispServerId);
  if (ret!=0) {
      //     ap_kill_timeout(r);
      CloseLispSocket(dcfg, sock);
      ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: failed to send server-id to lisp server");
      return HTTP_INTERNAL_SERVER_ERROR;
    }

  if (r->headers_in) {
      const apr_array_header_t *hdr_arr = apr_table_elts(r->headers_in);
      apr_table_entry_t *elts = (apr_table_entry_t *) hdr_arr->elts;
      int i;
      
      for (i = 0; i < hdr_arr->nelts; ++i) {
	  if (!elts[i].key) continue;
	  ret = SendLispHeaderLine(sock, 
				   !strcmp(elts[i].key, END_HEADER_TOKEN)?"end-header":elts[i].key, 
				   elts[i].val);
	  if (ret!=0) {
	      //	      ap_kill_timeout(r);
	      CloseLispSocket(dcfg, sock);
              ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: failed to send pair/value string to lisp server");
	      return HTTP_INTERNAL_SERVER_ERROR;
	    }
	  //ap_reset_timeout(r);
	}
    }
  
  if (SendLispString(sock, "end\n") != APR_SUCCESS) {
      //     ap_kill_timeout(r);
      CloseLispSocket(dcfg, sock);
      ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: failed to send end string to lisp server");
      return HTTP_INTERNAL_SERVER_ERROR;
    }
    
  /* If there is a request entity, send it */
  if (ap_should_client_block(r))  {
      char buffer[HUGE_STRING_LEN];
      int buffersize=1;
      
      /* If we did read something we'll post it to Lisp */
      while ((buffersize=ap_get_client_block(r,buffer,HUGE_STRING_LEN))>0) 
	{
	  /* Reset our writing timeout */
	  //	  ap_reset_timeout(r);
	  /* Check that what we writed was the same of what we read */
	  if (apr_socket_send(sock,buffer,&buffersize) != APR_SUCCESS) 
	    {
	      /* Discard all further characters left to read */
	      while (ap_get_client_block(r, buffer, HUGE_STRING_LEN) > 0);
	      CloseLispSocket(dcfg, sock);
              ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, "mod_lisp: failed to send string to lisp server");
	      return HTTP_INTERNAL_SERVER_ERROR;
	    }
	}
    }

  //  ap_kill_timeout(r);
  
  /* Receive the response from Lisp */
  // ap_hard_timeout("lisp-read", r);

  char *content;
  char data[HEADER_STR_LEN];
  int l = HEADER_STR_LEN;
  int hlength = 0,copied;
  //get header part
  status = apr_socket_recv(sock, data, &l);
  while (status == APR_SUCCESS) {
    if (l > 0)
      data[l] = 0;
    copied = copy_header_part(data, l, header+hlength);
    hlength += copied;
    if (copied < l) {//header end found
      content = apr_pstrdup(r->pool, data+copied);
      content[l-copied+1]=0;
      break;
    }
    if (!strncmp(header+hlength-END_TOKEN_LEN, END_HEADER_TOKEN, END_TOKEN_LEN))
      break;
    l = HEADER_STR_LEN;
    status = apr_socket_recv(sock, data, &l);
  }

  //process header elements
  int end = 0, start = 0;
  char key[HEADER_STR_LEN], value[HEADER_STR_LEN];
  while (end < hlength) {
    end = get_line(key, header, start, hlength);
    if (!strncmp(key, END_HEADER_TOKEN, END_TOKEN_LEN))
      break;
    start = end + 1;
    if (start >= hlength) break;
    end = get_line(value, header, start, hlength);
    start = end + 1;

    if (!strcmp(key, "Content-Type")) {
      char *tmp = apr_pstrdup(r->pool, value);
      ap_content_type_tolower(tmp);
      r->content_type = tmp;
    }
    else if (!strcmp(key, "Status")) {
      r->status = atoi(value);
      r->status_line = apr_pstrdup(r->pool, value);
    }
    else if (!strcmp(key, "Location")) {
      apr_table_set(r->headers_out, key, value);
    }
    else if (!strcmp(key, "Content-Length")) {
      apr_table_set(r->headers_out, key, value);
      ContentLength = atoi(value);
    }
    else if (!strcmp(key, "Last-Modified")) {
      apr_time_t mtime = apr_date_parse_http(value);
      ap_update_mtime(r, mtime);
      ap_set_last_modified(r);
    }
    else if (!strcmp(key, "Keep-Socket")) {
      KeepSocket = atoi(value);
    }
    else if (!strcmp(key, "Log-Emerg")) {
      ap_log_error("mod_lisp", 0, APLOG_EMERG|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Alert")) {
      ap_log_error("mod_lisp", 0, APLOG_ALERT|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Crit")){
      ap_log_error("mod_lisp", 0, APLOG_CRIT|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Error")) {
      ap_log_error("mod_lisp", 0, APLOG_ERR|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Warning")){
      ap_log_error("mod_lisp", 0, APLOG_WARNING|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Notice")){
      ap_log_error("mod_lisp", 0, APLOG_NOTICE|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Info")){
      ap_log_error("mod_lisp", 0, APLOG_INFO|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log-Debug")){
      ap_log_error(APLOG_MARK, APLOG_DEBUG|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Log")){
      ap_log_error("mod_lisp", 0, APLOG_ERR|APLOG_NOERRNO, APR_EGENERAL, r->server, "%s", value);
    }
    else if (!strcmp(key, "Note")){
      char *pv = strchr(value, ' ');
      if (pv != NULL) {
	*pv++ = 0;
	apr_table_setn(r->notes, apr_pstrdup(r->pool, value), apr_pstrdup(r->pool, pv));
      }
    }
    else if (!strcmp(key, "Set-Cookie")){
      apr_table_add(r->headers_out, key, value);
    }
    else 
      apr_table_set(r->headers_out, key, value);
  }
  
/* Send headers and data collected (if this was not a "header only" req. */
//ap_send_http_header(r);
  if (!r->header_only) {
    if (ContentLength > 0) {
      if (content != 0) {
	ContentLength = ContentLength-strlen(content);
	ap_rputs(content, r);
      }
      int readLength = ContentLength;
      if (readLength > 0) {
	char buffer[readLength];
	status = apr_socket_recv(sock, buffer, &readLength);
	buffer[readLength]=0;

	if (status == APR_SUCCESS) 
	  ap_rputs(buffer, r);
	else if (readLength < ContentLength || r->connection->aborted) {
	  ContentLength -= readLength;
	  do {
	    readLength = ContentLength;
	    status = apr_socket_recv(sock, buffer, &readLength);
	    ContentLength -= readLength;
	  } while (readLength > 0 && ContentLength > 0);
	}
      }
    }
  }
  //     else
  //	if (ContentLength == -1)
  //	  ap_send_fb(BuffSocket, r);

/* #ifdef WIN32 */
/*   KeepSocket = 0; */
/* #endif */

  /* Kill timeouts, close buffer and socket and return */
  // ap_kill_timeout(r);
  if (ContentLength == -1 || KeepSocket == 0)
    CloseLispSocket(dcfg, sock);
  dcfg->UnsafeLispSocket = 0;
  return OK;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Now let's declare routines for each of the callback phase in order.      */
/* (That's the order in which they're listed in the callback list, *not     */
/* the order in which the server calls them!  See the command_rec           */
/* declaration near the bottom of this file.)  Note that these may be       */
/* called for situations that don't relate primarily to our function - in   */
/* other words, the fixup handler shouldn't assume that the request has     */
/* to do with "lisp" stuff.                                              */
/*                                                                          */
/* With the exception of the content handler, all of our routines will be   */
/* called for each request, unless an earlier handler from another module   */
/* aborted the sequence.                                                    */
/*                                                                          */
/* Handlers that are declared as "int" can return the following:            */
/*                                                                          */
/*  OK          Handler accepted the request and did its thing with it.     */
/*  DECLINED    Handler took no action.                                     */
/*  HTTP_mumble Handler looked at request and found it wanting.             */
/*                                                                          */
/* What the server does after calling a module handler depends upon the     */
/* handler's return value.  In all cases, if the handler returns            */
/* DECLINED, the server will continue to the next module with an handler    */
/* for the current phase.  However, if the handler return a non-OK,         */
/* non-DECLINED status, the server aborts the request right there.  If      */
/* the handler returns OK, the server's next action is phase-specific;      */
/* see the individual handler comments below for details.                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/* 
 * This function is called during server initialisation.  Any information
 * that needs to be recorded must be in static cells, since there's no
 * configuration record.
 *
 * There is no return value.
 */

/*
 * module-initialiser
 */
static void lisp_init(server_rec *s, apr_pool_t *p)
{
//    ap_add_version_component(p, "mod_lisp/" MOD_LISP_VERSION);
/*
 * Set up any module cells that ought to be initialised.
 */
//    setup_module_cells();
}

/* 
 * This function is called during server initialisation when an heavy-weight
 * process (such as a child) is being initialised.  As with the
 * module-initialisation function, any information that needs to be recorded
 * must be in static cells, since there's no configuration record.
 *
 * There is no return value.
 */

/*
 *  process-initialiser
 */
static void lisp_child_init(apr_pool_t *p, server_rec *s)
{
  apr_pool_sub_make(&SocketPool, p, NULL);

  /*
   * Set up any module cells that ought to be initialised.
   */
  //    setup_module_cells();
}

/* 
 * This function is called when an heavy-weight process (such as a child) is
 * being run down or destroyed.  As with the child-initialisation function,
 * any information that needs to be recorded must be in static cells, since
 * there's no configuration record.
 *
 * There is no return value.
 */

/*
 * process-exit
 */
static apr_status_t lisp_child_exit(void *data)
{
  apr_pool_destroy(SocketPool);
  SocketPool = NULL;
  return APR_SUCCESS;
}

/*
 * This function gets called to create up a per-directory configuration
 * record.  This will be called for the "default" server environment, and for
 * each directory for which the parser finds any of our directives applicable.
 * If a directory doesn't have any of our directives involved (i.e., they
 * aren't in the .htaccess file, or a <Location>, <Directory>, or related
 * block), this routine will *not* be called - the configuration for the
 * closest ancestor is used.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 */
static void *lisp_create_dir_config(apr_pool_t *p, char *dirspec)
{

  excfg *cfg;
  char *dname = dirspec;

  /*
   * Allocate the space for our record from the pool supplied.
   */
  cfg = (excfg *) apr_pcalloc(p, sizeof(excfg));
  /*
   * Now fill in the defaults.  If there are any `parent' configuration
   * records, they'll get merged as part of a separate callback.
   */
  cfg->local = 0;
  cfg->congenital = 0;
  strcpy(cfg->LispServerIP, "127.0.0.1");
  strcpy(cfg->LispServerId, "apache");
  cfg->LispServerPort = 3000;
  cfg->DefaultLispServer = 1;
  cfg->LispSocket = 0;

  cfg->cmode = CONFIG_MODE_DIRECTORY;
  dname = (dname != NULL) ? dname : "";
  cfg->loc = apr_pstrcat(p, "DIR(", dname, ")", NULL);
  return (void *) cfg;
}

/*
 * This function gets called to merge two per-directory configuration
 * records.  This is typically done to cope with things like .htaccess files
 * or <Location> directives for directories that are beneath one for which a
 * configuration record was already created.  The routine has the
 * responsibility of creating a new record and merging the contents of the
 * other two into it appropriately.  If the module doesn't declare a merge
 * routine, the record for the closest ancestor location (that has one) is
 * used exclusively.
 *
 * The routine MUST NOT modify any of its arguments!
 *
 * The return value is a pointer to the created module-specific structure
 * containing the merged values.
 */
static void *lisp_merge_dir_config(apr_pool_t *p, void *parent_conf,
				   void *newloc_conf)
{

  excfg *merged_config = (excfg *) apr_pcalloc(p, sizeof(excfg));
  excfg *pconf = (excfg *) parent_conf;
  excfg *nconf = (excfg *) newloc_conf;

  /*
   * Some things get copied directly from the more-specific record, rather
   * than getting merged.
   */
  merged_config->local = nconf->local;
  merged_config->loc = apr_pstrdup(p, nconf->loc);
  /*
   * Others, like the setting of the `congenital' flag, get ORed in.  The
   * setting of that particular flag, for instance, is TRUE if it was ever
   * true anywhere in the upstream configuration.
   */
  merged_config->congenital = (pconf->congenital | pconf->local);
  /*
   * If we're merging records for two different types of environment (server
   * and directory), mark the new record appropriately.  Otherwise, inherit
   * the current value.
   */
  merged_config->cmode =
    (pconf->cmode == nconf->cmode) ? pconf->cmode : CONFIG_MODE_COMBO;

  if (nconf->DefaultLispServer == 0)
    {
      strcpy(merged_config->LispServerIP, nconf->LispServerIP);
      strcpy(merged_config->LispServerId, nconf->LispServerId);
      merged_config->LispServerPort = nconf->LispServerPort;
      merged_config->DefaultLispServer = 0;
    }
  else 
    if (pconf->DefaultLispServer == 0)
      {
	strcpy(merged_config->LispServerIP, pconf->LispServerIP);
	strcpy(merged_config->LispServerId, pconf->LispServerId);
	merged_config->LispServerPort = pconf->LispServerPort;
	merged_config->DefaultLispServer = 0;
      }
    else
      {
	strcpy(merged_config->LispServerIP, "127.0.0.1");
	strcpy(merged_config->LispServerId, "apache");
	merged_config->LispServerPort = 3000;
	merged_config->DefaultLispServer = 1;
      }

  merged_config->LispSocket = 0;
  return (void *) merged_config;
}

/*
 * This function gets called to create a per-server configuration
 * record.  It will always be called for the "default" server.
 *
 * The return value is a pointer to the created module-specific
 * structure.
 */
static void *lisp_create_server_config(apr_pool_t *p, server_rec *s)
{

  excfg *cfg;
  char *sname = s->server_hostname;

  /*
   * As with the lisp_create_dir_config() reoutine, we allocate and fill
   * in an empty record.
   */
  cfg = (excfg *) apr_pcalloc(p, sizeof(excfg));
  cfg->local = 0;
  cfg->congenital = 0;
  cfg->cmode = CONFIG_MODE_SERVER;
  strcpy(cfg->LispServerIP, "127.0.0.1");
  strcpy(cfg->LispServerId, "apache");
  cfg->LispServerPort = 3000;
  cfg->LispSocket = 0;
  cfg->DefaultLispServer = 1;
  sname = (sname != NULL) ? sname : "";
  cfg->loc = apr_pstrcat(p, "SVR(", sname, ")", NULL);
  return (void *) cfg;
}

/*
 * This function gets called to merge two per-server configuration
 * records.  This is typically done to cope with things like virtual hosts and
 * the default server configuration  The routine has the responsibility of
 * creating a new record and merging the contents of the other two into it
 * appropriately.  If the module doesn't declare a merge routine, the more
 * specific existing record is used exclusively.
 *
 * The routine MUST NOT modify any of its arguments!
 *
 * The return value is a pointer to the created module-specific structure
 * containing the merged values.
 */
static void *lisp_merge_server_config(apr_pool_t *p, void *server1_conf,
				      void *server2_conf)
{

  excfg *merged_config = (excfg *) apr_pcalloc(p, sizeof(excfg));
  excfg *s1conf = (excfg *) server1_conf;
  excfg *s2conf = (excfg *) server2_conf;

  /*
   * Our inheritance rules are our own, and part of our module's semantics.
   * Basically, just note whence we came.
   */
  merged_config->cmode =
    (s1conf->cmode == s2conf->cmode) ? s1conf->cmode : CONFIG_MODE_COMBO;
  merged_config->local = s2conf->local;
  merged_config->congenital = (s1conf->congenital | s1conf->local);
  merged_config->loc = apr_pstrdup(p, s2conf->loc);

  if (s2conf->DefaultLispServer == 0)
    {
      strcpy(merged_config->LispServerIP, s2conf->LispServerIP);
      strcpy(merged_config->LispServerId, s2conf->LispServerId);
      merged_config->LispServerPort = s2conf->LispServerPort;
      merged_config->DefaultLispServer = 0;
    }
  else 
    if (s1conf->DefaultLispServer == 0)
      {
	strcpy(merged_config->LispServerIP, s1conf->LispServerIP);
	strcpy(merged_config->LispServerId, s1conf->LispServerId);
	merged_config->LispServerPort = s1conf->LispServerPort;
	merged_config->DefaultLispServer = 0;
      }
    else
      {
	strcpy(merged_config->LispServerIP, "127.0.0.1");
	strcpy(merged_config->LispServerId, "apache");
	merged_config->LispServerPort = 3000;
	merged_config->DefaultLispServer = 1;
      }

  merged_config->LispSocket = NULL;

  return (void *) merged_config;
}

/*
 * This routine is called after the request has been read but before any other
 * phases have been processed.  This allows us to make decisions based upon
 * the input header fields.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are called for this phase.
 */
static int lisp_post_read_request(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine gives our module an opportunity to translate the URI into an
 * actual filename.  If we don't do anything special, the server's default
 * rules (Alias directives and the like) will continue to be followed.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are called for this phase.
 */
static int lisp_translate_handler(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine is called to check the authentication information sent with
 * the request (such as looking up the user in a database and verifying that
 * the [encrypted] password sent matches the one in the database).
 *
 * The return value is OK, DECLINED, or some HTTP_mumble error (typically
 * HTTP_UNAUTHORIZED).  If we return OK, no other modules are given a chance
 * at the request during this phase.
 */
static int lisp_check_user_id(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine is called to check to see if the resource being requested
 * requires authorisation.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * other modules are called during this phase.
 *
 * If *all* modules return DECLINED, the request is aborted with a server
 * error.
 */
static int lisp_auth_checker(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine is called to check for any module-specific restrictions placed
 * upon the requested resource.  (See the mod_access module for an lisp.)
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  All modules with an
 * handler for this phase are called regardless of whether their predecessors
 * return OK or DECLINED.  The first one to return any other status, however,
 * will abort the sequence (and the request) as usual.
 */
static int lisp_access_checker(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine is called to determine and/or set the various document type
 * information bits, like Content-type (via r->content_type), language, et
 * cetera.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, no
 * further modules are given a chance at the request for this phase.
 */
static int lisp_type_checker(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine is called to perform any module-specific fixing of header
 * fields, et cetera.  It is invoked just before any content-handler.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, the
 * server will still call any remaining modules with an handler for this
 * phase.
 */
static int lisp_fixer_upper(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  //    r->handler = "lisp-handler";
  return OK;
}

/*
 * This routine is called to perform any module-specific logging activities
 * over and above the normal server things.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, any
 * remaining modules with an handler for this phase will still be called.
 */
static int lisp_logger(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

/*
 * This routine is called to give the module a chance to look at the request
 * headers and take any appropriate specific actions early in the processing
 * sequence.
 *
 * The return value is OK, DECLINED, or HTTP_mumble.  If we return OK, any
 * remaining modules with handlers for this phase will still be called.
 */
static int lisp_header_parser(request_rec *r)
{
  excfg *cfg;

  cfg = our_dconfig(r);
  return DECLINED;
}

//const char *(*take3) (cmd_parms *parms, void *mconfig, const char *w,  const char *w2, const char *w3);
const char *SetLispServer(cmd_parms *cmd, void *mconfig, const char *serverIp, const char *port, const char *serverId)
{
  excfg *cfg = (excfg *) mconfig;

  strncpy(cfg->LispServerIP, serverIp, 19);
  cfg->LispServerIP[19] = 0;
  strncpy(cfg->LispServerId, serverId, 99);
  cfg->LispServerId[99] = 0;
  cfg->LispServerPort = atoi(port);
  cfg->DefaultLispServer = 0;
  cfg->LispSocket = 0;

  return NULL;
}

static void lisp_register_hooks(apr_pool_t *p)
{
  //    ap_hook_pre_config(lisp_pre_config, NULL, NULL, APR_HOOK_MIDDLE);
  //    ap_hook_post_config(lisp_post_config, NULL, NULL, APR_HOOK_MIDDLE);
  //    ap_hook_open_logs(lisp_open_logs, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_child_init(lisp_child_init, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_handler(lisp_handler, NULL, NULL, APR_HOOK_MIDDLE);
  /* [1] post read_request handling */
  ap_hook_post_read_request(lisp_post_read_request, NULL, NULL,
			    APR_HOOK_MIDDLE);
  ap_hook_log_transaction(lisp_logger, NULL, NULL, APR_HOOK_MIDDLE);

  ap_hook_translate_name(lisp_translate_handler, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_header_parser(lisp_header_parser, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_check_user_id(lisp_check_user_id, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_fixups(lisp_fixer_upper, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_type_checker(lisp_type_checker, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_access_checker(lisp_access_checker, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_auth_checker(lisp_auth_checker, NULL, NULL, APR_HOOK_MIDDLE);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* All of the routines have been declared now.  Here's the list of          */
/* directives specific to our module, and information about where they      */
/* may appear and how the command parser should pass them to us for         */
/* processing.  Note that care must be taken to ensure that there are NO    */
/* collisions of directive names between modules.                           */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/* 
 * List of directives specific to our module.
 */
static const command_rec lisp_cmds[] =
  {
    AP_INIT_TAKE3 (
		   "LispServer",                          /* directive name */
		   SetLispServer,                        /* config action routine */
		   NULL,                               /* argument to include in call */
		   OR_ALL,                         /* where available */
		   "the lisp server name, port and an Id string. Example : LispServer 127.0.0.1 3000 \"server1\""
		   ),
      AP_INIT_NO_ARGS (
            "Lisp",              /* directive name */
            cmd_lisp,            /* config action routine */
	    NULL,
            OR_ALL,             /* where available */
            "Lisp directive - no arguments" /* directive description */
        ),
    {NULL}
  };

/*--------------------------------------------------------------------------*/
/*                                                                          */
/* Finally, the list of callback routines and data structures that          */
/* provide the hooks into our module from the other parts of the server.    */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/* 
 * Module definition for configuration.  If a particular callback is not
 * needed, replace its routine name below with the word NULL.
 *
 * The number in brackets indicates the order in which the routine is called
 * during request processing.  Note that not all routines are necessarily
 * called (such as if a resource doesn't have access restrictions).
 */

module AP_MODULE_DECLARE_DATA lisp_module =
  {
    STANDARD20_MODULE_STUFF,
    lisp_create_dir_config,    /* per-directory config creator */
    lisp_merge_dir_config,     /* dir config merger */
    lisp_create_server_config, /* server config creator */
    lisp_merge_server_config,  /* server config merger */
    lisp_cmds,                 /* command table */
    lisp_register_hooks,       /* set up other request processing hooks */
  };