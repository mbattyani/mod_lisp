;;; -*- Mode:Lisp; Syntax:Common-lisp; Package:MODLISP; Base:10 -*-

;; modlisp-clisp.lisp
;; adapted from modlisp-acl.lisp

;; Original CLISP adaption by Rachel Richard <rr@u.washington.edu>
;; Changes by Nils Kassube <nika@kassube.de> on 2001-11-24:
;;
;; - replaced :lisp by :ext for CLISP's new naming convention,
;;   works now with recent CLISP distributions
;; - added modlisp-server to the :export line 

(in-package :user)

(provide :modlisp)

(eval-when (compile load eval)
  (defpackage :modlisp
    (:nicknames :lph)
    (:use :cl)
    (:export modlisp-server)
    ))

(in-package :modlisp)

;;;;;
;;;
;;; First part adapted from server.cl
;;;
;;;;;

(defvar *server-count* 0)	; used to name servers

(defun make-socket-server (&key (name (format nil "socket server ~d "
					      (incf *server-count*)))
				port 
				function 
				wait 
				(format :text))
  ;;
  ;; create a server process with the given name, listening on the
  ;; given port, running the given function on each connection that
  ;; comes in, and possibly waiting for that function's completion before
  ;; accepting a new connection.
  ;;
  ;; name - a string naming the server process -- if nil, then this 
  ;;	function will create a name.
  ;; port - if nil then an internet domain port number will be chosen
  ;;	    by the operating system.   If a number is given then that
  ;;	    port will be used (or an error will be signalled if it
  ;;	    is already in use).    If port is a string then a unix
  ;;	    domain port will be used.  (this will not work on Windows).
  ;; function - the function to run when a connection is made.  This
  ;;	    function must take one argument which is the stream used
  ;;	    used for reading from and writing to the process that connected
  ;;	    to this socket. 
  ;; wait - if true, then the function will be run in the server process
  ;;	    and thus the server won't accept a new connection until
  ;;	    the function finishes.
  ;; format  - :text (the default) or :binary.   This determes what kind
  ;;	    of data can sent to and read from the socket stream.
  ;;	    
  ;;
  ;;
  ;; The return value is the port number on which the server is
  ;;	listening.
  ;;
  (let ((passive-socket (ext:socket-server port) ;

))
	
	(start-socket-server passive-socket
			     :function function
			     :wait wait)
(ext:socket-server-port passive-socket)))

(defun start-socket-server (passive-socket &key function wait)
  ;; internal function run in the server lightweight process 
  ;; that continually processes the connection.
  ;; This code is careful to ensure that the sockets are 
  ;; properly closed something abnormal happens.
  (unwind-protect
      (loop (let ((connection  (ext:socket-accept passive-socket)))
	      (if wait
		 (progn 
		   (unwind-protect
			  (funcall function connection)
			  (handler-case (values-list (cons t
							   (multiple-value-list
							    (close connection))))
					(error (condition) (declare (ignore-if-unused condition))
					       nil))))


			   (unwind-protect
			       (funcall function connection)
			    	  (handler-case (values-list (cons t
							   (multiple-value-list
							    (close connection))))
					(error (condition) (declare (ignore-if-unused condition))
					       nil)))

		 )))
   
    	  (handler-case (values-list (cons t
							   (multiple-value-list
							    (close passive-socket))))
					(error (condition) (declare (ignore-if-unused condition))
					       nil))))


;;;;;
;;;
;;; Second part adapted from mod-lisp.lisp
;;;
;;;;;

(defconstant +apache-port+ 3000)
(defvar *apache-socket* nil) ;the socket to apache
(defvar *close-apache-socket* nil) ;set to t if you want to close the socket to apache
(defvar *apache-nb-use-socket* 0) ;the number of requests sent in this socket

(defun modlisp-server (&optional (port +apache-port+))
  
  (make-socket-server :name "test"
		      :port port
		      :function 'apache-listen))

(defun apache-listen (*apache-socket*)
   (let ((*close-apache-socket* t))
    (unwind-protect
	(loop for *apache-nb-use-socket* from 0
	    for command = (get-apache-command)
	    while command 
	    do (process-apache-command command)
	  (force-output *apache-socket*)
	    until *close-apache-socket*)
      (close *apache-socket*))))

(defun get-apache-command ()
  (ignore-errors
   (let* ((header (loop for key = (read-line *apache-socket* nil nil)
		    while (and key (string-not-equal key "end")) 	
		    collect (cons key (read-line *apache-socket* nil nil)) )))
     (let* ((content-length (cdr (assoc "content-length" header :test #'equal)))
	  (content (when content-length (make-string (parse-integer content-length :junk-allowed t)))))

     (when content
       (read-sequence content *apache-socket*)
       (push (cons "posted-content" content) header))
     header))))
				
(defun process-apache-command (command)
  (let ((html (if (equal (cdr (assoc "url" command :test #'string=)) 
			 "/modlisp/fixed")
		  (debug-table command)
		(fixed-html))))
    (write-header-line "Status" "200 OK")
    (write-header-line "Content-Type" "text/html")
    (write-header-line "Content-Length" (format nil "~d" (length html)))
    (write-header-line "Keep-Socket" "1")
    (write-string "end" *apache-socket*)
    (write-char #\NewLine *apache-socket*)
    (write-string html *apache-socket*)
    (setf *close-apache-socket* nil)))

(defun debug-table (command)
  (with-output-to-string (s)
   (write-string "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><TABLE bgcolor=\"#c0c0c0\">
<TR bgcolor=\"yellow\"><TH COLSPAN=2>CLISP + mod_lisp 2.0 + apache + Linux</TH></TR>
<TR bgcolor=\"yellow\"><TH>Key</TH><TH>Value</TH></TR>" s)
   (format s "<TR bgcolor=\"#F0F0c0\"><TD>apache-nb-use-socket</TD><TD>~a</TD></TR>"  *apache-nb-use-socket*)
   (loop for (key . value) in command do
	 (format s "<TR bgcolor=\"#F0F0c0\"><TD>~a</TD><TD>~a</TD></TR>" key value))
   (write-string "</TABLE></BODY></HTML>" s)))


(defun fixed-html ()
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><H1>mod_lisp 2.0</H1><P>This is a constant
  html string sent by mod_lisp 2.0 + CLISP + apache + Linux</P>
</BODY></HTML>")

(defun write-header-line (key value)
  (write-string key *apache-socket*)
  (write-char #\NewLine *apache-socket*)
  (write-string value *apache-socket*)
  (write-char #\NewLine *apache-socket*))
