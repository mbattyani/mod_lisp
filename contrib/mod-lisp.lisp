(require "comm")

(defconstant +apache-port+ 3000)
(defvar *apache-socket* nil) ;the socket to apache
(defvar *close-apache-socket* nil) ;set to t if you want to close the socket to apache
(defvar *apache-nb-use-socket* 0) ;the number of requests sent in this socket

(defun make-apache-instream (handle)
  (mp:process-run-function (format nil "apache socket ~d" handle) '()
                            'apache-listen (make-instance 'comm:socket-stream :socket handle
							  :direction :io :element-type 'base-char)))

(defun start-apache-listener ()
  (comm:start-up-server :function 'make-apache-instream :service +apache-port+))

(defun apache-listen (*apache-socket*)
  (let ((*close-apache-socket* t))
    (unwind-protect
      (loop for *apache-nb-use-socket* from 0
	    for command = (get-apache-command)
	    while command do (process-apache-command command)(force-output *apache-socket*)
	    until *close-apache-socket*)
      (close *apache-socket*))))

(defun get-apache-command ()
  (ignore-errors
    (let* ((header (loop for key = (read-line *apache-socket* nil nil)
			 while (and key (string-not-equal key "end"))
			 for value = (read-line *apache-socket* nil nil)
			 collect (cons key value)))
	   (content-length (cdr (assoc "content-length" header :test #'equal)))
	   (content (when content-length (make-string (parse-integer content-length :junk-allowed t)))))
      (when content
	(read-sequence content *apache-socket*)
	(push (cons "posted-content" content) header))
      header)))

(defun write-header-line (key value)
  (write-string key *apache-socket*)
  (write-char #\NewLine *apache-socket*)
  (write-string value *apache-socket*)
  (write-char #\NewLine *apache-socket*))

(defun process-apache-command (command)
  (let ((html (if (equal (cdr (assoc "url" command :test #'string=)) "/asp/fixed")
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
<TR bgcolor=\"yellow\"><TH COLSPAN=2>Lispworks + mod_lisp 2.0 + apache + FreeBSD</TH></TR>
<TR bgcolor=\"yellow\"><TH>Key</TH><TH>Value</TH></TR>" s)
   (format s "<TR bgcolor=\"#F0F0c0\"><TD>apache-nb-use-socket</TD><TD>~a</TD></TR>"  *apache-nb-use-socket*)
   (loop for (key . value) in command do
	 (format s "<TR bgcolor=\"#F0F0c0\"><TD>~a</TD><TD>~a</TD></TR>" key value))
   (write-string "</TABLE></BODY></HTML>" s)))

(defun fixed-html ()
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><H1>mod_lisp 2.0</H1><P>This is a constant html string sent by mod_lisp 2.0 + Lispworks + apache + FreeBSD</P></BODY></HTML>")



;;; A small test bench used to test and time the client/server protocol 

(defun fetch-mod-lisp-url (server url &key (nb-fetch 1) (port 3000) close-socket)
  (loop with server-socket and reply
	repeat nb-fetch
	do (unless server-socket
	     (setf server-socket (comm:open-tcp-stream server port)))
   	   (write-string "url" server-socket)
           (write-char #\NewLine server-socket)
	   (write-string url server-socket)
	   (write-char #\NewLine server-socket)
	   (write-string "end" server-socket)
	   (write-char #\NewLine server-socket)
	   (force-output server-socket)
	   (setf reply (read-reply server-socket))
	   (when close-socket
	     (close server-socket)
	     (setf server-socket nil))
	   finally (return reply)))

(defun read-reply (socket)
  (let* ((header (loop for key = (read-line socket nil nil)
		       while (and key (string-not-equal key "end"))
		       for value = (read-line socket nil nil)
		       collect (cons key value)))
	 (content-length (cdr (assoc "Content-Length" header :test #'string=)))
	 (content (when content-length (make-string (parse-integer content-length :junk-allowed t)))))
    (when content
      (read-sequence content socket)
      (push (cons "reply-content" content) header))
    header))

