;; modlisp-cmucl.lisp
;; original mod-lisp.lisp author marc.battyani@fractalconcept.com
;; ported to ECL by José Roberto B. A. Monteiro (betoes) jordeam@gmail.com

;; To test it eval this:
;; (start-listener)
;; (mp:process-run-function "html-handler" #'wait-for-request) ;; start a process called "html-handler"
;; ;; that will call PROCESS-APACHE-COMMAND function ; you can write your function to process urls
;; ;; and pass it to WAIT-FOR-REQUEST
;; (write (fetch-mod-lisp-url "localhost" "/lisp/index.html")) ;; write out what it got; note that
;; (write #\Newline) ;; "/lisp/*" can be anything else as you have configured in Apache.
;; (write (fetch-mod-lisp-url "localhost" "/lisp"))
;; (write #\Newline)

(require "sb-bsd-sockets")

(defconstant +apache-port+ 3000)
(defvar *apache-server-socket*)
(defvar *apache-socket*) ;the socket to apache
(defvar *close-apache-socket*) ;set to t if you want to close the socket to apache
(defvar *apache-nb-use-socket* 0) ;the number of requests sent in this socket
(defvar *apache-request* 0)

(defun nslookup (hostname)
   "Returns the address for HOSTNAME as a four element array, suitable
    for socket-connect. A host-not-found-error condition is thrown. if HOSTNAME is
    not found."
   (and hostname
       (sb-bsd-sockets:host-ent-address  (sb-bsd-sockets:get-host-by-name hostname))))

;; open a server port
(defun open-tcp-server (&key (interface (machine-instance)) (port +apache-port+) (backlog 5))
   "Returns a socket server bound to PORT on INTERFACE."
   (handler-case
     (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
       (sb-bsd-sockets:socket-bind server (nslookup interface) port)
       (sb-bsd-sockets:socket-listen server backlog)
       server)
     (sb-bsd-sockets:address-in-use-error ()
       (format t "ERROR: address ~A:~A is already in use~%" interface port)
       (force-output)
       nil)))

(defun start-listener (&key (port +apache-port+))
  (setf *apache-server-socket* (open-tcp-server :port port))
  (setf (sb-bsd-sockets:non-blocking-mode *apache-server-socket*) nil))

(defun wait-for-request (&rest handler)
  (loop
     (let ((soc (sb-bsd-sockets:socket-accept *apache-server-socket*)))
       (unwind-protect
            (if (not handler)
                (apache-listen (sb-bsd-sockets:socket-make-stream soc)  'process-apache-command)
                (apache-listen (sb-bsd-sockets:socket-make-stream soc) handler))
         (sb-bsd-sockets:socket-close soc)))))

(defun apache-listen (*apache-socket* handler)
  (let ((*close-apache-socket* t))
    (unwind-protect
      (loop for *apache-nb-use-socket* from 0
	    for command = (get-apache-command)
	    while command do (funcall handler command) (force-output *apache-socket*)
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
  (let ((html (if (equal (cdr (assoc "url" command :test #'string=)) "/lisp/index.html")
                  (debug-table command)
		  (fixed-html))))
    (write-header-line "Status" "200 OK")
    (write-header-line "Content-Type" "text/html")
    (write-header-line "Content-Length" (format nil "~d" (length html)))
    (write-header-line "Keep-Socket" "0")
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
   (format s "<TR bgcolor=\"#F0F0c0\"><TD>request</TD><TD>~a</TD></TR>"  *apache-request*)
   (incf *apache-request*)
   (format s "<TR bgcolor=\"#F0F0c0\"><TD>apache-nb-use-socket</TD><TD>~a</TD></TR>"  *apache-nb-use-socket*)
   (loop for (key . value) in command do
	 (format s "<TR bgcolor=\"#F0F0c0\"><TD>~a</TD><TD>~a</TD></TR>" key value))
   (write-string "</TABLE></BODY></HTML>" s)))

(defun fixed-html ()
  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<HTML><HEAD></HEAD><BODY><H1>mod_lisp 2.0</H1><P>This is a constant html string sent by mod_lisp 2.0 + Lispworks + apache + FreeBSD</P></BODY></HTML>")

;;; A small test bench used to test and time the client/server protocol 

(defun fetch-mod-lisp-url (server url &key (nb-fetch 1) (port +apache-port+) close-socket)
  (loop with server-socket and reply and soc
   repeat nb-fetch
   do (unless server-socket
        (setf soc (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (if (not (sb-bsd-sockets:socket-connect soc (nslookup server) port))
            (setf server-socket (sb-bsd-sockets:socket-make-stream soc))))
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
   finally (sb-bsd-sockets:socket-close soc)
     (return reply)))

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
