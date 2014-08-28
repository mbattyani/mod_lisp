;; modlisp-cmucl.lisp
;; original mod-lisp.lisp author marc.battyani@fractalconcept.com
;; ported to cmucl by dig@mail.com

;; To test it eval this:
;; (start-apache-listener)
;; (fetch-mod-lisp-url "localhost" "/asp/fixed")

(defconstant +apache-port+ 3000)
(defvar *apache-stream* nil) ;the socket to apache
(defvar *close-apache-stream* nil) ;set to t if you want to close the socket to apache
(defvar *apache-nb-use-socket* 0) ;the number of requests sent in this socket

(defun make-apache-listener (port)
  (let ((socket (ext:create-inet-listener port)))
    (unwind-protect
	 (loop
	  (mp:process-wait-until-fd-usable socket :input)
	  (multiple-value-bind (new-fd remote-host)
	      (ext:accept-tcp-connection socket)
	    (let ((stream (sys:make-fd-stream new-fd :input t :output t)))
	      (mp:make-process #'(lambda () (apache-listen stream))))))
      (unix:unix-close socket))))

(defun start-apache-listener ()
  (mp:make-process #'(lambda () (make-apache-listener +apache-port+))))

(defun apache-listen (*apache-stream*)
  (let ((*close-apache-stream* t))
    (unwind-protect
      (loop for *apache-nb-use-socket* from 0
	    for command = (get-apache-command)
	    while command do (process-apache-command command)(force-output *apache-stream*)
	    until *close-apache-stream*)
      (close *apache-stream*))))

(defun get-apache-command ()
  (ignore-errors
    (let* ((header (loop for key = (read-line *apache-stream* nil nil)
			 while (and key (string-not-equal key "end"))
			 for value = (read-line *apache-stream* nil nil)
			 collect (cons key value)))
	   (content-length (cdr (assoc "content-length" header :test #'equal)))
	   (content (when content-length (make-string (parse-integer content-length :junk-allowed t)))))
      (when content
	(read-sequence content *apache-stream*)
	(push (cons "posted-content" content) header))
      header)))

(defun write-header-line (key value)
  (write-string key *apache-stream*)
  (write-char #\NewLine *apache-stream*)
  (write-string value *apache-stream*)
  (write-char #\NewLine *apache-stream*))

(defun process-apache-command (command)
  (let ((html (if (equal (cdr (assoc "url" command :test #'string=)) "/asp/fixed")
		  (debug-table command)
		  (fixed-html))))
    (write-header-line "Status" "200 OK")
    (write-header-line "Content-Type" "text/html")
    (write-header-line "Content-Length" (format nil "~d" (length html)))
    (write-header-line "Keep-Socket" "1")
    (write-string "end" *apache-stream*)
    (write-char #\NewLine *apache-stream*)
    (write-string html *apache-stream*)
    (setf *close-apache-stream* nil)))

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


(defun fetch-mod-lisp-url (server url &key (nb-fetch 1) (port 3000) close-socket)
  (let ((socket (ext:connect-to-inet-socket server port))
	(reply))
    (unwind-protect
	(let ((stream (sys:make-fd-stream socket :input t :output t)))
	  (dotimes (i nb-fetch)
	    (write-string "url" stream)
	    (write-char #\NewLine stream)
	    (write-string url stream)
	    (write-char #\NewLine stream)
	    (write-string "end" stream)
	    (write-char #\NewLine stream)
	    (force-output stream)
	    (setf reply (read-reply stream))
	    (when close-socket
	      (close stream)
	      (setf stream nil))))
      (unix:unix-close socket))
    reply))

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



