;; Copyright (c) 2013, Jason R. Person
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies, 
;; either expressed or implied, of the FreeBSD Project.
(in-package #:cl-reddit)

;;url escapes
(eval-when (:execute :load-toplevel :compile-toplevel)
(defparameter *escape* '(#\$ #\& #\+ #\, #\/ #\: #\; #\= #\? #\@ #\space #\" #\< #\> #\# #\% #\{ #\} #\| #\\ #\^ #\~ #\[ #\] #\`))
(defparameter *user-agent* "cl-reddit/0.2 (common lisp api wrapper)")
(defparameter *reddit* "http://www.reddit.com")
)

;;;; URL helper functions ;;;;
(defmacro encode-param (x)
  "Encodes get params"
  (let ((c (gensym)))
    `(with-output-to-string (stream)
       (loop for ,c across ,x collect
             (case ,c 
               ((,@*escape*) (format stream "%~x" (char-code ,c)))
               (otherwise (write-char ,c stream)))))))

(defun build-get-params (params)
  "Builds get param list from cons list '((param . value) (param2 . value) ...)"
  (string-left-trim "&" 
    (with-output-to-string (stream)
      (loop for (p . v) in params do 
            (format stream "&~a=~a" (encode-param p) (if (stringp v) (encode-param v) v))))))

(defun get-json (url user)
  "Gets json data for url with options cookie-jar."
  (yason:parse
    (if (null user)
      (drakma:http-request url :method :get :user-agent *user-agent* :preserve-uri t :want-stream t)
      (drakma:http-request url :method :get :user-agent *user-agent* :cookie-jar (user-cookie user) :preserve-uri t :want-stream t))))

(defun post-request2 (url cookie-jar params)
  "Send post request to url with params list."
  (drakma:http-request url :method :post :user-agent *user-agent* :parameters params :cookie-jar cookie-jar :want-stream t))

(defun post-request (url user params)
  "Send post request to url with params list."
  (with-user (user)
    (drakma:http-request url :method :post :user-agent *user-agent* :parameters params :cookie-jar (user-cookie user) :want-stream t)))
