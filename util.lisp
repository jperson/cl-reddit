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

;;;; Helper functions ;;;;
(defun make-user (&key username password)
  "Make an instance of user class with username and password"
  (make-instance 'user :username username :password password))

(defun format-key-args (args)
  "Format a list of key arguments"
  (let ((params))
    (loop for arg in (cdr args) do
          (push arg params) 
          (push (values (intern (string-upcase `,arg) "KEYWORD")) params))
    params))

(defun symbol-string(s)
  "Convert the input symbol to the correct string for api call."
  (case s
    ('up "1")
    ('down "-1")
    ('unvote "0")
    (otherwise (string-downcase (symbol-name s)))))

;;;; Helper macros ;;;;
(defmacro with-user ((usr) &body body)
  "Does 'body' with logged-in user usr.  Logins in user if not logged-in."
  (let ((json (gensym)) (result (gensym)) (cks (gensym)))
    `(if (null (user-modhash ,usr))
       (progn
         (let* ((,cks (make-instance 'drakma:cookie-jar))
                (,result (drakma:http-request "http://www.reddit.com/api/login.json"
                                              :method :post
                                              :parameters `(("passwd" . ,(user-password ,usr))
                                                            ("user" . ,(user-username ,usr))
                                                            ("api_type" . "json"))
                                              :cookie-jar ,cks 
                                              :want-stream t)))
           (setf (flexi-streams:flexi-stream-external-format ,result) :utf-8)
           (let ((,json  (gethash "json" (yason:parse ,result))))
             (when (not (gethash "errors" ,json))
               (loop for ck in (drakma:cookie-jar-cookies ,cks)
                     do (if (string= "reddit_session" (drakma:cookie-name ck))
                          (setf (drakma:cookie-path ck) "/")))
               (setf (user-modhash ,usr) (gethash "modhash" (gethash "data",json)))
               (setf (user-cookie ,usr) ,cks)
               ,@body))))
       ,@body)))

(defmacro if-user-with (user then)
  `(if ,user
     (with-user (,user) 
       ,(if (listp (car (last then)))
          `(progn ,(append (butlast then) (list (append (car (last then)) (list user)))))
          `(,@then ,user)))
     (,@then)))

(defmacro api-post-generic (url user &key subreddit action id thing-id text vote spam flair-enabled)
  "Defines generic post request"
  (let ((params (gensym)) (result (gensym)))
    `(let ((,params nil))
       ,(when subreddit `(push `("sr_name" . ,subreddit) ,params))
       ,(when action `(push `("action" . ,(symbol-string ,action)) ,params))
       ,(when id `(push `("id" . ,id) ,params))
       ,(when thing-id `(push `("thing_id" . ,thing-id) ,params))
       ,(when text `(push `("text" . ,text) ,params))
       ,(when vote `(push `("dir" . ,(symbol-string ,vote)) ,params))
       ,(when spam `(push `("spam" . ,(if ,spam "1" "0")) ,params))
       ,(when flair-enabled `(push `("flair_enabled" . ,(if ,flair-enabled "1" "0")) ,params))
       (push `("api_type" . "json") ,params)
       (push `("uh" . ,(user-modhash ,user)) ,params)
       (post-request ,url ,user ,params))))

(defmacro def-post-api (api &rest args)
  "Defines an api call."
  `(defun ,(intern (format nil "API-~S" `,api)) (user ,@args)
     (api-post-generic ,(format nil "~a/api/~a.json" *reddit* (string-downcase api)) user ,@(format-key-args args))))
