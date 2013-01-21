;;;; util.lisp ;;;;

(in-package #:cl-reddit)

;;;; Helper functions ;;;;
(defun make-user (&key username password)
  (make-instance 'User :username username :password password))

(defun api-generic (url usr id)
  "Generic api call to url with id and modhash."
  (with-user (usr)
    (let ((params `(("id" . ,id)
                    ("uh" . ,(user-modhash usr))
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))

;;;; Helper macros ;;;;
(defmacro with-user ((usr) &body body)
  "Does 'body' with logged-in user usr.  Logins in user if not logged-in."
  (let ((json (gensym)) (result (gensym)) (cks (gensym)))
    `(let ((,cks (make-instance 'drakma:cookie-jar)))
       (if (null (user-modhash ,usr))
         (progn
           (let ((,result (drakma:http-request "http://www.reddit.com/api/login.json"
                                                  :method :post
                                                  :parameters `(("passwd" . ,(user-password ,usr))
                                                                 ("user" . ,(user-username ,usr))
                                                                 ("api_type" . "json"))
                                                  :cookie-jar ,cks 
                                                  :want-stream t)))
             (setf (flexi-streams:flexi-stream-external-format ,result) :utf-8)
             (let ((,json  (gethash "json" (yason:parse ,result))))
               (if (null (gethash "error" ,json))
                 (progn
                   (loop for ck in (drakma:cookie-jar-cookies ,cks)
                         do (if (string= "reddit_session" (drakma:cookie-name ck))
                              (setf (drakma:cookie-path ck) "/")))
                   (setf (user-modhash ,usr) (gethash "modhash" (gethash "data",json)))
                   (setf (user-cookie ,usr) ,cks)) 
                 (print "Error"))))))
       ,@body)))
