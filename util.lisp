;;;; util.lisp ;;;;

(in-package #:cl-reddit)

;;;; Helper macros ;;;;
(defmacro with-user ((usr) &body body)
  "Does 'body' with logged-in user usr.  Logins in user if not logged-in."
  (let ((json (gensym)) (result (gensym)) (cks (gensym)))
    `(let ((,cks (make-instance 'drakma:cookie-jar)))
       (if (null (modhash ,usr))
         (progn
           (let ((,result (drakma:http-request "http://www.reddit.com/api/login.json"
                                                  :method :post
                                                  :parameters `(("passwd" . ,(password ,usr))
                                                                 ("user" . ,(username ,usr))
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
                   (setf (modhash ,usr) (gethash "modhash" (gethash "data",json)))
                   (setf (cookie ,usr) ,cks)) 
                 (print "Error"))))))
       ,@body)))
