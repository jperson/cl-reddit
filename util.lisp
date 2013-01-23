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

(defun build-post-params (p)
  (loop for (p v) on p by #'cddr
        collect `(,(string-downcase (symbol-name p)) . ,v)))

(defun api-get-generic (url &key user query after before count limit restrict_sr show sort syntax time target)
  (let ((params nil))
    (when target (push `("target" . ,target) params))
    (when time (push `("time" . ,time) params))
    (when syntax (push `("syntax" . ,syntax) params))
    (when sort (push `("sort" . ,sort) params))
    (when show (push `("show" . ,show) params))
    (when restrict_sr (push `("restrict_sr" . "1") params))
    (when limit (push `("limit" . ,limit) params))
    (when count (push `("count" . ,count) params))
    (when before (push `("before" . ,before) params))
    (when after (push `("after" . ,after) params))
    (when query (push `("q" . ,query) params))
    (parse-json
      (if params
        (get-json (format nil "~a?~a" url (build-get-params params)) :cookie-jar (user-cookies user))
        (get-json url :cookie-jar (user-cookies user))))))

(defun format-key-args (args)
  (let ((params))
    (loop for arg in (cdr args) do
          (push arg params) 
          (push (values (intern (string-upcase `,arg) "KEYWORD")) params))
    params))

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

(defmacro api-post-generic (url user &key subreddit action id thing-id text vote spam flair-enabled)
  (let ((params (gensym)))
    `(let ((,params nil))
       ,(when subreddit `(push `("sr_name" . ,subreddit) ,params))
       ,(when action `(push `("action" . ,(case ,action (:sub "sub") (:unsub "unsub"))) ,params))
       ,(when id `(push `("id" . ,id) ,params))
       ,(when thing-id `(push `("thing_id" . ,thing-id) ,params))
       ,(when text `(push `("text" . ,text) ,params))
       ,(when vote `(push `("dir" . ,(case ,vote (:up "1") (:down "-1") (:unvote "0"))) ,params))
       ,(when spam `(push `("spam" . ,(if ,spam "1" "0")) ,params))
       ,(when flair-enabled `(push `("flair_enabled" . ,(if ,flair-enabled "1" "0")) ,params))
       (push `("api_type" . "json") ,params)
       (push `("uh" . ,(user-modhash ,user)) ,params)
       (yason:parse (post-request ,url ,user ,params)))))

(defmacro defapi (api method &rest args)
  "Defines an api call."
  `(defun ,(intern (format nil "API-~S" `,api)) (user ,@args)
     ,(case method
        (:get `(api-get-generic ,(format nil "~a/~a.json" *reddit* (string-downcase api)) ,@(format-key-args args)))
        (:post `(api-post-generic ,(format nil "~a/api/~a.json" *reddit* (string-downcase api)) user ,@(format-key-args args))))))
