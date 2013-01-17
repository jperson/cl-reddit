;; Copyright (c) 2012, Jason R. Person
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


;;;; cl-reddit.lisp
(in-package #:cl-reddit)

;;;; Base URL ;;;;
(defparameter *reddit* "http://www.reddit.com")

;;;; API ;;;;

;;user
(defun api-login (&key username password)
  "Login user username with password. Returns a User object with modhash,cookie set."
  (let ((usr (make-user :username username :password password)))
    (with-user (usr) usr)))

(defun api-me (usr)
  "Get info for user usr.  Returns user data."
  (let ((url (format nil "~a/api/me.json" *reddit*)))
    (with-user (usr) (get-json url :cookie-jar (user-cookie usr)))))

(defun api-subscribe (usr sr &optional (action :sub))
  "Sub or unsub from subreddit sr for user usr. Action can be :sub or :unsub"
  (with-user (usr)
    (let ((params `(("action" . ,(case action (:sub "sub") (otherwise "unsub")))
                    ("uh" . ,(user-modhash usr)) ("sr_name" . ,sr) ("api_type" . "json")))
          (url (format nil "~a/api/subscribe.json" *reddit*)))
      (post-request url (user-cookie usr) params))))

(defun get-user (r-user &optional usr)
  "Get /user/<r-user>.json.  Optional user usr."
  (let ((url (format nil "~a/user/~a.json" *reddit* r-user)))
    (if (null usr) (get-json url) (get-json url :cookie-jar (user-cookie usr)))))

(defun get-about-user (about-user &optional usr)
  "Get /user/<about-user>/about.json.  Optional user usr."
  (get-user (format nil "~a/about" about-user) usr))

(defun get-message (usr where)
  "Gets messages from inbox for user usr."
  (let ((url (format nil "~a/message/~a.json" *reddit* where)))
    (parse-json (get-json url :cookie-jar (user-cookie usr)))))

(defun get-subscribed (usr)
  "Gets subscribed subreddits"
  (let ((url (format nil "~a/reddits/mine.json" *reddit*)))
    (with-user (usr)
      (listing-children (parse-json (get-json url :cookie-jar (user-cookie usr)))))))

(defun get-comments (id usr &key article comment context depth limit sort)
  "Gets comments for link id in subreddit sr."
  (let ((params nil))
    (when sort (push `("sort" . ,sort) params))
    (when limit (push `("limit" . ,limit) params))
    (when depth (push `("depth" . ,depth) params))
    (when context (push `("context" . ,context) params))
    (when comment (push `("comment" . ,comment) params))
    (when article (push `("article" . ,article) params))
    (let ((url (format nil "~a/comments/~a.json?~a" *reddit* id (build-get-params params))))
      (with-user (usr)
        (butlast (listing-children (parse-json (second (get-json url :cookie-jar (user-cookie usr))))))))))

(defun api-comment (usr id text)
  "Comments text on id with user usr."
  (with-user (usr)
    (let ((url (format nil "~a/api/comment.json" *reddit*))
          (params `(("thing_id" . ,(format nil "t3_~a" id))
                    ("uh" . ,(user-modhash usr))
                    ("text" . ,text)
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))

(defun api-editusrtext (usr id text)
  "Edit user text on id with user usr."
  (with-user (usr)
    (let ((url (format nil "~a/api/editusertext.json" *reddit*))
          (params `(("thing_id" . ,(format nil "t3_~a" id))
                    ("uh" . ,(user-modhash usr))
                    ("text" . ,text)
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))

(defun api-vote (usr id &optional (dir :up))
  "Vote direction dir for thing with id with user usr."
  (with-user (usr)
    (let ((url (format nil "~a/api/vote.json" *reddit*))
          (params `(("dir" . ,(case dir (:up "1") (:down "-1") (:unvote "0") (otherwise "1")))
                    ("id" . ,id)
                    ("uh" . ,(user-modhash usr))
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))

(defun api-save (usr id)
  "Save thing with id."
  (api-generic (format nil "~a/api/save.json" *reddit*) usr id))

(defun api-unsave (usr id)
  "Unsave thing with id."
  (api-generic (format nil "~a/api/unsave.json" *reddit*) usr id))

(defun api-report (usr id)
  "Report thing with id."
  (api-generic (format nil "~a/api/report.json" *reddit*) usr id))

(defun api-marknsfw (usr id)
  "Mark thing with id as nsfw."
  (api-generic (format nil "~a/api/marknsfw.json" *reddit*) usr id))

(defun api-hide (usr id)
  "Hide thing with id."
  (api-generic (format nil "~a/api/hide.json" *reddit*) usr id))

(defun api-unhide (usr id)
  "Unhide thing with id."
  (api-generic (format nil "~a/api/unhide.json" *reddit*) usr id))

(defun api-del (usr id)
  "Delete thing with id."
  (api-generic (format nil "~a/api/del.json" *reddit*) usr id))

(defun api-block (usr id)
  "Block thing with id."
  (api-generic (format nil "~a/api/block.json" *reddit*) usr id))

(defun api-read-message (usr id)
  "Read message with id."
  (api-generic (format nil "~a/api/read_message.json" *reddit*) usr id))

(defun api-unread-message (usr id)
  "Unread message with id."
  (api-generic (format nil "~a/api/unread_messgae.json" *reddit*) usr id))

(defun api-approve (usr id)
  "Approve thing with id."
  (api-generic (format nil "~a/api/approve.json" *reddit*) usr id))

(defun api-leave-contributor (usr id)
  "Self removal as moderator of thing with id."
  (api-generic (format nil "~a/api/leavecontributor.json" *reddit*) usr id))

(defun api-leave-moderator (usr id)
  "Remove as moderator of subreddit with id."
  (api-generic (format nil "~a/api/leavemoderator.json" *reddit*) usr id))

(defun api-remove (usr id &key is-spam)
  "Remove thing with id. Is-spam t if spam, nil if not."
  (with-user (usr)
    (let ((url (format nil "~a/api/remove.json" *reddit*))
          (params `(("id" . ,id)
                    ("uh" . ,(user-modhash usr))
                    ("spam" . ,(if is-spam "1" "0"))
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))
 
(defun api-setflairenabled (usr &key flair-enabled)
  "Enable/disable flair."
  (with-user (usr)
    (let ((url (format nil "~a/api/setflairenabled.json" *reddit*))
          (params `(("uh" . ,(user-modhash usr))
                    ("flair_enabled" . ,(if flair-enabled "1" "0"))
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))

(defun api-generic (url usr id)
  "Generic api call to url with id and modhash."
  (with-user (usr)
    (let ((params `(("id" . ,id)
                    ("uh" . ,(user-modhash usr))
                    ("api_type" . "json"))))
      (yason:parse (post-request url (user-cookie usr) params)))))

;;Listings
(defun get-reddit (&optional usr)
  "Gets json data for reddit home page. Optional user usr."
  (let ((url (format nil "~a/.json" *reddit*)))
    (listing-children
      (parse-json 
        (if (null usr) (get-json url) (get-json url :cookie-jar (user-cookie usr)))))))

(defun get-subreddit (sub &optional usr)
  "Gets json data for subreddit sub.  Optional user usr."
  (let ((url (format nil "~a/r/~a.json" *reddit* sub)))
    (listing-children 
      (parse-json
        (if (null usr) (get-json url) (get-json url :cookie-jar (user-cookie usr)))))))

(defun get-subreddit-new (sub &optional usr)
  "Gets json data for /r/<sub>/new. Optional user usr."
  (get-subreddit (format nil "~a/new.json" sub) usr))

(defun get-subreddit-top (sub &optional usr)
  "Gets json data for top posts in subreddit sub. Optional user usr."
  (get-subreddit (format nil "~a/top.json" sub) usr))

(defun get-subreddit-about (sub &optional usr)
  "Gets r/<sub>/about.json. Returns Subreddit object about sub. Optional user usr."
  (get-subreddit (format nil "~a/about.json" sub) usr))

(defun get-search (query &key after before count limit restrict_sr show sort syntax time target sub)
  "Search for query."
  (let ((params nil))
    (when target (push `("target" . ,target) params))
    (when time (push `("time" . ,time) params))
    (when syntax (push `("syntax" . ,syntax) params))
    (when sort (push `("sort" . ,sort) params))
    (when show (push `("show" . ,show) params))
    (when restrict_sr (push `("restrict_sr" . ,restrict_sr) params))
    (when limit (push `("limit" . ,limit) params))
    (when count (push `("count" . ,count) params))
    (when before (push `("before" . ,before) params))
    (when after (push `("after" . ,after) params))
    (push `("q" . ,query) params)
    (listing-children
      (parse-json
        (if (null sub)
          (get-json (format nil "~a/search.json?~a" *reddit* (build-get-params params)))
          (get-json (format nil "~a/r/~a/search.json?~a" *reddit* sub (build-get-params params))))))))
