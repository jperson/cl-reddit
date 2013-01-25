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

;;;; API ;;;;
(defun api-login (&key username password)
  "Login user username with password. Returns a User object with modhash,cookie set."
  (let ((usr (make-user :username username :password password)))
    (with-user (usr) usr)))

; "Sub or unsub from subreddit sr for user usr. Action can be :sub or :unsub"
(def-post-api subscribe &key subreddit action)

; "Comments text on id with user usr."
(def-post-api comment &key thing-id text)

; "Edit user text on id with user usr."
(def-post-api editusertext &key thing-id text)

; "Vote direction dir for thing with id with user usr."
(def-post-api vote &key id vote)

; "Save thing with id."
(def-post-api save &key id)

; "Unsave thing with id."
(def-post-api unsave &key id)

; "Report thing with id."
(def-post-api report &key id)

; "Mark thing with id as nsfw."
(def-post-api marknsfw &key id)

; Unmark thing with id
(def-post-api unmarknsfw &key id)

; "Hide thing with id."
(def-post-api hide &key id)

; "Unhide thing with id."
(def-post-api unhide &key id)

; "Delete thing with id."
(def-post-api del &key id)

; "Block thing with id."
(def-post-api block &key id)

; "Read message with id."
(def-post-api read_message &key id)

; "Unread message with id."
(def-post-api unread_message &key id)

; "Approve thing with id."
(def-post-api approve &key id)

; "Self removal as moderator of thing with id."
(def-post-api leavecontributor &key id)

; "Remove as moderator of subreddit with id."
(def-post-api leavemoderator &key id)

; "Remove thing with id. Is-spam t if spam, nil if not."
(def-post-api remove &key id spam)

; "Enable/disable flair."
(def-post-api setflairenabled &key flair-enabled)

(defun api-me (user)
  "Get info for user usr.  Returns user data."
  (let ((url (format nil "~a/api/me.json" *reddit*)))
    (with-user (user) (get-json url user))))

(defun get-user (r-user &optional (user nil))
  "Get /user/<r-user>.json.  Optional user user."
  (let ((url (format nil "~a/user/~a.json" *reddit* r-user)))
    (if user
      (with-user (user) (get-json url user))
      (get-json url nil))))

(defun get-about-user (about-user &optional (user nil))
  "Get /user/<about-user>/about.json.  Optional user user."
  (let ((url (format nil "~a/user/~a/about.json" *reddit* about-user)))
    (if user
      (with-user (user) (get-json url user))
      (get-json url nil))))

(defun get-message (user where)
  "Gets messages from inbox for user user."
  (let ((url (format nil "~a/message/~a.json" *reddit* (string-symbol where))))
    (with-user (user)
      (parse-json (get-json url user)))))

(defun get-username-available (username)
  "Check if a username is available."
  (let ((url (format nil "~a/api/username_available.json~a" 
                     *reddit* (build-get-params `(("u" . ,username))))))
    (parse-json (get-json url))))

;;Listings
(defun get-reddit (&optional (user nil))
  "Gets json data for reddit home page. Optional user usr."
  (let ((url (format nil "~a/.json" *reddit*)))
    (listing-children 
      (if user
        (with-user (user) (parse-json (get-json url user)))
        (parse-json (get-json url nil))))))

(defun get-subreddit (sub &optional (user nil))
  "Gets json data for subreddit sub.  Optional user usr."
  (let ((url (format nil "~a/r/~a.json" *reddit* sub)))
    (listing-children 
      (if user
        (with-user (user) (parse-json (get-json url user)))
        (parse-json (get-json url nil))))))

(defun get-subreddit-new (sub &optional user)
  "Gets json data for /r/<sub>/new. Optional user usr."
  (get-subreddit (format nil "~a/new.json" sub) user))

(defun get-subreddit-top (sub &optional user)
  "Gets json data for top posts in subreddit sub. Optional user usr."
  (get-subreddit (format nil "~a/top.json" sub) user))

(defun get-subreddit-about (sub &optional user)
  "Gets r/<sub>/about.json. Returns Subreddit object about sub. Optional user usr."
  (get-subreddit (format nil "~a/about.json" sub) user))

(defun get-subscribed (user)
  "Gets subscribed subreddits"
  (let ((url (format nil "~a/reddits/mine.json" *reddit*)))
    (with-user (user)
      (listing-children 
        (parse-json (get-json url user))))))

(defun get-reddits-mine (user &key (where 'subscriber) after before count limit show target)
  "Gets listing of subreddits ('subscriber 'moderator 'contributor)for user."
  (let ((usr (format nil "~a/reddits/mine/~a.json" *reddit* (symbol-string where)))
        (params))
    (when after (push `("after" . ,after) params))
    (when before (push `("before" . ,before) params))
    (when count (push `("count" . ,count) params))
    (when limit (push `("limit" . ,limit) params))
    (when show (push `("show" . ,show) params))
    (when target (push `("target" . ,target) params))
    (with-user (user)
      (listing-children 
        (parse-json (get-json url user))))))

(defun get-reddits-where (user &key (where 'new) after before count limit show target)
  "Gets listing of subreddits (:subscriber :moderator :contributor)for user."
  (let ((usr (format nil "~a/reddits/~a.json" *reddit* (symbol-string where)))
        (params))
    (when after (push `("after" . ,after) params))
    (when before (push `("before" . ,before) params))
    (when count (push `("count" . ,count) params))
    (when limit (push `("limit" . ,limit) params))
    (when show (push `("show" . ,show) params))
    (when target (push `("target" . ,target) params))
    (with-user (user)
      (listing-children 
        (parse-json (get-json url user))))))

(defun get-search (query &key user after before count limit restrict-sr show sort syntax time target sub)
  "Search for query."
  (let ((params)
        (url (if sub (format nil "~a/r/~a/search.json" *reddit* sub) (format nil "~a/search.json" *reddit*))))
    (when after (push `("after" . ,after) params))
    (when before (push `("before" . ,before) params))
    (when count (push `("count" . ,count) params))
    (when limit (push `("limit" . ,limit) params))
    (when restrict-sr (push `("restrict_sr" . "1") params))
    (when show (push `("show" . ,show) params))
    (when sort (push `("sort" . ,(symbol-string sort)) params))
    (when time (push `("time" . ,time) params))
    (when target (push `("target" . ,target) params))
    (push `("q" . ,query) params)
    (when params (setf url (format nil "~a?~a" url (build-get-params params))))
    (listing-children
      (if user
        (with-user (user) (parse-json (get-json url user)))
        (parse-json (get-json url nil))))))

(defun get-comments (id user &key article comment context depth limit sort)
  "Gets comments for link id in subreddit sr."
  (let ((params nil))
    (when sort (push `("sort" . ,sort) params))
    (when limit (push `("limit" . ,limit) params))
    (when depth (push `("depth" . ,depth) params))
    (when context (push `("context" . ,context) params))
    (when comment (push `("comment" . ,comment) params))
    (when article (push `("article" . ,article) params))
    (let ((url (format nil "~a/comments/~a.json?~a" *reddit* id (build-get-params params))))
      (with-user (user)
        (butlast (listing-children (parse-json (second (get-json url user)))))))))
