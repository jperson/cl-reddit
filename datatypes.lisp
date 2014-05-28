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

;; Base classes
(defclass thing ()
  ((id
    :initarg :id :initform "" :accessor thing-id :type string
     :documentation "This item's identifiler, e.g. 8xwlg")
   (name
    :initarg :name :initform "" :accessor thing-name :type string
     :documentation "Fullname of comment, e.g. t1_c3v7f8u")
   (kind
    :initarg :kind :initform "" :accessor thing-kind :type string
     :documentation "String identifier that denotes the object's type")
   (data
     :initarg :data :initform nil :accessor thing-data
     :documentation "Custom data structure")))

(defclass listing ()
  ((before
     :initarg :before :initform "" :accessor listing-before :type (or null string)
     :documentation "The fullname of the listing that follows before this page")
   (after
     :initarg :after :initform "" :accessor listing-after :type (or null string)
     :documentation "The fullname of the listing that follows after this page")
   (modhash
     :initarg :modhash :initform "" :accessor listing-modhash :type string
     :documentation "Modhash value for this listing")
   (children
     :initarg :children :initform nil :accessor listing-children
     :documentation "A list of things that this listing wraps")))

(defclass volatile ()
  ((ups
     :initarg :ups :initform 0 :accessor volatile-ups :type integer
     :documentation "The number of upvotes")
   (downs
     :initarg :downs :initform 0 :accessor volatile-downs :type integer
     :documentation "The number of downvotes")
   (likes
     :initarg :likes :initform nil :accessor volatile-likes
     :documentation "True if liked by user, false if disliked, nil if user neutral")))

(defclass created ()
  ((created
     :initarg :created :initform 0 :accessor created-created)
   (created_utc
     :initarg :created_utc :initform 0 :accessor created-created_utc)))

;; Datastructures
(defclass comment (volatile created)
  ((id
     :initarg :id :initform "" :accessor comment-id :type string
     :documentation "The id of the comment.")
   (name
     :initarg :name :initform "" :accessor comment-name :type string
     :documentation "The name of the comment.")
   (author
     :initarg :author :initform nil :accessor comment-author :type string
     :documentation "The account name of the poster")
   (author_flair_css_class :initarg :author_flair_css_class :initform nil :accessor comment-author_flair_css_class :type string
     :documentation "The css class of the author's flair")
   (author_flair_text :initarg :author_flair_text :initform "" :accessor comment-author_flair_text :type string
     :documentation "The text of the author's flair")
   (body :initarg :body :initform "" :accessor comment-body :type string
     :documentation "The raw text")
   (body_html :initarg :body_html :initform "" :accessor comment-body_html :type string
     :documentation "The formatted html text")
   (link_id
     :initarg :link_id :initform "" :accessor comment-link_id :type string)
   (parent_id
     :initarg :parent_id :initform "" :accessor comment-parent_id :type string)
   (subreddit
     :initarg :subreddit :initform "" :accessor comment-subreddit :type string)
   (subreddit_id
     :initarg :subreddit_id :initform "" :accessor comment-subreddit_id :type string)
   (replies
     :initarg :replies :initform nil :accessor comment-replies
     :documentation "Comment replies."
     )))

(defclass link (volatile created)
 ((id
    :initarg :id :initform "" :accessor link-id :type string
    :documentation "The id of this link")
  (name
    :initarg :name :initform "" :accessor link-name :type string
    :documentation "The name of this link.")
  (author
    :initarg :author :initform "" :accessor link-author :type string
    :documentation "The account name of the poster")
  (author_flair_css_class :initarg author_flair_css_class :initform "" :accessor link-author_flair_css_class :type (or null string)
    :documentation "The css class of the author's flair")
  (author_flair_text :initarg :author_flair_text :initform "" :accessor link-author_flair_text :type (or null string)
    :documentation "The text of the author's flair")
  (clicked :initarg :clicked :initform nil :accessor link-clicked :type boolean
    :documentation "Probably always returns false")
  (domain :initarg :domain :initform "" :accessor link-domain :type string
    :documentation "The domain of this link")
  (hidden :initarg :hidden :initform nil :accessor link-hidden :type boolean
    :documentation "True if the post is hidden by the logged in user. False if not logged in or not hidden")
  (is_self :initarg :is_self :initform nil :accessor link-is_self :type boolean
    :documentation "True if this link is a selfpost")
  (media :initarg :media :initform nil :accessor link-media
    :documentation "unknown")
  (media_embed :initarg :media_embed :initform nil :accessor link-media_embed
    :documentation "unknown")
  (num_comments :initarg :num_comments :initform 0 :accessor link-num_comments :type integer
    :documentation "The number of comments that belong to this link")
  (over_18 :initarg :over_18 :initform nil :accessor link-over_18 :type boolean
    :documentation "True if the post is taggeed as NSFW. Nil otherwise")
  (permalink :initarg :permalink :initform "" :accessor link-permalink :type string
    :documentation "Relative url of the permanent link for this link")
  (saved :initarg :saved :initform nil :accessor link-saved :type boolean
    :documentation "True if this post is saved by the logged in user")
  (score
    :initarg :score :initform 0 :accessor link-score :type integer
    :documentation "The net-score of the link")
  (selftext
    :initarg :selftext :initform "" :accessor link-selftext :type string
    :documentation "The raw text")
  (selfttext_html :initarg :selftext_html :initform "" :accessor link-selftext_html :type (or null string)
    :documentation "The formatted escaped html text")
  (subreddit
    :initarg :subreddit :initform "" :accessor link-subreddit :type string)
  (subreddit_id
    :initarg :subreddit_id :initform "" :accessor link-subreddit_id :type string)
  (thumbnail :initarg :thumbnail :initform "" :accessor link-thumbnail :type string
    :documentation "Full url to the thumbnail for this link")
  (title
    :initarg :title :initform "" :accessor link-title :type string)
  (url
    :initarg :url :initform "" :accessor link-url :type string
    :documentation "The link of this post")
  (edited :initarg :edited :initform 0 :accessor link-edited :type (or null integer)
    :documentation "Indicates if link has been edited")))

(defclass subreddit (created)
 ((name
    :initarg :name :initform "" :accessor subreddit-name :type string)
  (id
    :initarg :id :initform "" :accessor subreddit-id :type string)
  (display_name
    :initarg :display_name :initform "" :accessor subreddit-display_name :type string)
  (subscribers
    :initarg :subscribers :initform 0 :accessor subreddit-subscribers :type integer
    :documentation "The number of users subscribed to this subreddit")
  (header_size
    :initarg :header_size :initform 0 :accessor subreddit-header_size :type list)
  (over18
    :initarg :over18 :initform nil :accessor subreddit-over18 :type boolean)
  (accounts_active
    :initarg :accounts_active :initform 0 :accessor subreddit-accounts_active :type integer)
  (public_description
    :initarg :public_description :initform "" :accessor subreddit-public_description :type string)
  (description
    :initarg :description :initform "" :accessor subreddit-description :type string)
  (description_html
    :initarg :description_html :initform "" :accessor subreddit-description_html :type string)
  (header_title
    :initarg :header_title :initform "" :accessor subreddit-header_title :type (or null string))
  (header_img
    :initarg :header_img :initform "" :accessor subreddit-header_img :type string)
  (title
    :initarg :title :initform "" :accessor subreddit-title :type string)
  (url
    :initarg :url :initform "" :accessor subreddit-url :type string
    :documentation "The relative URL of the subreddit")))

(defclass message (created)
 ((author
    :initarg :author :initform "" :accessor message-author :type string)
  (body
    :initarg :body :initform "" :accessor message-body :type string)
  (body_html
    :initarg :body_html :initform "" :accessor message-body_html :type string)
  (context
    :initarg :context :initform "" :accessor message-context :type string)
  (first_message
    :initarg :first_message :initform nil :accessor message-first_message)
  (name
    :initarg :name :initform "" :accessor message-name :type string)
  (new
    :initarg :new :initform nil :accessor message-new :type boolean)
  (parent_id
    :initarg :parent_id :initform "" :accessor message-parent_id :type string)
  (replies
    :initarg :replies :initform "" :accessor message-replies :type string)
  (subject
    :initarg :subject :initform "" :accessor message-subject :type string)
  (subreddit
    :initarg :subreddit :initform "" :accessor message-subreddit :type string)
  (was_comment
    :initarg :was_comment :initform nil :accessor message-was_comment :type boolean)))

(defclass account ()
 ((comment_karma
    :initarg :comment_karma :initform 0 :accessor account-comment_karma :type integer)
  (created
    :initarg :created :initform 0 :accessor account-created :type integer)
  (created_utc
    :initarg :created_utc :initform 0 :accessor account-created_utc :type integer)
  (has_mail
    :initarg :has_mail :initform nil :accessor account-has_mail :type boolean)
  (has_mod_mail
    :initarg :has_mod_mail :initform nil :accessor account-has_mod_mail :type boolean)
  (id
    :initarg :id :initform "" :accessor account-id :type string)
  (is_gold
    :initarg :is_gold :initform nil :accessor account-is_gold :type boolean)
  (is_mod
    :initarg :is_mod :initform nil :accessor account-is_mod :type boolean)
  (link_karma
    :initarg :link_karma :initform 0 :accessor account-link_karma :type integer)
  (modhash
    :initarg :modhash :initform "" :accessor account-modhash :type string)
  (name
    :initarg :name :initform "" :accessor account-name :type string)))

(defclass more ()
  ((cnt
     :initarg :cnt :initform 0 :accessor more-count :type integer)
   (parent_id
     :initarg :parent_id :initform "" :accessor more-parent_id :type string)
   (id
     :initarg :id :initform "" :accessor more-id :type string)
   (name
     :initarg :name :initform "" :accessor more-name :type string)
   (children
     :initarg :children :initform nil :accessor more-children)))

;; User class
(defclass user ()
  ((cookie
     :initarg :cookie :initform nil :accessor user-cookie)
   (password
     :initarg :password :initform nil :accessor user-password)
   (username
     :initarg :username :initform nil :accessor user-username)
   (modhash
     :initarg :modhash :initform nil :accessor user-modhash)
   (logged-in
     :initarg :logged-in :initform nil :accessor user-logged-in)))

(defun maybe-round (x)
  (and x (round x)))

;; json to thing constructors
(defun link-from-json (json)
  (make-instance
    'link
    :id (gethash "id" json)
    :name (gethash "name" json)
    :ups (gethash "ups" json)
    :downs (gethash "downs" json)
    :likes (gethash "likes" json)
    :created (maybe-round (gethash "created" json))
    :created_utc (maybe-round (gethash "created_utc" json))
    :author (gethash "author" json)
    ;:author_flair_css_class (gethash "author_flair_css_class" json)
    :author_flair_text (gethash "author_flair_text" json)
    :clicked (gethash "clicked" json)
    :domain (gethash "domain" json)
    :hidden (gethash "hidden" json)
    :is_self (gethash "is_self" json)
    :media (gethash "media" json)
    :media_embed (gethash "media_embed" json)
    :num_comments (gethash "num_comments" json)
    :over_18 (gethash "over_18" json)
    :permalink (gethash "permalink" json)
    :saved (gethash "saved" json)
    :score (gethash "score" json)
    :selftext (gethash "selftext" json)
    :selftext_html (gethash "selftext_html" json)
    :subreddit (gethash "subreddit" json)
    :subreddit_id (gethash "subreddit_id" json)
    :thumbnail (gethash "thumbnail" json)
    :title (gethash "title" json)
    :url (gethash "url" json)
    :edited (maybe-round (gethash "edited" json))))

(defun subreddit-from-json (json)
  (make-instance
    'subreddit
    :name (gethash "name" json)
    :id (gethash "id" json)
    :display_name (gethash "display_name" json)
    :subscribers (gethash "subscribers" json)
    :header_size (gethash "header_size" json)
    :over18 (gethash "over18" json)
    :accounts_active (gethash "accounts_active" json)
    :public_description (gethash "public_description" json)
    :created_utc (maybe-round (gethash "created_utc" json))
    :created (maybe-round (gethash "created" json))
    :url (gethash "url" json)
    :title (gethash "title" json)
    :description_html (gethash "description_html" json)
    :description (gethash "description" json)
    :header_title (gethash "header_title" json)
    :header_img (gethash "header_img" json)))

(defun comment-from-json (json)
  (let ((replies (gethash "replies" json)))
    (make-instance
      'comment
      :id (gethash "id" json)
      :name (gethash "name" json)
      :ups (gethash "ups" json)
      :downs (gethash "downs" json)
      :likes (gethash "lieks" json)
      :author (gethash "author" json)
      :author_flair_css_class (gethash "author_flair_css_class" json)
      :author_flair_text (gethash "author_flair_text" json)
      :body (gethash "body" json)
      :body_html (gethash "body_html" json)
      :link_id (gethash "link_id" json)
      :parent_id (gethash "parent_id" json)
      :subreddit (gethash "subreddit" json)
      :subreddit_id (gethash "subreddit_id" json)
      :replies (if (typep replies 'HASH-TABLE)
                 (listing-children (parse-json (gethash "replies" json)))
                 nil))))
;      :replies replies)))
      ;:replies (first (children (parse-json (gethash "replies" json)))))))

(defun account-from-json (json)
  (make-instance
    'account
    :comment_karma (gethash "comment_karma" json)
    :created (maybe-round (gethash "created" json))
    :created_utc (maybe-round (gethash "created_utc" json))
    :has_mail (gethash "has_mail" json)
    :has_mod_mail (gethash "has_mod_mail" json)
    :id (gethash "id" json)
    :is_gold (gethash "is_gold" json)
    :is_mod (gethash "is_mod" json)
    :link_karma (gethash "link_karma" json)
    :modhash (gethash "modhash" json)
    :name (gethash "name" json)))

(defun message-from-json (json)
  (make-instance
    'message
    :author (gethash "author" json)
    :body (gethash "body" json)
    :body_html (gethash "body_html" json)
    :context (gethash "context" json)
    :first_message (gethash "first_message" json)
    :name (gethash "name" json)
    :new (gethash "new" json)
    :parent_id (gethash "parent_id" json)
    :replies (gethash "replies" json)
    :subject (gethash "subject" json)
    :subreddit (gethash "subreddit" json)
    :was_comment (gethash "was_comment" json)))

(defun listing-from-json (json)
  (make-instance
    'listing
    :before (gethash "before" json)
    :after (gethash "after" json)
    :modhash (gethash "modhash" json)
    :children (map 'list #'parse-json (gethash "children" json))))

(defun more-from-json (json)
  (make-instance
    'more
    :cnt (gethash "count" json)
    :parent_id (gethash "parent_id" json)
    :id (gethash "id" json)
    :name (gethash "name" json)
    :children (gethash "children" json)))

(defun parse-json (data)
  (labels ((thing-from-json (json)
               (let ((kind (gethash "kind" json))
                     (data (gethash "data" json)))
                 (alexandria:switch (kind :test #'EQUAL)
                   ("t1" (comment-from-json data))
                   ("t2" (account-from-json data))
                   ("t3" (link-from-json data))
                   ("t4" (message-from-json data))
                   ("t5" (subreddit-from-json data))
                   ("Listing" (listing-from-json data))
                   ("more" (more-from-json data))
                   (otherwise (make-instance 'thing :kind kind :data data))))))
    (if (listp data)
      (loop for d in data collect (thing-from-json d))
      (thing-from-json data))))
