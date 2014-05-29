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
(defpackage #:cl-reddit
  (:use #:cl #:asdf)
            ;;API
  (:export #:api-login
           #:api-me
           #:api-subscribe
           #:get-user
           #:get-about-user
           #:get-message
           #:get-subscribed
           #:get-comments
           #:api-comment
           #:api-editusrtext
           #:api-vote
           #:api-save
           #:api-unsave
           #:api-report
           #:api-marknsfw
           #:api-hide
           #:api-unhide
           #:api-del
           #:api-block
           #:api-read_message
           #:api-unread_message
           #:api-approve
           #:api-leavecontributor
           #:api-leavemoderator
           #:api-remove
           #:api-setflairenabled
           #:get-reddit
           #:get-subreddit
           #:get-subreddit-new
           #:get-subreddit-top
           #:get-subreddit-about
           #:get-search
           ;;Datatypes
           :thing
           :listing
           :volatile
           :created
           :comment
           :link
           :subreddit
           :message
           :account
           :more
           :user
           ;;Accessors
           #:thing-id
           #:thing-name
           #:thing-kind
           #:thing-data
           #:listing-before
           #:listing-after
           #:listing-modhash
           #:listing-children
           #:volatile-ups
           #:volatile-downs
           #:volatile-likes
           #:created-created
           #:created-created_utc
           #:comment-id
           #:comment-name
           #:comment-author
           #:comment-author_flair_css_class
           #:comment-author_flair_text
           #:comment-body
           #:comment-body_html
           #:comment-link_id
           #:comment-parent_id
           #:comment-subreddit
           #:comment-replies
           #:link-id
           #:link-name
           #:link-author
           #:link-author_flair_css_class
           #:link-author_flair_text
           #:link-domain
           #:link-hidden
           #:link-is_self
           #:link-media
           #:link-media_embed
           #:link-num_comments
           #:link-over_18
           #:link-permalink
           #:link-saved
           #:link-score
           #:link-selftext
           #:link-selftext_html
           #:link-subreddit
           #:link-subreddit_id
           #:link-thumbnail
           #:link-title
           #:link-url
           #:link-edited
           #:subreddit-name
           #:subreddit-id
           #:subreddit-display_name
           #:subreddit-subscribers
           #:subreddit-header_size
           #:subreddit-header_size
           #:subreddit-over18
           #:subreddit-accounts_active
           #:subreddit-public_description
           #:subreddit-description_html
           #:subreddit-header_title
           #:subreddit-header_img
           #:subreddit-title
           #:subreddit-url
           #:message-author
           #:message-body
           #:message-body_html
           #:message-context
           #:message-first_message
           #:message-name
           #:message-new
           #:message-parent_id
           #:message-replies
           #:message-subject
           #:message-subreddit
           #:message-was_comment
           #:account-comment_karma
           #:account-created
           #:account-created_utc
           #:account-has_mail
           #:account-has_mod_mail
           #:account-id
           #:account-is_gold
           #:account-is_mod
           #:account-link_karma
           #:account-modhash
           #:account-name
           #:more-count
           #:more-parent_id
           #:more-id
           #:more-name
           #:more-children
           #:user-cookie
           #:user-password
           #:user-username
           #:user-modhash
           #:user-logged-in
           ;;Helpers
           #:with-user
           #:make-user
           ;;user-agent
           *user-agent*
           )
  (:documentation "Reddit api wrapper."))
