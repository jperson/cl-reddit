#CL-REDDIT

Common lisp reddit api wrapper

**cl-reddit is now included in [quicklisp](http://github.com/quicklisp)**
```cl
* (ql:quickload "cl-reddit")
```

**Create user and login and view user modhash**

```cl
CL-REDDIT> (defvar u (api-login :username "AzureDiamond" :password "hunter2"))
CL-REDDIT> (user-modhash u)
CL-REDDIT> "393eioafja78iafjioiwoijhgnhn223jik9rjfoq87fnbh13j"
```

**Search with keywords, loop through results**

```cl
CL-REDDIT> (defvar lst (get-search "Lance Armstrong"))
CL-REDDIT> (loop for l in lst do (format t "~a:~a~%" (link-score l) (link-title l)))
```

Search can take several optional parameters, to search restricted to a particular subreddit

```cl
CL-REDDIT> (defvar lst (get-search "Lance Armstrong" :sub "funny" :restrict-sr t))
```

API
===

All key params are required for api-* functions generated from def-post-api macro.
```cl
(defun api-login (&key username password)
  "Login user username with password. Returns a User object with modhash,cookie set.")

(defun api-subscribe (user &key id action))
  "Sub or unsub from subreddit sr for user usr. Action can be :sub or :unsub")

(defun api-comment (user &key id text)
  "Comments text on id with user usr.")

(defun api-editusrtext (user &key id text)
  "Edit user text on id with user usr.")

(defun api-vote (user &key id vote)
  "Vote direction :up :down :unvote for thing with id with user.")

(defun api-save (user &key id)
  "Save thing with id.")

(defun api-unsave (user &key id)
  "Unsave thing with id.")

(defun api-report (user &key id)
  "Report thing with id.")

(defun api-marknsfw (user &key id)
  "Mark thing with id as nsfw.")

(defun api-hide (user &key id)
  "Hide thing with id.")

(defun api-unhide (user &key id)
  "Unhide thing with id.")

(defun api-del (user &key id)
  "Delete thing with id.")

(defun api-block (user &key id)
  "Block thing with id.")

(defun api-read_message (user &key id)
  "Read message with id.")

(defun api-unread_message (user &key id)
  "Unread message with id.")

(defun api-approve (user &key id)
  "Approve thing with id.")

(defun api-leavecontributor (user &key id)
  "Self removal as moderator of thing with id.")

(defun api-leavemoderator (user &key id)
  "Remove as moderator of subreddit with id.")

(defun api-remove (user &key id spam)
  "Remove thing with id. Is-spam t if spam, nil if not.")

(defun api-setflairenabled (user &key flair-enabled)
  "Enable/disable flair.")

(defun get-user (r-user &optional user)
  "Get /user/<r-user>.json.  Optional user.")

(defun get-about-user (about-user &optional usr)
  "Get /user/<about-user>/about.json.  Optional user.")

(defun get-message (user where)
  "Gets messages from inbox for user user.")

(defun get-subscribed (user)
  "Gets subscribed subreddits")

(defun get-comments (id user &key article comment context depth limit sort)
  "Gets comments for link id in subreddit sr.")

(defun get-reddit (&optional user)
  "Gets json data for reddit home page. Optional user.")

(defun get-subreddit (sub &optional user)
  "Gets json data for subreddit sub.  Optional user usr.")

(defun get-subreddit-new (sub &optional user)
  "Gets json data for /r/<sub>/new. Optional user.")

(defun get-subreddit-top (sub &optional user)
  "Gets json data for top posts in subreddit sub. Optional user user.")

(defun get-subreddit-about (sub &optional user)
  "Gets r/<sub>/about.json. Returns Subreddit object about sub. Optional user.")

(defun get-search (query &key user after before count limit restrict-sr show sort syntax time target sub)
  "Search for query.")
```

See cl-reddit.lisp for full api.


Todo
===

Lots of stuff could be improved or extended, contributers are welcome!  Here are a few ideas, feel free to work on any (or all)
of these.

* Request throttling - Basically right now the api will let you send as many requests as you want, which can return errors if too many
get sent in a short time.  It would be nice if the api could handle this and either resend after a timeout or wait to send
the request if too many have been sent in a short time.

* Async api - The basic api is synchronous, it could be used to build async requests but having an async interface along with the basic one could be useful.

* High level api - The current api is pretty low level, mapping to the reddit api, having something that works at a higher level would make this library easier to use. I haven't though much about what this would look like so ideas are welcome.

* Error handling - Currently there isn't anything, so if a api request fails for some reason the result is probably a nil value.

* Better return types - Some of the methods return a hash table or a listing, see (cl-reddit:get-user), which doesn't always make sense.

* Use OAuth - See https://github.com/jperson/cl-reddit/issues/8

* Tests - It would be nice to have a test suite with lots of api coverage, maybe using a unit test framework.

* More documentation - What project couldn't use this?

* Cool apps that use this library so it gets more exposure.


If you have any opinions/ideas on how to make this thing better please share!
