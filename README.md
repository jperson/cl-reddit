#CL-REDDIT  

Common lisp reddit api wrapper


**Create user and login and view user modhash**  
    
    CL-REDDIT> (defvar u (api-login :username "AzureDiamond" :password "hunter2"))
    CL-REDDIT> (user-modhash u)
    CL-REDDIT> "393eioafja78iafjioiwoijhgnhn223jik9rjfoq87fnbh13jv"`
    
**Search with keywords, loop through results**

    CL-REDDIT> (defvar lst (get-search "Lance Armstrong"))
    CL-REDDIT> (loop for l in lst do (format t "~a:~a~%" (link-score l) (link-title l)))
    
Search can take several optional parameters, to search restricted to a particular subreddit

    CL-REDDIT> (defvar lst (get-search "Lance Armstrong" :sub "funny" :restrict_sr t))
    
API
===  

(defun api-login (&key username password)  
  "Login user username with password. Returns a User object with modhash,cookie set.")  

(defun api-me (usr)  
  "Get info for user usr.  Returns user data.")  

(defun api-subscribe (usr sr &optional (action :sub))  
  "Sub or unsub from subreddit sr for user usr. Action can be :sub or :unsub")  

(defun api-comment (usr id text)  
  "Comments text on id with user usr.")  

(defun api-editusrtext (usr id text)  
  "Edit user text on id with user usr.")  

(defun api-vote (usr id &optional (dir :up))  
  "Vote direction dir for thing with id with user usr.")  

(defun api-save (usr id)  
  "Save thing with id.")  

(defun api-unsave (usr id)  
  "Unsave thing with id.")  

(defun api-report (usr id)  
  "Report thing with id.")  

(defun api-marknsfw (usr id)  
  "Mark thing with id as nsfw.")  

(defun api-hide (usr id)  
  "Hide thing with id.")  

(defun api-unhide (usr id)  
  "Unhide thing with id.")  

(defun api-del (usr id)  
  "Delete thing with id.")  

(defun api-block (usr id)  
  "Block thing with id.")  

(defun api-read-message (usr id)  
  "Read message with id.")  

(defun api-unread-message (usr id)  
  "Unread message with id.")  

(defun api-approve (usr id)  
  "Approve thing with id.")  

(defun api-leave-contributor (usr id)  
  "Self removal as moderator of thing with id.")  

(defun api-leave-moderator (usr id)  
  "Remove as moderator of subreddit with id.")  

(defun api-remove (usr id &key is-spam)  
  "Remove thing with id. Is-spam t if spam, nil if not.")  
 
(defun api-setflairenabled (usr &key flair-enabled)  
  "Enable/disable flair.")  

(defun get-user (r-user &optional usr)  
  "Get /user/<r-user>.json.  Optional user usr.")  

(defun get-about-user (about-user &optional usr)  
  "Get /user/<about-user>/about.json.  Optional user usr.")  

(defun get-message (usr where)  
  "Gets messages from inbox for user usr.")  

(defun get-subscribed (usr)  
  "Gets subscribed subreddits")  

(defun get-comments (id usr &key article comment context depth limit sort)  
  "Gets comments for link id in subreddit sr.")  

(defun get-reddit (&optional usr)  
  "Gets json data for reddit home page. Optional user usr.")  

(defun get-subreddit (sub &optional usr)  
  "Gets json data for subreddit sub.  Optional user usr.")  

(defun get-subreddit-new (sub &optional usr)  
  "Gets json data for /r/<sub>/new. Optional user usr.")  

(defun get-subreddit-top (sub &optional usr)  
  "Gets json data for top posts in subreddit sub. Optional user usr.")  

(defun get-subreddit-about (sub &optional usr)  
  "Gets r/<sub>/about.json. Returns Subreddit object about sub. Optional user usr.")  

(defun get-search (query &key after before count limit restrict_sr show sort syntax time target sub)  
  "Search for query.")  
    
See cl-reddit.lisp for full api. This is still a work in progress and probably will change frequently.

    



