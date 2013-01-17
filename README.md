# cl-reddit


### Common lisp reddit api wrapper


####Examples

**Create user and login and view user modhash**  
    
    CL-REDDIT> (defvar u (api-login :username "AzureDiamond" :password "hunter2"))
    CL-REDDIT> (user-modhash u)
    CL-REDDIT> "393eioafja78iafjioiwoijhgnhn223jik9rjfoq87fnbh13jv"`
    
**Search with keywords, loop through results**

    CL-REDDIT> (defvar lst (get-search "Lance Armstrong"))
    CL-REDDIT> (loop for l in lst do (format t "~a:~a~%" (link-score l) (link-title l)))
    
Search can take several optional parameters, to search restricted to a particular subreddit

    CL-REDDIT> (defvar lst (get-search "Lance Armstrong" :sub "funny" :restrict_sr t))
    
    
Theres lots more funcationality, see the docstrings for more info.  
This is still a work in progress and probably will change frequently.

    



