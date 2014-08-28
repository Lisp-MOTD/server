(defpackage #:motd-server
  (:use #:cl)
  (:export :*motd-db*
           :motd-db
           :retrieve-all-motds-after
           :retrieve-most-recent-motds
           :retrieve-all-proposed-motds
           :retrieve-all-tags)
  (:export :create-message
           :create-authenticated-message
           :extract-authenticated-message)
  (:export :get-all-motds-after
           :get-most-recent-motds)
  (:export :get-all-proposed-motds
           :get-all-tags)
  (:export :propose-message
           :publish-message
           :add-translation
           :delete-translation
           :add-tag
           :delete-tag
           :delete-message))
