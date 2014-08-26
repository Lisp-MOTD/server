(defpackage #:motd-server
  (:use #:cl)
  (:export :create-message
           :create-authenticated-message
           :extract-authenticated-message)
  (:export :get-all-motds-after
           :get-most-recent-motds)
  (:export :get-all-proposed-messages
           :get-all-tags)
  (:export :propose-message
           :publish-message
           :add-translation
           :delete-translation
           :add-tag
           :delete-tag
           :delete-message))
