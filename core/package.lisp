(defpackage #:motd-server
  (:use #:cl)
  (:export :*motd-db-connection-info*
           :=motd-db=
           :motd-open-database
           :motd-close-database
           :open-database
           :close-database
           :retrieve-all-motds-after
           :retrieve-most-recent-motds
           :retrieve-all-proposed-motds
           :retrieve-all-tags
           :retrieve-public-key
           :propose-message
           :insert-translation
           :remove-translation
           :insert-tag
           :remove-tag)
  (:export :new-motd)
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
