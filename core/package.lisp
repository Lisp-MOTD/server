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
  (:export :get-all-proposed-messages
           :get-all-tags)
  (:export :propose-message
           :publish-message
           :add-translation
           :delete-translation
           :add-tag
           :delete-tag
           :delete-message))
(in-package :motd-server)

(defvar *motd-db* nil
  "Handle to the current database.")

(defclass motd-db ()
  ()
  (:documentation "Base class for the MOTD database."))
