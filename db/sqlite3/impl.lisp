(in-package #:motd-server-sqlite3)

(interface:define-implementation =sqlite3-motd-db= (motd-server:=motd-db=)
  :open-database #'open-sqlite3-motd-database
  :close-database #'close-sqlite3-motd-database
  :retrieve-all-motds-after #'retrieve-all-motds-after
  :retrieve-most-recent-motds #'retrieve-most-recent-motds
  :retrieve-all-proposed-motds #'retrieve-all-proposed-motds
  :retrieve-all-tags #'retrieve-all-tags
  :retrieve-public-key #'retrieve-public-key
  :propose-message #'propose-message
  :insert-translation #'insert-translation
  :remove-translation #'remove-translation
  :insert-tag #'insert-tag
  :remove-tag #'remove-tag)
