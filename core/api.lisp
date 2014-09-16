(in-package :motd-server)

(defgeneric create-message (translations tags &key
                                                id
                                                expiration
                                                timestamp)
  (:documentation "Create a new message with the given TRANSLATIONS
and TAGS.  The TRANSLATIONS must be a list of elements of the
form (LANGAUGE . TEXT) where the LANGUAGE is a keyword identifying a
language and TEXT is the translation into that language.  There must
be at least one element in the TRANSLATIONS list.  The TAGS list is a
list of keywords used to tag this message.  There must be at least one
element in the TAGS list.  The EXPIRATION-DATE if non-NIL must be
universal time stamp after which this message is no longer relevant."))

(defconstant +default-command-time-threshold+ 60
  "The maximum number of seconds difference allowable between the
  remote clock and the local clock.")

(defgeneric create-authenticated-message (payload author private-key)
  (:documentation "Returns a message which is signed with the given
  PRIVATE-KEY belonging to the given AUTHOR with the given PAYLOAD for
  the message.  The PAYLOAD must be a string."))
(defgeneric extract-authenticated-message
    (authenticated-message &optional *command-time-threshold*)
  (:documentation "Validate the given AUTHENTICATED-MESSAGE given.
  Return (VALUES PAYLOAD AUTHOR) where both PAYLOAD and AUTHOR will be
  NIL if the message is not authentic."))

(defgeneric get-all-motds-after (message-id))
(defgeneric get-most-recent-motds (how-many-to-retrieve))

(defgeneric get-all-proposed-messages ())
(defgeneric get-all-tags ())

(defgeneric propose-message (message))

(defgeneric publish-message (message-id))

(defgeneric add-translation (message-id language text))
(defgeneric delete-translation (message-id language))

(defgeneric add-tag (message-id tag))
(defgeneric delete-tag (message-id tag))

(defgeneric delete-message (message-id))
