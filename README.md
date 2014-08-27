## Lisp Message of the Day (Server)

This is the server side of the Lisp Message of the Day application.

### Requirements

A message has:

* An English string containing the message
* A forever unique ID
* An arbitrary number of translations into other languages
* An arbitrary number of tags associated with it
* An optional(??) expiration date for the message

A published message also has:

* A published timestamp

The server side needs to support:

* Fetching the latest N MOTDs or all since a given message ID
* Web form for proposing a message and its translations
* Support retrieving proposed messages (admin)
* Support adding a new message (admin)
* Support deleting a message by ID (admin)
* Support updating an existing message (admin)
* Support retrieving a list of all tags ever used to tag things

### License

This code is available through the [UNLICENSE][UN].

[UN]: http://unlicense.org/
