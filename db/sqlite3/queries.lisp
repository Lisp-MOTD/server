(in-package :motd-server-sqlite3)

(defconstant +retrieve-after-statement+
  "SELECT msg.id, msg.expiration,
          pub.timestamp,
          info.language, info.text, info.tag
     FROM messages AS msg
     JOIN published_messages AS pub ON msg.id = pub.message_id
     JOIN (
        SELECT message_id, language, text, NULL AS tag
          FROM message_translations AS trn
        UNION
        SELECT message_id, NULL AS language, NULL AS text, tag
          FROM message_tags AS tag
     ) AS info ON msg.id = info.message_id
     WHERE msg.id IN ( SELECT p1.message_id
                         FROM published_messages AS p1
                         WHERE timestamp >
                               IFNULL(( SELECT timestamp
                                          FROM published_messages AS p2
                                          WHERE p2.message_id = ? ), 0 ) )
     ORDER BY pub.timestamp DESC")

(defconstant +retrieve-recent-statement+
  "SELECT msg.id, msg.expiration,
          pub.timestamp,
          info.language, info.text, info.tag
     FROM messages AS msg
     JOIN published_messages AS pub ON msg.id = pub.message_id
     JOIN (
        SELECT message_id, language, text, NULL AS tag
          FROM message_translations AS trn
        UNION
        SELECT message_id, NULL AS language, NULL AS text, tag
          FROM message_tags AS tag
     ) AS info ON msg.id = info.message_id
     WHERE msg.id IN ( SELECT message_id
                         FROM published_messages
                         ORDER BY timestamp DESC
                         LIMIT ? )
     ORDER BY pub.timestamp DESC")

(defconstant +retrieve-proposed-statement+
  "SELECT msg.id, msg.expiration,
          info.language, info.text, info.tag
     FROM messages AS msg
     JOIN (
        SELECT message_id, language, text, NULL AS tag
          FROM message_translations AS trn
        UNION
        SELECT message_id, NULL AS language, NULL AS text, tag
          FROM message_tags AS tag
     ) AS info ON msg.id = info.message_id
     WHERE msg.id NOT IN ( SELECT message_id
                             FROM published_messages )
     ORDER BY msg.id DESC")

(defconstant +retrieve-tags-statement+
  "SELECT DISTINCT tag FROM message_tags
   ORDER BY tag ASC")

(defconstant +retrieve-public-key-statement+
  "SELECT public_key FROM public_keys
   WHERE user_name = ?")

(defconstant +insert-translation-statement+
  "INSERT INTO message_translations (message_id,language,text)
   VALUES (?,?,?)")
