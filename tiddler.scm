(include "utils.scm")
(include "utils.scm")
(include "mylogging.scm")
(import (only (chicken file)
			  file-exists?
			  delete-file*)
		(only (chicken io)
			  write-line)
		(only (chicken time posix)
			  time->string
			  seconds->utc-time)
		(only (srfi-1)
			  any
			  find))


; tiddler-content, timestamp, filepath are String
; logfile is EXISTING-RECORDS


; creates a tiddler-content String from record-info
; record-info timestamp -> tiddler-content
(define (create-tiddler-content record-info timestamp)
   (string-append "created: " timestamp "\n"
				  "creator: recogarden-script\n"
				  "modified: " timestamp "\n"
				  "modifier: recogarden-script\n"
				  "tags: music-record\n"
				  "title: " (record-info-title record-info) "\n"
				  "author: " (record-info-artist record-info) "\n"
				  "year: " (number->string (record-info-year record-info)) "\n"
				  "type: text/vnd.tiddlywiki"))


; creates a file from a tiddler-content
; tiddler-content filepath -> Boolean
(define (create-tiddler-file tiddler-content filepath)
   (with-output-to-file filepath (lambda () (write-string tiddler-content))))

; append a new groupid to the logfile
; record-info -> 
(define (append-groupid-to-logfile groupid logfile)
  (call-with-output-file logfile
						 (lambda (output-port)
						   (write-line (number->string groupid) output-port))
						 #:append))

; checks whether a groupid already exists in the logfile
; groupid -> Boolean
(define (record-exists? groupid logfile)
	(define result (find (lambda (line) (string=? line (number->string groupid)))
		  (with-input-from-file logfile (lambda () (read-lines)))))
	(string? result))

