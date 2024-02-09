(include "utils.scm")
(import (only (chicken file)
			  file-exists?
			  delete-file))


; tiddler-content, timestamp, filepath are String

(define capri-ri (make-record-info title: "CAPRISONGS" artist: "FKA Twigs" year: "2022"))

; creates a timestamp String
; -> timestamp
(define (get-timestamp)
  (time->string (seconds->utc-time) "%Y%m%d%H%M%S"))

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
				  "year: " (record-info-year record-info) "\n"
				  "type: text/vnd.tiddlywiki"))
(test #t (let ((timestamp (get-timestamp)))
		   (string=? (create-tiddler-content capri-ri timestamp)
					 (string-append "created: " timestamp "\ncreator: recogarden-script\nmodified: " timestamp "\nmodifier: recogarden-script\ntags: music-record\ntitle: CAPRISONGS\nauthor: FKA Twigs\nyear: 2022\ntype: text/vnd.tiddlywiki"))))

; creates a file from a tiddler-content
; tiddler-content filepath -> Boolean
(define (create-tiddler-file tiddler-content filepath)
   (with-output-to-file filepath (lambda () (write-string tiddler-content))))
(let* ((filepath "test-tiddler.tid"))
	(test filepath 
		  (begin (create-tiddler-file (create-tiddler-content capri-ri (get-timestamp)) filepath)
				 (and (file-exists? filepath) (delete-file filepath)))))
