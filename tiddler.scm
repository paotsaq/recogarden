(include "utils.scm")
(include "api-consts.scm")
(include "mylogger.scm")
(import (only (chicken file)
			  file-exists?
			  delete-file*)
		(only (chicken string)
			  string-translate)
		(only (chicken io)
			  write-line)
		(only (chicken time posix)
			  time->string
			  seconds->utc-time)
		(only (srfi-1)
			  any
			  find)
		(only (srfi-13)
			  string-downcase
			  ))


; tiddler-content, timestamp, filepath are String
; logfile is String, sourced from the EXISTING-RECORDS defined in api-consts.scm,
; and it keeps track of archived `groupid`

; creates a filename from record-info
; record-info -> String
(define (get-album-art-filename record-info)
   (define norm (lambda (str) (string-translate (string-downcase str) " " "-")))
   (norm (string-append (record-info-title record-info) "-" (record-info-artist record-info))))

;TODO 
;(string-append TIDDLERS-PATH "/"
			   ;(get-album-art-filename record-info)
			   ;"." extension)

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

; creates a image-meta-tiddler String from record-info
; record-info -> tiddler-content
(define (create-image-meta-tiddler record-info)
   (string-append "title: " (get-album-art-filename record-info) "\n"
				  "type: image/" (get-extension-from-image-uri (record-info-image-uri record-info)) "\n"))

; creates a file from a tiddler-content
; tiddler-content filepath -> Boolean
(define (create-tiddler-file tiddler-content filepath)
  (handle-exceptions exn
				   (begin
					 (let ((exn-message ((condition-property-accessor 'exn 'message) exn)))
						 (log-message 5 exn-message))
					 #f)
				   (with-output-to-file filepath (lambda () (write-string tiddler-content)))
				   #t))

; routine to save an album-art file; it does:
; - send a request to the image endpoint;
; - save the image;
; - create the meta tiddler.
; record-info -> boolean
(define (produce-record-info-album-art record-info tiddlers-path)
  (let*-values (((image-uri) (record-info-image-uri record-info))
				((album-art-filename) (get-album-art-filename record-info))
				; NOTE check for '/' in tiddlers-path
				((album-art-filepath) (string-append tiddlers-path album-art-filename))
				((meta-tiddler-filepath) (string-append album-art-filepath ".meta"))
				((image-saving-bool uri-obj image-response)
				 (request-and-save-image-file image-uri album-art-filepath)))
    (if image-saving-bool
		(create-tiddler-file (create-image-meta-tiddler record-info) meta-tiddler-filepath)
		#f)))

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
