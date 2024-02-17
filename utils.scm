(import (only medea
			  read-json
			  write-json))
(import defstruct)
(import (only (chicken time posix)
			  time->string
			  seconds->utc-time))

; creates a record-info struct
(defstruct record-info artist title year groupid image-uri)

; saves the output JSON into a file, if it doesn't exist yet
; (for subsequent testing purposes, so as not to overload the API)
; JSONObject String -> file
(define (save-obj-to-file obj filename)
	(call-with-output-file filename (lambda (port) (write-json obj port))))

; creates a timestamp String, formatted for Tiddlywiki
; -> timestamp
(define (get-tiddler-timestamp)
  (time->string (seconds->utc-time) "%Y%m%d%H%M%S"))

; creates a timestamp String, formatted for Tiddlywiki
; -> timestamp
(define (get-logger-timestamp)
  (time->string (seconds->utc-time) "[%y/%m/%d %Hh%Mm%Ss]"))


; retrieves the file extension from the image request response
; extension is one of "png", "jpg", "jpeg", "gif", etc.
; URIString -> extension
(define (get-extension-from-image-uri uri) 
	  (list-ref (string-split uri ".") 2 ))

; creates a filename from record-info
; record-info -> String
(define (get-album-art-filename record-info)
   (define norm (lambda (str) (string-translate (string-downcase str) " " "-")))
   (let ((extension (get-extension-from-image-uri (record-info-image-uri record-info))))
	   (norm (string-append (record-info-title record-info) "-"
							(record-info-artist record-info) "." extension))))

; saves the image file from the image response
; port intarweb#response filename -> boolean
(define (saves-image from filename)
  (begin (log-message DEBUG (string-append "saving album-art image to " filename))
		 (call-with-output-file filename (lambda (to) (copy-port from to)))
		 #t))
