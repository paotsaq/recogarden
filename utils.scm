(import (only medea
			  read-json
			  write-json))
(import defstruct)
(import (only (chicken time posix)
			  time->string
			  seconds->utc-time))

; creates a record-info struct
(defstruct record-info artist title year groupid)

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
