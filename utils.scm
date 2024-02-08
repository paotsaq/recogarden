(import (only medea
			  read-json
			  write-json))

; saves the output JSON into a file, if it doesn't exist yet
; (for subsequent testing purposes, so as not to overload the API)
; JSONObject String -> file
(define (save-obj-to-file obj filename)
	(call-with-output-file filename (lambda (port) (write-json obj port))))
