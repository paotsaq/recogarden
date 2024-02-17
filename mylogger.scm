(include "utils.scm")
(import simple-logger)
(import (chicken format))

;; A custom debug logger that allows the level to be dynamically set
;; (adapted from http://wiki.call-cc.org/eggref/5/simple-logger)
(define custom-debug-logger-config
  (make-parameter (make-logger-config)))

(define DEBUG 1)
(define INFO (+ DEBUG 1))
(define WARNING (+ INFO 1))
(define ERROR (+ WARNING 1))

; setups a logging message with level and content
; level is in [ERROR, INFO, WARNING, ERROR];
; content is a string.
; (log-message ERROR "Something happened!")
(define log-message
  (let ((logger (make-logger custom-debug-logger-config)))
    (lambda (level fmt . args)
      (parameterize ((custom-debug-logger-config
                      (config-logger
                       (custom-debug-logger-config)
                       prefix: (lambda () (format "LOG ~a | LEVEL ~a] " (get-logger-timestamp) level))
                       level: (* level 10))))
        (apply logger (cons fmt args))))))

(log-level ERROR)
