#!r6rs

(library (ucl process compat)
  (export process-launch)
  (import (rnrs) (core primitives))

  ;; Ypsilon also does the high-level approach.
  ;;   It gives us a list of the form:
  ;;   (pid stdin stdout stderr)
  (define (process-launch path . args)
    (let ((p (apply process path args)))
      (vector
       (transcoded-port (cadr p) (native-transcoder))
       (transcoded-port (caddr p) (native-transcoder))
       (transcoded-port (cadddr p) (native-transcoder))
       (car p)

       ;; PROCESS-KILL
       (let ((killed #f))
         (lambda ()
           (if (not killed)
               ;; But as with Mosh, we have no "process-kill" function,
               ;;   so we have to use the external program.
               (process-launch "/bin/kill" (number->string (car p)))
               (process-launch "/bin/kill" "-9" (number->string (car p))))
           (set! killed #t)))

       ;; PROCESS-WAIT
       (lambda ()
         ;; The #f means "never timeout"
         (process-wait (car p) #f)))))
)
