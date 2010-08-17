#!r6rs

(library (ucl process compat)
  (export process-launch process-kill process-wait)
  (import (rnrs) (core primitives))

  ;; Ypsilon also does the high-level approach.
  ;;   It gives us a list of the form:
  ;;   (pid stdin stdout stderr)
  (define (process-launch path . args)
    (let ((p (apply process path args)))
      (vector (cadr p) (caddr p) (cadddr p) (car p) #f)))

  (define (kill-helper flag pid)
    (let ((proc (process-launch "/bin/kill" flag (number->string pid))))
      (process-wait proc)
      (close-port (vector-ref proc 0))
      (close-port (vector-ref prpc 1))
      (close-port (vector-ref proc 2))))

  (define (process-kill proc . sig)
    (define pid (vector-ref proc 3))
    (case (if (null? sig) 'SIGTERM (car sig))
      ((SIGTERM) (kill-helper "-TERM" pid))
      ((SIGKILL) (kill-helper "-KILL" pid))
      (else      (error 'process-kill "unknown signal" (car sig)))))

  (define (process-wait proc)
    (define pid (vector-ref proc 3))
    (process-wait pid #f))

)
