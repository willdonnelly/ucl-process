#!r6rs

(library (ucl process compat)
  (export process-launch process-kill process-wait)
  (import (rnrs) (only (scheme base) subprocess subprocess-kill
                    subprocess-pid subprocess-wait subprocess-status))

  ;; Here we see the entirely high-level
  ;;   approach to subprocesses
  (define (process-launch path . args)
    ;; If we wanted, we could make our own pipes here, but the #f means
    ;;   "Give me some pipes automatically."
    (let-values (((proc out in err) (apply subprocess #f #f #f path args)))
      (vector in out err (subprocess-pid proc) proc)))

  (define (process-kill proc . sig)
    (case (if (null? sig) 'SIGTERM (car sig))
      ((SIGTERM) (subprocess-kill (vector-ref proc 4)))
      ((SIGKILL) (subprocess-kill (vector-ref proc 4)))
      (else      (error 'process-kill "unknown signal" (car sig)))))

  (define (process-wait proc)
    (subprocess-wait (vector-ref proc 4))
    (subprocess-status (vector-ref proc 4)))
)
