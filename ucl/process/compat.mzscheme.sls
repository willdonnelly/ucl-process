#!r6rs

(library (ucl process compat)
  (export process-launch)
  (import (rnrs) (only (scheme base) subprocess subprocess-kill subprocess-pid subprocess-wait subprocess-status))

  ;; Here we see the entirely high-level
  ;;   approach to subprocesses
  (define (process-launch path . args)
    ;; If we wanted, we could make our own pipes here, but the #f means
    ;;   "Give me some pipes automatically."
    (let-values (((proc out in err) (apply subprocess #f #f #f path args)))
      (vector
        in
        out
        err
        (subprocess-pid proc)

        ;; PROCESS-KILL
        (let ((killed #f))
          (lambda ()
            ;; The last argument to SUBPROCESS-KILL decides
            ;;   whether it will use SIGTERM or SIGKILL
            (subprocess-kill proc killed)
            (set! killed #t)))

        (lambda ()
          (subprocess-wait proc)
          (subprocess-status proc)))))
)
