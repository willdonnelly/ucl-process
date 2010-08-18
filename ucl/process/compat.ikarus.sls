#!r6rs
(library (ucl process compat)
  (export process-launch process-kill process-wait)
  (import (rnrs) (ikarus ipc))

  ;; Ikarus is another Scheme which does the
  ;;   high-level process semantics.
  (define (process-launch path . args)
    ;; No magic here, just launch and get the values
    (let-values (((pid stdin stdout stderr) (apply process path args)))
      ;; And then we return it all
      (vector stdin stdout stderr pid #f)))

  (define (process-kill proc . sig)
    (define pid (vector-ref proc 3))
    (case (if (null? sig) 'SIGTERM (car sig))
      ((SIGTERM) (kill pid 'SIGTERM))
      ((SIGKILL) (kill pid 'SIGKILL))
      (else      (error 'process-kill "unknown signal" (car sig)))))

  (define (process-wait proc)
    (define pid (vector-ref proc 3))
    (wstatus-exit-status (waitpid pid )))

)
