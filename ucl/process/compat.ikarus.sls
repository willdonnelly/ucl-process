#!r6rs
(library (ucl process compat)
  (export process-launch)
  (import (rnrs) (ikarus ipc))

  ;; Ikarus is another Scheme which does the
  ;;   high-level process semantics.
  (define (process-launch path . args)
    ;; No magic here, just launch and get the values
    (let-values (((pid stdin stdout stderr) (apply process path args)))
      ;; And then we return it all
      (vector
       stdin
       stdout
       stderr
       pid

       ;; PROCESS-KILL
       (let ((killed #f))
         (lambda ()
           (if (not killed)
               (kill pid 'SIGTERM)
               (kill pid 'SIGKILL))
           (set! killed #t)))

       ;; PROCESS-WAIT
       (lambda ()
         (wstatus-exit-status (waitpid pid))))))
)
