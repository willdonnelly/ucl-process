#!r6rs

(library (ucl process compat)
  (export process-launch process-kill process-wait)
  (import (rnrs) (mosh process))

  ;; Mosh does a sort of compromise approach
  ;;   between the high-level and the unix
  ;;   approaches. We have to provide our
  ;;   own pipes, but the spawn command will
  ;;   handle the rest.
  (define (process-launch path . args)
    ;; Make our pipes
    (let-values ([(mosi-r mosi-w) (pipe)]
                 [(miso-r miso-w) (pipe)]
                 [(mise-r mise-w) (pipe)])
      (let ((pid (fork)))
        (if (zero? pid)
            (begin
              (close-port mosi-w)
              (close-port miso-r)
              (close-port mise-r)
              (exec path args mosi-r miso-w mise-w))
            (begin
              (close-port mosi-r)
              (close-port miso-w)
              (close-port mise-w)
              (vector mosi-w miso-r mise-r pid #f))))))

  (define (kill-helper flag pid)
    (let ((proc (process-launch "/bin/kill" flag (number->string pid))))
      (process-wait proc)
      (close-port (vector-ref proc 0))
      (close-port (vector-ref proc 1))
      (close-port (vector-ref proc 2))))

  (define (process-kill proc . sig)
    (define pid (vector-ref proc 3))
    (case (if (null? sig) 'SIGTERM (car sig))
      ((SIGTERM) (kill-helper "-TERM" pid))
      ((SIGKILL) (kill-helper "-KILL" pid))
      (else      (error 'process-kill "unknown signal" (car sig)))))

  (define (process-wait proc)
    (define pid (vector-ref proc 3))
    (let-values (((p code) (waitpid pid))) code))
)
