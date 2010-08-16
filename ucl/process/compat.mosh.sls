#!r6rs

(library (ucl process compat)
  (export process-launch)
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
              (vector
                ;; Ports and PID
                (transcoded-port mosi-w (native-transcoder))
                (transcoded-port miso-r (native-transcoder))
                (transcoded-port mise-r (native-transcoder))
                pid

                ;; PROCESS-KILL
                ;; We actually use the kill program to do this,
                ;;   because Mosh doesn't provide that command.
                (let ((killed #f))
                  (lambda ()
                    (if (not killed)
                        (process-launch "/bin/kill"      (number->string pid))
                        (process-launch "/bin/kill" "-9" (number->string pid)))
                    (set! killed #t)))

                ;; PROCESS-WAIT
                (lambda ()
                  (let-values (((p code) (waitpid pid))) code))))))))
)
