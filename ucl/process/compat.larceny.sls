#!r6rs

(library (ucl process compat)
  (export process-launch process-kill process-wait)
  (import (rnrs) (primitives r5rs:require unix/pipe unix/fork unix/waitpid
                  unix/close unix/dup2 unix/execl unix/exit
                  open-output-descriptor open-input-descriptor))

  (define (process-launch path . args)
    ;; Larceny does the "low-level unix" approach.
    ;; Create some pipes.
    (let-values (((r1 mosi-r mosi-w) (unix/pipe))  ; Read, Write
                 ((r2 miso-r miso-w) (unix/pipe))  ; Read, Write
                 ((r3 mise-r mise-w) (unix/pipe))) ; Read, Write
      ;; Fork
      (let ((pid (unix/fork)))
        (if (zero? pid)
            ;; We're a child
            (begin
              ;; Close the parent's ports
              (unix/close mosi-w)
              (unix/close miso-r)
              (unix/close mise-r)
              ;; Redirect our i/o
              (unix/dup2 mosi-r 0)
              (unix/dup2 miso-w 1)
              (unix/dup2 mise-w 2)
              ;; And exec. Path is given twice, once as
              ;;   the executable, once as argv[0]
              (apply unix/execl path path args)
              ;; We should never get here without an error
              (unix/exit 1))
            ;; We're the parent
            (begin
              ;; Close the child's ports
              (unix/close mosi-r)
              (unix/close miso-w)
              (unix/close mise-w)
              ;; And return all our stuff
              (vector
               ;; Transcode the ports.
               (open-output-descriptor mosi-w)
               (open-input-descriptor  miso-r)
               (open-input-descriptor  mise-r)
               pid
               #f))))))

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
    (fix-endianness
      (let-values (((p code) (unix/waitpid pid))) code)))

  ;; larceny fucks up exit code endianness for me, so whatever
  (define (fix-endianness code)
    (fxior
      (* (fxand #x00FF code) 256)
      (/ (fxand #xFF00 code) 256)))

  ;; We have to require the libs down here, even though
  ;;   we imported the identifiers up top.
  (r5rs:require 'unix))
