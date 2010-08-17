#!r6rs

;; UCL PROCESS
;;  Launch and communicate with processes.
;;
;;   PROCESS-LAUNCH path . args
;;    Launch a process. Return a process object,
;;    consisting of a 5-element vector:
;;      * stdin
;;      * stdout
;;      * stderr
;;      * pid
;;      * extra data
;;    The ports are binary by default
;;
;;   PROCESS-STDIN proc
;;   PROCESS-STDOUT proc
;;   PROCESS-STDERR proc
;;   PROCESS-PID proc
;;    Synonyms to (VECTOR-REF proc [0-3]), mostly
;;    provided for code clarity
;;
;;   PROCESS-KILL proc [signal]
;;    Kills the process. The optional signal can be
;;    either 'SIGKILL or 'SIGTERM, defaulting to
;;    'SIGTERM if not provided.
;;
;;   PROCESS-WAIT proc
;;    Wait until the subprocess returns. Returns
;;     the exit code.
;;
;;   PROCESS-CLOSE proc
;;    Close all ports to the process.
;;
;;   SHELL-EXEC strip? . command
;;    Execute a command with /bin/sh and return
;;     a string holding the output. If the output
;;     ends in a newline, it can optionally be
;;     stripped.
;;    Uses multiple return values to also return
;;     the output on stderr and the exit code.
;;
;;   SHELL
;;    Execute a command with SHELL-EXEC after running
;;     the TEMPLATE function (from UCL-Prelude) on the
;;     input arguments.
;;    Raises an error on a nonzero exit code.

(library (ucl process)
  (export process-launch
          process-stdin
          process-stdout
          process-stderr
          process-kill
          process-pid
          process-wait
          process-close
          shell-exec
          shell)
  (import (rnrs) (ucl process compat) (ucl prelude))

  (define (process-stdin p)  (vector-ref p 0))
  (define (process-stdout p) (vector-ref p 1))
  (define (process-stderr p) (vector-ref p 2))
  (define (process-pid p)    (vector-ref p 3))

  ; process-wait comes from (ucl process compat)
  ; process-kill comes from (ucl process compat)

  (define (process-close p)
    (close-port (process-stdin p))
    (close-port (process-stdout p))
    (close-port (process-stderr p)))

  (define (get-all-transcoded p)
    (get-string-all (transcoded-port p (native-transcoder))))

  (define (shell-exec strip? . command)
    (let [(proc (process-launch "/bin/sh" "-c" (apply string-append command)))]
      (let ((exit-code (process-wait proc))
            (output    (get-all-transcoded (process-stdout proc)))
            (errput    (get-all-transcoded (process-stderr proc))))
        ;; trim the trailing newline, if desired
        (when (and strip? (string? output))
          (let ((outlen (string-length output)))
            (when (< 0 outlen)
              (when (equal? #\newline (string-ref output (- outlen 1)))
                (set! output (substring output 0 (- outlen 1)))))))
        (process-close proc)
        (values output errput exit-code))))

  (define (shell str . vals)
    (let ((cmd (apply print str vals)))
      (let-values (((output errput code) (shell-exec #t cmd)))
        (if (equal? code 0)
            output
            (error 'shell "shell command failed" cmd errput)))))
)
