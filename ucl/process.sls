#!r6rs

;; UCL PROCESS
;;  Launch and communicate with processes.
;;
;;   PROCESS-LAUNCH
;;    Launch a process. Return a process object.
;;
;;   PROCESS-STDIN
;;    Returns the textual output port to which
;;     input data can be provided.
;;
;;   PROCESS-STDOUT
;;    Returns the textual input port from which
;;     output data can be read.
;;
;;   PROCESS-STDERR
;;    Returns the textual input port from which
;;     error reports can be read.
;;
;;   PROCESS-PID
;;    Returns the PID of the subprocess.
;;
;;   PROCESS-KILL
;;    Kills the subprocess. Calling once will
;;     send SIGTERM to the process. Calling a
;;     second time will send SIGKILL.
;;
;;   PROCESS-WAIT
;;    Wait until the subprocess returns. Returns
;;     the exit code.
;;
;;   PROCESS-CLOSE
;;    Close all ports to the process.
;;
;;   SHELL-EXEC
;;    Execute a command with /bin/sh and return
;;     a string holding the output. If the output
;;     ends in a newline, it can optionally be
;;     stripped.
;;    Uses multiple return values to also return
;;     the exit code and the output on stderr.
;;
;;   SHELL
;;    Execute a command with SHELL-EXEC after
;;     running the PRINT function (from UCL Prelude)
;;     on the input arguments.
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

  (define (process-kill p)   ((vector-ref p 4)))
  (define (process-wait p)   ((vector-ref p 5)))

  (define (process-close p)
    (close-port (process-stdin p))
    (close-port (process-stdout p))
    (close-port (process-stderr p)))

  (define (shell-exec strip? . command)
    (let [(proc (process-launch "/bin/sh" "-c" (apply string-append command)))]
      (let ((exit-code (process-wait proc))
            (output    (get-string-all (process-stdout proc)))
            (errput    (get-string-all (process-stderr proc))))
        ;; trim the trailing newline, if desired
        (when strip?
          (when (string? output)
            (let ((outlen (string-length output)))
              (when (< 0 outlen)
                (when (equal? #\newline (string-ref output (- outlen 1)))
                  (set! output (substring output 0 (- outlen 1))))))))
        (process-close proc)
        (values output exit-code errput))))

  (define (shell str . vals)
    (let ((cmd (apply print str vals)))
      (let-values (((output code errput) (shell-exec #t cmd)))
        (if (equal? code 0)
            output
            (error 'shell "shell command failed" cmd errput)))))
)
