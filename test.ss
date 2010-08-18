#!r6rs
(import (rnrs) (ucl process))

(assert (equal? (shell "echo foobar baz") "foobar baz"))

(display "sleeping...")
(let ((proc (process-launch "/bin/sleep" "5")))
  (process-wait proc))
(display "done\n")

(display "aborted sleeping...")
(let ((proc (process-launch "/bin/sleep" "300")))
  (process-kill proc 'SIGKILL)
  (process-wait proc))
(display "done\n")

(display "success!\n")
(exit)
