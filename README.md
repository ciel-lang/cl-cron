# cl-cron

A simple tool that provides cron like facilities directly inside of common lisp.

a fork of https://bitbucket.org/mackram/cl-cron (unavailable)

Install with Quicklisp:

    (ql:quickload "cl-cron")

Note: Quicklisp does not point to this repository.

Example:

Print a message every minute.

~~~lisp
(defun say-hi ()
  (print "Hi!"))

(cl-cron:make-cron-job #'say-hi)
(cl-cron:start-cron)
~~~

Wait a minute to see output.

Stop all jobs with `stop-cron`.

## API

http://quickdocs.org/cl-cron/api

```lisp
(make-cron-job function-symbol &key (minute :every) (step-min 1) (hour :every) (step-hour 1) (day-of-month :every)

(delete-cron-job cron-key)

(time-to-run-job job)

start-cron

restart-cron

stop-cron
```


## Changelog

- 2020-10-13: we added a name to the cl-cron thread.

Licence: GPL
