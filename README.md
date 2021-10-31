# cl-cron

A simple tool that provides cron like facilities directly inside of common lisp.

a fork of https://bitbucket.org/mackram/cl-cron (unavailable)

Install with Quicklisp:

    (ql:quickload "cl-cron")

Note: Quicklisp points to this repository since the release of October, 2021.

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

```lisp
(make-cron-job function-symbol &key (minute :every) (step-min 1) (hour :every) (step-hour 1) (day-of-month :every)

(delete-cron-job cron-key)

(time-to-run-job job)

start-cron

restart-cron

stop-cron
```

Example:

~~~lisp
(cl-cron:make-cron-job
            (lambda ()
              (format t "Wake Up!~%"))

            ;; Days of week are numbered from 0,
            ;; where 0 is Monday.
            ;; Run every Sunday:
            :day-of-week 6
            :hour 10
            :minute 0
            ;; hash-key is the name
            :hash-key :sunday-alarm)

(cl-cron:delete-cron-job
            :sunday-alarm)
~~~

https://40ants.com/lisp-project-of-the-day/2020/06/0087-cl-cron.html

<!-- http://quickdocs.org/cl-cron/api -->


## Changelog

- 2020-10-13: we added a name to the cl-cron thread.

Licence: GPL
