;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-CRON;

;;;    Copyright (c) 2009, Mackram Ghassan Raydan 
;;;    This file is part of cl-cron.

;;;    cl-cron is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    cl-cron is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with cl-cron.  If not, see <http://www.gnu.org/licenses/>.


(in-package :cl-cron)


(defparameter *day-list* 
  '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defparameter *month-list* 
  '(:january :february :march :april :may :june :july :august :september :october :november :december))


(defclass cron-job ()
  ((minute :accessor job-minute :initarg :job-minute :initform :every)
   (hour :accessor job-hour :initarg :job-hour :initform :every)
   (day-of-month :accessor job-dom :initarg :job-dom :initform :every)
   (month :accessor job-month :initarg :job-month :initform :every)
   (day-of-week :accessor job-dow :initarg :job-dow :initform :every)
   (at-boot :accessor job-@boot :initarg :job-@boot :initform nil)
   (function-symbol :accessor job-func :initarg :job-func)))


(defparameter *cron-jobs-hash* (make-hash-table) 
  "contains a hash of all corn-job objects that need to be run")

(defparameter *cron-dispatcher-thread* nil
  "a parameter to that holds the cron-dispatcher thread")

(defparameter *cron-dispatcher-processing* (bordeaux-threads:make-lock)
  "allows us to not kill the thread unless the lock can be acquired")

(defparameter *cron-load-file* nil
  "a parameter which points to a lisp or fasl file which would be loaded once start-cron is called. The boot file should be made of as many make-cron-job calls as you like one after the other in normal s-expression fashion.")

(defparameter *cron-log-file* "./cl-cron.log"
  "a parameter to set the cron file log location.")

(defun make-cron-job (function-symbol &key (minute :every) (step-min 1) (hour :every) (step-hour 1) (day-of-month :every) 
		      (step-dom 1) (month :every) (step-month 1) (day-of-week :every) (step-dow 1) (boot-only nil) (hash-key nil))
  "creates a new instance of a cron-job object and appends it to the cron-jobs-list after processing its time. Note that if you wish to use multiple values for each parameter you need to provide a list of numbers or use the gen-list function. You can not have a list of symbols when it comes to month or day-of-week. Please note that as by ANSI Common Lisp for the month variable the possible values are between 1 and 12 inclusive with January=1 and for day of week the possible values are between 0 and 6 with Monday=0. Returns the hash-key"
  (if (eql hash-key nil) (setf hash-key (gensym "cron")))
  (setf (gethash hash-key *cron-jobs-hash*)
	(make-instance 'cron-job 
		       :job-minute (get-minutes minute step-min)
		       :job-hour (get-hours hour step-hour)
		       :job-dom (get-days-of-month day-of-month step-dom)
		       :job-month (get-months month step-month)
		       :job-dow (get-days-of-week day-of-week step-dow)
		       :job-@boot boot-only
		       :job-func function-symbol))
  hash-key)


(defun delete-cron-job (cron-key)
  "deletes the cron job with the corresponding hash key"
  (remhash cron-key *cron-jobs-hash*))

(defun time-to-run-job (job)
  "checks if it is time to run the current job based on the current time"
  (multiple-value-bind (usec umin uhour udom umonth udow)
      (decode-universal-time (get-universal-time))
    (declare (ignore usec))
    (and (member umin (job-minute job))
	 (member uhour (job-hour job))
	 (or (member udom (job-dom job))
	     (member udow (job-dow job)))
	 (member umonth (job-month job))
	 (not (job-@boot job)))))

(defun run-job-if-time (key job)
  "runs the cron-job object in a separate thread if it is its time"
  (if (time-to-run-job job)
      (bordeaux-threads:make-thread (job-func job))))

(defun run-job-if-boot (key job)
  "runs the cron-job object in a separate thread if it is a boot job"
  (if (job-@boot job)
      (bordeaux-threads:make-thread (job-func job))))

(defun cron-dispatcher ()
  "function that dispatches the jobs that are ready to be run"
  (do ()
      (nil nil)
    (sleep (time-until-full-minute (get-universal-time)))
    (bordeaux-threads:acquire-lock *cron-dispatcher-processing*)
    (maphash #'run-job-if-time *cron-jobs-hash*)
    (bordeaux-threads:release-lock *cron-dispatcher-processing*)))

(defun start-cron ()
  "function that starts cron by first loading the cron file defined in the variable, then it runs any cron-job that has the job-only-at-boot property set to t. Finally, it starts a thread that runs cron-dispatcher"
  (cond (*cron-dispatcher-thread*
	 (log-cron-message "You attempted to call start-cron while cron is already loaded and running..."))
	(t
	 (if *cron-load-file*
	     (load *cron-load-file* :verbose nil :print nil :if-does-not-exist nil))
	 (maphash #'run-job-if-boot *cron-jobs-hash*)
	 (setf *cron-dispatcher-thread* (bordeaux-threads:make-thread #'cron-dispatcher)))))

(defun restart-cron()
  "function that starts up cron but without loading the file or running any of the boot only cron jobs in the list"
  (if (not *cron-dispatcher-thread*)
      (setf *cron-dispatcher-thread* (bordeaux-threads:make-thread #'cron-dispatcher))
      (log-cron-message "You attempted to call restart-cron while cron is already loaded and running...")))

(defun stop-cron ()
  "allows the stoppage of cron through the killing of the cron-dispatcher. Note that cron-dispatcher is killed only if it is sleeping otherwise we wait till the cron jobs finish. To reuse cron after calling stop-cron, you would need to recall start-cron which would go through all the steps as if cron has just booted. If you wish to prevent these actions when you restart cron then please you restart-cron."
  (bordeaux-threads:acquire-lock *cron-dispatcher-processing*)
  (cond (*cron-dispatcher-thread*
	 (bordeaux-threads:destroy-thread *cron-dispatcher-thread*)
	 (setf *cron-dispatcher-thread* nil))
	(t
	 (log-cron-message "You attempted to call stop-cron while cron is already stopped...")))
  (bordeaux-threads:release-lock *cron-dispatcher-processing*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Utilities for cron that are needed;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-cron-get-methods (tag-name unit-total-list &optional (allows-symbols nil) (symbol-list nil) (symbol-offset 0))
  `(defun ,tag-name (unit step-unit)
     (cond ((eq unit :every)
	    (elements-within-step ,unit-total-list step-unit))
	   ((and (symbolp unit)
		 ,allows-symbols
		 (not (eq unit :every)))
	    (let ((num (position unit ,symbol-list)))
	      (if (numberp num)
		  (list (+ num ,symbol-offset))
		  (log-cron-message (format nil "~A could not find the symbol in its corresponding list." ',tag-name)))))
	   ((numberp unit)
	    (list unit))
	   ((and (consp unit)
		 (or (mapcan #'listp unit)))
	    (let ((expanded-list (expand-internal-lists unit)))
	      (if (and (mapcan #'numberp expanded-list))
		  (elements-within-step expanded-list step-unit)
		  (log-cron-message (format nil "~A could not expand the list of data you provided since it contained symbols." ',tag-name)))))	      
	   ((and (consp unit) (mapcan #'numberp unit))
	    (elements-within-step unit step-unit))
	   (t
	    (log-cron-message (format nil "~A could not retrieve an appropriate value from what you offered." ',tag-name))))))

(def-cron-get-methods get-minutes (gen-list 0 59))
(def-cron-get-methods get-hours (gen-list 0 23))
(def-cron-get-methods get-days-of-month (gen-list 1 31))
(def-cron-get-methods get-months (gen-list 1 12) t *month-list* 1)
(def-cron-get-methods get-days-of-week (gen-list 0 6) t *day-list*) 

(defun gen-list (start-list end-list &optional (increment 1))
  "functions that returns a list of numbers starting with start-list and ending with end-list"
  (if (> start-list end-list)
      nil
      (cons start-list (gen-list (+ increment start-list) end-list increment))))

(defun min-list (lst)
  "finds the minimum element of a list"
  (cond ((endp (cdr lst))
	 (car lst))
	(t
	 (min (car lst) (min-list (cdr lst))))))

(defun max-list (lst)
  "finds the minimum element of a list"
  (cond ((endp (cdr lst))
	 (car lst))
	(t
	 (max (car lst) (max-list (cdr lst))))))
	 
(defun expand-internal-lists (lst)
  "function that takes a list and returns a list but with all internal lists expanded"
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (expand-internal-lists (cdr lst))))
	(if (consp elt)
	    (append elt rest)
	    (cons elt rest)))))

(defun elements-within-step (lst step)
  "function that returns a list of elements that are within a step from each other starting with the first element in the list"
  (intersection (gen-list (min-list lst) (max-list lst) step) lst))

(defun time-until-full-minute (time)
  (let ((seconds (decode-universal-time time)))
    (- 60 seconds)))

	    
(defun log-cron-message (message &optional (type "error"))
  "Simply log the message sent with type as well"
  (if *cron-log-file*
      (with-open-file (out *cron-log-file* :direction :output :if-exists :append :if-does-not-exist :create)
	(format out "[~A] ~A ~1%" type message))))
			



  
