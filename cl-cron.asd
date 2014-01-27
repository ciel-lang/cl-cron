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

(asdf:defsystem #:cl-cron
  :version "0.1"
  :serial t
  :depends-on (:bordeaux-threads)
  :components ((:file "packages")
	       (:file "cl-cron")))
