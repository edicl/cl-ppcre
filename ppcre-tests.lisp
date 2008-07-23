;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/ppcre-tests.lisp,v 1.36 2008/06/25 14:04:28 edi Exp $

;;; Copyright (c) 2002-2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-ppcre-test)

(defparameter *cl-ppcre-test-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(defun full-gc ()
  "Start a full garbage collection."
  ;; what are the corresponding values for MCL and OpenMCL?
  #+:allegro (excl:gc t)
  #+(or :cmu :scl) (ext:gc :full t)
  #+:ecl (si:gc t)
  #+:clisp (ext:gc)
  #+:cormanlisp (loop for i from 0 to 3 do (cormanlisp:gc i))
  #+:lispworks4 (hcl:mark-and-sweep 3)
  #+:lispworks5 (hcl:gc-generation #+:lispworks-32bit 3 #+:lispworks-64bit :blocking-gen-num)
  #+:sbcl (sb-ext:gc :full t))

;; warning: ugly code ahead!!
;; this is just a quick hack for testing purposes

(defun time-regex (factor regex string
                          &key case-insensitive-mode
                               multi-line-mode
                               single-line-mode
                               extended-mode)
  (declare #.ppcre::*standard-optimize-settings*)
  "Auxiliary function used by TEST to benchmark a regex scanner
against Perl timings."
  (declare (type string string))
  (let* ((scanner (create-scanner regex
                                  :case-insensitive-mode case-insensitive-mode
                                  :multi-line-mode multi-line-mode
                                  :single-line-mode single-line-mode
                                  :extended-mode extended-mode))
         ;; make sure GC doesn't invalidate our benchmarking
         (dummy (full-gc))
         (start (get-internal-real-time)))
    (declare (ignore dummy))
    (dotimes (i factor)
      (funcall scanner string 0 (length string)))
    (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

#+(or scl
      lispworks
      (and sbcl sb-thread))
(defun threaded-scan (scanner target-string &key (threads 10) (repetitions 5000))
  (declare #.ppcre::*standard-optimize-settings*)
  "Auxiliary function used by TEST to check whether SCANNER is thread-safe."
  (full-gc)
  (let ((collector (make-array threads))
        (counter 0))
    (loop for i below threads
          do (let* ((j i)
                    (fn
                      (lambda ()
                        (let ((r (random repetitions)))
                          (loop for k below repetitions
                                if (= k r)
                                  do (setf (aref collector j)
                                             (let ((result
                                                     (multiple-value-list
                                                       (cl-ppcre:scan scanner target-string))))
                                               (unless (cdr result)
                                                 (setq result '(nil nil #() #())))
                                               result))
                                else
                                  do (cl-ppcre:scan scanner target-string))
                          (incf counter)))))
               #+scl (thread:thread-create fn)
               #+lispworks (mp:process-run-function "" nil fn)
               #+(and sbcl sb-thread) (sb-thread:make-thread fn)))
    (loop while (< counter threads)
          do (sleep .1))
    (destructuring-bind (first-start first-end first-reg-starts first-reg-ends)
        (aref collector 0)
      (loop for (start end reg-starts reg-ends) across collector
            if (or (not (eql first-start start))
                   (not (eql first-end end))
                   (/= (length first-reg-starts) (length reg-starts))
                   (/= (length first-reg-ends) (length reg-ends))
                   (loop for first-reg-start across first-reg-starts
                         for reg-start across reg-starts
                         thereis (not (eql first-reg-start reg-start)))
                   (loop for first-reg-end across first-reg-ends
                         for reg-end across reg-ends
                         thereis (not (eql first-reg-end reg-end))))
            do (return (format nil "~&Inconsistent results during multi-threading"))))))

(defun create-string-from-input (input)
  (cond ((or (null input)
             (stringp input))
          input)
        (t
          (cl-ppcre::string-list-to-simple-string
           (loop for element in input
                 if (stringp element)
                 collect element
                 else
                 collect (string (code-char element)))))))

(defun test (&key (file-name 
                   (make-pathname :name "testdata"
                                  :type nil :version nil
                                  :defaults *cl-ppcre-test-base-directory*)
                   file-name-provided-p)
                  threaded)
  (declare #.ppcre::*standard-optimize-settings*)
  (declare (ignorable threaded))
  "Loop through all test cases in FILE-NAME and print report. Only in
LispWorks and SCL: If THREADED is true, also test whether the scanners
work multi-threaded."
  (with-open-file (stream file-name
                          #+(or :allegro :clisp :scl :sbcl)
                          :external-format
                          #+(or :allegro :clisp :scl :sbcl)
                          (if file-name-provided-p
                            :default
                            #+(or :allegro :scl :sbcl) :iso-8859-1
                            #+:clisp charset:iso-8859-1))
    (loop with testcount of-type fixnum = 0
          with *regex-char-code-limit* = (if file-name-provided-p
                                           *regex-char-code-limit*
                                           ;; the standard test suite
                                           ;; doesn't need Unicode
                                           ;; support
                                           256)
          with *allow-quoting* = (if file-name-provided-p
                                   *allow-quoting*
                                   t)
          for input-line = (read stream nil nil)
          for (counter info-string regex
                       case-insensitive-mode multi-line-mode
                       single-line-mode extended-mode
                       string perl-error factor
                       perl-time ex-result ex-subs) = input-line
          while input-line
          do (let ((info-string (create-string-from-input info-string))
                   (regex (create-string-from-input regex))
                   (string (create-string-from-input string))
                   (ex-result (create-string-from-input ex-result))
                   (ex-subs (mapcar #'create-string-from-input ex-subs))
                   (errors '()))
               ;; provide some visual feedback for slow CL
               ;; implementations; suggested by JP Massar
               (incf testcount)
               #+(or scl
                     lispworks
                     (and sbcl sb-thread))
               (when threaded
                 (format t "Test #~A (ID ~A)~%" testcount counter)
                 (force-output))
               (unless #-(or scl
                             lispworks
                             (and sbcl sb-thread))
                       nil
                       #+(or scl
                             lispworks
                             (and sbcl sb-thread))
                       threaded
                 (when (zerop (mod testcount 10))
                   (format t ".")
                   (force-output))
                 (when (zerop (mod testcount 100))
                   (terpri)))
               (handler-case
                 (let* ((*use-bmh-matchers* (if (and (> factor 1) (plusp perl-time))
                                              *use-bmh-matchers*
                                              ;; if we only check for
                                              ;; correctness we don't
                                              ;; care about speed that
                                              ;; match (but rather
                                              ;; about space
                                              ;; constraints of the
                                              ;; trial versions)
                                              nil))
                        (scanner (create-scanner regex
                                                :case-insensitive-mode case-insensitive-mode
                                                :multi-line-mode multi-line-mode
                                                :single-line-mode single-line-mode
                                                :extended-mode extended-mode)))
                   (multiple-value-bind (result1 result2 sub-starts sub-ends)
                       (scan scanner string)
                     (cond (perl-error
                             (push (format nil
                                           "~&expected an error but got a result")
                                   errors))
                           (t
                             (when (not (eq result1 ex-result))
                               (if result1
                                 (let ((result (subseq string result1 result2)))
                                   (unless (string= result ex-result)
                                     (push (format nil
                                                   "~&expected ~S but got ~S"
                                                   ex-result result)
                                           errors))
                                   (setq sub-starts (coerce sub-starts 'list)
                                         sub-ends (coerce sub-ends 'list))
                                   (loop for i from 0
                                         for ex-sub in ex-subs
                                         for sub-start = (nth i sub-starts)
                                         for sub-end = (nth i sub-ends)
                                         for sub = (if (and sub-start sub-end)
                                                     (subseq string sub-start sub-end)
                                                     nil)
                                         unless (string= ex-sub sub)
                                           do (push (format nil
                                                            "~&\\~A: expected ~S but got ~S"
                                                            (1+ i) ex-sub sub) errors)))
                                 (push (format nil
                                               "~&expected ~S but got ~S"
                                               ex-result result1)
                                       errors)))))
                     #+(or scl
                           lispworks
                           (and sbcl sb-thread))
                     (when threaded
                       (let ((thread-result (threaded-scan scanner string)))
                         (when thread-result
                         (push thread-result errors))))))
                 (condition (msg)
                   (unless perl-error
                     (push (format nil "~&got an unexpected error: '~A'" msg)
                           errors))))
               (setq errors (nreverse errors))
               (cond (errors
                       (when (or (<= factor 1) (zerop perl-time))
                         (format t "~&~4@A (~A):~{~&   ~A~}~%"
                                 counter info-string errors)))
                     ((and (> factor 1) (plusp perl-time))
                       (let ((result (time-regex factor regex string
                                                 :case-insensitive-mode case-insensitive-mode
                                                 :multi-line-mode multi-line-mode
                                                 :single-line-mode single-line-mode
                                                 :extended-mode extended-mode)))
                         (format t "~&~4@A: ~,4F (~A repetitions, Perl: ~,4F seconds, CL-PPCRE: ~,4F seconds)" counter
                                 (float (/ result perl-time)) factor perl-time result)
                         #+:cormanlisp (force-output *standard-output*)))
                     (t nil))))
    (values)))
