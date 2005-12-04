;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/util.lisp,v 1.32 2005/08/23 10:32:30 edi Exp $

;;; Utility functions and constants dealing with the hash-tables
;;; we use to encode character classes

;;; Hash-tables are treated like sets, i.e. a character C is a member of the
;;; hash-table H iff (GETHASH C H) is true.

;;; Copyright (c) 2002-2005, Dr. Edmund Weitz. All rights reserved.

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

(in-package #:cl-ppcre)

#+:lispworks
(import 'lw:with-unique-names)

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar *regex-char-code-limit* char-code-limit
    "The upper exclusive bound on the char-codes of characters which
can occur in character classes.  Change this value BEFORE creating
scanners if you don't need the Unicode support of implementations like
AllegroCL, CLISP, LispWorks, or SBCL.")
  (declaim (type fixnum *regex-char-code-limit*))
  
  (defun make-char-hash (test)
    (declare #.*special-optimize-settings*)
    "Returns a hash-table of all characters satisfying test."
    (loop with hash = (make-hash-table)
          for c of-type fixnum from 0 below char-code-limit
          for chr = (code-char c)
          if (and chr (funcall test chr))
            do (setf (gethash chr hash) t)
          finally (return hash)))

  (declaim (inline word-char-p))
  
  (defun word-char-p (chr)
    (declare #.*standard-optimize-settings*)
    "Tests whether a character is a \"word\" character.
In the ASCII charset this is equivalent to a-z, A-Z, 0-9, or _,
i.e. the same as Perl's [\\w]."
    (or (alphanumericp chr)
        (char= chr #\_)))

  (unless (boundp '+whitespace-char-string+)
    (defconstant +whitespace-char-string+
      (coerce
       '(#\Space #\Tab #\Linefeed #\Return #\Page)
       'string)
      "A string of all characters which are considered to be whitespace.
Same as Perl's [\\s]."))

  (defun whitespacep (chr)
    (declare #.*special-optimize-settings*)
    "Tests whether a character is whitespace,
i.e. whether it would match [\\s] in Perl."
    (find chr +whitespace-char-string+ :test #'char=)))

;; the following DEFCONSTANT statements are wrapped with
;; (UNLESS (BOUNDP ...) ...) to make SBCL happy

(unless (boundp '+digit-hash+)
  (defconstant +digit-hash+
    (make-char-hash (lambda (chr) (char<= #\0 chr #\9)))
    "Hash-table containing the digits from 0 to 9."))

(unless (boundp '+word-char-hash+)
  (defconstant +word-char-hash+
    (make-char-hash #'word-char-p)
    "Hash-table containing all \"word\" characters."))

(unless (boundp '+whitespace-char-hash+)
  (defconstant +whitespace-char-hash+
    (make-char-hash #'whitespacep)
    "Hash-table containing all whitespace characters."))

(defun merge-hash (hash1 hash2)
  (declare #.*standard-optimize-settings*)
  "Returns the \"sum\" of two hashes. This is a destructive operation
on HASH1."
  (cond ((> (hash-table-count hash2)
            *regex-char-code-limit*)
          ;; don't walk through, e.g., the whole +WORD-CHAR-HASH+ if
          ;; the user has set *REGEX-CHAR-CODE-LIMIT* to a lower value
          (loop for c of-type fixnum from 0 below *regex-char-code-limit*
                for chr = (code-char c)
                if (and chr (gethash chr hash2))
                do (setf (gethash chr hash1) t)))
        (t
          (loop for chr being the hash-keys of hash2
                do (setf (gethash chr hash1) t))))
  hash1)

(defun merge-inverted-hash (hash1 hash2)
  (declare #.*standard-optimize-settings*)
  "Returns the \"sum\" of HASH1 and the \"inverse\" of HASH2. This is
a destructive operation on HASH1."
  (loop for c of-type fixnum from 0 below *regex-char-code-limit*
        for chr = (code-char c)
        if (and chr (not (gethash chr hash2)))
          do (setf (gethash chr hash1) t))
  hash1)

(defun create-ranges-from-hash (hash &key downcasep)
  (declare #.*standard-optimize-settings*)
  "Tries to identify up to three intervals (with respect to CHAR<)
which together comprise HASH. Returns NIL if this is not possible.
If DOWNCASEP is true it will treat the hash-table as if it represents
both the lower-case and the upper-case variants of its members and
will only return the respective lower-case intervals."
  ;; discard empty hash-tables
  (unless (and hash (plusp (hash-table-count hash)))
    (return-from create-ranges-from-hash nil))
  (loop with min1 and min2 and min3
        and max1 and max2 and max3
        ;; loop through all characters in HASH, sorted by CHAR<
        for chr in (sort (the list
                           (loop for chr being the hash-keys of hash
                                 collect (if downcasep
                                           (char-downcase chr)
                                           chr)))
		         #'char<)
        for code = (char-code chr)
        ;; MIN1, MAX1, etc. are _exclusive_
        ;; bounds of the intervals identified so far
        do (cond
             ((not min1)
               ;; this will only happen once, for the first character
               (setq min1 (1- code)
                     max1 (1+ code)))
             ((<= (the fixnum min1) code (the fixnum max1))
               ;; we're here as long as CHR fits into the first interval
               (setq min1 (min (the fixnum min1) (1- code))
                     max1 (max (the fixnum max1) (1+ code))))
             ((not min2)
               ;; we need to open a second interval
               ;; this'll also happen only once
               (setq min2 (1- code)
                     max2 (1+ code)))
             ((<= (the fixnum min2) code (the fixnum max2))
               ;; CHR fits into the second interval
               (setq min2 (min (the fixnum min2) (1- code))
                     max2 (max (the fixnum max2) (1+ code))))
             ((not min3)
               ;; we need to open the third interval
               ;; happens only once
               (setq min3 (1- code)
                     max3 (1+ code)))
             ((<= (the fixnum min3) code (the fixnum max3))
               ;; CHR fits into the third interval
               (setq min3 (min (the fixnum min3) (1- code))
                     max3 (max (the fixnum max3) (1+ code))))
             (t
               ;; we're out of luck, CHR doesn't fit
               ;; into one of the three intervals
               (return nil)))
        ;; on success return all bounds
        ;; make them inclusive bounds before returning
        finally (return (values (code-char (1+ min1))
                                (code-char (1- max1))
                                (and min2 (code-char (1+ min2)))
                                (and max2 (code-char (1- max2)))
                                (and min3 (code-char (1+ min3)))
                                (and max3 (code-char (1- max3)))))))

(defmacro maybe-coerce-to-simple-string (string)
  (with-unique-names (=string=)
    `(let ((,=string= ,string))
      (cond ((simple-string-p ,=string=)
              ,=string=)
            (t
              (coerce ,=string= 'simple-string))))))

(declaim (inline nsubseq))
(defun nsubseq (sequence start &optional (end (length sequence)))
  "Return a subsequence by pointing to location in original sequence."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defun normalize-var-list (var-list)
  "Utility function for REGISTER-GROUPS-BIND and
DO-REGISTER-GROUPS. Creates the long form \(a list of \(FUNCTION VAR)
entries) out of the short form of VAR-LIST."
  (loop for element in var-list
        if (consp element)
          nconc (loop for var in (rest element)
                      collect (list (first element) var))
        else
          collect (list '(function identity) element)))

(defun string-list-to-simple-string (string-list)
  (declare #.*standard-optimize-settings*)
  "Concatenates a list of strings to one simple-string."
  ;; this function provided by JP Massar; note that we can't use APPLY
  ;; with CONCATENATE here because of CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (declare (type fixnum total-size))
    (dolist (string string-list)
      #-genera (declare (type string string))
      (incf total-size (length string)))
    (let ((result-string (make-sequence 'simple-string total-size))
          (curr-pos 0))
      (declare (type fixnum curr-pos))
      (dolist (string string-list)
        #-genera (declare (type string string))
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))
