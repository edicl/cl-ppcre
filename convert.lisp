;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /home/manuel/bknr-cvs/cvs/thirdparty/cl-ppcre/convert.lisp,v 1.1 2004/06/23 08:27:10 hans Exp $

;;; Here the parse tree is converted into its internal representation
;;; using REGEX objects.  At the same time some optimizations are
;;; already applied.

;;; Copyright (c) 2002-2003, Dr. Edmund Weitz. All rights reserved.

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

;;; The flags that represent the "ism" modifiers are always kept
;;; together in a three-element list. We use the following macros to
;;; access individual elements.

(defmacro case-insensitive-mode-p (flags)
  "Accessor macro to extract the first flag out of a three-element flag list."
  `(first ,flags))

(defmacro multi-line-mode-p (flags)
  "Accessor macro to extract the second flag out of a three-element flag list."
  `(second ,flags))

(defmacro single-line-mode-p (flags)
  "Accessor macro to extract the third flag out of a three-element flag list."
  `(third ,flags))

(defun set-flag (token)
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  (declare (special flags))
  "Reads a flag token and sets or unsets the corresponding entry in
the special FLAGS list."
  (case token
    ((:case-insensitive-p)
      (setf (case-insensitive-mode-p flags) t))
    ((:case-sensitive-p)
      (setf (case-insensitive-mode-p flags) nil))
    ((:multi-line-mode-p)
      (setf (multi-line-mode-p flags) t))
    ((:not-multi-line-mode-p)
      (setf (multi-line-mode-p flags) nil))
    ((:single-line-mode-p)
      (setf (single-line-mode-p flags) t))
    ((:not-single-line-mode-p)
      (setf (single-line-mode-p flags) nil))
    (otherwise
      (signal-ppcre-syntax-error "Unknown flag token ~A" token))))

(defun add-range-to-hash (hash from to)
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  (declare (special flags))
  "Adds all characters from character FROM to character TO (inclusive)
to the char class hash HASH. Does the right thing with respect to
case-(in)sensitivity as specified by the special variable FLAGS."
  (let ((from-code (char-code from))
        (to-code (char-code to)))
    (when (> from-code to-code)
      (signal-ppcre-syntax-error "Invalid range from ~A to ~A in char-class"
                                 from to))
    (cond ((case-insensitive-mode-p flags)
            (loop for code from from-code to to-code
                  for chr = (code-char code)
                  do (setf (gethash (char-upcase chr) hash) t
                           (gethash (char-downcase chr) hash) t)))
          (t
            (loop for code from from-code to to-code
                  do (setf (gethash (code-char code) hash) t))))
    hash))

(defun convert-char-class-to-hash (list)
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  "Combines all items in LIST into one char class hash and returns it.
Items can be single characters, character ranges like \(:RANGE #\\A
#\\E), or special character classes like :DIGIT-CLASS. Does the right
thing with respect to case-\(in)sensitivity as specified by the
special variable FLAGS."
  (loop with hash = (make-hash-table :size (ceiling (expt *regex-char-code-limit* (/ 1 4)))
                                     :rehash-size (float (expt *regex-char-code-limit* (/ 1 4)))
                                     :rehash-threshold 1.0)
        for item in list
        if (characterp item)
          ;; treat a single character C like a range (:RANGE C C)
          do (add-range-to-hash hash item item)
        else if (symbolp item)
          ;; special character classes
          do (setq hash
                     (case item
                       ((:digit-class)
                         (merge-hash hash +digit-hash+))
                       ((:non-digit-class)
                         (merge-inverted-hash hash +digit-hash+))
                       ((:whitespace-char-class)
                         (merge-hash hash +whitespace-char-hash+))
                       ((:non-whitespace-char-class)
                         (merge-inverted-hash hash +whitespace-char-hash+))
                       ((:word-char-class)
                         (merge-hash hash +word-char-hash+))
                       ((:non-word-char-class)
                         (merge-inverted-hash hash +word-char-hash+))
                       (otherwise
                         (signal-ppcre-syntax-error
                          "Unknown symbol ~A in character class"
                          item))))
        else if (and (consp item)
                     (eq (car item) :range))
          ;; proper ranges
          do (add-range-to-hash hash
                                (second item)
                                (third item))
        else do (signal-ppcre-syntax-error "Unknown item ~A in char-class list"
                                           item)
        finally (return hash)))

(defun maybe-split-repetition (regex
                               greedyp
                               minimum
                               maximum
                               min-len
                               length
                               reg-seen)
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  (declare (type fixnum minimum)
           (type (or fixnum null) maximum))
  "Splits a REPETITION object into a constant and a varying part if
applicable, i.e. something like
  a{3,} -> a{3}a*
The arguments to this function correspond to the REPETITION slots of
the same name."
  ;; note the usage of COPY-REGEX here; we can't use the same REGEX
  ;; object in both REPETITIONS because they will have different
  ;; offsets
  (when maximum
    (when (zerop maximum)
      ;; trivial case: don't repeat at all
      (return-from maybe-split-repetition
        (make-instance 'void)))
    (when (= 1 minimum maximum)
      ;; another trivial case: "repeat" exactly once
      (return-from maybe-split-repetition
        regex)))
  ;; first set up the constant part of the repetition
  ;; maybe that's all we need
  (let ((constant-repetition (if (plusp minimum)
                               (make-instance 'repetition
                                              :regex (copy-regex regex)
                                              :greedyp greedyp
                                              :minimum minimum
                                              :maximum minimum
                                              :min-len min-len
                                              :len length
                                              :contains-register-p reg-seen)
                               ;; don't create garbage if minimum is 0
                               nil)))
    (when (and maximum
               (= maximum minimum))
      (return-from maybe-split-repetition
        ;; no varying part needed because min = max
        constant-repetition))
    ;; now construct the varying part
    (let ((varying-repetition
            (make-instance 'repetition
                           :regex regex
                           :greedyp greedyp
                           :minimum 0
                           :maximum (if maximum (- maximum minimum) nil)
                           :min-len min-len
                           :len length
                           :contains-register-p reg-seen)))
      (cond ((zerop minimum)
              ;; min = 0, no constant part needed
              varying-repetition)
            ((= 1 minimum)
              ;; min = 1, constant part needs no REPETITION wrapped around
              (make-instance 'seq
                             :elements (list (copy-regex regex)
                                             varying-repetition)))
            (t
              ;; general case
              (make-instance 'seq
                             :elements (list constant-repetition
                                             varying-repetition)))))))

;; During the conversion of the parse tree we keep track of the start
;; of the parse tree in the special variable STARTS-WITH which'll
;; either hold a STR object or an EVERYTHING object. The latter is the
;; case if the regex starts with ".*" which implicitely anchors the
;; regex at the start (perhaps modulo #\Newline).

(defmethod maybe-accumulate ((str str))
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  (declare (special accumulate-start-p starts-with))
  (declare (ftype (function (t) fixnum) len))
  "Accumulate STR into the special variable STARTS-WITH if
ACCUMULATE-START-P (also special) is true and STARTS-WITH is either
NIL or a STR object of the same case mode. Always returns NIL."
  (when accumulate-start-p
    (etypecase starts-with
      (str
        ;; STARTS-WITH already holds a STR, so we check if we can
        ;; concatenate
        (cond ((eq (case-insensitive-p starts-with)
                   (case-insensitive-p str))
                ;; we modify STARTS-WITH in place
                (setf (len starts-with)
                        (+ (len starts-with) (len str)))
                ;; note that we use SLOT-VALUE because the accessor
                ;; STR has a declared FTYPE which doesn't fit here
                (adjust-array (slot-value starts-with 'str)
                              (len starts-with)
                              :fill-pointer t)
                (setf (subseq (slot-value starts-with 'str)
                              (- (len starts-with) (len str)))
                        (str str)
                      ;; STR objects that are parts of STARTS-WITH
                      ;; always have their SKIP slot set to true
                      ;; because the SCAN function will take care of
                      ;; them, i.e. the matcher can ignore them
                      (skip str) t))
              (t (setq accumulate-start-p nil))))
      (null
        ;; STARTS-WITH is still empty, so we create a new STR object
        (setf starts-with
                (make-instance 'str
                               :str ""
                               :case-insensitive-p (case-insensitive-p str))
              ;; INITIALIZE-INSTANCE will coerce the STR to a simple
              ;; string, so we have to fill it afterwards
              (slot-value starts-with 'str)
                (make-array (len str)
                            :initial-contents (str str)
                            :element-type 'character
                            :fill-pointer t
                            :adjustable t)
              (len starts-with)
                (len str)
              ;; see remark about SKIP above
              (skip str) t))
      (everything
        ;; STARTS-WITH already holds an EVERYTHING object - we can't
        ;; concatenate
        (setq accumulate-start-p nil))))
  nil)

(defun convert-aux (parse-tree)
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  (declare (special flags reg-num accumulate-start-p starts-with max-back-ref))
  "Converts the parse tree PARSE-TREE into a REGEX object and returns it.

Will also
  - split and optimize repetitions,
  - accumulate strings or EVERYTHING objects into the special variable
    STARTS-WITH,
  - keep track of all registers seen in the special variable REG-NUM,
  - keep track of the highest backreference seen in the special
    variable MAX-BACK-REF,
  - maintain and adher to the currently applicable modifiers in the special
    variable FLAGS, and
  - maybe even wash your car..."
  (cond ((consp parse-tree)
          (case (first parse-tree)
            ;; (:SEQUENCE {<regex>}*)
            ((:sequence)
              (cond ((cddr parse-tree)
                      ;; this is essentially like
                      ;; (MAPCAR 'CONVERT-AUX (REST PARSE-TREE))
                      ;; but we don't cons a new list
                      (loop for parse-tree-rest on (rest parse-tree)
                            while parse-tree-rest
                            do (setf (car parse-tree-rest)
                                       (convert-aux (car parse-tree-rest))))
                      (make-instance 'seq
                                     :elements (rest parse-tree)))
                    (t (convert-aux (second parse-tree)))))
            ;; (:GROUP {<regex>}*)
            ;; this is a syntactical construct equivalent to :SEQUENCE
            ;; intended to keep the effect of modifiers local
            ((:group)
              ;; make a local copy of FLAGS and shadow the global
              ;; value while we descend into the enclosed regexes
              (let ((flags (copy-list flags)))
                (declare (special flags))
                (cond ((cddr parse-tree)
                        (loop for parse-tree-rest on (rest parse-tree)
                              while parse-tree-rest
                              do (setf (car parse-tree-rest)
                                         (convert-aux (car parse-tree-rest))))
                        (make-instance 'seq
                                       :elements (rest parse-tree)))
                      (t (convert-aux (second parse-tree))))))
            ;; (:ALTERNATION {<regex>}*)
            ((:alternation)
              ;; we must stop accumulating objects into STARTS-WITH
              ;; once we reach an alternation
              (setq accumulate-start-p nil)
              (loop for parse-tree-rest on (rest parse-tree)
                    while parse-tree-rest
                    do (setf (car parse-tree-rest)
                               (convert-aux (car parse-tree-rest))))
              (make-instance 'alternation
                             :choices (rest parse-tree)))
            ;; (:BRANCH <test> <regex>)
            ;; <test> must be look-ahead, look-behind or number;
            ;; if <regex> is an alternation it must have one or two
            ;; choices
            ((:branch)
              (setq accumulate-start-p nil)
              (let* ((test-candidate (second parse-tree))
                     (test (cond ((numberp test-candidate)
                                   (when (zerop (the fixnum test-candidate))
                                     (signal-ppcre-syntax-error
                                      "Register 0 doesn't exist: ~S"
                                      parse-tree))
                                   (1- (the fixnum test-candidate)))
                                 (t (convert-aux test-candidate))))
                     (alternations (convert-aux (third parse-tree))))
                (when (and (not (numberp test))
                           (not (typep test 'lookahead))
                           (not (typep test 'lookbehind)))
                  (signal-ppcre-syntax-error
                   "Branch test must be look-ahead, look-behind or number: ~S"
                   parse-tree))
                (typecase alternations
                  (alternation
                    (case (length (choices alternations))
                      ((0)
                        (signal-ppcre-syntax-error "No choices in branch: ~S"
                                                   parse-tree))
                      ((1)
                        (make-instance 'branch
                                       :test test
                                       :then-regex (first
                                                    (choices alternations))))
                      ((2)
                        (make-instance 'branch
                                       :test test
                                       :then-regex (first
                                                    (choices alternations))
                                       :else-regex (second
                                                    (choices alternations))))
                      (otherwise
                        (signal-ppcre-syntax-error
                         "Too much choices in branch: ~S"
                         parse-tree))))
                  (t
                    (make-instance 'branch
                                   :test test
                                   :then-regex alternations)))))
            ;; (:POSITIVE-LOOKAHEAD|:NEGATIVE-LOOKAHEAD <regex>)
            ((:positive-lookahead :negative-lookahead)
              ;; keep the effect of modifiers local to the enclosed
              ;; regex and stop accumulating into STARTS-WITH
              (setq accumulate-start-p nil)
              (let ((flags (copy-list flags)))
                (declare (special flags))
                (make-instance 'lookahead
                               :regex (convert-aux (second parse-tree))
                               :positivep (eq (first parse-tree)
                                              :positive-lookahead))))
            ;; (:POSITIVE-LOOKBEHIND|:NEGATIVE-LOOKBEHIND <regex>)
            ((:positive-lookbehind :negative-lookbehind)
              ;; keep the effect of modifiers local to the enclosed
              ;; regex and stop accumulating into STARTS-WITH
              (setq accumulate-start-p nil)
              (let* ((flags (copy-list flags))
                     (regex (convert-aux (second parse-tree)))
                     (len (regex-length regex)))
                (declare (special flags))
                ;; lookbehind assertions must be of fixed length
                (unless len
                  (signal-ppcre-syntax-error
                   "Variable length look-behind not implemented (yet): ~S"
                   parse-tree))
                (make-instance 'lookbehind
                               :regex regex
                               :positivep (eq (first parse-tree)
                                              :positive-lookbehind)
                               :len len)))
            ;; (:GREEDY-REPETITION|:NON-GREEDY-REPETITION <min> <max> <regex>)
            ((:greedy-repetition :non-greedy-repetition)
              ;; remember the value of ACCUMULATE-START-P upon entering
              (let ((local-accumulate-start-p accumulate-start-p))
                (let ((minimum (second parse-tree))
                      (maximum (third parse-tree)))
                  (declare (type fixnum minimum))
                  (declare (type (or null fixnum) maximum))
                  (unless (and maximum
                               (= 1 minimum maximum))
                    ;; set ACCUMULATE-START-P to NIL for the rest of
                    ;; the conversion because we can't continue to
                    ;; accumulate inside as well as after a proper
                    ;; repetition
                    (setq accumulate-start-p nil))
                  (let* (reg-seen
                         (regex (convert-aux (fourth parse-tree)))
                         (min-len (regex-min-length regex))
                         (greedyp (eq (first parse-tree) :greedy-repetition))
                         (length (regex-length regex)))
                    ;; note that this declaration already applies to
                    ;; the call to CONVERT-AUX above
                    (declare (special reg-seen))
                    (when (and local-accumulate-start-p
                               (not starts-with)
                               (zerop minimum)
                               (not maximum))
                      ;; if this repetition is (equivalent to) ".*"
                      ;; and if we're at the start of the regex we
                      ;; remember it for ADVANCE-FN (see the SCAN
                      ;; function)
                      (setq starts-with (everythingp regex)))
                    (if (or (not reg-seen)
                            (not greedyp)
                            (not length)
                            (zerop length)
                            (and maximum (= minimum maximum)))
                      ;; the repetition doesn't enclose a register, or
                      ;; it's not greedy, or we can't determine it's
                      ;; (inner) length, or the length is zero, or the
                      ;; number of repetitions is fixed; in all of
                      ;; these cases we don't bother to optimize
                      (maybe-split-repetition regex
                                              greedyp
                                              minimum
                                              maximum
                                              min-len
                                              length
                                              reg-seen)
                      ;; otherwise we make a transformation that looks
                      ;; roughly like one of
                      ;;   <regex>* -> (?:<regex'>*<regex>)?
                      ;;   <regex>+ -> <regex'>*<regex>
                      ;; where the trick is that as much as possible
                      ;; registers from <regex> are removed in
                      ;; <regex'>
                      (let* (reg-seen   ; new instance for REMOVE-REGISTERS
                             (remove-registers-p t)
                             (inner-regex (remove-registers regex))
                             (inner-repetition
                               ;; this is the "<regex'>" part
                               (maybe-split-repetition inner-regex
                                                       ;; always greedy
                                                       t
                                                       ;; reduce minimum by 1
                                                       ;; unless it's already 0
                                                       (if (zerop minimum)
                                                         0
                                                         (1- minimum))
                                                       ;; reduce maximum by 1
                                                       ;; unless it's NIL
                                                       (and maximum
                                                            (1- maximum))
                                                       min-len
                                                       length
                                                       reg-seen))
                             (inner-seq
                               ;; this is the "<regex'>*<regex>" part
                               (make-instance 'seq
                                              :elements (list inner-repetition
                                                              regex))))
                        ;; note that this declaration already applies
                        ;; to the call to REMOVE-REGISTERS above
                        (declare (special remove-registers-p reg-seen))
                        ;; wrap INNER-SEQ with a greedy
                        ;; {0,1}-repetition (i.e. "?") if necessary
                        (if (plusp minimum)
                          inner-seq
                          (maybe-split-repetition inner-seq
                                                  t
                                                  0
                                                  1
                                                  min-len
                                                  nil
                                                  t))))))))
            ;; (:REGISTER <regex>)
            ((:register)
              ;; keep the effect of modifiers local to the enclosed
              ;; regex; also, assign the current value of REG-NUM to
              ;; the corresponding slot of the REGISTER object and
              ;; increase this counter afterwards
              (let ((flags (copy-list flags))
                    (stored-reg-num reg-num))
                (declare (special flags reg-seen))
                (setq reg-seen t)
                (incf (the fixnum reg-num))
                (make-instance 'register
                               :regex (convert-aux (second parse-tree))
                               :num stored-reg-num)))
            ;; (:STANDALONE <regex>)
            ((:standalone)
              ;; keep the effect of modifiers local to the enclosed
              ;; regex
              (let ((flags (copy-list flags)))
                (declare (special flags))
                (make-instance 'standalone
                               :regex (convert-aux (second parse-tree)))))
            ;; (:BACK-REFERENCE <number>)
            ((:back-reference)
              (let ((backref-number (second parse-tree)))
                (declare (type fixnum backref-number))
                (when (or (not (typep backref-number 'fixnum))
                          (<= backref-number 0))
                  (signal-ppcre-syntax-error
                   "Illegal back-reference: ~S"
                   parse-tree))
                ;; stop accumulating into STARTS-WITH and increase
                ;; MAX-BACK-REF if necessary
                (setq accumulate-start-p nil
                      max-back-ref (max (the fixnum max-back-ref)
                                        backref-number))
                (make-instance 'back-reference
                               ;; we start counting from 0 internally
                               :num (1- backref-number)
                               :case-insensitive-p (case-insensitive-mode-p
                                                    flags))))
            ;; (:CHAR-CLASS|:INVERTED-CHAR-CLASS {<item>}*)
            ;; where item is one of
            ;;   - a character
            ;;   - a character range: (:RANGE <char1> <char2>)
            ;;   - a special char class symbol like :DIGIT-CHAR-CLASS
            ((:char-class :inverted-char-class)
              ;; first create the hash-table and some auxiliary values
              (let* (hash
                     hash-keys
                     (count most-positive-fixnum)
                     (item-list (rest parse-tree))
                     (invertedp (eq (first parse-tree) :inverted-char-class))
                     word-char-class-p)
                (cond ((every (lambda (item) (eq item :word-char-class))
                              item-list)
                        ;; treat "[\\w]" like "\\w"
                        (setq word-char-class-p t))
                      ((every (lambda (item) (eq item :non-word-char-class))
                              item-list)
                        ;; treat "[\\W]" like "\\W"
                        (setq word-char-class-p t)
                        (setq invertedp (not invertedp)))
                      (t
                        (setq hash (convert-char-class-to-hash item-list)
                              count (hash-table-count hash))
                        (when (<= count 2)
                          ;; collect the hash-table keys into a list if
                          ;; COUNT is smaller than 3
                          (setq hash-keys
                                  (loop for chr being the hash-keys of hash
                                        collect chr)))))
                (cond ((and (not invertedp)
                            (= count 1))
                        ;; convert one-element hash table into a STR
                        ;; object and try to accumulate into
                        ;; STARTS-WITH
                        (let ((str (make-instance 'str
                                                  :str (string
                                                        (first hash-keys))
                                                  :case-insensitive-p nil)))
                          (maybe-accumulate str)
                          str))
                      ((and (not invertedp)
                            (= count 2)
                            (char-equal (first hash-keys) (second hash-keys)))
                        ;; convert two-element hash table into a
                        ;; case-insensitive STR object and try to
                        ;; accumulate into STARTS-WITH if the two
                        ;; characters are CHAR-EQUAL
                        (let ((str (make-instance 'str
                                                  :str (string
                                                        (first hash-keys))
                                                  :case-insensitive-p t)))
                          (maybe-accumulate str)
                          str))
                      (t
                        ;; the general case; stop accumulating into STARTS-WITH
                        (setq accumulate-start-p nil)
                        (make-instance 'char-class
                                       :hash hash
                                       :case-insensitive-p
                                         (case-insensitive-mode-p flags)
                                       :invertedp invertedp
                                       :word-char-class-p word-char-class-p)))))
            ;; (:FLAGS {<flag>}*)
            ;; where flag is a modifier symbol like :CASE-INSENSITIVE-P
            ((:flags)
              ;; set/unset the flags corresponding to the symbols
              ;; following :FLAGS
              (mapc #'set-flag (rest parse-tree))
              ;; we're only interested in the side effect of
              ;; setting/unsetting the flags and turn this syntactical
              ;; construct into a VOID object which'll be optimized
              ;; away when creating the matcher
              (make-instance 'void))
            (otherwise
              (signal-ppcre-syntax-error
               "Unknown token ~A in parse-tree"
               (first parse-tree)))))
        ((or (characterp parse-tree) (stringp parse-tree))
          ;; turn characters or strings into STR objects and try to
          ;; accumulate into STARTS-WITH
          (let ((str (make-instance 'str
                                    :str (string parse-tree)
                                    :case-insensitive-p
                                      (case-insensitive-mode-p flags))))
            (maybe-accumulate str)
            str))
        (t
          ;; and now for the tokens which are symbols
          (case parse-tree
            ((:void)
              (make-instance 'void))
            ((:word-boundary)
              (make-instance 'word-boundary :negatedp nil))             
            ((:non-word-boundary)
              (make-instance 'word-boundary :negatedp t))             
            ;; the special character classes
            ((:digit-class
              :non-digit-class
              :word-char-class
              :non-word-char-class
              :whitespace-char-class
              :non-whitespace-char-class)
              ;; stop accumulating into STARTS-WITH
              (setq accumulate-start-p nil)
              (make-instance 'char-class
                             ;; use the constants defined in util.lisp
                             :hash (case parse-tree
                                     ((:digit-class
                                       :non-digit-class)
                                       +digit-hash+)
                                     ((:word-char-class
                                       :non-word-char-class)
                                       nil)
                                     ((:whitespace-char-class
                                       :non-whitespace-char-class)
                                       +whitespace-char-hash+))
                             ;; this value doesn't really matter but
                             ;; NIL should result in slightly faster
                             ;; matchers
                             :case-insensitive-p nil
                             :invertedp (member parse-tree
                                                '(:non-digit-class
                                                  :non-word-char-class
                                                  :non-whitespace-char-class)
                                                :test #'eq)
                             :word-char-class-p (member parse-tree
                                                        '(:word-char-class
                                                          :non-word-char-class)
                                                        :test #'eq)))
            ((:start-anchor             ; Perl's "^"
              :end-anchor               ; Perl's "$"
              :modeless-end-anchor-no-newline
                                        ; Perl's "\z"
              :modeless-start-anchor    ; Perl's "\A"
              :modeless-end-anchor)     ; Perl's "\Z"
              (make-instance 'anchor
                             :startp (member parse-tree
                                             '(:start-anchor
                                               :modeless-start-anchor)
                                             :test #'eq)
                             ;; set this value according to the
                             ;; current settings of FLAGS (unless it's
                             ;; a modeless anchor)
                             :multi-line-p
                               (and (multi-line-mode-p flags)
                                    (not (member parse-tree
                                                 '(:modeless-start-anchor
                                                   :modeless-end-anchor
                                                   :modeless-end-anchor-no-newline)
                                                 :test #'eq)))
                             :no-newline-p
                               (eq parse-tree
                                   :modeless-end-anchor-no-newline)))
            ((:everything)
              ;; stop accumulating into STARTS-WITHS
              (setq accumulate-start-p nil)
              (make-instance 'everything
                             :single-line-p (single-line-mode-p flags)))
            ;; special tokens corresponding to Perl's "ism" modifiers
            ((:case-insensitive-p
              :case-sensitive-p
              :multi-line-mode-p
              :not-multi-line-mode-p
              :single-line-mode-p
              :not-single-line-mode-p)
              ;; we're only interested in the side effect of
              ;; setting/unsetting the flags and turn these tokens
              ;; into VOID objects which'll be optimized away when
              ;; creating the matcher
              (set-flag parse-tree)
              (make-instance 'void))
            (otherwise
              (signal-ppcre-syntax-error "Unknown token ~A in parse-tree"
                                         parse-tree))))))

(defun convert (parse-tree)
  (declare (optimize speed
                     (safety 0)
                     (space 0)
                     (debug 0)
                     (compilation-speed 0)
                     #+:lispworks (hcl:fixnum-safety 0)))
  "Converts the parse tree PARSE-TREE into an equivalent REGEX object
and returns three values: the REGEX object, the number of registers
seen and an object the regex starts with which is either a STR object
or an EVERYTHING object (if the regex starts with something like
\".*\") or NIL."
  ;; this function basically just initializes the special variables
  ;; and then calls CONVERT-AUX to do all the work
  (let* ((flags (list nil nil nil))
         (reg-num 0)
         (accumulate-start-p t)
         starts-with
         (max-back-ref 0)
         (converted-parse-tree (convert-aux parse-tree)))
    (declare (special flags reg-num accumulate-start-p starts-with max-back-ref))
    ;; make sure we don't reference registers which aren't there
    (when (> (the fixnum max-back-ref)
             (the fixnum reg-num))
      (signal-ppcre-syntax-error
       "Backreference to register ~A which has not been defined"
       max-back-ref))
    (when (typep starts-with 'str)
      (setf (slot-value starts-with 'str)
              (coerce (slot-value starts-with 'str) 'simple-string)))
    (values converted-parse-tree reg-num starts-with)))
