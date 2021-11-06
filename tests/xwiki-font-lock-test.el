;;; xwiki-font-lock-test.el --- Tests for font-lock-keywords in xwiki -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Ackerley Tng

;; Author: Ackerley Tng <ackerleytng@gmail.com>
;; Maintainer: Ackerley Tng <ackerleytng@gmail.com>
;; Created: Oct 15, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: XWiki
;; URL: https://github.com/ackerleytng/xwiki-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.md for details

;;; Code:

(require 'ert)
(require 'cl-extra)
(require 'xwiki-mode)

(defmacro xwiki-test-string-mode (mode string &rest body)
  "Run BODY in a temporary buffer containing STRING in MODE."
  (declare (indent 2))
  `(let ((win (selected-window)))
     (unwind-protect
         (with-temp-buffer
           (set-window-buffer win (current-buffer) t)
           (erase-buffer)
           (insert ,string)
           (funcall ,mode)
           (setq-default indent-tabs-mode nil)
           (goto-char (point-min))
           (font-lock-ensure)
           (prog1 ,@body (kill-buffer))))))

(defmacro xwiki-test-string (string &rest body)
  "Run BODY in a temporary buffer containing STRING in `xwiki-mode'."
  (declare (indent 1))
  `(xwiki-test-string-mode 'xwiki-mode ,string ,@body))

(defun xwiki-test-report-property-range (begin end prop)
  "Report buffer substring and property PROP from BEGIN to END."
  (message "Buffer substring: %s" (buffer-substring begin (1+ end)))
  (message "Properties in range are as follows:")
  (dolist (loc (number-sequence begin end))
    (message "%d: (%s) %s" loc (string (char-after loc)) (get-char-property loc prop))))

(defun xwiki-test-loc-has-property (loc prop value)
  "Return whether that character at loc (1-based) as prop equal to or containing VALUE."
  (let* ((property (get-char-property loc prop))
         (succeed (if (and property (listp property))
                      (memq value property)
                    (eq property value))))
    (if-let ((c (char-after loc)))
        (if succeed
            (message "loc: %d (%s) => correct" loc (string c))
          (message "loc: %d (%s) => expected %s: %s, actual: %s"
                   loc (string c) prop value property))
      (message "loc: %d => nothing here" loc))
    succeed))

(defun xwiki-test-loc-has-face (loc value)
  "Return whether character at loc (1-based) as prop equal to or containing VALUE."
  (xwiki-test-loc-has-property loc 'face value))

(defun xwiki-test-range-has-property (begin end prop value)
  "Verify that range BEGIN to END has PROP equal to or containing VALUE."
  (let (vals fail-loc)
    (setq fail-loc
          (catch 'fail
            (dolist (loc (number-sequence begin end))
              (setq vals (get-char-property loc prop))
              (if (and vals (listp vals))
                  (unless (memq value vals)
                    (throw 'fail loc))
                (unless (eq vals value)
                  (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (xwiki-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun xwiki-test-range-has-face (begin end face)
  "Verify that the range from BEGIN to END has face FACE."
  (xwiki-test-range-has-property begin end 'face face))

;;; Helpers ============================================================

(defun alistp (list)
  "Return T if LIST is an association list."
  (and (listp list)
       (cl-every (lambda (x) (consp x)) list)))

(defun xwiki--test-with-marked-string (marked-string markings)
  (unless (alistp markings)
    (error "markings has to be a proper alist"))
  (let (out)
    (dolist (i (number-sequence 0 (1- (length marked-string))))
      (let ((char (aref marked-string i)))
        (push (if-let ((face (alist-get char markings)))
                  (xwiki-test-loc-has-face (1+ i) face)
                (xwiki-test-loc-has-face (1+ i) nil))
              out)))
    (should (cl-every 'identity out))))

;;; Tests ============================================================

(ert-deftest test-xwiki-view-mode/xwiki-underline-face ()
  "Basic test for `xwiki-underline-face' of `xwiki-view-mode'."
  (let ((  test-string "regular __underline__ regular")
        (marked-string "regular ##@@@@@@@@@## regular")
        (markings '((?# . xwiki-markup-face)
                    (?@ . xwiki-underline-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

;; TODO don't underline the \n character
(ert-deftest test-xwiki-view-mode/xwiki-underline-face-multiline ()
  "Test `xwiki-underline-face' of `xwiki-view-mode'."
  (let ((test-string "__underline

second line__")
        ;; newlines must also be marked
        (marked-string "%%@@@@@@@@@@@@@@@@@@@@@@%%")
        (markings '((?% . xwiki-markup-face)
                    (?@ . xwiki-underline-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-bold-face ()
  "Basic test for `xwiki-bold-face' of `xwiki-view-mode'."
  (let ((  test-string "regular **bold** regular")
        (marked-string "regular %%@@@@%% regular")
        (markings '((?% . xwiki-markup-face)
                    (?@ . xwiki-bold-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-italic-face ()
  "Basic test for `xwiki-italic-face' of `xwiki-view-mode'."
  (let ((  test-string "regular //italic// regular")
        (marked-string "regular %%@@@@@@%% regular")
        (markings '((?% . xwiki-markup-face)
                    (?@ . xwiki-italic-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-strike-through-face ()
  "Basic test for `xwiki-strike-through-face' of `xwiki-view-mode'."
  (let ((  test-string "
regular --strike-through-- regular
--strike-through----
")
        (marked-string "
regular %%@@@@@@@@@@@@@@%% regular
%%@@@@@@@@@@@@@@%%--
")
        (markings '((?% . xwiki-markup-face)
                    (?@ . xwiki-strike-through-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-strike-through-face-not ()
  "Basic non-strikethrough test for `xwiki-strike-through-face' of `xwiki-view-mode'."
  (let ((test-string "-- newlines ignored
--"))
    (xwiki-test-string
        test-string
      (xwiki-test-range-has-face 1 (length test-string) nil))))

(ert-deftest test-xwiki-view-mode/xwiki-monospace-face ()
  "Basic test for `xwiki-monospace-face' of `xwiki-view-mode'."
  (let ((  test-string "regular ##monospace## regular")
        (marked-string "regular %%@@@@@@@@@%% regular")
        (markings '((?% . xwiki-markup-face)
                    (?@ . xwiki-inline-code-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-subscript-face ()
  "Basic test for `xwiki-subscript-face' of `xwiki-view-mode'."
  (let ((  test-string "regular ,,subscript,, regular")
        (marked-string "regular ##@@@@@@@@@## regular")
        (markings '((?# . xwiki-markup-face)
                    (?@ . xwiki-subscript-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-superscript-face ()
  "Basic test for `xwiki-superscript-face' of `xwiki-view-mode'."
  (let ((  test-string "regular ^^superscript^^ regular")
        (marked-string "regular ##@@@@@@@@@@@## regular")
        (markings '((?# . xwiki-markup-face)
                    (?@ . xwiki-superscript-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-list-face-numbered ()
  "Test for bulleted lists for `xwiki-list-face' of `xwiki-view-mode'."
  (let ((test-string "
1. foo
11. bar
1. baz")
        (marked-string "
@@ foo
@@@ bar
@@ baz")
        (markings '((?@ . xwiki-list-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-list-face-bulleted ()
  "Test for bulleted lists for `xwiki-list-face' of `xwiki-view-mode'."
  (let ((test-string "
* foo
** bar
* baz")
        (marked-string "
@ foo
@@ bar
@ baz")
        (markings '((?@ . xwiki-list-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-list-face-mixed ()
  "Test for mixed numbered/bulleted lists for `xwiki-list-face' of `xwiki-view-mode'."
  (let ((test-string "
1. foo
1*. bar
1. baz")
        (marked-string "
@@ foo
@@@ bar
@@ baz")
        (markings '((?@ . xwiki-list-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-list-face-not ()
  "Test for list-like but not lists: `xwiki-list-face' of `xwiki-view-mode'."
  (let ((test-string "
 1. foo
 * bar
*. baz
*quux"))
    (xwiki-test-string
        test-string
      (xwiki-test-range-has-face 1 (length test-string) nil))))

(ert-deftest test-xwiki-view-mode/xwiki-definition-list-face ()
  "Test for `xwiki-definition-list-face' of `xwiki-view-mode'."
  (let* ((test-string "
; term
: definition

:; nested term
:: nested definition

:not
::not
")
         (marked-string "
@ term
@ definition

@@ nested term
@@ nested definition

:not
::not
")
         (markings '((?@ . xwiki-definition-list-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-horizontal-line-face ()
  "Test for `xwiki-horizontal-line-face' of `xwiki-view-mode'."
  (let* ((test-string "
---- this is fontified as strike-through but without any contents
  ---- same here

----
")
         (marked-string "
#### this is fontified as strike-through but without any contents
  #### same here

@@@@
")
         (markings '((?@ . xwiki-horizontal-line-face)
                     (?# . xwiki-markup-face))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-1 ()
  "Test for `xwiki-header-face-1' of `xwiki-view-mode'."
  (let* ((test-string "
= header =

= hea=der =

other text
")
         (marked-string "
@@@@@@@@@@

@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-1))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-2 ()
  "Test for `xwiki-header-face-2' of `xwiki-view-mode'."
  (let* ((test-string "
== header ==

== hea=der ==

other text
")
         (marked-string "
@@@@@@@@@@@@

@@@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-2))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-3 ()
  "Test for `xwiki-header-face-3' of `xwiki-view-mode'."
  (let* ((test-string "
=== header ===

=== hea=der ===

other text
")
         (marked-string "
@@@@@@@@@@@@@@

@@@@@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-3))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-4 ()
  "Test for `xwiki-header-face-4' of `xwiki-view-mode'."
  (let* ((test-string "
==== header ====

==== hea=der ====

other text
")
         (marked-string "
@@@@@@@@@@@@@@@@

@@@@@@@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-4))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-5 ()
  "Test for `xwiki-header-face-5' of `xwiki-view-mode'."
  (let* ((test-string "
===== header =====

===== hea=der =====

other text
")
         (marked-string "
@@@@@@@@@@@@@@@@@@

@@@@@@@@@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-5))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-6 ()
  "Test for `xwiki-header-face-6' of `xwiki-view-mode'."
  (let* ((test-string "
====== header ======

====== hea=der ======

other text
")
         (marked-string "
@@@@@@@@@@@@@@@@@@@@

@@@@@@@@@@@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-6))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-priority ()
  "Test for header priority of `xwiki-view-mode'."
  (let* ((test-string "
====== header //italic// ======

other text
")
         (marked-string "
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

other text
")
         (markings '((?@ . xwiki-header-face-6))))
    (xwiki-test-string test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-header-face-not ()
  "Test for non-headers of `xwiki-view-mode'."
  (let* ((test-string "
======= no header 7 =======

other text
"))
    (xwiki-test-string
        test-string
      (xwiki-test-range-has-face 1 (length test-string) nil))))

(ert-deftest test-xwiki-view-mode/xwiki-newline-face ()
  "Test for `xwiki-newline-face` of `xwiki-view-mode'."
  (let* ((test-string "
foo\\\\bar
")
         (marked-string "
foo@@bar
")
         (markings '((?@ . xwiki-newline-face))))
    (xwiki-test-string
        test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-link-face ()
  "Test for `xwiki-link-face` of `xwiki-view-mode'."
  (let* ((test-string "
[[label>>destination]]

[[label]]

[[PageA.PageB||anchor=\"anchor\"]]

[[label>>||anchor=\"anchor\"]]

[[https://www.google.com]]
")
         (marked-string "
##@@@@@##destination##

##@@@@@##

##@@@@@@@@@@@##anchor=\"anchor\"##

##@@@@@##||anchor=\"anchor\"##

##@@@@@@@@@@@@@@@@@@@@@@##
")
         (markings '((?# . xwiki-markup-face)
                     (?@ . xwiki-link-face))))
    (xwiki-test-string
        test-string
      (xwiki--test-with-marked-string marked-string markings))))

(ert-deftest test-xwiki-view-mode/xwiki-parameter-face ()
  "Test for `xwiki-parameter-face` of `xwiki-view-mode'."
  (let* ((test-string "
(% class=\"myclass\" %)
text

(% color=\"red\" %)text(%%)
")
         (marked-string "
@@@@@@@@@@@@@@@@@@@@@
text

@@@@@@@@@@@@@@@@@text@@@@
")
         (markings '((?@ . xwiki-parameter-face))))
    (xwiki-test-string
        test-string
      (xwiki--test-with-marked-string marked-string markings))))

(provide 'xwiki-font-lock-test)
;;; xwiki-font-lock-test.el ends here
