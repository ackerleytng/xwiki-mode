;;; xwiki-mode.el --- Major mode for xwiki-formatted text -*- lexical-binding: t; -*-

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

(require 'font-lock)

(defgroup xwiki-faces nil
  "Faces used in xwiki-mode."
  :group 'xwiki
  :group 'faces)

(defface xwiki-markup-face
  '((t (:inherit shadow :slant normal :weight normal)))
  "Face for markup elements."
  :group 'xwiki-faces)

(defface xwiki-bold-face
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'xwiki-faces)

(defface xwiki-underline-face
  '((t (:inherit underline)))
  "Face for underlined text."
  :group 'xwiki-faces)

(defface xwiki-strike-through-face
  '((t (:strike-through t)))
  "Face for strike-through text."
  :group 'xwiki-faces)

(defface xwiki-italic-face
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'xwiki-faces)

(defface xwiki-code-face
  '((t (:inherit fixed-pitch)))
  "Face for code or monospace text."
  :group 'xwiki-faces)

(defface xwiki-inline-code-face
  '((t (:inherit (markdown-code-face font-lock-constant-face))))
  "Face for inline code."
  :group 'xwiki-faces)

(defface xwiki-superscript-face
  '((t (:height 0.8)))
  "Face for superscript code."
  :group 'xwiki-faces)

(defface xwiki-subscript-face
  '((t (:height 0.8)))
  "Face for subscript code."
  :group 'xwiki-faces)

(defface xwiki-list-face
  '((t (:inherit xwiki-markup-face)))
  "Face for markup elements."
  :group 'xwiki-faces)

(defface xwiki-definition-list-face
  '((t (:inherit xwiki-markup-face)))
  "Face for markup elements."
  :group 'xwiki-faces)

(defface xwiki-horizontal-line-face
  '((t (:inherit xwiki-markup-face)))
  "Face for markup elements."
  :group 'xwiki-faces)

(defface xwiki-header-face
  '((t (:inherit (variable-pitch font-lock-builtin-face bold))))
  "Base face for headers."
  :group 'xwiki-faces)

(defface xwiki-header-face-1
  '((t (:inherit xwiki-header-face
                 :height 2.0)))
  "Face for header 1."
  :group 'xwiki-faces)

(defface xwiki-header-face-2
  '((t (:inherit xwiki-header-face
                 :height 1.7)))
  "Face for header 2."
  :group 'xwiki-faces)

(defface xwiki-header-face-3
  '((t (:inherit xwiki-header-face
                 :height 1.4)))
  "Face for header 3."
  :group 'xwiki-faces)

(defface xwiki-header-face-4
  '((t (:inherit xwiki-header-face
                 :height 1.1)))
  "Face for header 4."
  :group 'xwiki-faces)

(defface xwiki-header-face-5
  '((t (:inherit xwiki-header-face
                 :height 1.0)))
  "Face for header 5."
  :group 'xwiki-faces)

(defface xwiki-header-face-6
  '((t (:inherit xwiki-header-face
                 :height 1.0)))
  "Face for header 6."
  :group 'xwiki-faces)

(defface xwiki-newline-face
  '((t (:inherit xwiki-markup-face)))
  "Face for newline."
  :group 'xwiki-faces)

(defface xwiki-link-face
  '((t (:inherit link)))
  "Face for links."
  :group 'xwiki-faces)

(defface xwiki-parameter-face
  '((t (:inherit font-lock-type-face)))
  "Face for parameters."
  :group 'xwiki-faces)

(defface xwiki-quotation-face
  '((t (:inherit xwiki-markup-face)))
  "Face for quotations."
  :group 'xwiki-faces)

;;; Font Lock ===================================================

(defconst xwiki-regex-bold
  (rx (group "**")
      (group (minimal-match (zero-or-more anything)))
      (group "**")))

(defconst xwiki-regex-underline
  (rx (group "__")
      (group (minimal-match (zero-or-more anything)))
      (group "__")))

(defconst xwiki-regex-italic
  (rx (group "//")
      (group (minimal-match (zero-or-more anything)))
      (group "//")))

(defconst xwiki-regex-strike-through
  (rx (group "--")
      (group (minimal-match (zero-or-more not-newline)))
      (group "--")))

(defconst xwiki-regex-monospace
  (rx (group "##")
      (group (minimal-match (zero-or-more anything)))
      (group "##")))

(defconst xwiki-regex-superscript
  (rx (group "^^")
      (group (minimal-match (zero-or-more anything)))
      (group "^^")))

(defconst xwiki-regex-subscript
  (rx (group ",,")
      (group (minimal-match (zero-or-more anything)))
      (group ",,")))

(defconst xwiki-regex-list
  (rx (and line-start
           (group (or (and (1+ "1") (0+ "*")  ".")
                      (1+ "*")))
           space)))

(defconst xwiki-regex-definition-list
  (rx (and line-start
           (group (0+ ":")  (or ";" ":"))
           space)))

(defconst xwiki-regex-horizontal-line
  (rx (and line-start
           (group (repeat 4 ?-))
           (0+ space)
           line-end)))

(defconst xwiki-regex-header-1
  (rx (and line-start
           (group (repeat 1 ?=)
                  (not ?=)
                  (zero-or-more not-newline)
                  (repeat 1 ?=))
           (zero-or-more space)
           line-end)))

(defconst xwiki-regex-header-2
  (rx (and line-start
           (group (repeat 2 ?=)
                  (not ?=)
                  (zero-or-more not-newline)
                  (repeat 2 ?=))
           (zero-or-more space)
           line-end)))

(defconst xwiki-regex-header-3
  (rx (and line-start
           (group (repeat 3 ?=)
                  (not ?=)
                  (zero-or-more not-newline)
                  (repeat 3 ?=))
           (zero-or-more space)
           line-end)))

(defconst xwiki-regex-header-4
  (rx (and line-start
           (group (repeat 4 ?=)
                  (not ?=)
                  (zero-or-more not-newline)
                  (repeat 4 ?=))
           (zero-or-more space)
           line-end)))

(defconst xwiki-regex-header-5
  (rx (and line-start
           (group (repeat 5 ?=)
                  (not ?=)
                  (zero-or-more not-newline)
                  (repeat 5 ?=))
           (zero-or-more space)
           line-end)))

(defconst xwiki-regex-header-6
  (rx (and line-start
           (group (repeat 6 ?=)
                  (not ?=)
                  (zero-or-more not-newline)
                  (repeat 6 ?=))
           (zero-or-more space)
           line-end)))

(defconst xwiki-regex-newline
  (rx (group "\\")))

(defconst xwiki-regex-anchor-link
  (rx (and (group "[[")
           (minimal-match (zero-or-more not-newline))
           (group "]]"))))

(defconst xwiki-regex-parameter
  (rx (and (group "(%")
           (group (minimal-match (zero-or-more not-newline)))
           (group "%)"))))

(defconst xwiki-regex-quotation
  (rx (and line-start
           (group (0+ ">"))
           space)))

(defconst xwiki-regex-group-start
  (rx "((("))

(defconst xwiki-regex-group-end
  (rx ")))"))

(defvar xwiki-mode-font-lock-keywords
  `((,xwiki-regex-header-1 . ((1 'xwiki-header-face-1)))
    (,xwiki-regex-header-2 . ((1 'xwiki-header-face-2)))
    (,xwiki-regex-header-3 . ((1 'xwiki-header-face-3)))
    (,xwiki-regex-header-4 . ((1 'xwiki-header-face-4)))
    (,xwiki-regex-header-5 . ((1 'xwiki-header-face-5)))
    (,xwiki-regex-header-6 . ((1 'xwiki-header-face-6)))
    (,xwiki-regex-horizontal-line . ((1 'xwiki-horizontal-line-face)))
    (,xwiki-regex-bold . ((1 'xwiki-markup-face)
                          (2 'xwiki-bold-face)
                          (3 'xwiki-markup-face)))
    (,xwiki-regex-italic . ((1 'xwiki-markup-face)
                            (2 'xwiki-italic-face)
                            (3 'xwiki-markup-face)))
    (,xwiki-regex-strike-through . ((1 'xwiki-markup-face)
                                    (2 'xwiki-strike-through-face)
                                    (3 'xwiki-markup-face)))
    (,xwiki-regex-monospace . ((1 'xwiki-markup-face)
                               (2 'xwiki-inline-code-face)
                               (3 'xwiki-markup-face)))
    (,xwiki-regex-underline . ((1 'xwiki-markup-face)
                               (2 'xwiki-underline-face)
                               (3 'xwiki-markup-face)))
    (,xwiki-regex-subscript . ((1 'xwiki-markup-face)
                               (2 'xwiki-subscript-face)
                               (3 'xwiki-markup-face)))
    (,xwiki-regex-superscript . ((1 'xwiki-markup-face)
                                 (2 '(face xwiki-superscript-face display (raise 0.3)))
                                 (3 'xwiki-markup-face)))
    (,xwiki-regex-list . ((1 'xwiki-list-face)))
    (,xwiki-regex-definition-list . ((1 'xwiki-definition-list-face)))
    (,xwiki-regex-newline . ((1 'xwiki-newline-face)))
    (,xwiki-regex-anchor-link
     (1 'xwiki-markup-face)
     (2 'xwiki-markup-face)
     (,(rx (and "[["
                (group (minimal-match (one-or-more not-newline)))
                (group (or ">>" "||" "]]"))))
      (re-search-backward (rx "[[")) nil
      (1 'xwiki-link-face) (2 'xwiki-markup-face)))
    (,xwiki-regex-parameter . ((0 'xwiki-parameter-face)))
    (,xwiki-regex-quotation . ((1 'xwiki-quotation-face)))
    (,xwiki-regex-group-start . ((0 'xwiki-markup-face)))
    (,xwiki-regex-group-end . ((0 'xwiki-markup-face)))))

(eval-when-compile
  (defconst xwiki-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     ("\\({\\){{" (1 "< b"))
     ("}}\\(}\\)" (1 "> b")))))

;;;###autoload
(define-derived-mode xwiki-mode text-mode "XWiki"
  "Major mode for editing XWiki files"

  (setq-local comment-start "{{{")
  (setq-local comment-end "}}}")

  (setq-local syntax-propertize-function (syntax-propertize-rules xwiki-syntax-propertize-rules))

  (setq-local font-lock-defaults '(xwiki-mode-font-lock-keywords)))

;; AWESOME blogposts/references on writing a major mode
;; https://fuco1.github.io/2017-06-01-The-absolute-awesomeness-of-anchored-font-lock-matchers.html
;; https://futureboy.us/frinktools/emacs/frink-mode.el

(provide 'xwiki-mode)
;;; xwiki-mode.el ends here
