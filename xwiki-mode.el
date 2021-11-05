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

(defvar xwiki-mode-font-lock-keywords
  `((,xwiki-regex-horizontal-line . ((1 'xwiki-horizontal-line-face)))
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
    (,xwiki-regex-definition-list . ((1 'xwiki-definition-list-face)))))

;;;###autoload
(define-derived-mode xwiki-mode text-mode "XWiki"
  "Major mode for editing XWiki files"

  (setq font-lock-defaults '(xwiki-mode-font-lock-keywords t)))

(provide 'xwiki-mode)
;;; xwiki-mode.el ends here
