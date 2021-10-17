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

;;; Font Lock ===================================================

(defconst xwiki-regex-bold
  (rx (group "**")
      (group (minimal-match (zero-or-more anything)))
      (group "**")))

(defconst xwiki-regex-underline
  (rx (group "__")
      (group (minimal-match (zero-or-more anything)))
      (group "__")))

;; (string-match xwiki-underscore-regex "something __test__")
;; (match-data)
;; (match-string 10 "something __test__")

(defvar xwiki-mode-font-lock-keywords
  `((,xwiki-regex-bold . ((1 'xwiki-markup-face prepend)
                          (2 'xwiki-bold-face append)
                          (3 'xwiki-markup-face prepend)))
    (,xwiki-regex-underline . ((1 'xwiki-markup-face prepend)
                               (2 'xwiki-underline-face append)
                               (3 'xwiki-markup-face prepend)))))

;;;###autoload
(define-derived-mode xwiki-mode text-mode "XWiki"
  "Major mode for editing XWiki files"

  (setq font-lock-defaults '(xwiki-mode-font-lock-keywords)))

(provide 'xwiki-mode)
;;; xwiki-mode.el ends here
