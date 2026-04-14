;;; ab-translate.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

(require 'auto-browser)

;;; Code:

(defcustom ab-translate-url "https://fanyi.eudic.net/#/home"
  ""
  :type 'string)

(defcustom ab-translate-input-selector "textarea"
  "The selector of the input element."
  :type 'string)

(defcustom ab-translate-answer-selector ".dest-container .ng-star-inserted .content"
  "The selector of the answer element."
  :type 'string)

;;; Internal Variables:
(defvar ab-translate-default-callback nil)

(defvar ab-translate-input-str nil)

(defun ab-translate-input (&optional str callback)
  ""
  (interactive)
  (unless str
    (setq str (read-string "Input your translate str: ")))
  (setq ab-translate-input-str str)
  (if callback
      (setq ab-translate-default-callback callback)
    (setq ab-translate-default-callback #'ab-translate-posframe-show))
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,ab-translate-url t)
     (auto-browser-locate-element ab-translate-input-selector)
     (auto-browser-input ,str t)
     (auto-browser-wait-for-element ab-translate-answer-selector #'ab-translate-get-result))))

(defun ab-translate-get-result (trace-id)
  (auto-browser-run-linearly
   `((auto-browser-locate-element ab-translate-answer-selector)
     (auto-browser-get-element "html")
     (ab-translate-render))
   trace-id))

(defun ab-translate-render (traceId html)
  (funcall ab-translate-default-callback html))

(defun ab-translate-posframe-show (html)
  (posframe-show "*ab-translate*"
                 :string (concat ab-translate-input-str
                                 "\n\n"
                                 (dom-texts (auto-browser-dom-parse-html html)))
                 :timeout 5
                 :border-width 2
                 :border-color "black"))

(defun ab-translate-copy-answer (html)
  (kill-new (dom-texts (auto-browser-dom-parse-html html))))

(defun ab-translate-insert-answer (html)
  (insert (dom-texts (auto-browser-dom-parse-html html))))

(defun ab-translate-at-point ()
  (interactive)
  (let ((str))
    (if (region-active-p)
        (setq str (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq str (thing-at-point 'symbol)))
    (ab-translate-input  str)))

(provide 'ab-translate)
;;; ab-translate.el ends here
