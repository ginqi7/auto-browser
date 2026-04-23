;;; ab-say.el ---                                    -*- lexical-binding: t; -*-

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

;;; Code:

(defcustom ab-say-url "https://fanyi.eudic.net/#/home"
  ""
  :type 'string)

(defcustom ab-say-input-selector "textarea"
  "The selector of the input element."
  :type 'string)

(defcustom ab-say-action-selector ".action-container .leftBox .left-action .ant-btn"
  "The selector of the action element."
  :type 'string)

;;; Internal Variables:

(defun ab-say-input (&optional str)
  ""
  (interactive)
  (unless str
    (setq str (read-string "Input your say str: ")))
  (setq ab-say-input-str str)
  (auto-browser-run-linearly
   `((auto-browser-get-tab ,ab-say-url t)
     (auto-browser-locate-element ab-say-input-selector)
     (auto-browser-input ,str t)
     (auto-browser-locate-element ab-say-action-selector)
     (auto-browser-click))))

(provide 'ab-say)
;;; ab-say.el ends here
