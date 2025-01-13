;;; auto-browser-dom-test.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

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

(require 'ert)
(require 'auto-browser-dom)

(ert-deftest test-auto-browser-dom--parse-selector-str ()
  "Test the add-numbers function."
  (should (equal (auto-browser-dom--parse-selector-str "#id")
                 '(:id "id")))
  (should (equal (auto-browser-dom--parse-selector-str ".class")
                 '(:class "class")))
  (should (equal (auto-browser-dom--parse-selector-str "#id[name=123]")
                 '(:id "id" :attr-key "name" :attr-value "123")))
  (should (equal (auto-browser-dom--parse-selector-str ".class[name=123]")
                 '(:class "class" :attr-key "name" :attr-value "123")))
  (should (equal (auto-browser-dom--parse-selector-str "div")
                 '(:tag "div")))
  (should (equal (auto-browser-dom--parse-selector-str "div[name=123]")
                 '(:tag "div" :attr-key "name" :attr-value "123")))
  (should (equal (auto-browser-dom--parse-selector-str "div a[href=123]")
                 '((:tag "div") (:tag "a" :attr-key "href" :attr-value "123")))))

(provide 'auto-browser-dom-test)
;;; auto-browser-dom-test.el ends here
