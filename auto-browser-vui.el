;;; auto-browser-vui.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
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

(require 'vui)

(defvar-local auto-browser-vui--selected nil)

(defvar-local auto-browser-vui--instance nil)

(defun auto-browser-vui-sorted-by (sorted-key reversed table)
  "Sort TABLE by the hash value of SORTED-KEY using string comparison."
  (sort table
        :key (lambda (row) (gethash sorted-key row))
        :lessp #'string<
        :reverse reversed))

(defun auto-browser-vui-filter-by (key value table)
  "Sort TABLE by the hash value of SORTED-KEY using string comparison."
  (seq-filter (lambda (row) (equal value (gethash key row))) table))

(defun auto-browser-vui--set-sort-header (headers sort)
  (mapcar
   (lambda (header)
     (cons (car header)
           (plist-put
            (plist-put (cdr header) :sorted (equal (car header) sort))
            :reversed (if (plist-get (cdr header) :reversed) nil t))))
   headers))

(defun auto-browser-vui--set-sort (data)
  (let* ((start (button-start data))
         (end (button-end data))
         (sort  (intern (buffer-substring-no-properties start end)))
         (props (vui-instance-props auto-browser-vui--instance))
         (headers (plist-get props :headers)))
    (vui-update-props auto-browser-vui--instance
                      (plist-put props :headers
                                 (auto-browser-vui--set-sort-header headers sort)))))

(defun auto-browser-vui--to-column (header)
  "Convert a single HEADER (a cons cell) into a vui column spec."
  (list :header (buttonize (format "%s" (car header)) #'auto-browser-vui--set-sort)
        :width 50 :truncate t
        :align :left))

(defun auto-browser-vui--to-columns (headers)
  "Convert HEADERS into a vui column spec list, prepending and appending an empty column."
  (append
   (list (list :header ""))
   (mapcar #'auto-browser-vui--to-column headers)))

(cl-defun auto-browser-vui--run-actions (&key actions row id-key table)
  "Run the ACTIONS transient prefix with the appropriate rows.

If SELECTED is non-nil, filter TABLE to rows whose id-key is in SELECTED;
otherwise pass just ROW as a single-element list."
  (funcall
   actions
   (if auto-browser-vui--selected
       (cl-remove-if-not (lambda (row) (member (gethash id-key row) auto-browser-vui--selected)) table)
     (list row)))
  (setq auto-browser-vui--selected nil))

(defun auto-browser-vui--select (data)
  (let* ((begin (button-start data))
         (end (button-end data))
         (key (buffer-substring-no-properties begin end))
         (buffer-read-only nil)
         (inhibit-read-only t))
    (if (member key auto-browser-vui--selected)
        (progn
          (message (format "Unselect %s" key))
          (setq auto-browser-vui--selected (remove key auto-browser-vui--selected)))
      (message (format "Select %s" key))
      (add-to-list 'auto-browser-vui--selected key))
    (save-excursion
      (delete-region begin end)
      (insert (auto-browser-vui--format-select key)))))

(defun auto-browser-vui--format-select (key)
  (if (member key auto-browser-vui--selected)
      (buttonize (propertize key 'display "☑") #'auto-browser-vui--select)
    (buttonize (propertize key 'display "☐") #'auto-browser-vui--select)))

(cl-defun auto-browser-vui--to-row (&key headers actions table id-key row)
  "Build a vui row from ROW using HEADERS, SELECTED, ACTIONS, TABLE, and ID-KEY.

Each row contains a checkbox, cell values for each header, and an Actions button."
  (append
   (list
    (auto-browser-vui--format-select (gethash id-key row)))
   (mapcar
    (lambda (header)
      (let* ((key (car header))
             (value (gethash key row))
             (props (cdr header))
             (on-click (plist-get props :on-click)))
        (if on-click
            (buttonize value (lambda (_) (funcall on-click (gethash id-key row) value)))
          ;; I don't know why the vui-button can't align.
          ;; (vui-button value
          ;;   :max-width 15
          ;;   :no-decoration t
          ;;   :on-click (lambda () (print on-click) (funcall on-click (gethash id-key row) value)))
          ;; The test demo is correct.
         ;;  (vui-defcomponent user-form ()
         ;;   :render
         ;;   (vui-table
         ;;    :columns '((:min-width 8 :header "test") (:min-width 10) (:min-width 8))
         ;;    :border :ascii
         ;;    :rows (list
         ;;           (list (vui-button "测试超级长度的字符串，测试超级长度的字符串，测试超级长度的字符串，测试超级长度的字符串" :max-width 15)
         ;;                 (vui-button "测试超级长度的字符串，测试超级长度的字符串，测试超级长度的字符串，测试超级长度的字符串" :max-width 15))
         ;;           (list (vui-button "测试超级长度的字符串，测试超级长度的字符串，测试超级长度的字符串，测试超级长度的字符串" :max-width 15)))))

         ;; (vui-mount (vui-component 'user-form) "*user-form*")

         value)))
    headers)))

(cl-defun auto-browser-vui--to-rows (&key headers table actions id-key)
  "Map TABLE to a list of vui rows, sorted by SORTED and filtered by SELECTED."
  (mapcar
   (apply-partially #'auto-browser-vui--to-row
                    :headers headers
                    :actions actions
                    :table table
                    :id-key id-key
                    :row)
   (let ((sort-header (cl-find-if (lambda (header) (plist-get (cdr header) :sorted)) headers)))
     (auto-browser-vui-sorted-by (car sort-header) (plist-get (cdr sort-header) :reversed) table))))

(cl-defun auto-browser-vui-render (&key name buffer-name headers table actions title id-key)
  "Render a vui table in BUFFER-NAME with the given parameters.

NAME is the component name, HEADERS defines columns, TABLE is the data,
ACTIONS is the transient prefix, TITLE is displayed as heading, and
ID-KEY is the hash key used to uniquely identify each row for selection."
  (eval
   `(vui-defcomponent ,name (headers table actions id-key)
      :render
      (vui-table
       :sticky-header t
       :columns
       (auto-browser-vui--to-columns headers)
       :rows
       (auto-browser-vui--to-rows
        :headers headers
        :table table
        :actions actions
        :id-key id-key)
       :border :unicode))
   t)
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((props (list :headers headers
                       :table table
                       :actions actions
                       :id-key id-key)))
      (if (and auto-browser-vui--instance
               (vui-instance-p auto-browser-vui--instance))
          (vui-update-props auto-browser-vui--instance props)
        (setq auto-browser-vui--instance
              (vui-mount (apply #'vui-component name props) buffer-name))))))

(provide 'auto-browser-vui)
;;; auto-browser-vui.el ends here
