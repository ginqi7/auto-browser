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

(defvar-local auto-browser-vui--instance nil
  "The active vui instance for the current buffer.")

(defvar-local auto-browser-vui--selected nil
  "List of currently selected row ID keys for batch operations.")

(defvar-local auto-browser-vui-selected-actions nil
  "")

(cl-defun auto-browser-vui--to-row (&key headers table id-key row)
  "Build a vui row from ROW using HEADERS, TABLE, and ID-KEY.

Each row contains a checkbox, cell values for each header, and an
optional on-click handler per column."
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

(cl-defun auto-browser-vui--to-rows (&key headers table id-key)
  "Convert TABLE rows into a list of vui rows sorted by the active sort header.

HEADERS defines the column structure, TABLE is a list of hash tables,
and ID-KEY identifies each row uniquely for selection."
  (mapcar
   (apply-partially #'auto-browser-vui--to-row
                    :headers headers
                    :table table
                    :id-key id-key
                    :row)
   (let ((sort-header (cl-find-if (lambda (header) (plist-get (cdr header) :sorted)) headers)))
     (auto-browser-vui-sorted-by (car sort-header) (plist-get (cdr sort-header) :reversed) table))))

(cl-defun auto-browser-vui-render (&key name buffer-name headers table title id-key)
  "Render a vui table component with HEADERS and TABLE data.

NAME is the component symbol, BUFFER-NAME is the target buffer,
HEADERS is a list of (key . plist) cons cells, TABLE is a list of
hash tables, TITLE is displayed in the header, and ID-KEY is the
hash key used to uniquely identify each row."
  (eval
   `(vui-defcomponent ,name (headers table id-key)
      :render
      (vui-table
       :sticky-header t
       :columns
       (auto-browser-vui--to-columns headers)
       :rows
       (auto-browser-vui--to-rows
        :headers headers
        :table table
        :id-key id-key)
       :border :unicode))
   t)
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((props (list :headers headers
                       :table table
                       :id-key id-key))
          (point (point)))
      (if (and auto-browser-vui--instance
               (vui-instance-p auto-browser-vui--instance))
          (progn
            (vui-update auto-browser-vui--instance props)
            (goto-char point))
        (setq auto-browser-vui--instance
              (vui-mount (apply #'vui-component name props) buffer-name))))))

(defun auto-browser-vui--format-column (header)
  "Format HEADER into a clickable column label with optional sort indicator.

HEADER is a cons cell (name . props) where props may contain :sorted
and :reversed keys to control the sort arrow display."
  (let ((name (car header))
        (props (cdr header)))
    (concat (buttonize (symbol-name name) #'auto-browser-vui--set-sort)
            (if (plist-get props :sorted)
                (if (plist-get props :reversed) " ↓" " ↑")
              ""))))

(defun auto-browser-vui--format-select (key)
  "Return a checkbox button for KEY, toggled if already selected."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") (lambda () (interactive)
                                  (funcall auto-browser-vui-selected-actions key)
                                  ))
    (define-key map (kbd "SPC") (lambda () (interactive) (auto-browser-vui--select)))
    (propertize (buttonize key nil)
                'display (if (member key auto-browser-vui--selected) "☑" "☐")
                'keymap map)))

(defun auto-browser-vui--select ()
  "Toggle selection state for the checkbox at point."
  (let* ((data (button-at (point)))
         (begin (button-start data))
         (end (button-end data))
         (key (buffer-substring-no-properties begin end))
         (selected-p (member key auto-browser-vui--selected))
         (buffer-read-only nil)
         (inhibit-read-only t))
    (if selected-p
        (progn
          (message "Unselect %s" key)
          (setq auto-browser-vui--selected (remove key auto-browser-vui--selected)))
      (message "Select %s" key)
      (add-to-list 'auto-browser-vui--selected key))
    (put-text-property begin end 'display (if selected-p "☐" "☑"))))

(defun auto-browser-vui--set-sort (data)
  "Set the sort column and direction based on the header button clicked in DATA.

Reads the column name from the button at point, toggles the sort
direction if the same column is clicked again, and updates the vui
instance props."
  (let* ((start (button-start data))
         (end (button-end data))
         (sort  (intern (buffer-substring-no-properties start end)))
         (props (vui-instance-props auto-browser-vui--instance))
         (headers (plist-get props :headers)))
    (vui-update auto-browser-vui--instance
                      (plist-put props :headers
                                 (auto-browser-vui--set-sort-header headers sort)))
    (goto-char start)))

(defun auto-browser-vui--set-sort-header (headers sort)
  "Update HEADERS to mark the SORT column as sorted, toggling reversal.

Returns a new headers list with :sorted set for the matching column
and :reversed toggled between t and nil."
  (mapcar
   (lambda (header)
     (cons (car header)
           (plist-put
            (plist-put (cdr header) :sorted (equal (car header) sort))
            :reversed (if (plist-get (cdr header) :reversed) nil t))))
   headers))

(defun auto-browser-vui--to-column (header)
  "Convert a single HEADER (a cons cell) into a vui column spec."
  (list :header (auto-browser-vui--format-column header)
        :width 50 :truncate t
        :align :left))

(defun auto-browser-vui--to-columns (headers)
  "Convert HEADERS into a vui column spec list, prepending an empty checkbox column."
  (append
   (list (list :header ""))
   (mapcar #'auto-browser-vui--to-column headers)))

(defun auto-browser-vui-filter-by (key value table)
  "Filter TABLE to rows whose hash value for KEY equals VALUE."
  (seq-filter (lambda (row) (equal value (gethash key row))) table))

(defun auto-browser-vui-sorted-by (sorted-key reversed table)
  "Sort TABLE by the hash value of SORTED-KEY using string comparison."
  (sort table
        :key (lambda (row) (gethash sorted-key row))
        :lessp #'string<
        :reverse reversed))

(provide 'auto-browser-vui)

;;; auto-browser-vui.el ends here
