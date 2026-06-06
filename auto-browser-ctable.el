;;; auto-browser-ctable.el ---                       -*- lexical-binding: t; -*-

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

(require 'ctable)

(defvar-local auto-browser-ctable--last-sort-idx 0
  "Buffer-local variable storing the index of the last sorted column.")

(cl-defun auto-browser-ctable-sort (&key sort-idx)
  "Sort the current ctable model by the specified column index and record that index as the last sort."
  (setq-local auto-browser-ctable--last-sort-idx sort-idx)
  (ctbl:cmodel-sort-action (ctbl:cp-get-component) auto-browser-ctable--last-sort-idx))

(cl-defun auto-browser-ctable-render (&key buffer-name headers table actions sort-idx cell)
  "Render a clickable ctable in BUFFER-NAME from TABLE using HEADERS.

HEADERS is a list of (KEY . WIDTH-RATIO) pairs used to extract values from each row hash table and to determine relative column widths.

TABLE is a list of hash tables, each representing one row.

ACTIONS, when non-nil, is called when a row is clicked.

Optional SORT-IDX selects the initial sort column, and CELL sets the initial selected cell."
  (with-current-buffer (get-buffer-create buffer-name)
    (let* ((column-model
            (mapcar
             (lambda (header)
               (make-ctbl:cmodel :title (format "%s" (car header))
                                 :align 'left
                                 :sorter 'ctbl:sort-string-lessp
                                 :max-width (* (window-width) (cdr header))))
             headers))
           (data (mapcar
                  (lambda (row)
                    (append (mapcar (lambda (header) (gethash (car header) row)) headers)
                            (list row)))
                  table))
           (model (make-ctbl:model :column-model column-model :data data))
           (component)
           (inhibit-read-only t))
      (erase-buffer)
      (switch-to-buffer (current-buffer))
      (setq component (ctbl:create-table-component-region :model model))
      (ctbl:cmodel-sort-action component (or sort-idx auto-browser-ctable--last-sort-idx))
      (when actions
        (ctbl:cp-add-click-hook component (lambda () (funcall actions))))
      (goto-char 0)
      (ctbl:navi-goto-cell (or cell '(0 . 0))))))

(provide 'auto-browser-ctable)
;;; auto-browser-ctable.el ends here
