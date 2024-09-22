;;; prog-setup.el --- my personal setup for prog-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andrew De Angelis

;; Author: Andrew De Angelis <andyjda@Andrews-MacBook-Air-2.local>
;; Keywords: local, languages

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

;;; Code:
;;;; appearance
;;;###autoload
(defun prog-setup-appearance (&optional absolute)
  "Set preferred appearance for `prog-mode'.
If ABSOLUTE is non-nil, call `absolute-line-numbers-setup', otherwise,
call `relative-line-numbers-setup'"
  (display-line-numbers-mode t)
  (auto-fill-mode t)
  (if absolute
      (absolute-line-numbers-setup)
    (relative-line-numbers-setup))
  (electric-pair-local-mode t))

(defun adj/:around-goto-line-read-args (origfn)
  "Temporarily set the line numbers to absolute while calling ORIGFN."
  (let ((display-line-numbers 'absolute))
    (funcall origfn)))

(defun relative-line-numbers-setup ()
  "Set `display-line-numbers' to relative.
Make sure line-numbers will be absolute when calling `goto-line'."
  (interactive)
  (display-line-numbers-mode t)
  (setq display-line-numbers 'relative)
  (advice-add 'goto-line-read-args :around #'adj/:around-goto-line-read-args))

(defun absolute-line-numbers-setup ()
  "Set `display-line-numbers' to relative."
  (interactive)
  (setq display-line-numbers 'absolute)
  (display-line-numbers-mode)
  ;; in case this advice was added by `relative-line-numbers-setup',
  ;; we can remove it:
  (advice-remove 'goto-line-read-args #'adj/:around-goto-line-read-args))

(defun line-numbers-global (setup)
  "Generic way to set the line-numbers style.
Call SETUP in every buffer where `display-line-numbers-mode' is t"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when display-line-numbers-mode
        (funcall setup)))))

(defun absolute-line-numbers-global ()
  "Set the line-numbers to absolute globally.
Make sure any new buffer in `prog-mode' will have the same setup"
  (interactive)
  (line-numbers-global #'absolute-line-numbers-setup)
  (remove-hook 'prog-mode-hook #'prog-setup-appearance)
  (add-hook 'prog-mode-hook (lambda () (prog-setup-appearance 'absolute))))

(defun relative-line-numbers-global ()
  "Set the line-numbers to relative globally.
Make sure any new buffer in `prog-mode' will have the same setup"
  (interactive)
  (line-numbers-global #'relative-line-numbers-setup)
  (remove-hook 'prog-mode-hook (lambda () (prog-setup-appearance 'absolute)))
  (add-hook 'prog-mode-hook #'prog-setup-appearance))

;;;; commands
;;;;; debug-statements
;;;###autoload
(defun debug-print (with-line-nmb lang-format)
  "Insert a debug-print statement around the current region/line.
Use LANG-FORMAT to determine how to format the print-statement.
When WITH-LINE-NMB is non-nil, or if the current line is
empty, also print the current line."
  (let* ((thing (unless (current-line-empty-p)
                  (progn
                    (make-it-quiet (dwim-kill))
                    (pop kill-ring))))
         (line-number (when (or with-line-nmb (not thing))
                        (line-number-at-pos)))
         ;; TODO add support to get the name of the current
         ;; function/class/block?
         )
    (insert (funcall lang-format thing line-number))))

(provide 'prog-setup)

;;; prog-setup.el ends here
