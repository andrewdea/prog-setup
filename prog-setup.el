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
  "Set preferred appearance for prog-mode"
  (display-line-numbers-mode t)
  (if absolute
      (absolute-line-numbers-setup)
    (relative-line-numbers-setup))
  (electric-pair-local-mode t))

(defun adj/:around-goto-line-read-args (origfn)
  (let ((display-line-numbers 'absolute))
    (funcall origfn)))

(defun relative-line-numbers-setup ()
  (interactive)
  (display-line-numbers-mode t)
  (setq display-line-numbers 'relative)
  (defun adj/:around-goto-line-read-args (origfn)
    (let ((display-line-numbers 'absolute))
      (funcall origfn)))
  (advice-add 'goto-line-read-args :around #'adj/:around-goto-line-read-args))

(defun absolute-line-numbers-setup ()
  (interactive)
  (setq display-line-numbers 'absolute)
  (display-line-numbers-mode)
  (advice-remove 'goto-line-read-args #'adj/:around-goto-line-read-args))

(defun line-numbers-global (setup)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when display-line-numbers-mode
        (funcall setup)))))

(defun absolute-line-numbers-global ()
  (interactive)
  (line-numbers-global #'absolute-line-numbers-setup)
  (remove-hook 'prog-mode-hook #'prog-setup)
  (add-hook 'prog-mode-hook (lambda () (prog-setup 'absolute))))

(defun relative-line-numbers-global ()
  (interactive)
  (line-numbers-global #'relative-line-numbers-setup)
  (remove-hook 'prog-mode-hook (lambda () (prog-setup 'absolute)))
  (add-hook 'prog-mode-hook #'prog-setup))

;;;; commands
;;;;; debug-statements
;;;###autoload
(defun debug-print (arg &optional lang-format)
  (interactive "P")
  (let* ((lang-format (or lang-format local-lang-format
                          ;; TODO make this an actual error that can be caught
                          (error "no lang format provided")))
         (thing (unless (current-line-empty-p)
                  (progn
                    (make-it-quiet (dwim-kill))
                    (pop kill-ring))))
         (line-number (when (or arg (not thing))
                        (line-number-at-pos)))
         ;; TODO add support to get the name of the current
         ;; function/class/block
         )
    (insert (funcall lang-format thing line-number))))

(provide 'prog-setup)

;;; prog-setup.el ends here
