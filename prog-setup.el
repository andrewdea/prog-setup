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
;;;; declare a bunch of things we're using
(defvar display-line-numbers-mode)
(declare-function current-line-empty-p user-init-file)
(declare-function make-it-quiet user-init-file)
(declare-function dwim-kill user-init-file)
(declare-function window-vertically-split-p user-init-file)
(declare-function named-shell-file user-init-file)

;;;; appearance
;;;###autoload
(defun prog-setup-appearance (&optional absolute)
  "Set preferred appearance for `prog-mode'.
If ABSOLUTE is non-nil, call `absolute-line-numbers-setup', otherwise,
call `relative-line-numbers-setup'"
  (display-line-numbers-mode t)
  (auto-fill-mode t)
  (setq-local parse-sexp-ignore-comments nil)
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
(defun treesit-defun-name-at-point ()
  "Use `treesit' to get the name of the current function-block.
treesitter nodes might not always match with the intuitive
code-boundaries:
if we're at the very end of a function block, treesit thinks a new
node is starting so it returns nil. So in that case, try the previous
line as well"
  (treesit-defun-name
   (or (treesit-defun-at-point)
       (save-excursion
         (previous-line)
         (treesit-defun-at-point)))))

(defvar  prog-defun-name-at-point-function #'treesit-defun-name-at-point
  "Function to detect the name of the definition-block at point.
By default, use `treesit', but you can implement your own.
NOTE that the default will throw a no-parser error if called from a
mode without treesitter.")

(defun prog-defun-name-at-point ()
  "Use the customizable `prog-defun-name-at-point-function' to get the
name of the definition-block at point"
  (interactive)
  (let ((outer-res (funcall prog-defun-name-at-point-function)))
    outer-res))

(defun prog-debug--get-info-at-point (&optional verbose)
  "Get information at the current point to print-debug.
When VERBOSE is non-nil, or if the current line is
empty, also print:
 - line-number at point
 - name of the function-block at point (use treesit when available, or
a language-specific function)"
  (let* ((exp (unless (current-line-empty-p)
                (make-it-quiet (dwim-kill))
                (pop kill-ring)))
         (line-number (when (or verbose (not exp))
                        (line-number-at-pos)))
	 ;; TODO instead of the lsp, use `treesit-add-log-current-defun'
         (block-name (when (or verbose (not exp))
                       (prog-defun-name-at-point))))
    ;; remove text properties
    (when block-name
      (set-text-properties 0 (length block-name) nil block-name))

    (list exp line-number block-name)))

;;;###autoload
(defun prog-debug-print (verbose lang-format)
  "Insert a debug-print statement around the current region/line.
Use LANG-FORMAT to determine how to format the print-statement.
When VERBOSE is non-nil, add more information to the statement."
  (cl-multiple-value-bind
      (exp line-number block-name)
      (prog-debug--get-info-at-point verbose)
    (insert (funcall lang-format exp line-number block-name))))

;;;;; run-this

(defun prog--compile (func)
  "Call FUNC to compile the current file/project.
Compose a reasonable window setup with the new *compilation* buffer."
  ;; compile
  (funcall func)
  ;; compose a reasonable window setup
  (pop-to-buffer "*compilation*")
  (when (not (window-vertically-split-p))
    (split-window-below)))

;;;###autoload
(defun prog-run-this (file compile-func run-command &rest setup-funcs)
  "Generic run-this command.
Start a shell for FILE, setup the shell using SETUP-FUNCS, call
the function COMPILE-FUNC (if non-nil), compose a reasonable window
setup, and insert the RUN-COMMAND into the shell."
  (when compile-func
    (prog--compile compile-func 'to-run))
  ;; TODO: explore using `async-shell-command' here instead
  (named-shell-file file setup-funcs)
  (insert run-command))

;;;;; language-specific
(defun emacs-lisp-defun-name-at-point ()
  (save-excursion
    (let ((og-point (point)))
      (search-backward-regexp " *(defun *\\([^ ]+\\) *(" nil 'noerr)
      (when (<= og-point (progn (forward-sexp) (point)))
        (match-string 1)))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local
             prog-defun-name-at-point-function
             #'emacs-lisp-defun-name-at-point)))

(provide 'prog-setup)

;;; prog-setup.el ends here
