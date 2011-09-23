;;; shen-mode.el --- A major mode for editing shen source code

;; Copyright (C) 2011 Eric Schulte

;; Author: Eric Schulte <schulte dot eric at gmail dot com>

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(defcustom shen-mode-hook nil
  "Normal hook run when entering `shen-mode'."
  :type 'hook
  :group 'shen)

(defvar shen-mode-map
  ((lambda (map) (set-keymap-parent map lisp-mode-shared-map) map)
   (make-sparse-keymap))
  "Currently just inherits from `lisp-mode-shared-map'.")

(defconst shen-font-lock-keywords
  (eval-when-compile
    `( ;; definitions
      (,(concat "(\\(" (regexp-opt '("defun" "defmacro" "lambda" "/."))
                "\\)\\>"
                "[ \t]*(?"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; control structures
      (,(concat
         "("
         (regexp-opt
          (append
           '("let" "=" "eval-without-reader-macros" "freeze" "type") ; generic
           '("if" "and" "or" "cond")) t) ; boolean
         "\\>") . 1)
      ;; built-in
      (,(concat
         "("
         (regexp-opt
          (append
           '("intern" "function")                ; symbols
           '("pos" "tlstr" "cn" "str" "string?") ; strings
           '("set" "value")                      ; assignment
           '("simple-error" "trap-error" "error-to-string")    ; error
           '("cons" "hd" "tl" "cons?")                         ; lists
           '("absvector" "address->" "<-address" "absvector?") ; vector
           '("pr" "read-byte" "open" "close")                 ; stream
           '("get-time")                                      ; time
           '("+" "-" "*" "/" ">" "<" ">=" "<=" "number?")) t) ; arithmetic
         "\\>")
       1 font-lock-builtin-face)))
  "Default expressions to highlight in Shen mode.")

(define-derived-mode shen-mode prog-mode "shen"
  "Major mode for editing Shen code."
  ;; set a variety of local variables
  ((lambda (local-vars)
     (dolist (pair local-vars)
       (set (make-local-variable (car pair)) (cdr pair))))
   '((adaptive-fill-mode . nil)
     (indent-line-function . lisp-indent-line)
     (parse-sexp-ignore-comments . t)
     (outline-regexp . ";;; \\|(....")
     (comment-start . ";")
     (comment-add . 1)
     (font-lock-comment-start-skip . ";+ *")
     (parse-sexp-ignore-comments . t)
     (lisp-indent-function . lisp-indent-function)
     (font-lock-defaults
      . (shen-font-lock-keywords
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . lisp-font-lock-syntactic-face-function))))))

(provide 'shen-mode)
;;; shen-mode.el ends here
