;;; prelude-xterm.el --- Emacs Prelude: xterm support.
;;
;; Copyright © 2018-2018 jouyouyun
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; CoffeeScript is a nice little language that comples to JavaScript.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(prelude-require-package 'multi-term)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(autoload 'mwheel-install "mwheel")

(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))

(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)


;; for multi-term
(global-set-key (kbd "C-c M-t") 'multi-term)
(setq multi-term-program "/bin/zsh"
	  ;; TERM is restored to xterm-256-color after that.
	  term-term-name "xterm-256color"
	  ;; background: black
	  term-default-bg-color "#000000"
	  ;; foreground: yellow
	  term-default-fg-color "#dddd00")

;; update current directory
(defadvice term-send-input (after update-current-directory)
  "Update the current directory."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))
(defadvice term-send-raw (after update-current-directory)
  "Update the current directory."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))

(eval-after-load "term"
  `(progn
     (ad-activate 'term-send-raw)
     (ad-activate 'term-send-input)
     ;; no limit buffer length
     (setq show-trailing-whitespace nil)
     (setq term-bind-key-alist
           (list (cons "C-c C-c" 'term-interrupt-subjob)
                 ;; send 'ESC' to terminal
				 (cons "C-c M-e" 'term-send-esc)
                 ;; jump terminals
                 (cons "C-c M-[" 'multi-term-prev)
                 (cons "C-c M-]" 'multi-term-next)
                 (cons "C-p" 'previous-line)
                 (cons "C-n" 'next-line)
                 (cons "M-f" 'term-send-forward-word)
                 (cons "M-b" 'term-send-backward-word)
                 (cons "C-c C-j" 'term-line-mode)
                 (cons "C-c C-k" 'term-char-mode)
                 (cons "M-DEL" 'term-send-backward-kill-word)
                 (cons "M-d" 'term-send-forward-kill-word)
                 (cons "C-r" 'term-send-reverse-search-history)))
     ;; paste
     (define-key term-raw-map (kbd "C-y") 'term-paste)))


(provide 'prelude-xterm)
