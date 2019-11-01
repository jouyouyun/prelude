;;; prelude-gtags.el --- Emacs Prelude: gtags configuration.
;;
;; Copyright © 2018-2018 jouyouyun
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for gtags mode.

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

(prelude-require-packages '(ggtags helm-gtags))

;; etags
; <- your ctags path here
;; (setq tags-file-name "~/.emacs.d/TAGS")
;; (visit-tags-table "~/.emacs.d/TAGS")
;; 
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; ggtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-t"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c t a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(defun gtags-ext-produce-tags-if-needed (dir)
   (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((default-directory dir))
        (shell-command "gtags")
        (message "tagfile created by GNU Global"))
    ;;  tagfile already exists; update it
    (shell-command "global -u")
    (message "tagfile updated by GNU Global")))

;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
(defun gtags-ext-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (gtags-ext-produce-tags-if-needed (read-directory-name
                            "gtags: top of source tree:" default-directory)))

(defun gtags-ext-add-gtagslibpath (libdir &optional del)
  "add external library directory to environment variable GTAGSLIBPATH.\ngtags will can that directory if needed.\nC-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
  (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
      ;; create tags
      (let ((default-directory libdir))
        (shell-command "gtags")
        (message "tagfile created by GNU Global")))

  (setq libdir (directory-file-name libdir)) ;remove final slash
  (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
  (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
  (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":"))
  ))

(defun gtags-ext-print-gtagslibpath ()
  "print the GTAGSLIBPATH (for debug purpose)"
  (interactive)
  (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH")))

(provide 'prelude-gtags)

;;; prelude-gtags.el ends here
