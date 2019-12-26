;;; prelude-c.el --- Emacs Prelude: cc-mode configuration.
;;
;; Copyright © 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for cc-mode and the modes derived from it.

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

(require 'prelude-programming)

(defun prelude-c-mode-common-defaults ()
  (setq c-default-style "linux"
        c-basic-offset 4
        tab-width 4
        indent-tabs-mode t)
  (c-set-offset 'substatement-open 1))

(setq prelude-c-mode-common-hook 'prelude-c-mode-common-defaults)

(defun wen-ext-c-style-8 (style-kr)
  (interactive "sstyle kr:")
  (c-set-style "k&r")
  (setq c-default-style "k&r"
        c-basic-offset 8
        tab-width 8
        indent-tabs-mode t)
  (c-set-offset 'substatement-open 1))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'prelude-c-mode-common-hook)))

(c-add-style "wen-cpp-style"
			 '("stroustrup"
			   (indent-tabs-mode . nil)        ; use spaces rather than tabs
			   (c-basic-offset . 4)            ; indent by four spaces
			   (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
								   (brace-list-open . 0)
								   (statement-case-open . +)))))

(defun wen-cpp-indent-hook ()
  (c-set-style "wen-cpp-style")
  (auto-fill-mode))
  ;(c-toggle-auto-hungry-state 1)) ;; auto open next line

(add-hook 'c-mode-common-hook 'wen-cpp-indent-hook)
;(add-hook 'c++-mode-hook 'wen-cpp-indent-hook)

(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))
(provide 'prelude-c)

;;; prelude-c.el ends here
