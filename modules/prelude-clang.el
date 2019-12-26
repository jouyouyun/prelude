;;; prelude-clang.el --- Emacs Prelude: clang company configuration.
;;
;; Copyright © 2018-2018 jouyouyun
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for cc-mode clang company.

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

(prelude-require-package 'company-c-headers)
(prelude-require-package 'c-eldoc)

(require 'company-c-headers)
(require 'c-eldoc)

(defun wen-c-headers-company ()
  (add-to-list 'company-backends 'company-c-headers))

(add-hook 'c-mode-hook 'wen-c-headers-company)
(add-hook 'c++-mode-hook 'wen-c-headers-company)

;; add header directory
(defun wen-append-header-directory (header-dir)
  "Append header directory.
`HEADER-DIR' is the target directory"
  (interactive "sheader dir: ")
  (message "Debug: will append %s to c headers." header-dir)
  (if (file-exists-p header-dir)
      (setq company-c-headers-path-system
	    (append company-c-headers-path-system
		    (split-string header-dir)))
    (message "Warning: directory %s not exists for headers." header-dir))
  )

;; add clang arguments
(defun wen-append-clang-arguments (header-dir)
  "Append clang arguments.
`HEADER-DIR' is the target directory"
  (interactive "sheader dir: ")
  (wen-append-header-directory header-dir)
  (message "Debug: will append %s to clang arguments." header-dir)
  (if (file-exists-p header-dir)
      (setq company-clang-arguments
            (append company-clang-arguments
		    (split-string (concat "-I" header-dir))))
    (message "Warning: directory %s not exists for clang." header-dir))
  )

;; autocomplete headers
(defun wen-pkg-config-enable-clang-headers (pkg-config-lib)
  "This function will add necessary header file path of a
specified by `pkg-config-lib' to `company-c-headers-path-system`', which make it
completionable by company-c-headers"
  (interactive "spkg-config lib: ")
  (if (executable-find "pkg-config")
      (if (= (shell-command
              (format "pkg-config %s" pkg-config-lib))
             0)
          (setq company-c-headers-path-system
                (append company-c-headers-path-system
                        (split-string
                         (shell-command-to-string
                          (format "pkg-config --cflags-only-I %s|sed 's/-I//g'"
                                  pkg-config-lib)))))
        (message "Error, pkg-config lib %s not found." pkg-config-lib))
    (message "Error: pkg-config tool not found.")))


;; autocomplete via company-clang
(defun wen-pkg-config-enable-clang-flag (pkg-config-lib)
  "This function will add necessary header file path of a
specified by `pkg-config-lib' to `company-clang-arguments', which make it
completionable by company-clang"
  (interactive "spkg-config lib: ")
  (wen-pkg-config-enable-clang-headers pkg-config-lib)
  (if (executable-find "pkg-config")
      (if (= (shell-command
              (format "pkg-config %s" pkg-config-lib))
             0)
          (setq company-clang-arguments
                (append company-clang-arguments
                        (split-string
                         (shell-command-to-string
                          (format "pkg-config --cflags-only-I %s"
                                  pkg-config-lib)))))
        (message "Error, pkg-config lib %s not found." pkg-config-lib))
    (message "Error: pkg-config tool not found.")))

(defun set-common-clang-args ()
  (setq command "echo | g++ -v -x c++ -E - 2>&1 |
                 grep -A 20 starts | grep include | grep -v search | grep -v '#'")
  (setq company-clang-arguments
        (mapcar (lambda (item)
                  (concat "-I" item))
                (split-string
                 (shell-command-to-string command))))
  )

(defun wen-set-c-clang-args ()
  (wen-append-clang-arguments "/usr/src/linux-common/include") ;; must create symlink before used
  (set-common-clang-args)
  (wen-pkg-config-enable-clang-flag "glib-2.0")
  ;; (wen-pkg-config-enable-clang-flag "gtk+-3.0")
  ;; (wen-pkg-config-enable-clang-flag "popper-glib")
  ;; (wen-pkg-config-enable-clang-flag "libpulse-mainloop-glib")
  ;; (wen-pkg-config-enable-clang-flag "librsvg-2.0")
  (set 'flycheck-clang-args company-clang-arguments)
  )

(defun wen-set-cpp-clang-args ()
  (set-common-clang-args)
  ;; (wen-pkg-config-enable-clang-flag "Qt5Core")
  ;; (wen-pkg-config-enable-clang-flag "Qt5Gui")
  ;; (wen-pkg-config-enable-clang-flag "Qt5Widgets")
  ;; (wen-pkg-config-enable-clang-flag "Qt5DBus")
  ;; (wen-pkg-config-enable-clang-flag "Qt5Network")
  ;; (wen-pkg-config-enable-clang-flag "Qt5Sql")
  ;; (wen-pkg-config-enable-clang-flag "Qt5Svg")
  ;; (wen-pkg-config-enable-clang-flag "Qt5Xml")
  (set 'flycheck-clang-args company-clang-arguments)
  )

(add-hook 'c-mode-hook 'wen-set-c-clang-args)
(add-hook 'c++-mode-hook 'wen-set-cpp-clang-args)


(provide 'prelude-clang)

;;; prelude-clang.el ends here
