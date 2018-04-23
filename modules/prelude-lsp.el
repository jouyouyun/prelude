;;; prelude-lsp.el --- Emacs Prelude: lsp configuration.
;;
;; Copyright © 2018-2018 jouyouyun
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for vue mode.

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

(prelude-require-packages '(lsp-mode lsp-ui company-lsp lsp-vue typescript-mode lsp-javascript-typescript))


(require 'lsp-mode)
(require 'company-lsp)
(push 'company-lsp company-backends)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(require 'lsp-vue)
;;(add-hook 'vue-mode-hook #'lsp-vue-mmm-enable)
(add-hook 'major-mode-hook #'lsp-vue-enable)

;;; depends: npm i -g javascript-typescript-langserver
(require 'lsp-javascript-typescript)
(add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
(add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support
(defun my-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my-js-hook nil
  (make-local-variable 'company-transformers)
  (push 'my-company-transformer company-transformers))

(add-hook 'js-mode-hook 'my-js-hook)

;;; depends: npm i -g flow-language-server
;;(require 'lsp-javascript-flow)
;;(add-hook 'js-mode-hook #'lsp-javascript-flow-enable)
;;(add-hook 'js2-mode-hook #'lsp-javascript-flow-enable) ;; for js2-mode support
;;(add-hook 'rjsx-mode #'lsp-javascript-flow-enable) ;; for rjsx-mode support

;;; npm i -g typescript-language-server
;;(require 'lsp-typescript)
;;(add-hook 'js-mode-hook #'lsp-typescript-enable)
;;(add-hook 'js2-mode-hook #'lsp-typescript-enable) ;; for js2-mode support
;;(add-hook 'rjsx-mode #'lsp-typescript-enable) ;; for rjsx-mode support


(provide 'prelude-lsp)
