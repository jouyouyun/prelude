;;; prelude-misc.el --- Emacs Prelude: misc configuration.
;;
;; Copyright © 2018-2018 jouyouyun
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for misc.

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

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; multiple-cursors
(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-s") 'mc/skip-to-next-like-this)

;; popup-kill-ring
(prelude-require-package 'popup-kill-ring)
(global-set-key (kbd "M-y") 'popup-kill-ring)

(prelude-require-package 'whole-line-or-region)
;; Comment or uncomment
;; (global-set-key (kbd "M-;") 'whole-line-or-region-comment-dwim-2)
(global-set-key (kbd "M-w") 'whole-line-or-region-copy-region-as-kill)
;; 当光标放在行的始端，或者行的中间位置，即为注释该行代码
;; 当光标放在行的末端，即为给该行代码添加注释
(defun improve-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'improve-comment-dwim-line)

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(prelude-require-package 'youdao-dictionary)
;; Enable Cache
(setq url-automatic-caching t)
(global-set-key (kbd "C-c M-\\") 'youdao-dictionary-search-at-point+)


(prelude-require-package 'ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; markdown review
(prelude-require-package 'gh-md)
;; gh-md-render-region and gh-md-render-buffer to generate a preview of the markdown content of a buffer.


(provide 'prelude-misc)

;;; prelude-misc.el ends here
