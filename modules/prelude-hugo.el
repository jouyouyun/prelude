;;; 'wen-hugo.el' --- Emacs hugo section

(prelude-require-packages '(
                        ox-hugo
                        easy-hugo
                        ))

(with-eval-after-load 'ox
  (require 'ox-hugo))

(require 'easy-hugo)
(setq easy-hugo-basedir "/Data/Projects/Private/blog-with-hugo/")
(setq easy-hugo-url "https://jouyouyun.jouyouyun.top")
(setq easy-hugo-root "/Data/Projects/Private/blog-with-hugo/")
(setq easy-hugo-previewtime "300")
;; (define-key global-map (kbd "C-c C-e") 'easy-hugo)
(setq easy-hugo-default-ext ".md")

(provide 'prelude-hugo)
