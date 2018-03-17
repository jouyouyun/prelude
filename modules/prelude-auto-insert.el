;;; prelude-auto-insert.el --- Emacs Prelude: programs template configuration.
;;
;; Copyright © 2018-2018 jouyouyun
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for programs template and the modes derived from it.

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


;; C++
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     '("Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       "\n */" > \n \n
       "#include <iostream>" \n \n
       "using namespace std;" \n \n
       "main()" \n
       "{" \n
       > _ \n
       "}" > \n)))

;; C
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       "\n */" > \n \n
       "#include <stdio.h>" \n
       "#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int" \n
       "main(int argc, char *argv[])" \n
       "{" > \n
       > _ \n
       "}" > \n)))

;; C/C++ header
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.h\\'" . "C/C++ skeleton")
     '(
       "Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       " */" > \n \n
       "#ifndef __XX_H__" \n
       "#define __XX_H__" \n \n
       "#endif" \n)))

;; ruby
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.rb\\'" . "Ruby skeleton")
     '(
       "Short description: "
       "#!/usr/bin/ruby -w\n"
       "# -*-coding: utf-8 -*-\n\n"
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n###" \n \n)))

;; python
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.py\\'" . "Python skeleton")
     '(
       "Short description: "
       "#!/usr/bin/env python3\n"
       "# -*-coding: utf-8 -*-\n\n"
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n###" \n \n)))

;; shell
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.sh\\'" . "Shell skeleton")
     '(
       "Short description: "
       "#!/bin/bash\n\n"
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n###" \n \n)))


(provide 'prelude-auto-insert)

;;; prelude-auto-insert.el ends here
