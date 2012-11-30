;;; zotonic-tpl-mode.el --- Support for the zotonic template language

;; Copyright (C) 2012 Andreas Stenius

;; Author: Andreas Stenius <git@astekk.se>
;; Created: 27 Nov 2012
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This header is following the guidelines presented in
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html#Library-Headers

;;; Code:

(defvar zotonic-tpl-comment-start "{# ")
(defvar zotonic-tpl-comment-end " #}")
(defvar zotonic-tpl-comment-start-skip "\\({# \\|{% comment %}\\) *")

(defun zotonic-tpl-syntax-table ()
  "Modify the current buffers syntax table for zotonic templates."
  (modify-syntax-entry ?{ "(}1")
  (modify-syntax-entry ?} "){4")
  (modify-syntax-entry ?# ". 23")
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<")
  )

(defconst zotonic-tpl-keywords-re
  (eval-when-compile
    (regexp-opt
     '(
       "for" "empty" "endfor" "in" "include" "catinclude" "block"
       "endblock" "extends" "overrules" "inherit" "autoescape"
       "endautoescape" "if" "else" "elif" "elseif" "endif" "not" "or"
       "and" "xor" "comment" "endcomment" "cycle" "firstof"
       "ifchanged" "ifequal" "endifequal" "ifnotequal"
       "endifnotequal" "now" "regroup" "rsc" "spaceless"
       "endspaceless" "ssi" "templatetag" "load" "call" "with" "url"
       "print" "image" "image_url" "media" "_" "with" "endwith" "all"
       "lib" "cache" "endcache" "filter" "endfilter" "javascript"
       "endjavascript"
       )
     'symbols))
  "Zotonic template keywords")

(defconst zotonic-tpl-custom-tags-re
  (eval-when-compile
    (regexp-opt
     '(
       "admin_translation_statistics" "button" "chart_pie"
       "chart_pie3d" "debug" "draggable" "droppable" "geomap_static"
       "google_chart" "inplace_textbox" "lazy" "loremipsum"
       "mailinglist_subscribe" "menu" "pager" "poll" "script"
       "sortable" "sorter" "spinner" "stream" "survey_example" "tabs"
       "validate" "wire" "wire_args"
       )
     'symbols))
  "Custom tags (aka scomps) shipped with Zotonic.")

(defun zotonic-tpl--font-lock-keywords-rule (re face)
  "Take regexp string and return a item for the font-lock-keywords list"
  (list re (regexp-opt-depth re) face))

(defun zotonic-tpl-font-lock-keywords-level-1 ()
  "Basic font-lock-keywords table."
  (list
   '("{{\\|[%}]}" . font-lock-constant-face)
   (cons
    "{%"
    (list
     '(0 font-lock-constant-face)
     (list
      zotonic-tpl-keywords-re nil nil '(1 font-lock-keyword-face))
     (list
      zotonic-tpl-custom-tags-re nil nil '(1 font-lock-builtin-face))
     ))
   )
  )

(defun zotonic-tpl-font-lock-keywords-default-level ()
  (zotonic-tpl-font-lock-keywords-level-1)
  )

(defvar zotonic-tpl-font-lock-defaults
  (list
   (zotonic-tpl-font-lock-keywords-default-level)
   ;;(zotonic-tpl-font-lock-keywords-level-1)
   ))

(define-derived-mode zotonic-tpl-mode prog-mode "Zotonic"
  "Major mode for editing Zotonic templates."
  (set (make-local-variable 'comment-start)
       zotonic-tpl-comment-start)
  (set (make-local-variable 'comment-end)
       zotonic-tpl-comment-end)
  (set (make-local-variable 'comment-start-skip)
       zotonic-tpl-comment-start-skip)
  (zotonic-tpl-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       zotonic-tpl-font-lock-defaults)
  )

(provide 'zotonic-tpl-mode)

;;; zotonic-tpl-mode.el ends here
