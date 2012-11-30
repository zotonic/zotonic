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
  (modify-syntax-entry ?# "_ 23")
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<")
  (modify-syntax-entry ?| ".")
  (modify-syntax-entry ?% ".")
  (modify-syntax-entry ?_ "w")
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
       "endjavascript" "as"
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

(defconst zotonic-tpl-filters-re
  (eval-when-compile
    (regexp-opt
     '(
       "add_day" "add_month" "add_week" "add_year" "after" "append"
       "as_atom" "before" "brlinebreaks" "capfirst" "center" "chunk"
       "date" "date_range" "datediff" "element" "eq_day" "escape"
       "escape_ical" "escape_link" "escapejs" "escapejson" "escapexml"
       "filesizeformat" "first" "fix_ampersands" "force_escape"
       "format_integer" "format_number" "format_price" "gravatar_code"
       "group_by" "group_firstchar" "group_title_firstchar" "if"
       "in_future" "in_past" "index_of" "inject_recipientdetails"
       "insert" "is_a" "is_defined" "is_even" "is_list" "is_not_a"
       "is_rtl" "is_undefined" "is_visible" "join" "language" "last"
       "length" "linebreaksbr" "ljust" "lower" "make_list" "match"
       "max" "member" "menu_flat" "menu_subtree" "menu_trail" "min"
       "minmax" "ne_day" "nthtail" "pprint" "rand" "random"
       "randomize" "range" "replace" "replace_args" "reversed" "rjust"
       "sha1" "show_media" "slice" "slugify" "split" "split_in"
       "stringify" "striptags" "sub_day" "sub_month" "sub_week"
       "sub_year" "summary" "survey_answer_split" "survey_is_submit"
       "survey_prepare_matching" "survey_prepare_narrative"
       "survey_prepare_thurstone" "tail" "timesince" "to_binary"
       "to_integer" "to_json" "truncate" "twitter" "unescape" "upper"
       "urlencode" "urlize" "utc" "vsplit_in" "without_embedded_media"
       "yesno"
       )
     'symbols))
  "Filters shipped with Zotonic.")

(defconst zotonic-tpl-keywords-matcher
  (list
   zotonic-tpl-keywords-re nil nil '(1 font-lock-keyword-face))
  "Highlight keywords")

(defconst zotonic-tpl-custom-tags-matcher
  (list
   zotonic-tpl-custom-tags-re nil nil '(1 font-lock-builtin-face))
  "Highlight custom tags")

(defconst zotonic-tpl-filters-matcher
  (list
   zotonic-tpl-filters-re nil nil
   '(1 font-lock-builtin-face))
  "Highlight filters")

(defconst zotonic-tpl-lookup-matcher
  (list
   "\\(\\w+\\)[.[]"
   nil nil
   '(1 font-lock-function-name-face))
  "Highlight lookup expressions")

(defconst zotonic-tpl-tuple-matcher
  (list
   "{\\(\\w+\\)"
   nil nil
   '(1 font-lock-type-face))
  "Highlight tuple like expressions")

(defconst zotonic-tpl-variable-matcher
  (list
   "\\w+"
   nil nil
   '(0 font-lock-variable-name-face))
  "Highlight variable expressions")

(defun zotonic-tpl-font-lock-keywords-level-1 ()
  "Basic font-lock-keywords table."
  (list
   ;; mark {{, }} and %} as constants
   '("{{\\|[%}]}" . font-lock-constant-face)
   (cons
    "{%"
    (list
     ;; mark {% as constant
     '(0 font-lock-constant-face)
     ;; mark keywords and custom tags following {%
     zotonic-tpl-keywords-matcher
     zotonic-tpl-custom-tags-matcher
     ))
   (cons
    "{[{%][^|]+\\(|\\)"
    (list
     ;; mark | as constant
     '(1 font-lock-constant-face)
     ;; mark filters following |
     zotonic-tpl-filters-matcher
     ))
   ))

(defun zotonic-tpl-font-lock-keywords-level-2 ()
  "Extended font-lock-keywords entries."
  (mapcar
   (lambda (matcher)
     (cons
      ;; mark tokens following {{ and {%
      "{[{%]"
      matcher
      ))
   (list
    zotonic-tpl-lookup-matcher
    zotonic-tpl-tuple-matcher
    zotonic-tpl-variable-matcher
    )))

(defun zotonic-tpl-font-lock-keywords-default-level ()
  (append
   (zotonic-tpl-font-lock-keywords-level-1)
   (zotonic-tpl-font-lock-keywords-level-2))
  )

(defvar zotonic-tpl-font-lock-defaults
  (list
   (zotonic-tpl-font-lock-keywords-default-level)
   ;;(zotonic-tpl-font-lock-keywords-level-1)
   ))

(define-derived-mode zotonic-tpl-mode prog-mode "Zotonic"
  "Major mode for editing Zotonic templates."
  (zotonic-tpl-syntax-table)
  (set (make-local-variable 'comment-start)
       zotonic-tpl-comment-start)
  (set (make-local-variable 'comment-end)
       zotonic-tpl-comment-end)
  (set (make-local-variable 'comment-start-skip)
       zotonic-tpl-comment-start-skip)
  (set (make-local-variable 'font-lock-defaults)
       zotonic-tpl-font-lock-defaults)
  )

(provide 'zotonic-tpl-mode)

;;; zotonic-tpl-mode.el ends here
