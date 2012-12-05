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

;; FIXME: all keywords are accepted by erlydtl after a opening {%,
;; but only a limited set after that...
;; but here, zotonic-tpl-mode accepts all keywords within a {% %}-tag.
(defconst zotonic-tpl-keywords-matcher
  (list
   zotonic-tpl-keywords-re
   '(progn (goto-char (match-end 1)) (match-end 0)) nil
   '(1 font-lock-keyword-face))
  "Highlight keywords")

(defconst zotonic-tpl-custom-tags-matcher
  (list
   (concat "{% *" zotonic-tpl-custom-tags-re)
   '(progn (goto-char (match-beginning 1)) (match-end 2)) nil
   '(1 font-lock-builtin-face))
  "Highlight custom tags")

(defconst zotonic-tpl-filters-matcher
  (list
   (concat "\\(|\\)\\(" zotonic-tpl-filters-re "\\|\\(\\_<\\w+\\_>\\)\\)")
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-constant-face)
   '(3 font-lock-builtin-face t t)
   '(4 font-lock-function-name-face t t))
  "Highlight filters")

(defconst zotonic-tpl-lookup-matcher
  (list
   "\\(\\w+\\)\\(\\.\\)"
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-variable-name-face)
   '(2 font-lock-constant-face))
  "Highlight lookup expressions")

(defconst zotonic-tpl-index-matcher
  (list
   "\\(\\w+\\)\\(\\[\\)[^]]*\\(]\\.?\\)"
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-variable-name-face)
   '(2 font-lock-constant-face)
   '(3 font-lock-constant-face))
  "Highlight index expressions")

(defconst zotonic-tpl-tuple-matcher
  (list
   "\\({\\)[ \t\n]*\\(\\w+\\)"
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-constant-face)
   '(2 font-lock-type-face))
  "Highlight tuple expressions")

(defconst zotonic-tpl-tuple-close-matcher
  (list
   "}"
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(0 font-lock-constant-face))
  "Highlight tuple expressions")


(defun zotonic-tpl-font-lock-tags ()
  "font-lock keywords list for highlighting Zotonic template {% %}-tags."
  (list
   (cons
    ;; find next {% ... %} tag (notice: we need to keep the captures in sync)
    "\\({%\\) *\\(\\w+\\)\\([^%]\\|\n\\|\\(%[^}]\\)\\)*\\(%}\\)"
    ;; captures: 1. {%  2. tag/keyword 3-4. tag contents  5. %}
    (list
     '(1 font-lock-constant-face)
     '(5 font-lock-constant-face)
     zotonic-tpl-keywords-matcher
     zotonic-tpl-custom-tags-matcher
     zotonic-tpl-filters-matcher
     zotonic-tpl-lookup-matcher
     zotonic-tpl-index-matcher
     zotonic-tpl-tuple-matcher
     zotonic-tpl-tuple-close-matcher
     ))
   (cons
    ;; find next {{ ... }} tag (notice: we need to keep the captures in sync)
    "\\({{\\)\\( *\\)\\([^}]\\|\n\\|\\(}[^}]\\)\\)*\\(}}\\)"
    ;; captures: 1. {{  2. dummy (leading space) 3-4. tag contents  5. }}
    (list
     '(1 font-lock-constant-face)
     '(5 font-lock-constant-face)
     zotonic-tpl-filters-matcher
     zotonic-tpl-lookup-matcher
     zotonic-tpl-index-matcher
     ))
   ))

(defvar zotonic-tpl-font-lock-defaults
  (list
   (zotonic-tpl-font-lock-tags)
   ))

(defun zotonic-tpl-font-lock-extend-region ()
  "Move fontification boundaries to surround any template tags."
  (save-excursion
    (let ((changed nil))
      (goto-char font-lock-beg)
      (when (zotonic-tpl-prev-tag-boundary (point-min))
        (setq changed t
              font-lock-beg (point)))
      (goto-char font-lock-end)
      (when (zotonic-tpl-next-tag-boundary (point-max))
        (setq changed t
              font-lock-end (point)))
      changed))
  )

(defconst zotonic-tpl-tag-boundary-re
  "\\({[{%]\\)\\|\\([%}]}\\)"
  "Regular expression used to locate tag boundaries.")

(defun zotonic-tpl-prev-tag-boundary (limit)
  "Search backwards for the closest open/close tag and move point there.
That is, it will move point to just before the opening tag,
or just after the closing tag. Returns t if point was moved, otherwise nil."
  (interactive (list (point-min)))
  (let ((start (point)))
    (when (re-search-backward zotonic-tpl-tag-boundary-re limit t)
      (unless (looking-at-p "{") (forward-char 2))
      (not (eq start (point)))
      )))

(defun zotonic-tpl-next-tag-boundary (limit)
  "Search forwards for the closest open/close tag and move point there.
Returns t if point was moved, otherwise nil."
  (interactive (list (point-max)))
  (let ((start (point)))
    (when (re-search-forward zotonic-tpl-tag-boundary-re limit t)
      (backward-char 2)
      (unless (looking-at-p "{") (forward-char 2))
      (not (eq start (point)))
      )))

(defun zotonic-tpl-indent-tag-line ()
  "Indent zotonic line inside tag."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at-p "[ \t]*[%}]}")
      (indent-line-to (save-excursion
                        (zotonic-tpl-prev-tag-boundary (point-min))
                        (current-column)))
      )
     )))

(defun zotonic-tpl-indent-line ()
  "Indent zotonic template code."
  (interactive)
  (let ((start (point)))
    (if (and
         (zotonic-tpl-prev-tag-boundary (point-min))
         (looking-at-p "{"))
        (progn
          (goto-char start)
          (zotonic-tpl-indent-tag-line))
      (goto-char start)
      (indent-relative))))

(define-derived-mode zotonic-tpl-mode prog-mode "Zotonic"
  "Major mode for editing Zotonic templates."
  (zotonic-tpl-syntax-table)
  (set (make-local-variable 'comment-start) zotonic-tpl-comment-start)
  (set (make-local-variable 'comment-end) zotonic-tpl-comment-end)
  (set (make-local-variable 'comment-start-skip) zotonic-tpl-comment-start-skip)
  (set (make-local-variable 'font-lock-defaults) zotonic-tpl-font-lock-defaults)
  (set (make-local-variable 'indent-line-function) #'zotonic-tpl-indent-line)
  (setq font-lock-multiline t)
  (setq font-lock-extend-region-functions
        (append font-lock-extend-region-functions
                '(zotonic-tpl-font-lock-extend-region)))
  )

(provide 'zotonic-tpl-mode)

;;; zotonic-tpl-mode.el ends here
