;;; zotonic-tpl-mode.el --- Support for the zotonic template language

;; Copyright (C) 2012 Andreas Stenius

;; Author: Andreas Stenius <git@astekk.se>
;; Created: 27 Nov 2012
;; Keywords: languages

;; This file is part of Zotonic (http://zotonic.com).

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

;;; Multiline font-lock:

;; Support for multiline font-lock has been achieved by setting
;; font-lock-multiline to `t', plus some neat trickery in the keywords
;; table. The trick is to match the anchor to entire tags, that may be
;; spanning multiple lines. Then each anchored sub highlighter will have
;; to move point back in the `pre-form', yet still return the `end-of-tag'
;; that was matched in order to mark it too as a multiline construct.
;; This is all the goto-char match-end you'll see in the matchers.

;;; Tag soup support:

;; Tag soup, a.k.a html, is supported directly in this mode. It ought to
;; be supported by an accompanying sub-mode. But I can't be bothered with it.

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
  (modify-syntax-entry ?_ "_")
  )

(defconst zotonic-tpl-identifer-re "\\(\\(?:\\w\\|_\\)+\\)")

(defconst zotonic-tpl-keywords-re
  (eval-when-compile
    (regexp-opt
     '(
       "for" "empty" "endfor" "in" "include" "catinclude" "block"
       "endblock" "extends" "overrules" "inherit" "autoescape"
       "endautoescape" "if" "else" "elif" "elseif" "endif" "not" "or"
       "and" "xor" "comment" "endcomment" "cycle" "firstof"
       "ifchanged" "ifequal" "endifequal" "ifnotequal" "endifnotequal"
       "now" "regroup" "rsc" "spaceless" "endspaceless" "ssi"
       "templatetag" "load" "call" "url" "print" "image" "image_url"
       "media" "_" "with" "endwith" "all" "lib" "cache" "endcache"
       "filter" "endfilter" "javascript" "endjavascript" "as" "optional"
       )
     'symbols))
  "Zotonic template keywords")

(defconst zotonic-tpl-begin-keywords-re
  (eval-when-compile
    (regexp-opt
     '(
       "for" "empty" "block" "autoescape" "if" "else" "elif" "elseif"
       "comment" "ifchanged" "ifequal" "ifnotequal" "spaceless" "with"
       "cache" "filter" "javascript"
       )
     'symbols))
  "Zotonic template keywords that opens a block")

(defconst zotonic-tpl-end-keywords-re
  (eval-when-compile
    (regexp-opt
     '(
       "empty" "endfor" "endblock" "endautoescape" "else" "elif"
       "elseif" "endif" "endcomment" "endifequal" "endifnotequal"
       "endspaceless" "endwith" "endcache" "endfilter" "endjavascript"
       )
     'symbols))
  "Zotonic template keywords that closes a block")

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
   (concat
    ;; |<built in filter or identifier>
    "\\(|\\)\\(" zotonic-tpl-filters-re
    "\\|" zotonic-tpl-identifer-re "\\)")
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-constant-face)
   '(3 font-lock-builtin-face t t)
   '(4 font-lock-function-name-face t t))
  "Highlight filters")

(defconst zotonic-tpl-lookup-matcher
  (list
   ;; identifier .
   (concat zotonic-tpl-identifer-re "\\(\\.\\)")
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-variable-name-face)
   '(2 font-lock-constant-face))
  "Highlight lookup expressions")

(defconst zotonic-tpl-index-matcher
  (list
   ;; identifier [ ... ].
   (concat zotonic-tpl-identifer-re "\\(\\[\\)[^]]*\\(]\\.?\\)")
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-variable-name-face)
   '(2 font-lock-constant-face)
   '(3 font-lock-constant-face))
  "Highlight index expressions")

(defconst zotonic-tpl-tuple-matcher
  (list
   ;; { identifier
   (concat "\\({\\)[ \t\n]*" zotonic-tpl-identifer-re)
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(1 font-lock-constant-face)
   '(2 font-lock-type-face))
  "Highlight tuple expressions")

(defconst zotonic-tpl-const-matcher
  (list
   "}\\|="
   '(progn (goto-char (match-end 2)) (match-end 0)) nil
   '(0 font-lock-constant-face))
  "Highlight constant characters")


(defun zotonic-tpl-font-lock-tags ()
  "font-lock keywords list for highlighting Zotonic template {% %}-tags."
  (list
   (cons
    ;; find next {% ... %} tag (notice: we need to keep the captures in sync)
    (concat
     "\\({%\\) *" zotonic-tpl-identifer-re ; open tag followed by identifier
     "\\(\\(?:.\\|\n\\)*?\\)\\(%}\\)") ; skip tag contents up to close
    ;; captures: 1. {%  2. tag/keyword 3. tag contents  4. %}
    (list
     '(1 font-lock-constant-face)
     '(4 font-lock-constant-face)
     zotonic-tpl-keywords-matcher
     zotonic-tpl-custom-tags-matcher
     zotonic-tpl-filters-matcher
     zotonic-tpl-lookup-matcher
     zotonic-tpl-index-matcher
     zotonic-tpl-tuple-matcher
     zotonic-tpl-const-matcher
     ))
   (cons
    ;; find next {{ ... }} tag (notice: we need to keep the captures in sync)
    "\\({{\\)\\(\\(?:.\\|\n\\)*?\\)\\(}}\\)"
    ;; captures: 1. {{  2. tag contents  3. }}
    (list
     '(1 font-lock-constant-face)
     '(3 font-lock-constant-face)
     zotonic-tpl-filters-matcher
     zotonic-tpl-lookup-matcher
     zotonic-tpl-index-matcher
     ))
   ;; this does NOT work - emacs hangs with this construct
   ;; (cons
   ;;  ;; find comment blocks
   ;;  "{%[ \t]*comment[ \t]*%}\\(.*\\|\n*\\)*{%[ \t]*endcomment[ \t]*%}"
   ;;  (list
   ;;   '(1 font-lock-comment-face)
   ;;   ))
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
  "\\({[{%#]\\)\\|\\([#%}]}\\)"
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

(defun zotonic-tpl-within-tag (pos)
  "Test if POS is inside a template tag {% ... %} or {{ ... }}."
  (interactive (list (point)))
  (save-excursion
    (goto-char pos)
    (and
     (zotonic-tpl-prev-tag-boundary (point-min))
     (looking-at-p "{"))))

(defun zotonic-tpl-goto-indent-column (offset)
  "Move point to the first character (i.e. the indentation column) of line,
looking at lines going in OFFSET direction. -1 or 1 is sensible offset values."
  (unless (eq 0 offset)
    (let ((res t))
      (while
          (if (not (eq 0 (forward-line offset)))
              (setq res nil)
            (skip-chars-forward " \t")
            (eolp)))
      res)))

(defun zotonic-tpl-get-indent-for-line (offset)
  "Calculate the indentation to use for the line following the line
 offset lines from the current line."
  (save-excursion
    (if (zotonic-tpl-goto-indent-column offset)
        (let ((indent (current-column)))
          (skip-chars-forward "]}")
          (while (not (eolp))
            ;; indent next line if we open with [ or {
            (if (looking-at-p "[[{]") (setq indent (+ indent tab-width))
              ;; un-indent if we close with ] or }
              (if (looking-at-p "[]}]") (setq indent (- indent tab-width))))
            (forward-char))
          ;; un-indent if next line starts with a closing ] or }
          (if (looking-at-p "\n[ \t]*[]}]") (setq indent (- indent tab-width)))

          ;; indent the next line, if we end the line with a = sign
          (if (eq ?\= (char-after (1- (point))))
              (setq indent (+ indent tab-width)))
          ;; check if this line was indented by a = on the previous line
          ;; and if so, undo it for the next line
          (forward-line -1)
          (end-of-line)
          (if (eq ?\= (char-after (1- (point))))
              (setq indent (- indent tab-width)))
          ;; return indentation
          indent)
      0)))

(defun zotonic-tpl-indent-tag-line ()
  "Indent zotonic line inside tag. Returns the column the line was indented to."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "[ \t]*{[{%#]")
        ;; indent first line of a tag in the context of tag soup
        (zotonic-tpl-tag-soup-indent)
      (let ((indent
             (if (looking-at-p "[ \t]*[#%}]}")
                 ;; indent closing tag
                 (save-excursion
                   (zotonic-tpl-prev-tag-boundary (point-min))
                   (current-column))
               ;; indent line inside tag
               (zotonic-tpl-get-indent-for-line -1))))
        (indent-line-to indent)
        indent
        ))))

(defun zotonic-tpl-indent-line ()
  "Indent zotonic template code. Returns the indentation used."
  (interactive)
  (let ((indent
         (save-excursion
           (beginning-of-line)
           (if (zotonic-tpl-within-tag (point))
               (zotonic-tpl-indent-tag-line)
             ;; point is not in a template tag
             (zotonic-tpl-tag-soup-indent)))))
    (unless (> (current-column) indent)
      ;; if we're looking at the indentation, jump to it
      ;; in case of tabs, movement by 1 char != 1 column
      (while (not (eq indent (current-column)))
        (forward-char (- indent (current-column))))
      ;; if the line is empty, leave it empty
      (if (eolp)
          (progn
            (indent-line-to 0)
            (setq indent 0))))
    indent))

(defun zotonic-tpl-indent-buffer ()
  "Indent entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;
;;; Tag soup functions
;;;

(defun zotonic-tpl-tag-soup-font-lock-tags ()
  "font-lock keywords list for highlighting tag soup tags in zotonic templates."
  (list
   (cons
    ;; find tags to highlight
    "\\(</?\\)\\(\\w+\\)\\([^/>]\\|\\(?:/+[^>]\\)\\)*\\(/?>\\)"
    ;; captures: 1. start tag  2. tag name  3. tag contents  4. end tag
    (list
     '(1 font-lock-constant-face)
     '(2 font-lock-keyword-face)
     '(4 font-lock-constant-face)
     ;; zotonic-tpl-...-matcher
     ))
   ))

(defun zotonic-tpl-tag-soup-find-open-tag (limit)
  "Search backwards for the open tag that is being closed by the tag at point."
  (interactive (list (point-min)))
  (save-match-data
    (if (looking-at "[ \t]*</\\([^>]*\\)>")
        (let ((res)
              (tag (match-string 1)))
          (while
              (if (re-search-backward (concat "</?" tag) limit t)
                  (if  (looking-at-p "</")
                      (zotonic-tpl-tag-soup-find-open-tag limit)
                    (setq res (point))
                    nil)))
          res)
      )))

(defun zotonic-tpl-tag-soup-get-indent-for-line (offset)
  "Calculate the indentation to use for the line following the line
 offset lines from the current line."
  (save-excursion
    (if (zotonic-tpl-goto-indent-column offset)
        (progn
          ;; don't indent relative to the middle of a template tag
          (if (zotonic-tpl-within-tag (point))
              (zotonic-tpl-prev-tag-boundary (point-min)))
          ;; go through line, looking for indentation modifiers
          ;; such as opening and closing soup tags or
          ;; template block tags (those that has matching pairs)
          ;; like block .. endblock or if .. endif, etc.
          (let ((start (point))
                (indent (current-column))
                (end (progn
                       (end-of-line)
                       (point))))
            ;; look for the last closing soup tag
            (if (re-search-backward "</" start t)
                (save-excursion
                  (if (zotonic-tpl-tag-soup-find-open-tag (point-min))
                      (setq indent (current-column))))
              (goto-char start))
            ;; look for ending multiline soup tag
            (if (looking-at-p ">")
                (setq indent (+ indent tab-width)))
            ;; look at the rest of line (either the enitre line,
            ;; or that following the soup close tag
            (while (not (eolp))
              ;; indent after block begin tags
              (if (looking-at-p (concat "{%[ \t\n]*"
                                        zotonic-tpl-begin-keywords-re))
                  (progn
                    (setq indent (+ indent tab-width))
                    (forward-char)
                    (zotonic-tpl-next-tag-boundary end))
                  ;;; else
                ;; un-indent after block end tags (or close tag)
                ;; that has junk in front of it
                (if (looking-at-p (concat "\\(%}\\)\\|\\({%[ \t\n]*"
                                          zotonic-tpl-end-keywords-re
                                          "\\)"))
                    (progn
                      (unless (eq start (point))
                        (setq indent (- indent tab-width)))
                      (forward-char)
                      (zotonic-tpl-next-tag-boundary end))
                    ;;; else
                  ;; skip over other tags
                  (if (looking-at-p "{%")
                      (progn
                        (forward-char)
                        (zotonic-tpl-next-tag-boundary end))
                      ;;; else
                    ;; indent after opening soup tags (or multiline open tags)
                    (if (looking-at-p
                         "<\\w+\\(?:[^/>]\\|\\(?:/[^>]\\)\\)*\\(?:>\\|\n\\)")
                        (setq indent (+ indent tab-width)))
                    ;; unindent after self closing multiline soup tag
                    (if (looking-at-p "/>")
                        (save-excursion
                          (if (not (re-search-backward "<\\w+" start t))
                              (setq indent (- indent tab-width)))))
                    (forward-char)))))
            indent))
      0)))

(defun zotonic-tpl-tag-soup-indent ()
  "Indent zotonic line in midst of a tag soup (e.g. html, xml, et. al.).
Returns the column the line was indented to."
  (save-excursion
    (beginning-of-line)
    (let ((indent
           (if (looking-at-p "[ \t]*</")
               ;; indent closing tag matching it's open tag
               (save-excursion
                 (zotonic-tpl-tag-soup-find-open-tag (point-min))
                 (current-column))
             ;; indent tag soup
             (zotonic-tpl-tag-soup-get-indent-for-line -1))))
      ;; un-indent block end tags and ending multiline soup tags
      (if (or
           (looking-at-p (concat "[ \t]*{%[ \t\n]*"
                                 zotonic-tpl-end-keywords-re))
           (looking-at-p "[ \t]*/?>"))
          (setq indent (- indent tab-width)))
      (indent-line-to indent)
      indent)
    ))

;;;
;;; Test routines
;;;

(defun zotonic-tpl-test-current-line-ok ()
  "Test indent current line, return t if it was indented according to mode."
  (save-excursion
    (let ((expect (save-excursion
                    (progn
                      (beginning-of-line)
                      (skip-chars-forward " \t")
                      (current-column))))
          (actual (zotonic-tpl-indent-line)))
      (if (equal expect actual) t
        ;; restore indentation on mismatch
        (indent-line-to expect) nil))))

(defun zotonic-tpl-test-report-error (prefix)
  "Returns a formatted error message with PREFIX."
  (format "%s line %d: %s\n"
          prefix
          (line-number-at-pos)
          (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun zotonic-tpl-test-save-results (results)
  "Update {# Test results #} ... {# End results #} section with RESULTS."
  (save-excursion
    (let* ((start (progn
                    (goto-char (point-min))
                    (search-forward "{# Test results #}" (point-max) t)))
           (end (progn
                  (goto-char (point-max))
                  (search-backward "{# End results #}" start t))))
      (unless (eq nil start)
        (delete-region start (if (eq nil end) (point-max) end))
        (goto-char start)
        (insert (concat "\n" results))))))

(defun zotonic-tpl-mode-test-buffer (buffer)
  "Test indentation in BUFFER. Each line in the BUFFER where
`zotonic-tpl-indent-line` would change it is reported/returned."
  (interactive "*bRun zotonic-tpl-mode tests in buffer: ")
  (let ((errors ""))
    (with-current-buffer buffer
      (save-excursion
        ;; clear previous results, if any
        (zotonic-tpl-test-save-results "")
        (goto-char (point-min))
        (while (not (or
                     (eobp)
                     (search-forward "{# Test results #}" (point-at-eol) t)))
          ;; test line indentation with point at beginning of line
          (if (not (zotonic-tpl-test-current-line-ok))
              (setq errors
                    (concat errors (zotonic-tpl-test-report-error
                                    "indentation error from beginning of")))
            ;; if that succeeds, test with point at end of line
            ;; (there may be subtle differences from that of beginning of line)
            (end-of-line)
            (unless (zotonic-tpl-test-current-line-ok)
              (setq errors
                    (concat errors (zotonic-tpl-test-report-error
                                    "indentation error from end of")))))
          ;; go to next line to test
          (forward-line))))
    ;; write results to test results section, if any
    (zotonic-tpl-test-save-results (if (eq "" errors) "All tests OK\n" errors))
    ;; show message if run interactively
    (if (called-interactively-p 'any)
        (message "%s" (if (eq "" errors) "All tests OK" errors))
      errors)))

;;;
;;; Define zotonic major mode
;;;

(defvar zotonic-tpl-font-lock-defaults
  (list
   (append
    (zotonic-tpl-font-lock-tags)
    (zotonic-tpl-tag-soup-font-lock-tags))
   ))

(defvar zotonic-tpl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-\M-\\" 'zotonic-tpl-indent-buffer)
    map)
  "The zotonic-tpl-mode key map")

(define-derived-mode zotonic-tpl-mode prog-mode "Zotonic"
  "Major mode for editing Zotonic templates.

Keys defined in this mode are:
\\{zotonic-tpl-mode-map}
"
  (zotonic-tpl-syntax-table)
  (use-local-map zotonic-tpl-mode-map)
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
