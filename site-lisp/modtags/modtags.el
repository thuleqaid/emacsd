;;; modtags.el --- add tags around modifications

;; Copyright (C) 2017 Thule Qaid

;; Author: Thule Qaid <thuleqaid@163.com>

;; modtags is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; modtags is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with calendar-fate.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Install:
;;  (require 'modtags)

;; Step1(Done) add Add/Chg/Del tag
;; Step2(Done) get tag list in the file
;; Step3(Done) update line count
;; Step4 OK/NG modifications
;; Step5(Done) Recursively list modifications
;; Step6(Done) Batch update line count
;; Step7(Done) Summary line count
;; Step8 Static checks(devide 0, infinit loop)
;; Step9 Diff files

;; Usage:
;; Method 1 (Using project-local-variables)
;; (require 'project-local-variabls)
;; add file[.emacs-project] in the root folder
;; in file[.emacs-project] add the following lines
;; ;; General format
;; (setl ModTag:TagStart "/*$$$$$$$$$$$$$$$$$CorrectStart$$$$$$$$$$$$$$$$$$*/")
;; (setl ModTag:TagEnd "/*$$$$$$$$$$$$$$$$$$CorrectEnd$$$$$$$$$$$$$$$$$$$*/")
;; (setl ModTag:CmtStart "/*$$$ ")
;; (setl ModTag:CmtEnd " */")
;; (setl ModTag:CmtSeperator ",")
;; (setl ModTag:DateFormat "%Y/%m/%d")
;; (setl ModTag:PreCompileMode nil)
;; (setl ModTag:PreCompileSW "")
;; (setl ModTag:ConstTrue "1")
;; (setl ModTag:ConstFalse "0")
;; (setl ModTag:CmtPatterns '("//.*" "/\\*\\(.\\|\n\\)*?\\*/"))
;; (setl ModTag:AllowReason t)
;; ;; File filter
;; (setl ModTag:ExcludeDirs grep-find-ignored-directories)
;; (setl ModTag:ExcludeFiles grep-find-ignored-files)
;; (setl ModTag:IncludeFiles (split-string (cdr (assoc "ch" grep-files-aliases)) " "))
;; ;; Grep setting
;; (setl ModTag:GrepExe "python d:/gitrepo/pytools/py3scripts/Demo/EncodingGrep/eg.py")
;; (setl ModTag:GrepOption "-SG")
;; ;; Author setting
;; (setl ModTag:User "Anonymous")
;; ;; Project setting
;; (setl ModTag:Keys '("Key1" "Key2"))
;; (setl ModTag:Reasons '("Reason1" "Reason2"))
;; Method 2 (Using Emacs' built-in Per-Directory Local Variables feature)
;; set same option as method 1

;;; Code:
(require 'compile)
(require 'subr-x)

(setq ModTag:BuiltInGrep
      (when load-file-name
        (expand-file-name "eg.py" (file-name-directory load-file-name))))

(defcustom ModTag:TerminalCoding locale-coding-system
  "System terminal coding."
  :type 'coding-system
  :group 'ModTag)

(defcustom ModTag:GrepExe (format "%s %s" (or (executable-find "python3") (executable-find "python")) ModTag:BuiltInGrep)
  "Grep-like programs.
The follwing command \"GrepExe -nH GrepOption -e KeyWord -r --exclude-dir=ExcludeDirs --exclude=ExcludeFiles --include=IncludeFiles rootdir\" will be run."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:GrepOption "-SG"
  "Additional options."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:User "Anonymous"
  "Username of current programmer."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:TagMarkLen 3
  "Mark length.  All marks should be the same length."
  :type 'integer
  :group 'ModTag)
(defcustom ModTag:TagMarkADD "ADD"
  "Mark for add modification."
  :type 'string
  :group 'ModTag)
(defcustom ModTag:TagMarkCHG "CHG"
  "Mark for change modification."
  :type 'string
  :group 'ModTag)
(defcustom ModTag:TagMarkDEL "DEL"
  "Mark for del modification."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:TagStart "/*$$$$$$$$$$$$$$$$$CorrectStart$$$$$$$$$$$$$$$$$$*/"
  "Start line of modification source."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:TagEnd "/*$$$$$$$$$$$$$$$$$$CorrectEnd$$$$$$$$$$$$$$$$$$$*/"
  "End line of modification source."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:CmtStart "/*$$$ "
  "Start part of comment line."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:CmtEnd " */"
  "End part of comment line."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:CmtSeperator ","
  "Seperator between parts of comment line."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:DateFormat "%Y/%m/%d"
  "Datetime format in the keyword line."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:Keys '("Key1" "Key2")
  "Keys in the keyword line, used to seperate different modifying work."
  :type '(repeat string)
  :group 'ModTag)

(defcustom ModTag:AllowReason t
  "Whether insert a reason line."
  :type 'boolean
  :group 'ModTag)

(defcustom ModTag:Reasons '("Reason1" "Reason2")
  "Reasons for possible modifications."
  :type '(repeat string)
  :group 'ModTag)

(defcustom ModTag:PreCompileMode nil
  "Whether #if statements for ADD modification."
  :type 'boolean
  :group 'ModTag)

(defcustom ModTag:PreCompileSW ""
  "Compile switch symbol."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:ConstTrue "1"
  "True value when compile switch is not defined."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:ConstFalse "0"
  "False value when compile switch is not defined."
  :type 'string
  :group 'ModTag)

(defcustom ModTag:CmtPatterns '("//.*" "/\\*\\(.\\|\n\\)*?\\*/")
  "Comments that would be ignored for counting lines."
  :type '(repeat string)
  :group 'ModTag)

(defcustom ModTag:ExcludeDirs grep-find-ignored-directories
  "Exclude dirs for grep."
  :type '(repeat string)
  :group 'ModTag)

(defcustom ModTag:ExcludeFiles grep-find-ignored-files
  "Exclude files for grep."
  :type '(repeat string)
  :group 'ModTag)

(defcustom ModTag:IncludeFiles (split-string (cdr (assoc "ch" grep-files-aliases)) " ")
  "Include files for grep."
  :type '(repeat string)
  :group 'ModTag)

(defun ModTag:ConstructKeyword ()
  "Consturct keywords part."
  (format "[%s]" (string-join ModTag:Keys "]["))
  )

(defun ModTag:ConstructKeywordLine (type)
  "Construct keyword line according to TYPE."
  (let ((linecnt (cond ((string-equal type ModTag:TagMarkADD) (format "%s[]_[]" ModTag:TagMarkADD))
                       ((string-equal type ModTag:TagMarkDEL) (format "%s[]_[]" ModTag:TagMarkDEL))
                       ((string-equal type ModTag:TagMarkCHG) (format "%s[]_[] -> []_[]" ModTag:TagMarkCHG))))
        (datetime (format-time-string ModTag:DateFormat))
        )
    (format "%s%s%s%s%s%s%s%s%s"
            ModTag:CmtStart
            linecnt ModTag:CmtSeperator
            (ModTag:ConstructKeyword) ModTag:CmtSeperator
            datetime ModTag:CmtSeperator
            ModTag:User
            ModTag:CmtEnd)
    )
  )

(defun ModTag:ConstructReasonLine ()
  "Construct modification reason line."
  (if ModTag:AllowReason
      (format "%s%s%s"
              ModTag:CmtStart
              (completing-read "Reason:" ModTag:Reasons)
              ModTag:CmtEnd)
    nil)
  )

(defun ModTag:ConstructIfLine (type)
  "Construct #if line before modification according to TYPE."
  (cond ((string-equal type ModTag:TagMarkADD)
         (if ModTag:PreCompileMode
             (if (string-blank-p ModTag:PreCompileSW)
                 (format "#if %s" ModTag:ConstTrue)
               (format "#ifndef %s" ModTag:PreCompileSW))
           nil))
        (t (if (string-blank-p ModTag:PreCompileSW)
               (format "#if %s" ModTag:ConstFalse)
             (format "#ifdef %s" ModTag:PreCompileSW)))
        )
  )

(defun ModTag:ConstructElseLine (type)
  "Construct #else line between old source and new source according to TYPE."
  (cond ((string-equal type ModTag:TagMarkCHG)
         (if (string-blank-p ModTag:PreCompileSW)
             "#else"
           (format "#else /* %s */" ModTag:PreCompileSW))
         )
        (t nil)
        )
  )

(defun ModTag:ConstructEndLine (type)
  "Construct #endif line after modification according to TYPE."
  (cond ((string-equal type ModTag:TagMarkADD)
         (if ModTag:PreCompileMode
             (if (string-blank-p ModTag:PreCompileSW)
                 "#endif"
               (format "#endif /* %s */" ModTag:PreCompileSW))
           nil))
        (t (if (string-blank-p ModTag:PreCompileSW)
               "#endif"
             (format "#endif /* %s */" ModTag:PreCompileSW)))
        )
  )

(defun ModTag:PointPos ()
  "Get current selection range."
  (let* ((line-r1 (line-number-at-pos (region-beginning)))
         (line-r2 (line-number-at-pos (region-end)))
         (line-p (line-number-at-pos (point)))
         (regionp (use-region-p))
         (line1 (if regionp line-r1 line-p))
         (line2 (if regionp
                    (if (and (boundp 'evil-visual-selection)
                             (string-equal evil-visual-selection "line"))
                        line-r2
                      (1+ line-r2))
                  (1+ line-p)))
         )
    (list regionp line1 line2)
    )
  )

(defun ModTag:GotoLine (linenumber)
  "Goto the beginning of specified LINENUMBER."
  (goto-char (point-min))
  (beginning-of-line linenumber))

(defun ModTag:InsertLine (text)
  "Insert TEXT in the current position."
  (when text
    (insert text)
    (insert "\n"))
  )

(defun ModTag:ModifyTag (type)
  "Add modification tag according to TYPE."
  (let ((pos (ModTag:PointPos))
        (reason (ModTag:ConstructReasonLine))
        insertpos ; cursor position after this function finished
        )
    (if (car pos)
        (progn
                                        ; region selected
          (deactivate-mark)
          (ModTag:GotoLine (nth 2 pos))
          (when (string-equal type ModTag:TagMarkCHG)
            (ModTag:InsertLine (ModTag:ConstructElseLine type))
            (ModTag:InsertLine "")
            )
          (ModTag:InsertLine (ModTag:ConstructEndLine type))
          (ModTag:InsertLine ModTag:TagEnd)
          (cond ((string-equal type ModTag:TagMarkCHG)
                 (setq insertpos
                       (if ModTag:AllowReason
                           (+ (line-number-at-pos (point)) 1)
                         (+ (line-number-at-pos (point)) 0)))
                 )
                (t
                 (setq insertpos
                       (if ModTag:AllowReason
                           (+ (line-number-at-pos (point)) 4)
                         (+ (line-number-at-pos (point)) 3)))
                 (when (and (string-equal type ModTag:TagMarkADD)
                            (not ModTag:PreCompileMode))
                   (setq insertpos (1- insertpos))
                   )
                 )
                )
          (ModTag:GotoLine (nth 1 pos))
          (ModTag:InsertLine ModTag:TagStart)
          (ModTag:InsertLine (ModTag:ConstructKeywordLine type))
          (ModTag:InsertLine reason)
          (ModTag:InsertLine (ModTag:ConstructIfLine type))
          )
      (progn
                                        ; no-selection
        (if (string-equal type ModTag:TagMarkADD)
            (progn
                                        ; insert code before current line
              (ModTag:GotoLine (nth 1 pos))
              (ModTag:InsertLine ModTag:TagStart)
              (ModTag:InsertLine (ModTag:ConstructKeywordLine type))
              (ModTag:InsertLine reason)
              (ModTag:InsertLine (ModTag:ConstructIfLine type))
              (ModTag:InsertLine "")
              (ModTag:InsertLine (ModTag:ConstructEndLine type))
              (ModTag:InsertLine ModTag:TagEnd)
              (setq insertpos
                    (if ModTag:PreCompileMode
                        (- (line-number-at-pos (point)) 3)
                      (- (line-number-at-pos (point)) 2)
                      ))
              )
          (progn
                                        ; treat current line as selection
            (ModTag:GotoLine (nth 2 pos))
            (when (string-equal type ModTag:TagMarkCHG)
              (ModTag:InsertLine (ModTag:ConstructElseLine type))
              (ModTag:InsertLine "")
              (setq insertpos
                    (if ModTag:AllowReason
                        (+ (line-number-at-pos (point)) 3)
                      (+ (line-number-at-pos (point)) 2)))
              )
            (ModTag:InsertLine (ModTag:ConstructEndLine type))
            (ModTag:InsertLine ModTag:TagEnd)
            (when (string-equal type ModTag:TagMarkDEL)
              (setq insertpos
                    (if ModTag:AllowReason
                        (+ (line-number-at-pos (point)) 4)
                      (+ (line-number-at-pos (point)) 3))))
            (ModTag:GotoLine (nth 1 pos))
            (ModTag:InsertLine ModTag:TagStart)
            (ModTag:InsertLine (ModTag:ConstructKeywordLine type))
            (ModTag:InsertLine reason)
            (ModTag:InsertLine (ModTag:ConstructIfLine type))
            )))
      )
    (ModTag:GotoLine insertpos)
    )
  )

(defun ModTag:ModificationList ()
  "Get modification list of current buffer."
  (let ((list-start '()) ; temp list for TagStart
        (list-key '()) ; temp list for Keyword
        (list-rst '()) ; result list
        (pat (format "%s\\|%s\\|%s"
                     (regexp-quote ModTag:TagStart)
                     (regexp-quote ModTag:TagEnd)
                     (regexp-quote (ModTag:ConstructKeyword))))
        txtpos matchtxt)
    (ModTag:GotoLine 1)
    (while (setq txtpos (re-search-forward pat nil t))
      (setq matchtxt (match-string-no-properties 0))
      (setq txtpos (line-number-at-pos txtpos))
      (cond
       ((string-equal ModTag:TagStart matchtxt)
        (push txtpos list-start)
        )
       ((string-equal ModTag:TagEnd matchtxt)
        (when (> (length list-start) 0)
          (if (> (length list-key) 0)
              (progn
                (pop list-key)
                (push (list (pop list-start) txtpos) list-rst)
                )
            (pop list-start)
            )
          )
        )
       (t
        (when (and (> (length list-start) 0)
                   (= (car (last list-start)) (1- txtpos)))
          (push txtpos list-key))
        )
       )
      )
    (nreverse list-rst)
    )
  )

(defun ModTag:CountLinesInRegion (startline endline)
  "Count lines in the between STARTLINE and ENDLINE."
  (save-excursion
    (save-restriction
      (let* ((curline (line-number-at-pos (point)))
             (pos1 (line-beginning-position (- startline curline -1)))
             (pos2 (line-beginning-position (- endline curline -1)))
             text-backup
             result
             )
        (narrow-to-region pos1 pos2)
        ;; backup narrowed region
        (setq text-backup (buffer-substring-no-properties (point-min) (point-max)))
        ;; delete comments
        (dolist (cmtpat ModTag:CmtPatterns)
          (goto-char (point-min))
          (while (re-search-forward cmtpat nil t)
            (replace-match ""))
          )
        ;; flush empty lines
        (goto-char (point-min))
        (flush-lines "^\\s-*$")
        ;; save result
        (setq result (list (- endline startline) (count-lines (point-min) (point-max))))
        ;; restore narrowed region
        (delete-region (point-min) (point-max))
        (insert text-backup)
        ;; return result
        result
        )
      ))
  )

(defun ModTag:PairPrecompile ()
  "Jump between #if..#el[se|if]..#endif."
  (let ((pat "^\\s-*#\\(if\\|el\\|end\\)")
        (pat1 "^\\s-*#if")
        (pat2 "^\\s-*#el")
        (pat3 "^\\s-*#end")
        (pos (line-beginning-position))
        (pos1 -1)
        (level 0)
        (posidx -1)
        pos2
        poslist '()
        poslist-filter '()
        line
        )
    ;; get all #if/#el/#end in the buffer
    ;; data structure (level type pos ... )
    ;; type: 1(#if) 2(#el) 3(#end)
    (goto-char (point-min))
    (re-search-forward pat nil t)
    (setq pos2 (line-beginning-position))
    (while (not (eq pos1 pos2))
      (when (= pos2 pos)
        (setq posidx (length poslist)))
      (setq line (buffer-substring-no-properties pos2 (line-end-position)))
      (cond ((string-match pat1 line)
             (progn
               (setq level (1+ level)
                     poslist (append poslist (list level 1 pos2)))
               ))
            ((string-match pat2 line)
             (progn
               (setq poslist (append poslist (list level 2 pos2)))
               ))
            ((string-match pat3 line)
             (progn
               (setq poslist (append poslist (list level 3 pos2))
                     level (1- level))
               ))
            )
      (re-search-forward pat nil t)
      (setq pos1 pos2
            pos2 (line-beginning-position))
      )
    ;; filter #if/#el/#end for the current line
    (when (>= posidx 0)
      (setq poslist-filter (list (nth (+ posidx 2) poslist))
            pos2 0)
      (when (< (nth (1+ posidx) poslist) 3)
        (setq pos1 (+ posidx 3))
        (while (< pos1 (length poslist))
          (when (= (nth posidx poslist) (nth pos1 poslist))
            (setq poslist-filter (append poslist-filter (list (nth (+ pos1 2) poslist))))
            (when (>= (nth (1+ pos1) poslist) 3)
              (setq pos1 (length poslist))
              )
            )
          (setq pos1 (+ pos1 3))
          )
        )
      (when (> (nth (1+ posidx) poslist) 1)
        (setq pos1 (- posidx 3))
        (while (>= pos1 0)
          (when (= (nth posidx poslist) (nth pos1 poslist))
            (setq poslist-filter (append (list (nth (+ pos1 2) poslist)) poslist-filter)
                  pos2 (1+ pos2))
            (when (<= (nth (1+ pos1) poslist) 1)
              (setq pos1 -1)
              )
            )
          (setq pos1 (- pos1 3))
          )
        )
      ;; set jump pos
      (setq pos (nth (mod (1+ pos2) (length poslist-filter)) poslist-filter))
      )
    (goto-char pos)
    )
  )

(defun ModTag:UpdateLineCount (startline endline)
  "Update line count between STARTLINE and ENDLINE."
  (let* ((linestart (line-beginning-position (- startline (line-number-at-pos (point)) -2)))
         (content-start (if ModTag:AllowReason
                            (+ startline 3)
                          (+ startline 2)))
         (content-end endline)
         (key (ModTag:ConstructKeyword))
         lines lines2 elseline keypos
         typepos type linestart2
         )
    (goto-char linestart)
    (search-forward ModTag:CmtStart)
    (setq typepos (point)
          linestart2 (- typepos (length ModTag:CmtStart))
          type (buffer-substring-no-properties typepos (+ typepos ModTag:TagMarkLen)))
    ;; get end position of line number part
    (search-forward key)
    (setq keypos (- (point) (length key)))
    ;; count lines and update line number part
    (cond
     ((string-equal ModTag:TagMarkCHG type)
      ;; count lines
      (goto-char (line-beginning-position (- content-start (line-number-at-pos (point)) -1)))
      (ModTag:PairPrecompile)
      (setq elseline (line-number-at-pos))
      ;; (message "CHG %d-%d-%d" (1+ content-start) elseline (1- content-end))
      (setq lines (ModTag:CountLinesInRegion (1+ content-start) elseline))
      (setq lines2 (ModTag:CountLinesInRegion (+ 1 elseline) (1- content-end)))
      ;; update line number part
      (delete-region typepos keypos)
      (goto-char typepos)
      (insert (format "%s[%d]_[%d] -> [%d]_[%d]%s" type (car lines) (nth 1 lines) (car lines2) (nth 1 lines2) ModTag:CmtSeperator))
      )
     (t
      ;; count lines
      (unless (and (string-equal ModTag:TagMarkADD type) (not ModTag:PreCompileMode))
        (setq content-start (1+ content-start)
              content-end (1- content-end))
        )
      ;; (message "%s %d %d %d %d" type startline content-start content-end endline)
      (setq lines (ModTag:CountLinesInRegion content-start content-end))
      ;; update line number part
      (delete-region typepos keypos)
      (goto-char typepos)
      (insert (format "%s[%d]_[%d]%s" type (car lines) (nth 1 lines) ModTag:CmtSeperator))
      )
     )
    )
  )

(defun ModTag:ModifyTagAdd ()
  "Add tags for add source."
  (interactive)
  (ModTag:ModifyTag ModTag:TagMarkADD))
(defun ModTag:ModifyTagChg ()
  "Add tags for change source."
  (interactive)
  (ModTag:ModifyTag ModTag:TagMarkCHG))
(defun ModTag:ModifyTagDel ()
  "Add tags for del source."
  (interactive)
  (ModTag:ModifyTag ModTag:TagMarkDEL))
(defun ModTag:CountLines ()
  "Update line count of current buffer."
  (interactive)
  (save-excursion
    (let ((parts (ModTag:ModificationList))
          )
      (dolist (part parts)
        (ModTag:UpdateLineCount (car part) (nth 1 part))
        )
      ))
  )
(defun ModTag:ManualCountLines ()
  "Calculate line count of selection range."
  (interactive)
  (let ((pos (ModTag:PointPos))
        lines
        )
    (when (car pos)
      (setq lines (ModTag:CountLinesInRegion (nth 1 pos) (nth 2 pos)))
      (message "Source:[%d][%d]" (car lines) (nth 1 lines))
      )
    )
  )

(defun ModTag:GrepAction (key root)
  "Generate command for grepping KEY recursively in the ROOT folder and run it."
  (let* ((key (shell-quote-argument key))
         (excludedir (if (> (length ModTag:ExcludeDirs) 0)
                         (concat " --exclude-dir=" (mapconcat #'shell-quote-argument ModTag:ExcludeDirs " --exclude-dir="))
                       ""))
         (excludefile (if (> (length ModTag:ExcludeFiles) 0)
                          (concat " --exclude=" (mapconcat #'shell-quote-argument ModTag:ExcludeFiles " --exclude="))
                        ""))
         (includefile (if (> (length ModTag:IncludeFiles) 0)
                          (concat " --include=" (mapconcat #'shell-quote-argument ModTag:IncludeFiles " --include="))
                        ""))
         (rootdir (shell-quote-argument root))
         (cmd (format "%s -nH %s -e %s -r%s%s%s %s" ModTag:GrepExe ModTag:GrepOption key excludedir excludefile includefile rootdir))
         )
    ;; (grep cmd)
    (let ((coding-system-for-read ModTag:TerminalCoding)
          (coding-system-for-write ModTag:TerminalCoding))
      (grep cmd)
      )
    )
  )

(defvar ModTag:LastGrepBuffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  (defadvice compilation-start (after ModTag:SaveLastGrepBuffer activate)
    (setq ModTag:LastGrepBuffer next-error-last-buffer))
  )

(defun ModTag:GrepResult ()
  "Get grep result."
  (save-excursion
    (let ((result '())
          oldpos
          pos
          pos1 ; pos of first seperator
          pos2 ; start pos of line number
          pos3 ; pos of second seperator
          pos4 ; start pos of text
          prop loc hitfile hitpos
          )
      (set-buffer ModTag:LastGrepBuffer)
      (setq oldpos -1
            pos (goto-char (point-min)))
      (while (not (eq pos oldpos))
        (setq prop (get-text-property pos 'compilation-message))
        (when prop
          (setq loc (compilation--message->loc prop)
                hitfile (car (caar (cddr loc)))
                hitpos (cadr loc)
                )
          (goto-char (+ pos (length hitfile) (length (number-to-string hitpos))))
          (search-forward ":")
          (setq result (cons (list hitfile
                                   hitpos
                                   (buffer-substring-no-properties (point) (line-end-position)))
                             result))
          )
        (beginning-of-line 2)
        (setq oldpos pos
              pos (point))
        )
      result
      )
    )
  )

(defun ModTag:Summary ()
  "Summarize line count."
  (let ((info (ModTag:GrepResult))
        (result '())
        line_a1 line_a2                 ;; line count for ADD
        line_c1 line_c2 line_c3 line_c4 ;; line count for CHG
        line_d1 line_d2                 ;; line count for DEL
        lastinfo
        line typepos type
        )
    (dolist (item info)
      (setq line (nth 2 item)
            typepos (+ (string-match (regexp-quote ModTag:CmtStart) line) (length ModTag:CmtStart))
            type (substring-no-properties line typepos (+ typepos ModTag:TagMarkLen))
            line_a1 0
            line_a2 0
            line_c1 0
            line_c2 0
            line_c3 0
            line_c4 0
            line_d1 0
            line_d2 0
            )
      (cond ((string-equal type ModTag:TagMarkADD)
             (progn
               (string-match "\\[\\([0-9]+\\)\\]_\\[\\([0-9]+\\)\\]" line typepos)
               (setq line_a1 (string-to-number (match-string 1 line))
                     line_a2 (string-to-number (match-string 2 line))
                     )
               ))
            ((string-equal type ModTag:TagMarkCHG)
             (progn
               (string-match "\\[\\([0-9]+\\)\\]_\\[\\([0-9]+\\)\\] -> \\[\\([0-9]+\\)\\]_\\[\\([0-9]+\\)\\]" line typepos)
               (setq line_c1 (string-to-number (match-string 1 line))
                     line_c2 (string-to-number (match-string 2 line))
                     line_c3 (string-to-number (match-string 3 line))
                     line_c4 (string-to-number (match-string 4 line))
                     )
               ))
            ((string-equal type ModTag:TagMarkDEL)
             (progn
               (string-match "\\[\\([0-9]+\\)\\]_\\[\\([0-9]+\\)\\]" line typepos)
               (setq line_d1 (string-to-number (match-string 1 line))
                     line_d2 (string-to-number (match-string 2 line))
                     )
               ))
            )
      (setq lastinfo (lax-plist-get result (car item)))
      (if lastinfo
          (progn
            (setq line_a1 (+ line_a1 (nth 0 lastinfo))
                  line_a2 (+ line_a2 (nth 1 lastinfo))
                  line_c1 (+ line_c1 (nth 2 lastinfo))
                  line_c2 (+ line_c2 (nth 3 lastinfo))
                  line_c3 (+ line_c3 (nth 4 lastinfo))
                  line_c4 (+ line_c4 (nth 5 lastinfo))
                  line_d1 (+ line_d1 (nth 6 lastinfo))
                  line_d2 (+ line_d2 (nth 7 lastinfo))
                  result (lax-plist-put result (car item) (list line_a1 line_a2 line_c1 line_c2 line_c3 line_c4 line_d1 line_d2))
                  )
            )
        (setq result (append result (list (car item) (list line_a1 line_a2 line_c1 line_c2 line_c3 line_c4 line_d1 line_d2))))
        )
      )
    result
    ))

(defun ModTag:RootDir ()
  "Get project root dir."
  (let* ((curdir (file-name-directory (buffer-file-name)))
         (tag-root1 (locate-dominating-file curdir "TAGS"))
         (tag-root2 (locate-dominating-file curdir "GTAGS"))
         (result (directory-file-name (cond
                                       (tag-root1 tag-root1)
                                       (tag-root2 tag-root2)
                                       (t
                                        (read-directory-name "Set root dir:" curdir)
                                        ))))
         )
    result
    )
  )

(defun ModTag:GrepFiles ()
  "Grep files for current modification work."
  (interactive)
  ;; (ModTag:GrepAction (ModTag:ConstructKeyword) (ModTag:RootDir))
  (ModTag:GrepAction (regexp-quote (ModTag:ConstructKeyword)) (ModTag:RootDir))
  )

(defun ModTag:BatchCountLines ()
  "Update line count of current modification work after grep."
  (interactive)
  "Update line count of every file in the grep result."
  (let ((info (ModTag:GrepResult))
        (filelist '())
        buffer
        )
    (dolist (item info)
      (unless (member (car item) filelist)
        (setq filelist (push (car item) filelist))
        (setq buffer (find-file-noselect (car item)))
        (set-buffer buffer)
        (ModTag:CountLines)
        (save-buffer)
        (kill-buffer buffer)
        )
      )
    )
  )

(with-eval-after-load 'evil-leader
  (evil-leader/set-key
    "ta" 'ModTag:ModifyTagAdd
    "tc" 'ModTag:ModifyTagChg
    "td" 'ModTag:ModifyTagDel
    "tu" 'ModTag:CountLines
    "tm" 'ModTag:ManualCountLines
    "tg" 'ModTag:GrepFiles
    )
  )

(provide 'modtags)
;;; modtags.el ends here
