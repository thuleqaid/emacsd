(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun sanityinc/directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the HTML file with same basename as current buffer as a URL using `browse-url'."
  (interactive)
  (let* ((curfile (buffer-file-name))
         (filelist (directory-files (file-name-directory curfile) t (file-name-base curfile)))
         (restfiles (length filelist))
         (curext (downcase (file-name-extension curfile))))
    (while (> restfiles 0)
      (setq curfile (car filelist))
      (setq filelist (cdr filelist))
      (setq restfiles (length filelist))
      (setq curext (downcase (file-name-extension curfile)))
      (when (or (string= curext "htm") (string= curext "html"))
        (setq restfiles -1)))
    (when (< restfiles 0)
      ;; (unless (symbol-function 'tramp-tramp-file-p)
      ;;   (require 'tramp))
      (if (tramp-tramp-file-p curfile)
          (error "Cannot open tramp file")
        (browse-url (concat "file://" curfile))))))
(global-set-key (kbd "<f8>") 'browse-current-file)

(defun clear-buffer ()
  "kill all buffers."
  (interactive)
  (let ((buffers (buffer-list))
        (curbuffer (current-buffer))
        bname)
    (dolist (curbuffer buffers)
      (setq bname (buffer-name curbuffer))
      ;; kill all other buffers except *scratch* and *Messages*
      (unless (or (string= bname "*scratch*") (string= bname "*Messages*"))
        (kill-buffer curbuffer)))))
(defun retain-buffer ()
  "kill all buffers but the current one."
  (interactive)
  (let ((buffers (buffer-list))
        (curbuffer (current-buffer))
        iterbuffer
        bname)
    (dolist (iterbuffer buffers)
      (unless (equal iterbuffer curbuffer)
        (setq bname (buffer-name iterbuffer))
        ;; kill all other buffers except *scratch* and *Messages*
        (unless (or (string= bname "*scratch*") (string= bname "*Messages*"))
          (kill-buffer iterbuffer))
        ))))


(provide 'init-utils)
