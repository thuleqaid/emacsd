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
         (filelist (directory-files (file-name-directory curfile) t (format "^%s\.html\?$" (file-name-base curfile)) ))
         (restfiles (length filelist)))
    (if (> restfiles 0)
        (progn
          (setq curfile (car filelist))
          (unless (symbol-function 'tramp-tramp-file-p)
            (require 'tramp))
          (if (tramp-tramp-file-p curfile)
              (error "Cannot open tramp file")
            (progn
              (message "File Openning...")
              (browse-url (concat "file://" curfile)))
            ))
      (message "No HTML File Found")
      )))

(defun kill-all-buffers ()
  "kill all buffers."
  (interactive)
  (let ((buffers (buffer-list))
        (curbuffer (current-buffer))
        bname)
    (dolist (curbuffer buffers)
      (setq bname (buffer-name curbuffer))
      ;; kill all other buffers except *scratch* and *Messages*
      (unless (or (string= bname "*scratch*") (string= bname "*Messages*"))
        (kill-buffer curbuffer)))
    (switch-to-buffer "*scratch*")))

(defun kill-other-buffers ()
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

(defun kill-other-dired-buffers ( &optional current-buf)
  "kill all dired-buffers and diredp-w32-drivers-mode(w32 use this mode )
  except current-buf ,if current-buf is nil then kill all"
  (interactive)
  (let ((buffers (buffer-list))
        (curbuffer (current-buffer))
        iterbuffer
        )
    (dolist (iterbuffer buffers)
      (with-current-buffer iterbuffer
        (when (and (not (eq curbuffer iterbuffer))
                   (or (eq 'dired-mode major-mode)
                       (eq 'diredp-w32-drivers-mode major-mode)
                       )
                   )
          (kill-buffer iterbuffer)
          )))))

(defun thuleqaid/coding-system (func coding)
  "Set coding system for external program"
  (eval-expression
   `(defadvice ,func (around ,(make-symbol (format "coding-system/%s/%s" func coding)) activate)
      (let ((coding-system-for-read ',coding)
            (coding-system-for-write ',coding))
        ad-do-it
        )
      )
   )
  )

(global-set-key (kbd "<f8>") 'browse-current-file)


(provide 'init-utils)
