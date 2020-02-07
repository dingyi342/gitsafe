;;; gitsafe.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(require 'magit)

(defcustom gitsafe-auto-save-silent nil
  "Nothing to dirty minibuffer if this option is non-nil."
  :type 'boolean
  :group 'gitsafe)

(defcustom gitsafe-auto-save-when-idle t
  "Save current buffer automatically when Emacs is idle."
  :group 'gitsafe
  :type 'boolean
  :package-version '(gitsafe . "0.2.0"))

(defcustom gitsafe-auto-save-idle-duration 1
  "The number of seconds Emacs has to be idle, before auto-saving the current buffer.
See `gitsafe-auto-save-when-idle'."
  :group 'gitsafe
  :type 'integer
  :package-version '(gitsafe . "0.2.0"))

;;; custom

(defgroup gitsafe nil
  "Smart-saving of buffers."
  :group 'tools
  :group 'convenience)

(defcustom gitsafe-magit-stage-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'gitsafe
  :type 'boolean
  :package-version '(gitsafe . "0.3.0"))

(defcustom gitsafe-magit-stage-exclude '(".elc" ".gpg" ".el.gz")
  "A list of regexps for buffer-file-name excluded from gitsafe.
When a buffer-file-name matches any of the regexps it is ignored."
  :group 'gitsafe
  :type '(repeat (choice regexp))
  :package-version '(gitsafe . "0.4.0"))

(defun gitsafe-magit-stage-include-p (filename)
  "Return non-nil if FILENAME doesn't match any of the `gitsafe-magit-stage-exclude'."
  (let ((checks gitsafe-magit-stage-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
                          (if (stringp (car checks))
                              (string-match (car checks) filename))))
            checks (cdr checks)))
    keepit))

(defcustom gitsafe-auto-save-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'gitsafe
  :type 'boolean
  :package-version '(gitsafe . "0.3.0"))

(defcustom gitsafe-auto-save-exclude '(".gpg" ".elc" ".el.gz")
  "A list of regexps for buffer-file-name excluded from gitsafe.
When a buffer-file-name matches any of the regexps it is ignored."
  :group 'gitsafe
  :type '(repeat (choice regexp))
  :package-version '(gitsafe . "0.4.0"))

(defun gitsafe-auto-save-include-p (filename)
  "Return non-nil if FILENAME doesn't match any of the `gitsafe-auto-save-exclude'."
  (let ((checks gitsafe-auto-save-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
                          (if (stringp (car checks))
                              (string-match (car checks) filename))))
            checks (cdr checks)))
    keepit))

;;; gitsafe-auto-save-command

;;;; buffer 判断,是否执行自动保存

(defvar gitsafe-auto-save-predicators-list '()
  "gitsafe auto-save predicators")

(defun gitsafe-auto-save-basic-predicator ()
  "A basic predicator for auto-saving

If the buffer is read not only or not associated with a file, and the file is
 writable and the buffer is modified. Do auto-save.
"
  (and (buffer-live-p (current-buffer))
       (buffer-file-name)
       (buffer-modified-p)
       (file-writable-p (buffer-file-name))
       (file-exists-p (buffer-file-name))
       ))

(defun gitsafe-auto-save-exclude-yasnippet ()
  "当 yasnippet 激活的时候"
  (or (not (boundp 'yas--active-snippets))
      (not yas--active-snippets)))

(defun gitsafe-auto-save-exclude-company ()
  "当 compnay 激活的时候,不自动保存"
  (or (not (boundp 'company-candidates))
      (not company-candidates)))

(defun gitsafe-auto-save-include-files ()
  "是否是包括的文件"
  (gitsafe-auto-save-include-p (buffer-file-name)))

(defun gitsafe-auto-save-include-remote-files ()
  "是否包括多远程文件"
  (if (when buffer-file-name (file-remote-p (buffer-file-name))) gitsafe-auto-save-remote-files t))

(defun gitsafe-auto-save-only-include-git-track-file ()
  "只自动保存被跟踪的文件"
  (gitsafe-git-track-p))

(add-to-list 'gitsafe-auto-save-predicators-list 'gitsafe-auto-save-basic-predicator)
(add-to-list 'gitsafe-auto-save-predicators-list 'gitsafe-auto-save-exclude-yasnippet)
(add-to-list 'gitsafe-auto-save-predicators-list 'gitsafe-auto-save-exclude-company)
(add-to-list 'gitsafe-auto-save-predicators-list 'gitsafe-auto-save-include-files)
(add-to-list 'gitsafe-auto-save-predicators-list 'gitsafe-auto-save-include-remote-files)
(add-to-list 'gitsafe-auto-save-predicators-list 'gitsafe-auto-save-only-include-git-track-file)

;;;; 保存命令

(defun gitsafe-auto-save--command (&optional force-save-p)
  "Auto save buffers

If FORCE-SAVE-P is non-nil, force save all possible buffers. Otherwise filting
buffers by using all predicators in `gitsafe-auto-save-predicators-list'.
"
  (let ((gitsafe-auto-save-buffer-list))
      (save-excursion
        (dolist (buf (buffer-list))
          (set-buffer buf)
          (when (or force-save-p
                    (not (memq nil (mapcar #'funcall gitsafe-auto-save-predicators-list))))
            (push (buffer-name) gitsafe-auto-save-buffer-list)
            (if gitsafe-auto-save-silent
                ;; `inhibit-message' can shut up Emacs, but we want
                ;; it doesn't clean up echo area during saving
                (with-temp-message ""
                  (let ((inhibit-message t))
                    (basic-save-buffer)))
              (basic-save-buffer))
            ))
        ;; Tell user when auto save files.
        (unless gitsafe-auto-save-silent
          (cond
           ;; It's stupid tell user if nothing to save.
           ((= (length gitsafe-auto-save-buffer-list) 1)
            (message "# Saved %s" (car gitsafe-auto-save-buffer-list)))
           ((> (length gitsafe-auto-save-buffer-list) 1)
            (message "# Saved %d files: %s"
                     (length gitsafe-auto-save-buffer-list)
                     (mapconcat 'identity gitsafe-auto-save-buffer-list ", ")))))
        )))

(defun gitsafe-auto-save-command ()
  (interactive)
  ;; 删除了一个文件老是告诉我 run-idle 错误,那个文件不存在.
  ;; 因为删除了文件,那个 buffer 没删除.
  (gitsafe-auto-save--command))

;;; idle 自动保存

(defvar gitsafe-auto-save-idle-timer nil)
;; (setq gitsafe-auto-save-idle-timer nil)

(defun gitsafe-auto-save-initialize-idle-timer ()
  "Initialize gitsafe idle timer if `gitsafe-auto-save-when-idle' is true."
  (setq gitsafe-auto-save-idle-timer
        (when gitsafe-auto-save-when-idle
          (run-with-idle-timer gitsafe-auto-save-idle-duration t #'gitsafe-auto-save-command))))

(defun gitsafe-auto-save-stop-idle-timer ()
  "Stop gitsafe idle timer if `gitsafe-auto-save-idle-timer' is set."
  (when gitsafe-auto-save-idle-timer
    (cancel-timer gitsafe-auto-save-idle-timer)
    (cancel-function-timers 'gitsafe-auto-save-command)))

(defun gitsafe-auto-save-enable ()
  (gitsafe-auto-save-initialize-idle-timer)
  (add-hook 'before-save-hook 'font-lock-flush))

(defun gitsafe-auto-save-disable ()
  (gitsafe-auto-save-stop-idle-timer)
  (remove-hook 'before-save-hook 'font-lock-flush))

;;; gitsafe 判断函数

(defun gitsafe-buffer-useful-p ()
  "就是正常要编辑的 buffer"
  (and
   (not (minibufferp))
   (buffer-file-name)
   (or
    (derived-mode-p 'prog-mode)
    (derived-mode-p 'text-mode))
   (buffer-live-p (current-buffer))
   (file-writable-p (buffer-file-name))))

(defun gitsafe-git-ignore-p ()
  "current file is ignore-p"
  (member
   (when (gitsafe-buffer-useful-p)
     (file-name-nondirectory (buffer-file-name)))
   (magit-ignored-files)))

(defun gitsafe-git-repo-p ()
  "当前文件是否在一个 git repo 中"
  (when-let ((root (magit-toplevel default-directory)))
    (magit-git-repo-p root)))

(defun gitsafe-git-track-p ()
  "当前文件在一个 git repo 并且被 track"
   ;; (when (gitsafe-git-repo-p)
    (magit-file-tracked-p (buffer-file-name (current-buffer))))

(defun gitsafe-buffer-anything-modified-p ()
  "对已跟踪的文件,有任何未暂存的或已暂存的,就是没提交完全.
未跟踪的没意义,排除.s"
  (and
   (gitsafe-buffer-useful-p)
   (gitsafe-git-track-p)
   (magit-anything-modified-p nil (buffer-file-name))))

(defun gitsafe-buffer-anything-unstaged-p ()
  "先判断当前文件是否被跟踪,其次是否要未提交的内容.
如果未跟踪,也会提示未提交的,但是对我无意义."
  (and
   ;; (buffer-file-name)
   ;; (file-writable-p (buffer-file-name))
   (gitsafe-buffer-useful-p)
   (gitsafe-git-track-p)
   (magit-anything-unstaged-p nil (buffer-file-name))))

(defun gitsafe-buffer-only-uncommited-p ()
  "当前 buffer 只有未提交内容"
  (and (gitsafe-buffer-useful-p)
       (gitsafe-git-track-p)
       (not (gitsafe-buffer-anything-unstaged-p))
       (gitsafe-buffer-anything-modified-p)))

(defun gitsafe-buffer-unsafe-p ()
  "判断当前文件是否被修改,如果有返回 t,如果没有再判断是否有没暂存的."
  (unless (buffer-modified-p)
    (gitsafe-buffer-anything-unstaged-p)))

(defun gitsafe-buffer-safe-p ()
  "判断当前是否安全."
  (and (gitsafe-buffer-useful-p)
       (not (gitsafe-buffer-unsafe-p))))

;;; gitsafe-cleanup-buffer
(defun gitsafe-cleanup-buffer ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      ;; (message "white space cleaned")
      )))

(defun gitsafe-delete-trailing-whitespace-except-current-line ()
  "删除当前 buffer 内除了当前行的行尾空格"
  (interactive)
  (let ((begin (line-beginning-position))
	(end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
	(save-restriction
	  (narrow-to-region (point-min) (1- begin))
	  (delete-trailing-whitespace)))
      (when (> (point-max) end)
	(save-restriction
	  (narrow-to-region (1+ end) (point-max))
	  (delete-trailing-whitespace))))))

;;; gitsafe-save-buffer
(defun gitsafe-save-buffer ()
  (let ((inhibit-message t))
    (gitsafe-cleanup-buffer))
  (save-buffer))

;;; gitsafe-magit-stage-command
(defvar gitsafe-magit-display-uncommit nil)
(defvar gitsafe-magit-display-unstaged t)

(defun gitsafe-magit-stage--command ()
  (gitsafe-save-buffer)
  ;; 跟踪文件
  (unless (gitsafe-git-track-p)
         (when (y-or-n-p "this file is not tracking by magit ,[y]track this file?")
           (magit-stage-file (magit-current-file))
           (message "magit tracks current file")))
  (when (gitsafe-buffer-only-uncommited-p)
    (if gitsafe-magit-display-uncommit
        (let ((magit-display-buffer-function 'gitsafedisplay-buffer-traditional))
          (magit-diff-buffer-file)
          (run-with-timer 0.5 nil '(lambda ()
                                     (gitsafesection-show-level-2-all)
                                     (beginning-of-buffer)
                                     (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
                                     ))
          (message "this file only have something uncommited."))
      (message "this file only have something uncommited.")))
  (when (gitsafe-buffer-anything-unstaged-p)
    (if gitsafe-magit-display-unstaged
        (progn
          (magit-diff-unstaged nil (list (magit-file-relative-name)))
          (magit-section-show-level-3-all)
	  (goto-char (point-min))
          (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
          (message "this file have something unstaged and uncommited"))
      (message "this file have something unstaged and uncommited")))
  (unless (gitsafe-buffer-anything-modified-p)
         (message "this file commit completely.")))

(defun gitsafe-magit-stage-command ()
  "Save the current buffer if needed."
  (interactive)
  (when (and (gitsafe-buffer-useful-p)
             (if (file-remote-p buffer-file-name) gitsafe-magit-stage-remote-files t)
             (gitsafe-buffer-anything-modified-p)
             (gitsafe-magit-stage-include-p buffer-file-name)
             (not (gitsafe-git-ignore-p)))
    (gitsafe-magit-stage--command)))

;;; gitsafe-kill-this-buffer

;;;###autoload
(defun gitsafe-kill-this-buffer (&optional arg)
  "判断当前 buffer 是否全部暂存,是否已经被跟踪,不行提交暂存,行就杀 buffer"
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (gitsafe-buffer-anything-unstaged-p)
        (gitsafe-magit-stage-command)
      (if (equal '(4) arg)
          (kill-buffer)
        (kill-buffer-and-window)))))
;; 绑定到 M-k
;;; gitsafe-after-find-file
(defun gitsafe-after-find-file ()
  "find file hook run magiit-save-command"
  (interactive)
  (run-with-timer
   1 nil (lambda () (interactive)
           (let ((magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1))
             (gitsafe-magit-stage-command)))))

;; (add-hook 'find-file-hook 'gitsafe-after-find-file)

;;; gitsafe-save-all
;;;###autoload
(defun gitsafe-save-all ()
  "遍历 buffer 列表,发现有没有没暂存的,.有的话就就弹出来
这个不紧能对当前的 buffer,还能对 buffer 列表有用."
  (interactive)
  (let (gitsafe-buffer-unstaged-list)
    (save-excursion
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (and (gitsafe-buffer-useful-p)
		   (gitsafe-git-track-p)
		   (gitsafe-buffer-anything-unstaged-p))
	  (cl-pushnew buf gitsafe-buffer-unstaged-list))))
    (if gitsafe-buffer-unstaged-list
	(progn
	  (set-buffer (pop gitsafe-buffer-unstaged-list))
	  (gitsafe-magit-stage-command))
      (message "staged all"))))

;;; gitsafe-kill-emacs
(defun gitsafe-kill-emacs (old-func &rest args)
  "遍历 buffer 列表,发现有没有没暂存的,没有就问是否要真的退出 emacs.有的话就就弹出来"
  (let (gitsafe-buffer-unstaged-list)
    (save-excursion
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (and (gitsafe-buffer-useful-p)
		   (gitsafe-git-track-p)
		   (gitsafe-buffer-anything-unstaged-p))
	  (cl-pushnew buf gitsafe-buffer-unstaged-list))))
    (if gitsafe-buffer-unstaged-list
	(let ((buf (pop gitsafe-buffer-unstaged-list)))
	  (if (eq (current-buffer) buf)
	    (gitsafe-magit-stage-command)
	    (switch-to-buffer buf)))
      (when (y-or-n-p "really quit emacs:")
	(apply old-func args)))))

;; (nmap :keymaps 'override "M-s" 'gitsafe-save-all)
;; (advice-add 'kill-emacs :around 'gitsafe-kill-emacs)
;; (advice-add 'save-buffers-kill-emacs :around 'gitsafe-kill-emacs)
;; (advice-add 'save-buffers-kill-terminal :around 'gitsafe-kill-emacs)

;;; gitsafe-switch-buffer-functions
(defun gitsafe-switch-buffer-functions (prev curr)
  (cl-assert (eq curr (current-buffer)))  ;; Always t
  (with-current-buffer prev
    (when (and (gitsafe-buffer-useful-p)
	       ;; (gitsafe-git-track-p)
	       )
      (with-current-buffer curr
        (when (and (gitsafe-buffer-useful-p)
		   (gitsafe-git-track-p))
          (gitsafe-magit-stage-command))))))

;; 实际上是对 window-buffer-change-functioin 的一个扩展.
;; 传入 prev-buffer 和 cur-buffer,更好的判断.
;; (add-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)

;;; gitsafe-mode
(defun gitsafe-enable ()
  ;; 只对 git track 的文件实行自动保存.
  (gitsafe-auto-save-enable)
  ;; 只在两个都是 git track 的 buffer 切换时触发.
  (add-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)
  (advice-add 'kill-emacs :around 'gitsafe-kill-emacs)
  (advice-add 'save-buffers-kill-emacs :around 'gitsafe-kill-emacs)
  (advice-add 'save-buffers-kill-terminal :around 'gitsafe-kill-emacs)
  (add-hook 'find-file-hook 'gitsafe-after-find-file))

(defun gitsafe-disable ()
  (gitsafe-auto-save-disable)
  (remove-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)
  (advice-remove 'kill-emacs 'gitsafe-kill-emacs)
  (advice-remove 'save-buffers-kill-emacs 'gitsafe-kill-emacs)
  (advice-remove 'save-buffers-kill-terminal 'gitsafe-kill-emacs)
  (remove-hook 'find-file-hook 'gitsafe-after-find-file))

;;;###autoload
(define-minor-mode gitsafe-mode
  "A minor mode that saves your Emacs buffers when predicate"
  :global t
  (cond
   (gitsafe-mode (gitsafe-enable))
   (t (gitsafe-disable))))

;;; gitsafe.el ends here.
(provide 'gitsafe)
