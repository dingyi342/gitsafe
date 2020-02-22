;;; gitsafe.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(require 'magit)

(defcustom gitsafe-auto-save-silent t
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
(defvar gitsafe-auto-save-only-work-on-current-buffer t
  "只对当前 buffer auto-save")

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

(defun gitsafe-auto-save-all-command (&optional force-save-p)
  "Auto save buffers

If FORCE-SAVE-P is non-nil, force save all possible buffers. Otherwise filting
buffers by using all predicators in `gitsafe-auto-save-predicators-list'.
"
  (let ((gitsafe-auto-save-buffer-list))
    ;; 因为如果文件不存在的话,会一直报错
    (ignore-errors
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
            )))
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

(defun gitsafe-auto-save-current-command ()
  "只保存当前 buffer "
  (when (not (memq nil (mapcar #'funcall gitsafe-auto-save-predicators-list)))
        (if gitsafe-auto-save-silent
            ;; `inhibit-message' can shut up Emacs, but we want
            ;; it doesn't clean up echo area during saving
            (with-temp-message ""
              (let ((inhibit-message t))
                (basic-save-buffer)))
          (basic-save-buffer))))

(defun gitsafe-auto-save-command ()
  ;; (interactive)
  ;; 删除了一个文件老是告诉我 run-idle 错误,那个文件不存在.
  ;; 因为删除了文件,那个 buffer 没删除.
  (if gitsafe-auto-save-only-work-on-current-buffer
      (gitsafe-auto-save-current-command)
    (gitsafe-auto-save-all-command)))

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
  ;; 这个会导致在有js的org-mode里保存的时候,把搜狗输入法切换..
  ;; (add-hook 'before-save-hook 'font-lock-flush)
  )

(defun gitsafe-auto-save-disable ()
  (gitsafe-auto-save-stop-idle-timer)
  ;; (remove-hook 'before-save-hook 'font-lock-flush)
  )

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
  ;; (vc-git-registered (buffer-file-name))
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

(defvar gitsafe-my-git-repo '("~/.emacs.d/"))
(defun gitsafe-my-git-repo-p ()
  (member (magit-toplevel default-directory) gitsafe-my-git-repo))

;;; gitsafe-cleanup-buffer
(defun gitsafe-cleanup-buffer ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
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
    (when (gitsafe-my-git-repo-p)
      (gitsafe-cleanup-buffer)))
  (save-buffer))

;;; gitsafe-magit-stage-command
(defvar gitsafe-magit-display-uncommit nil
  "magit 是否 popup 显示 uncommmit 的内容")

(defvar gitsafe-magit-display-unstaged t
  " magit 是否 popup 显示 unstaged 的内容")

(defvar gitsafe-commit-buffer-file-buffer nil
  "magit stage 命令作用的 buffer")

(defvar gitsafe-magit-query-untrack t
  "询问是否要 track 当前文件")

(defun gitsafe-magit-stage--command ()
  ;; 清理 buffer 并保存.
  (gitsafe-save-buffer)
  ;; 设置变量
  (setq gitsafe-commit-buffer-file-buffer (current-buffer))
  ;; 跟踪文件
  (unless (gitsafe-git-track-p)
    (if gitsafe-magit-query-untrack
        (when (y-or-n-p "this file is not tracking by magit ,track this file[y/n]?")
          (magit-stage-file (magit-current-file))
          (message "magit tracks current file"))
      (message "Untrack")))
  (when (gitsafe-buffer-only-uncommited-p)
    (if gitsafe-magit-display-uncommit
        (let ((magit-display-buffer-function 'gitsafedisplay-buffer-traditional))
          (magit-diff-buffer-file)
          (run-with-timer 0.5 nil '(lambda ()
                                     (gitsafesection-show-level-2-all)
                                     (beginning-of-buffer)
                                     (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
                                     ))
          (message  "Uncommited"))
      (message "Uncommited")))
  (when (gitsafe-buffer-anything-unstaged-p)
    (if gitsafe-magit-display-unstaged
        (progn
          (magit-diff-unstaged nil (list (magit-file-relative-name)))
          (magit-section-show-level-3-all)
          (goto-char (point-min))
          (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
          (message "Unstaged and uncommited"))
      (message "Unstaged and uncommited")))
  (unless (gitsafe-buffer-anything-modified-p)
    (message "this file commit completely.")))

;;;###autoload
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

(defun gitsafe-kill-buffer-hook ()
  (if (gitsafe-buffer-anything-unstaged-p)
        (gitsafe-magit-stage-command)))

;; (add-hook 'kill-buffer-query-functions 'gitsafe-kill-buffer-query-functions)
;; (setq kill-buffer-query-functions nil)

;; 这会导致每次保存 buffer 前都会触发,不懂...
;; (advice-add 'kill-buffer :around (lambda (old-func &rest args)
;;                                    (if (gitsafe-buffer-anything-unstaged-p)
;;                                        (gitsafe-magit-stage-command)
;;                                      (apply old-func args))))
;; (advice-remove 'kill-buffer (lambda (old-func &rest args)
;;                                    (if (gitsafe-buffer-anything-unstaged-p)
;;                                        (gitsafe-magit-stage-command)
;;                                      (apply old-func args))))

;; 绑定到 M-k
;;; gitsafe-after-find-file
(defun gitsafe-after-find-file ()
  "find file hook run magiit-save-command"
  (run-with-timer
   1 nil (lambda () (interactive)
           (let ((magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1))
             (gitsafe-magit-stage-command)))))

;; (add-hook 'find-file-hook 'gitsafe-after-find-file)

;;; gitsafe/magit-stage-all
;;;###autoload
(defun gitsafe/magit-stage-all ()
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
      (message "staged all")
      ;; 如果都暂存了,就对当前文件执行,就是显示 magit-diff 窗口,然后可以继续执行提交等
      (magit-diff-unstaged nil (list (magit-file-relative-name))))))

;;; gitsafe/magit-stage-current-buffer

;;;###autoload
(defun gitsafe/magit-stage-current-buffer ()
  "magit stage 当前文件,处理未跟踪的情况,和窗口打开后的情况"
  (interactive)
  ;; 先保存文件
  (gitsafe-save-buffer)
  ;; 保存当前 buffer 对象到
  (setq gitsafe-commit-buffer-file-buffer (current-buffer))
  ;; 处理未跟踪的情况
  (unless (magit-file-tracked-p (buffer-file-name (current-buffer)))
    (when (y-or-n-p "this file not track by magit ,[y]track this file")
      (magit-stage-file (magit-current-file))
      (message "magit tracks current file")))
  ;; 当文件存在 modified 的时候
  (when (gitsafe-buffer-anything-modified-p)
    (magit-diff-unstaged nil (list (magit-file-relative-name)))
    (goto-char (point-min))
    (magit-section-show-level-3-all)
    (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))))

;;; gitsafe/magit-commit-current-buffer
(defun gitsafe/magit-commit-buffer (buffer &optional amend)
  "提交当前 buffer"
  (let ((args (list "--only" (buffer-file-name buffer)))
	amend)
    (if amend
	(magit-commit-amend args)
      (magit-commit-create args))))

;; (defun gitsafe/magit-commit-current-buffer (&optional amend)
;;   (interactive)
;;   (gitsafe/magit-commit-buffer (current-buffer) amend))

;;;###autoload
(defun gitsafe/magit-diff-buffer-file ()
  "调整 magit-diff-buffer-file 的默认行为."
  (interactive)
  (magit-diff-buffer-file)
  (run-with-idle-timer
   0.01 nil (lambda ()
	      (magit-section-show-level-2-all)
	      (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
	      (goto-char (point-min))
	      ))
  ;; (my-toggle-current-window-dedication)
  )

;; 不管暂存区的其他文件,只把暂存区内当前文件,commit.
;; (magit-commit-at-point)
;; (magit-commit-create (list (format "--only %s" (buffer-file-name (current-buffer)))))
;; 传的参数要是个列表,就是一个一个的.不能混成一个
;; (magit-commit-create (list "--only" (buffer-file-name (current-buffer))))
;; (defvar tx/magit-commit-current-file )
;;;###autoload
(defun gitsafe/magit-commit-current-buffer (&optional amend)
  "提交当前文件,如果是在当前要提交的 buffer 里,直接传(current-buffer),
如果是在显示当前文件 unstaged 的 magit-pop 里,则利用 window-parameter 参数里储存的之前所谓当前文件的窗口信息,来提供 buffer 对象
amend 为t 则执行 commit-amend"
  (interactive)
  (if (string-prefix-p "magit-diff" (buffer-name (current-buffer)))
      ;; 如果当前的窗口,是 magit-diff 窗口,就 commit  magit-diff 升起的那个窗口的文件
      (let* ((buffer gitsafe-commit-buffer-file-buffer)
	     ;; (window-buffer (car (last (window-parameter (selected-window) 'quit-restore) 2))))
	     (args (list "--only" (buffer-file-name buffer))))
	;; (setq gitsafe-magit-current-buffer buffer)
	;; 隐藏 magit-diff buffer.
	(magit-mode-bury-buffer)
	;; 提交时候升起的 diff 窗口,改成当前文件.
	(remove-hook 'server-switch-hook 'magit-commit-diff)
	(add-hook 'server-switch-hook 'gitsafe/magit-commit-buffer-file-diff)
	(if amend
	    (magit-commit-amend args)
	  (magit-commit-create args)))
    (let ((magit-commit-show-diff t))
      (remove-hook 'server-switch-hook 'gitsafe/magit-commit-buffer-file-diff)
      (add-hook 'server-switch-hook 'magit-commit-diff)
      (if amend
	  (gitsafe/magit-commit-buffer (current-buffer) t)
	(gitsafe/magit-commit-buffer (current-buffer))))))

(add-hook 'with-editor-post-finish-hook
          (lambda ()
            (add-hook 'server-switch-hook 'magit-commit-diff)
            (remove-hook 'server-switch-hook 'gitsafe/magit-commit-buffer-file-diff)))

(add-hook 'with-editor-post-cancel-hook
          (lambda ()
            (add-hook 'server-switch-hook 'magit-commit-diff)
            (remove-hook 'server-switch-hook 'gitsafe/magit-commit-buffer-file-diff)))

;;;###autoload
(defun gitsafe/magit-amend-commit-current-buffer ()
  "对当前文件的提交 amend 到之前的 commit"
  (interactive)
  (gitsafe/magit-commit-current-buffer t)
  )

(defun gitsafe/magit-commit-buffer-file-diff (&optional buffer)
  "magit-commit-diff 是用来控制 commit 时升起的 diff 窗口的,"
  (let* ((buffer (or buffer gitsafe-commit-buffer-file-buffer)))
    (when (and git-commit-mode magit-commit-show-diff)
      (when-let ((diff-buffer (gitsafe/magit-diff-buffer-file-get-buffer buffer)))
      ;; This window just started displaying the commit message
      ;; buffer.  Without this that buffer would immediately be
      ;; replaced with the diff buffer.  See #2632.
      (unrecord-window-buffer nil diff-buffer))
    (condition-case nil
        (let ((args (car (magit-diff-arguments)))
              (magit-inhibit-save-previous-winconf 'unset)
              ;; (magit-display-buffer-noselect t)
              (magit-display-buffer-noselect t)
              (inhibit-quit nil))
          (message "Diffing changes to be committed (C-g to abort diffing)")
          ;; (cl-case last-command
            ;; (magit-commit
             ;; (magit-diff-staged nil args)
             (with-current-buffer buffer
               (magit-diff-buffer-file))
             ;; )
            ;; (magit-commit-all
            ;;  (magit-diff-working-tree nil args))
            ;; ((magit-commit-amend
            ;;   magit-commit-reword
            ;;   magit-rebase-reword-commit)
            ;;  (magit-diff-while-amending args))
            ;; (t (if (magit-anything-staged-p)
            ;;        (magit-diff-staged nil args)
             ;;      (magit-diff-while-amending args))))
             )
      (setq gitsafe-commit-buffer-file-buffer nil)
      (quit)))))

(defun gitsafe/magit-diff-buffer-file-get-buffer (&optional buffer)
  "获取当前 buffer 对应的 magit-diff-buffer"
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer))
         (name (magit-file-relative-name file))
         (mode "magit-diff-mode"))
    (--each (buffer-list)
      (with-current-buffer it
        (when (and (string= major-mode mode)
                   (string-match-p
                    (if (stringp name) name)
                    (buffer-name)))
          (message (buffer-name it))
          it
          )))))

(defun gitsafe/magit-diff-buffer-file-switch-buffer ()
  "切换到与当前 buffer 对应的 magit-diff-buffer"
  (let* ((buffer (current-buffer))
         (file (buffer-file-name buffer))
         (name (magit-file-relative-name file))
         )
    (--each (buffer-list)
      (with-current-buffer it
        (when (and (string= major-mode "magit-diff-mode")
                   (string-match-p
                    (if (stringp name) name "")
                    (buffer-name)))
          (switch-to-buffer it)
          )))))

;;;###autoload
(defun gitsafe/magit-commit-current-buffer-fullscreen ()
  (interactive)
  (toggle-frame-fullscreen)
  (gitsafe/magit-commit-current-buffer))

(add-hook 'with-editor-post-finish-hook
	  (lambda ()
	    (when (frame-parameter nil 'fullscreen)
	      (set-frame-parameter nil 'fullscreen nil))))

(add-hook 'with-editor-post-cancel-hook
	  (lambda ()
	    (when (frame-parameter nil 'fullscreen)
	      (set-frame-parameter nil 'fullscreen nil))))

;;; gitsafe/magit-stage-current-then-all
;;;###autoload
(defun gitsafe/magit-stage-current-then-all ()
  "先尝试暂存当前 buffer,如果全部暂存了,就 buffer 列表逐一查看还没暂存的."
  (interactive)
  (gitsafe-save-buffer)
  (if (not (magit-file-tracked-p (buffer-file-name)))
      (when (y-or-n-p "this file not track by magit ,[y]track this file")
	(magit-stage-file (magit-current-file))
	(message "magit tracks current file"))
    (if (magit-anything-unstaged-p nil (buffer-file-name))
	(gitsafe/magit-stage-current-buffer)
      (gitsafe/magit-stage-all))))

;;; gitsafe-kill-emacs
;;;###autoload
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
      (message "stage-all")
      (apply old-func args))))

;; (nmap :keymaps 'override "M-s" 'gitsafe-save-all)
;; (advice-add 'kill-emacs :around 'gitsafe-kill-emacs)
;; (advice-add 'save-buffers-kill-emacs :around 'gitsafe-kill-emacs)
;; (advice-add 'save-buffers-kill-terminal :around 'gitsafe-kill-emacs)

;;; gitsafe-switch-buffer-functions
;; 实际上是对 window-buffer-change-function 的一个扩展.
;; 传入 prev-buffer 和 cur-buffer,更好的判断.
;; (add-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)
(defvar gitsafe-switch-buffer--last nil)
(defun gitsafe-switch-buffer-display-stage (&rest _)
  "add-hook 到 window-buffer-change-function, 不用传入的 frame 参数,而是用上一个和当前 buffer 两个参数.
也起到保存了作用."
  (unless (eq (current-buffer) gitsafe-switch-buffer--last)
    (let ((curr (current-buffer))
          (prev gitsafe-switch-buffer--last))
      (setq gitsafe-switch-buffer--last curr)
      (with-current-buffer prev
        (when (and (gitsafe-buffer-useful-p)
                   ;; (gitsafe-git-track-p)
                   )
          (with-current-buffer curr
            (when (and (gitsafe-buffer-useful-p)
                       (gitsafe-git-track-p))
                (gitsafe-magit-stage-command))))))))
;; 现在是弹出来 buffer 显示 stage 情况
;; 还可以不弹窗口,只显示 message
;; 还可以调用系统级的提示,比如用 hs.alert
;; 还可以和其他插件联动,比如 modeling,显示为红色,或者 tabbar 显示为红色来达到提示的效果.
;; 确实,弹出窗口太明显了.

;; 如果这是 message 的话,就可以不 prevbuffer 的限制放宽了.
;; 或者就不需要传入 prev buffer 的参数了.
;; 导致切换 buffer 有点卡顿.应该还是 magit 判断函数的性能导致的.
(defun gitsafe-switch-buffer-display-message (&rest _)
  (let ((gitsafe-magit-display-unstaged nil)
        (gitsafe-magit-query-untrack nil))
    (when (and (gitsafe-buffer-useful-p))
      (gitsafe-magit-stage--command))))
;;; gitsafe-mode
(defun gitsafe-enable ()
  ;; 只对 git track 的文件实行自动保存.
  (gitsafe-auto-save-enable)
  ;; 只在两个都是 git track 的 buffer 切换时触发.
  ;; 性能不好.
  ;; (add-hook 'window-buffer-change-functions 'gitsafe-switch-buffer-function)
  ;; (add-hook 'window-buffer-change-functions 'gitsafe-switch-buffer-display-message)
  (add-hook 'kill-buffer-hook 'gitsafe-kill-buffer-hook)
  (advice-add 'kill-emacs :around 'gitsafe-kill-emacs)
  (advice-add 'save-buffers-kill-emacs :around 'gitsafe-kill-emacs)
  (advice-add 'save-buffers-kill-terminal :around 'gitsafe-kill-emacs)
  (add-hook 'find-file-hook 'gitsafe-after-find-file))

(defun gitsafe-disable ()
  (gitsafe-auto-save-disable)
  ;; 性能不好
  ;; (remove-hook 'window-buffer-change-functions 'gitsafe-switch-buffer-function)
  ;; (remove-hook 'window-buffer-change-functions 'gitsafe-switch-buffer-display-message)
  (remove-hook 'kill-buffer-hook 'gitsafe-kill-buffer-hook)
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
