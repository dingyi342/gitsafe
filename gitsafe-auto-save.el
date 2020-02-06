;;; gitsafe-auto-save.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; 显示保存的消息吗

(defcustom gitsafe-auto-save-silent nil
  "Nothing to dirty minibuffer if this option is non-nil."
  :type 'boolean
  :group 'gitsafe)

;;; before-save 不设置,在 magit-stage 之前设置.

;;; [禁用]触发保存的命令

(defcustom gitsafe-auto-save-triggers
  '(kill-buffer switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer)
  "A list of commands which would trigger `gitsafe-auto-save-command'."
  :group 'gitsafe
  :type '(repeat symbol)
  :package-version '(gitsafe . "0.1.0"))

;;; [禁用]触发保存的 hook

(defcustom gitsafe-auto-save-hook-triggers
  '(mouse-leave-buffer-hook focus-out-hook)
  "A list of hooks which would trigger `gitsafe-auto-save-command'."
  :group 'gitsafe
  :type '(repeat symbol)
  :package-version '(gitsafe . "0.3.0"))

;;; [主用]idle 触发自动保存

(defcustom gitsafe-auto-save-when-idle t
  "Save current buffer automatically when Emacs is idle."
  :group 'gitsafe
  :type 'boolean
  :package-version '(gitsafe . "0.2.0"))

;;; idle 的延时设为 1s

(defcustom gitsafe-auto-save-idle-duration 1
  "The number of seconds Emacs has to be idle, before auto-saving the current buffer.
See `gitsafe-auto-save-when-idle'."
  :group 'gitsafe
  :type 'integer
  :package-version '(gitsafe . "0.2.0"))

;;; 是否自动保存远程文件

(defcustom gitsafe-auto-save-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'gitsafe
  :type 'boolean
  :package-version '(gitsafe . "0.3.0"))

;;; 根据正则排除文件
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

;;;; 封装

(defun gitsafe-auto-save-command ()
  (interactive)
  (gitsafe-auto-save--command))

;;; idle激活,撤销

(defvar gitsafe-auto-save-idle-timer nil)
;; (setq gitsafe-auto-save-idle-timer nil)

(defun gitsafe-auto-save-initialize-idle-timer ()
  "Initialize gitsafe idle timer if `gitsafe-auto-save-when-idle' is true."
  (setq gitsafe-auto-save-idle-timer
        (when gitsafe-auto-save-when-idle
          (run-with-idle-timer gitsafe-auto-save-idle-duration t #'gitsafe-auto-save-command))))

(defun gitsafe-auto-save-stop-idle-timer ()
  "Stop gitsafe idle timer if `gitsafe-auto-save-idle-timer' is set."
  (when gitsafe-auto-save-idle-timer    ;[nil 0 1 0 t gitsafe-auto-save-command nil idle 0]
    (cancel-timer gitsafe-auto-save-idle-timer)
    (cancel-function-timers 'gitsafe-auto-save-command)))

;; (setq timer-idle-list (delq gitsafe-auto-save-idle-timer timer-idle-list))
;; ([t 0 0 125000 t show-paren-function nil idle 0] [t 0 0 200000 t which-key--update nil idle 0] [t 0 0 500000 t #[0 "?\205 \301 \207" [jit-lock--antiblink-grace-timer jit-lock-context-fontify] 1] nil idle 0] [t 0 1 0 t gitsafe-auto-save-command nil idle 0] [t 0 1 0 t amx-idle-update nil idle 0] [nil 0 10 0 t garbage-collect nil idle 0] [nil 0 30 0 nil desktop-auto-save nil idle 0] [nil 0 30 0 t js-gc nil idle 0])

;;; advice 命令,使其执行自动保存

(defun gitsafe-auto-save-command-advice (&rest _args)
  "A simple wrapper around `gitsafe-auto-save-command' that's advice-friendly."
  (gitsafe-auto-save-command))

(defun gitsafe-auto-save-advise-trigger-commands ()
  "Apply gitsafe-auto-save advice to the commands listed in `gitsafe-auto-save-triggers'."
  (mapc (lambda (command)
          (advice-add command :before #'gitsafe-auto-save-command-advice))
        gitsafe-auto-save-triggers))

(defun gitsafe-auto-save-remove-advice-from-trigger-commands ()
  "Remove gitsafe-auto-save advice from to the commands listed in `gitsafe-auto-save-triggers'."
  (mapc (lambda (command)
          (advice-remove command #'gitsafe-auto-save-command-advice))
        gitsafe-auto-save-triggers))

;;; initialize
;; 每闲置 1s 就自动保存了, 其实其他 hook 可以不用了.

(defun gitsafe-auto-save-initialize ()
  "Setup gitsafe auto save's advices and hooks."
  ;; (gitsafe-auto-save-advise-trigger-commands)
  ;; (dolist (hook gitsafe-auto-save-hook-triggers)
  ;;   (add-hook hook #'gitsafe-auto-save-command))
  (gitsafe-auto-save-initialize-idle-timer)
  ;; 不知道有什么用.
  (add-hook 'before-save-hook 'font-lock-flush)
  )

(defun gitsafe-auto-save-stop ()
  "Cleanup gitsafe's advices and hooks."
  ;; (gitsafe-auto-save-remove-advice-from-trigger-commands)
  ;; (dolist (hook gitsafe-auto-save-hook-triggers)
  ;;   (remove-hook hook #'gitsafe-auto-save-command))
  (gitsafe-auto-save-stop-idle-timer)
  (remove-hook 'before-save-hook 'font-lock-flush)
  )

;;; gitsafe-auto-save-mode

;;;###autoload
(define-minor-mode gitsafe-auto-save-mode
  "A minor mode that saves your Emacs buffers when predicate"
  :global t
  (cond
   (gitsafe-auto-save-mode (gitsafe-auto-save-initialize))
   (t (gitsafe-auto-save-stop))))

;;; gitsafe-auto-save.el ends here.
(provide 'gitsafe-auto-save)
