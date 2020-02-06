;;; gitsafe.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(require 'magit)
(require 'gitsafe-auto-save)
(require 'gitsafe-magit-stage)

(defgroup gitsafe nil
  "Smart-saving of buffers."
  :group 'tools
  :group 'convenience)

(defvar gitsafe-mode-map (make-sparse-keymap)
  "gitsafe mode's keymap.")

;;; gitsafe 判断函数

(defun gitsafe-buffer-useful-p ()
  "就是正常要编辑的 buffer"
  (and
   (not (minibufferp))
   (buffer-file-name)
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
   (gitsafe-git-track-p)
   (magit-anything-modified-p nil (buffer-file-name))))

(defun gitsafe-buffer-anything-unstaged-p ()
  "先判断当前文件是否被跟踪,其次是否要未提交的内容.
如果未跟踪,也会提示未提交的,但是对我无意义."
  (and
   ;; (buffer-file-name)
   ;; (file-writable-p (buffer-file-name))
   (gitsafe-git-track-p)
   (magit-anything-unstaged-p nil (buffer-file-name))))

(defun gitsafe-buffer-only-uncommited-p ()
  (and (not (gitsafe-buffer-anything-unstaged-p))
       (gitsafe-buffer-anything-modified-p)))

(defun gitsafe-buffer-unsafe-p ()
  "判断当前文件是否被修改,如果有返回 t,如果没有再判断是否有没暂存的."
  (unless (buffer-modified-p)
    (gitsafe-buffer-anything-unstaged-p)))

(defun gitsafe-buffer-safe-p ()
  "判断当前是否安全."
  (not (gitsafe-buffer-unsafe-p)))

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
      (message "white space cleaned"))))

;;; gitsafe-save-buffer
(defun gitsafe-save-buffer ()
  (let ((inhibit-message t))
    (gitsafe-cleanup-buffer))
  (save-buffer))

;;;; 清除行尾空格
(defun gitsafe-auto-save-delete-trailing-whitespace-except-current-line ()
  (interactive)
  (when gitsafe-auto-save-delete-trailing-whitespace
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
            (delete-trailing-whitespace)))))))

;;; gitsafe-kill-this-buffer
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
;;; gitsafe-find-file
(defun gitsafe-after-find-file ()
  "find file hook run magiit-save-command"
  (interactive)
  (run-with-timer
   1 nil (lambda () (interactive)
           (let ((gitsafedisplay-buffer-function 'magit-display-buffer-fullcolumn-most-v1))
             (gitsafe-magit-stage-command)))))

;; (add-hook 'find-file-hook 'gitsafe-after-find-file)

;;; gitsafe-kill-emacs
(defun gitsafe-kill-emacs (old-func &rest args)
  "遍历 buffer 列表,发现有没有没暂存的,没有就问是否要真的退出 emacs.有的话就就弹出来"
  (let (gitsafe-buffer-unstaged-list)
    (mapcar (function (lambda (buf)
                        (when (gitsafe-buffer-anything-unstaged-p)
                          (pushnew buf gitsafe-buffer-unstaged-list))))
            (buffer-list))
    (if gitsafe-buffer-unstaged-list
        (switch-to-buffer (pop gitsafe-buffer-unstaged-list))
      (when (y-or-n-p "really quit emacs:")
        (apply old-func args)))))

;; (advice-add 'kill-emacs :around 'gitsafe-kill-emacs)

;;; gitsafe-command-switch-buffer-functions
(defun gitsafe-switch-buffer-functions (prev curr)
  (cl-assert (eq curr (current-buffer)))  ;; Always t
  (with-current-buffer prev
    (when (gitsafe-buffer-useful-p)
      (with-current-buffer curr
        (when (gitsafe-buffer-useful-p)
          (gitsafe-magit-stage-command))))))

;; 实际上是对 window-buffer-change-functioin 的一个扩展.
;; 传入 prev-buffer 和 cur-buffer,更好的判断.
;; (add-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)

;;; gitsafe-mode
(defun gitsafe-enable ()
  "Setup gitsafe's advices and hooks."
  (gitsafe-auto-save-mode 1)
  (gitsafe-magit-stage-mode 1)
  (add-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)
  (advice-add 'kill-emacs :around 'gitsafe-kill-emacs)
  ;; (add-hook 'find-file-hook 'gitsafe-after-find-file)
  )

(defun gitsafe-disable ()
  "Cleanup gitsafe's advices and hooks."
  (gitsafe-auto-save-mode -1)
  (gitsafe-magit-stage-mode -1)
  (remove-hook 'switch-buffer-functions 'gitsafe-switch-buffer-functions)
  (advice-remove 'kill-emacs 'gitsafe-kill-emacs)
  ;; (remove-hook 'find-file-hook 'gitsafe-after-find-file)
  )

;;;###autoload
(define-minor-mode gitsafe-mode
  "A minor mode that saves your Emacs buffers when predicate"
  :global t
  (cond
   (gitsafe-mode (gitsafe-enable))
   (t (gitsafe-disable))))

;;; gitsafe.el ends here.
(provide 'gitsafe)
