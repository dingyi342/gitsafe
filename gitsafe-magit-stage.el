;;; gitsafe-magit-stage.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; triggers 就是 要 advice 的函数,在执行之前先判断 buffer file 有没有unstaged
;; kill-buffer 会导致
;; switch-to-buffer
;; find-file
;; next-buffer
;; previous-buffer
;; 窗口移动的就算了.
(defcustom gitsafe-magit-stage-triggers '()
  ;; '(kill-buffer switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer)
  "A list of commands which would trigger `gitsafe-command'."
  :group 'gitsafe
  :type '(repeat symbol)
  :package-version '(gitsafe . "0.1.0"))

;;; hooks 执行 gitsafe-magig-stage-commnad

(defcustom gitsafe-magit-stage-hook-triggers '()
  ;; '(mouse-leave-buffer-hook focus-out-hook find-file-hook)
  "A list of hooks which would trigger `gitsafe-command'."
  :group 'gitsafe
  :type '(repeat symbol)
  :package-version '(gitsafe . "0.3.0"))

;;; 是否对远程文件执行

(defcustom gitsafe-magit-stage-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'gitsafe
  :type 'boolean
  :package-version '(gitsafe . "0.3.0"))

;;; 根据正则排除一些文件,不执行

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
          (beginning-of-buffer)
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

;;; advice triggers
(defun gitsafe-magit-stage-command-advice (old-func &rest args)
  "A simple wrapper around `gitsafe-command' that's advice-friendly."
  (if (gitsafe-buffer-anything-unstaged-p)
      (gitsafe-magit-stage-command)
    (apply old-func args)))

(defun gitsafe-magit-stage-advice-trigger-commands ()
  "Apply gitsafe advice to the commands listed in `gitsafe-magit-stage-triggers'."
  (mapc (lambda (command)
          (advice-add command :around #'gitsafe-magit-stage-command-advice))
        gitsafe-magit-stage-triggers))

(defun gitsafe-magit-stage-remove-advice-from-trigger-commands ()
  "Remove gitsafe advice from to the commands listed in `gitsafe-triggers'."
  (mapc (lambda (command)
          (advice-remove command #'gitsafe-magit-stage-command-advice))
        gitsafe-magit-stage-triggers))

;;; idle 闲置 没必要
;; 这个部 auto save 没必要影响正常编辑.

;;; gitsafe-magit-stage-mode

(defun gitsafe-magit-stage-initialize ()
  "Setup gitsafe's advices and hooks."
  (gitsafe-magit-stage-advice-trigger-commands)
  (dolist (hook gitsafe-magit-stage-hook-triggers)
    (add-hook hook #'gitsafe-magit-stage-command)))

(defun gitsafe-magit-stage-stop ()
  "Cleanup gitsafe's advices and hooks."
  (gitsafe-magit-stage-remove-advice-from-trigger-commands)
  (dolist (hook gitsafe-magit-stage-hook-triggers)
    (remove-hook hook #'gitsafe-magit-stage-command)))

;;;###autoload
(define-minor-mode gitsafe-magit-stage-mode
  "A minor mode that saves your Emacs buffers when predicate"
  :global t
  (cond
   (gitsafe-magit-stage-mode (gitsafe-magit-stage-initialize))
   (t (gitsafe-magit-stage-stop))))

;;; gitsafe-magit-stage.el ends here.
(provide 'gitsafe-magit-stage)
