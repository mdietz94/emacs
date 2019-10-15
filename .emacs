(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "artifactsqa:9090")
        ("https" . "artifactsqa:9090")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(global-evil-leader-mode)
(evil-mode 1)
(global-flycheck-mode)

(defvar tramp-default-method)
(defvar tramp-shell-prompt-pattern)
(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(projectile-mode +1)

(unless window-system
  (require 'mwheel)
  (require 'mouse)
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (defun track-mouse (e))
  (defvar mouse-sel-mode)
  (setq mouse-sel-mode t)
  )

(require 'fzf)
(defconst fzf/args "-x --color --print-query")
(defun fzf-grep-arg (arg)
  "Greps for the element in the current project.
ARG: what to grep for."
  (fzf/start (locate-dominating-file default-directory ".git")
             (concat "git grep -I -w --line-number " arg)))

(defun fzf-grep-thing-at-point ()
  "Greps for symbol at point."
  (interactive)
  (funcall 'fzf-grep-arg (thing-at-point 'symbol)))

(defvar show-paren-mode-delay)
(setq show-paren-mode-delay 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)

(ac-config-default)

(load-theme 'zenburn t)

(setq confirm-kill-emacs 'yes-or-no-p)
(electric-pair-mode)
(counsel-mode)
(ivy-prescient-mode)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-display-line-numbers-mode)
(winner-mode 1)

(advice-add 'risky-local-variable-p :override #'ignore)

(setq make-backup-files nil)
(setq auto-save-default nil)

(defvar clang-format-executable)
(defvar c-default-style)
(defvar c-basic-offset)

(setq clang-format-executable "/n/nix/tech/var/nix/profiles/ciusers/bin/clang-format")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default c-default-style "linux")

(defun setup-c-mode ()
  "Setup options for 'c-mode'."
  (fset 'c-indent-region 'clang-format-region)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset 'topmost-intro [0])
  (c-set-offset 'access-label [2])
  (c-set-offset 'inline-open 0)
  )

(add-hook 'c-mode-common-hook 'setup-c-mode)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-18"))
(set-face-attribute 'default t :font "Source Code Pro-18")

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git Diff
(defun brh/_diff-buffer (ref)
  "Diff the current buffer against ref"
  (save-buffer 0)
  (vc-version-ediff (list buffer-file-name) ref ""))

(defun brh/diff-origin-master ()
  "Diff the current buffer against origin/master"
  (interactive)
    (brh/_diff-buffer "origin/master"))

(defun brh/diff-ref ()
  "Diff the current buffer against an entered ref"
  (interactive)
  (brh/_diff-buffer (read-string "Ref to diff against: ")))

(general-define-key
:keymaps '(normal insert emacs)
:prefix "SPC"
:non-normal-prefix "M-SPC"
"b" '(:ignore t :which-key "buffers")
"b b" 'buffer-menu
"f" '(:ignore t :which-key "files")
"f e d" (lambda() (interactive)(find-file "~/.emacs"))
"f f" 'counsel-find-file
"b k" 'buf-move-up
"b j" 'buf-move-down
"b h" 'buf-move-left
"b l" 'buf-move-right
"b d" 'kill-this-buffer
"m m" 'brh/diff-origin-master
"m r" 'brh/diff-ref
"g d" 'fzf-grep-thing-at-point
"g l" 'avy-goto-line
"g c" 'avy-goto-char
"g 2 c" 'avy-goto-char-2
"g w" 'avy-goto-word-1
"o f f" 'fzf-git-files
"o f r" 'ff-find-related-file
"p p" 'projectile-switch-project
"w d" 'evil-window-delete
"w /" 'split-window-right
"w <up>" 'evil-window-up
"w <down>" 'evil-window-down
"w <left>" 'evil-window-left
"w <right>" 'evil-window-right
"w d" 'evil-window-delete
"w r" 'evil-window-rotate-upwards
"w u" 'winner-undo
"w U" 'winner-redo
"TAB" 'er-switch-to-previous-buffer
"SPC" 'counsel-M-x)

(general-define-key
:keymaps '(normal)
"s" 'avy-goto-char)


; Code in order to make sure Flycheck errors split horizontally
; and do not take up too much space
(defun display-buffer-window-below-and-shrink (buffer alist)
  (let ((window (or (get-buffer-window buffer)
                                        (display-buffer-below-selected buffer alist))))
        (when window
          (message "Here window is %s" window)
          (fit-window-to-buffer window 20)
          (shrink-window-if-larger-than-buffer window)
          window)))

(add-to-list 'display-buffer-alist
                         `(,(rx string-start (eval flycheck-error-list-buffer) string-end)
                           (display-buffer-window-below-and-shrink . ((reusable-frames . t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit rg git-gutter ivy-prescient counsel evil-leader clang-format fzf zenburn-theme auto-complete projectile flycheck evil avy general which-key))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
