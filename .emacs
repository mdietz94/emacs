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

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'command-projectile-map)

(setq show-paren-mode-delay 0)
(show-paren-mode 1)

(ac-config-default)

(load-theme 'zenburn t)

(fset 'c-indent-region 'clang-format-region)

(electric-pair-mode)
(counsel-mode)
(ivy-prescient-mode)

(advice-add 'risky-local-variable-p :override #'ignore)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq clang-format-executable "/n/anaconda/2.5.0/envs/1.8/bin/clang-format")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-18"))
(set-face-attribute 'default t :font "Source Code Pro-18")

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(general-define-key
:keymaps '(normal insert emacs)
:prefix "SPC"
:non-normal-prefix "M-SPC"
"b" '(:ignore t :which-key "buffers")
"b b" 'buffer-menu
"f" '(:ignore t :which-key "files")
"fed" (lambda() (interactive)(find-file "~/.emacs"))
"f f" 'counsel-find-file
"b k" 'buf-move-up
"b j" 'buf-move-down
"b h" 'buf-move-left
"b l" 'buf-move-right
"b d" 'kill-buffer-and-window
"g d" 'git-gutter-mode
"o f f" 'fzf
"o f r" 'ff-find-related-file
"w d" 'evil-window-delete
"w /" 'split-window-right
"w <up>" 'evil-window-up
"w <down>" 'evil-window-down
"w <left>" 'evil-window-left
"w <right>" 'evil-window-right
"TAB" 'er-switch-to-previous-buffer
"SPC" 'counsel-M-x)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-gutter ivy-prescient counsel evil-leader clang-format fzf zenburn-theme auto-complete projectile flycheck evil)))
 '(safe-local-variable-values
   (quote
    ((flycheck-clang-args "-isystem/n/tech/3rd/boost/1.58.0/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/anaconda/2.5.0/envs/1.8/include" "-isystem/n/tech/3rd/eigen/3.2.2/install/x86_64.redhat.7/gcc49_64/include" "-isystem/n/anaconda/2.5.0/envs/1.8/lib/python2.7/site-packages/numpy/core/include" "-isystem/n/tech/3rd/blosc/1.2.3/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/tech/usagetrack/versions/20160318_02/usagetrack/usagetrack/include" "-isystem/n/tech/dev/rtech/rolling_release/releases/latest/influx/include")
     (flycheck-c/c++-clang-executable . "/n/tech/3rd/clang/3.8/install/x86_64.redhat.7/bin/clang++")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
