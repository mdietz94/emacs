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

(ac-config-default)

(load-theme 'zenburn t)

(fset 'c-indent-region 'clang-format-region)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq clang-format-executable "/n/anaconda/2.5.0/envs/1.8/bin/clang-format")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-18"))
(set-face-attribute 'default t :font "Source Code Pro-18")

(general-define-key
:keymaps '(normal insert emacs)
:prefix "SPC"
:non-normal-prefix "M-SPC"
"b" '(:ignore t :which-key "buffers")
"f" '(:ignore t :which-key "files")
"fed" 'find-user-init-file
"fek" (find-file "~/.emacs.d/keybindings.el")
"f f" 'find-file
"b k" 'buf-move-up
"b j" 'buf-move-down
"b h" 'buf-move-left
"b l" 'buf-move-right
"b d" 'kill-buffer-and-window
"f z f" 'fzf
"SPC" 'execute-extended-command)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-leader clang-format fzf zenburn-theme auto-complete projectile flycheck evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
