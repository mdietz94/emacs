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

(unless window-system
  (require 'mwheel)
  (require 'mouse)
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (defun track-mouse (e))
  (defvar mouse-sel-mode)
  (setq mouse-sel-mode t)
  )

(defvar show-paren-mode-delay)
(setq show-paren-mode-delay 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)

(ac-config-default)

(load-theme 'zenburn t)

(fset 'c-indent-region 'clang-format-region)

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

(defconst pdt-c-style
  '("linux"
    (c-offsets-alist . ((topmost-intro . [0])
						(access-label . [2])))
    ))

(c-add-style "pdt" pdt-c-style)

(setq-default c-default-style "pdt"
			  tab-width 4
			  c-basic-offset 4)

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
"f e d" (lambda() (interactive)(find-file "~/.emacs"))
"f f" 'counsel-find-file
"b k" 'buf-move-up
"b j" 'buf-move-down
"b h" 'buf-move-left
"b l" 'buf-move-right
"b d" 'kill-this-buffer
"g d" 'git-gutter-mode
"g l" 'avy-goto-line
"g c" 'avy-goto-char
"g 2 c" 'avy-goto-char-2
"g w" 'avy-goto-word-1
"o f f" 'fzf
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
	(git-gutter ivy-prescient counsel evil-leader clang-format fzf zenburn-theme auto-complete projectile flycheck evil)))
 '(safe-local-variable-values
   (quote
	((flycheck-clang-args "-isystem/n/tech/3rd/boost/1.58.0/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/anaconda/2.5.0/envs/1.8/include" "-isystem/n/tech/3rd/blosc/1.2.3/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/tech/3rd/gmock/1.7.0/install/x86_64.redhat.7/gcc49_64/include" "-isystem/n/tech/3rd/gtest/1.7.0/install/x86_64.redhat.7/gcc49_64/include")
	 (flycheck-clang-args "-isystem/n/tech/3rd/boost/1.58.0/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/anaconda/2.5.0/envs/1.8/include" "-isystem/n/tech/3rd/eigen/3.2.2/install/x86_64.redhat.7/gcc49_64/include" "-isystem/n/anaconda/2.5.0/envs/1.8/lib/python2.7/site-packages/numpy/core/include" "-isystem/n/tech/3rd/blosc/1.2.3/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/tech/usagetrack/versions/20160318_02/usagetrack/usagetrack/include")
	 (flycheck-python-pylint-executable . "/n/anaconda/2.5.0/envs/2.5/bin/pylint")
	 (flycheck-python-flake8-executable . "/n/anaconda/2.5.0/envs/2.5/bin/flake8")
	 (flycheck-clang-args "-isystem/n/tech/3rd/boost/1.58.0/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/anaconda/2.5.0/envs/1.8/include" "-isystem/n/tech/3rd/eigen/3.2.2/install/x86_64.redhat.7/gcc49_64/include" "-isystem/n/anaconda/2.5.0/envs/1.8/lib/python2.7/site-packages/numpy/core/include" "-isystem/n/tech/3rd/blosc/1.2.3/install/x86_64.redhat.7/gcc49_64/anaconda-2.5.0-1/include" "-isystem/n/tech/usagetrack/versions/20160318_02/usagetrack/usagetrack/include" "-isystem/n/tech/dev/rtech/rolling_release/releases/latest/influx/include")
	 (flycheck-c/c++-clang-executable . "/n/tech/3rd/clang/3.8/install/x86_64.redhat.7/bin/clang++")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
