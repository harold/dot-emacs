(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(indent-tabs-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(show-paren-mode t)
 '(tab-width 2))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 68 :width normal :foundry "outline" :family "ProFontWindows")))))

(setq make-backup-files nil)

(tool-bar-mode -1)

(global-set-key [M-insert] 'yank-pop)

(defun my-go-slime ()
 (interactive)
 (split-window-horizontally)
 (other-window 1)
 (slime))

(global-set-key [f5] 'my-go-slime)

(add-hook 'slime-connected-hook 
					(lambda () 
						(slime-redirect-inferior-output)))

;; f4 kill-buffer
(global-set-key [f4] (lambda()(interactive)(kill-buffer nil)))

;; colors
(set-cursor-color "gray75")
(set-foreground-color "white")
(set-background-color "gray5")

(set-face-background 'region "coral4")

(defun ruby-run ()
 (interactive)
 (save-buffer)
 (shell-command (concat "ruby \"" (buffer-file-name (nth 0 (buffer-list))) "\"")))

(global-set-key [C-f5] 'ruby-run)
(put 'downcase-region 'disabled nil)

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)

(global-set-key (kbd "<f12>") ; make F12 switch to .emacs; create if needed
  (lambda()(interactive)(find-file "~/.emacs")))

(global-set-key [mouse-4]      'previous-buffer)
(global-set-key [mouse-5]      'next-buffer)
(global-set-key [drag-mouse-4] 'previous-buffer)
(global-set-key [drag-mouse-5] 'next-buffer)
(global-set-key [C-tab]        'bury-buffer)
(global-set-key (kbd "C-SPC")  'slime-complete-symbol)

(require 'erc)
(global-set-key [f11] (lambda()(interactive)(erc :server "irc.freenode.net" :port 6667 :nick "_hrrld" :full-name "Harold")))
