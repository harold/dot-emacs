;; packages
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(defvar my-packages '(zenburn-theme neotree cider company rainbow-delimiters smartparens ace-jump-mode ag undo-tree))
(dolist (p my-packages)
  (when (not (package-installed-p p))
        (package-install p)))


;;theme
(setq custom-safe-themes '("ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" default))
(load-theme 'zenburn t)


;; startup
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode 1)


;; sanity
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq scroll-error-top-bottom t)
(setq make-backup-files nil)
(setq split-height-threshold nil
      split-width-threshold nil) ;; insanity?


;; windmove
(windmove-default-keybindings 'meta)


;; my grep
(defun grep-under-cursor ()
  (interactive)
  (rgrep (grep-tag-default) "*.clj*" "~/src"))

;; keys
(global-set-key (kbd "<select>") 'move-end-of-line) ;; is this the real life?
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-q") 'delete-window)
(global-set-key (kbd "C-M-w") 'bury-buffer)
(global-set-key (kbd "C-M-S-w") 'unbury-buffer)
(global-set-key (kbd "<backtab>") 'indent-sexp)
(global-set-key (kbd "<S-f8>") 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key (kbd "<M-delete>") 'backward-kill-word)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "M-<insert>") 'yank-pop)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "<f7>") 'grep-under-cursor)


;; smartparens mode
(add-hook 'prog-mode-hook 'smartparens-mode)
(require 'smartparens-clojure)
(global-set-key (kbd "M-[") 'sp-forward-barf-sexp)
(global-set-key (kbd "M-]") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-{") 'sp-backward-slurp-sexp)
(global-set-key (kbd "M-}") 'sp-backward-barf-sexp)
(add-hook 'smartparens-mode-hook (lambda () (sp-pair "'" nil :actions :rem)))

;; d/q template
(defun d-slash-q-template ()
  (interactive)
  (insert "(d/q '[:find ] (d/db model/*db-conn*))")
  (backward-char 25))

;; cider
(setq cider-repl-display-help-banner nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-prompt-save-file-on-load 'always-save)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-use-pretty-printing t)
(setq cider-pprint-fn 'puget)
(setq same-window-regexps '("\*cider-repl.*"))
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(setq nrepl-log-messages nil)
(global-set-key (kbd "<f5>") 'cider-toggle-trace-var)
(global-set-key (kbd "<f6>") 'd-slash-q-template)
(global-set-key (kbd "C-M-j") 'cider-jack-in)
(global-set-key (kbd "C-M-S-t") 'cider-test-run-ns-tests)

;; neotree
(require 'neotree)
(global-set-key [f9] (lambda ()
                       (interactive)
                       (neotree-dir "~/src")))
(global-set-key [f10] 'neotree-hide)


;; buffer full file paths
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name)


;; grep
(setq grep-find-ignored-directories '(".git" "out" "cn-test/resources" "daemons/data"
				      "cn-deploy/webapp" "cn-deploy/daemons"
				      "cn-deploy/data" "cn-deploy/elk/data"
				      "target/cljsbuild-compiler-0"
				      "target/cljsbuild-compiler-1"
				      "target/cljsbuild-compiler-2"
				      "s3cache"
				      "macosx"
				      "out-dc"))

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files "*.jar")
     (add-to-list 'grep-find-ignored-files "*.dylib")
     (add-to-list 'grep-find-ignored-files "db-backup.tar.gz")
     (add-to-list 'grep-find-ignored-files "figwheel_server.log")
     (add-to-list 'grep-find-ignored-files "site.min.css")))


;; fancy
(global-set-key (kbd "C-x C-x") (lambda ()
                                  (interactive)
                                  (progn (split-window-right)
                                         (split-window-right)
                                         (other-window 1)
                                         (other-window 1)
                                         (split-window-right))))


;; ido
(require 'ido)
(ido-mode t)


;; ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; xsel (sudo apt-get install xsel)
(defun xsel-copy ()
  (interactive)
  (when (use-region-p)
    (call-process-region (region-beginning) (region-end)
			 "xsel" nil nil nil "--clipboard" "--input")
    (message "Copied!")))

(defun xsel-paste ()
  (interactive)
  (call-process "xsel" nil t nil "--clipboard")
  (message "Pasted!"))

(global-set-key (kbd "C-M-c") 'xsel-copy)
(global-set-key (kbd "C-M-v") 'xsel-paste)


;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; sudo-edit
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r") 'sudo-edit)


;; font?
(ignore-errors
  (set-frame-font "-unknown-Ubuntu Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1"))

;; confusing commands
(put 'erase-buffer 'disabled nil)

;; ace-jump-mode
(define-key global-map (kbd "C-f") 'ace-jump-mode)

;; pop buffers here
(defun pop-buffers-here ()
  (interactive)
  (walk-windows
   (lambda (win)
     (set-window-dedicated-p win "yay")))
  (set-window-dedicated-p (selected-window) nil)
  (message "New buffers will be popped here."))

(global-set-key (kbd "C-x C-p") 'pop-buffers-here)
(put 'upcase-region 'disabled nil)

;; find file wildcards
(setq find-file-wildcards t)

;; ag
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

;; undo-tree
(global-undo-tree-mode)
(global-set-key (kbd "C-M-/") 'undo-tree-visualize)
