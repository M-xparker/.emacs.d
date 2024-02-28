(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

(set-face-attribute 'default nil :font "Fira Code" :height 140)
(column-number-mode 0)
(line-number-mode 0)
(global-display-line-numbers-mode t)

(defun any-horizontal-splits-p ()
  "Return t if any windows have been split horizontally."
  (when (member nil (mapcar #'window-full-width-p (window-list))) t))

(defun resize-font (frame)
  (if (any-horizontal-splits-p)
      (set-face-attribute 'default nil :height 120)
    (set-face-attribute 'default nil :height 140)))

(add-hook 'window-buffer-change-functions
	  'resize-font)

;; Keyboard changes
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq mac-right-command-modifier 'control)
(setq ns-right-command-modifier 'control)

(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms
  (cons `(,(car (car auto-save-file-name-transforms))
          ,(concat "~/.config/emacs/auto-save-list" "\\2") t) auto-save-file-name-transforms))

(setq dired-dwim-target t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(require 'subr-x)

(load "~/.emacs.d/utils.el")

(defalias 'yes-or-no-p 'y-or-n-p)
(setq dired-omit-files "^\\.$|~$")

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-persp-name nil)
  (doom-modeline-icon nil)
  (doom-modeline-lsp nil)
  (doom-modeline-buffer-encoding nil)
  (vc-display-status nil))
(setq doom-modeline-persp-name nil)
(use-package company
  :config
  (setq completion-ignore-case  t))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out"))

(use-package org
  :hook (org-mode . visual-line-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-switchb))
  :config (org-babel-do-load-languages
	   'org-babel-load-languages
	   '((js         . t)
	     (emacs-lisp . t)
	     (clojure    . t)
	     (python     . t)
	     (dot        . t)
	     (css        . t)
	     (sql . t)
	     (calc . t)
	     (restclient . t)))
  (setq org-startup-folded t)
  (setq org-agenda-files '("~/gtd/inbox.org"
                           "~/gtd/gtd.org"
                           "~/gtd/tickler.org"))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline "~/gtd/inbox.org" "Tasks")
				 "* TODO %i%?")
				("T" "Tickler" entry
				 (file+headline "~/gtd/tickler.org" "Tickler")
				 "* %i%? \n %U")))
  (setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
			     ("~/gtd/someday.org" :level . 1)
			     ("~/gtd/tickler.org" :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-custom-commands
	'(("n" "Admin batch" tags-todo "admin"
           ((org-agenda-overriding-header "Admin")))))
  (setq org-deadline-warning-days 30)
  (add-to-list 'org-modules 'org-habit)
  (setq org-load-modules-maybe t)
  (setq org-adapt-indentation nil))

(require 'org-tempo)

(defun org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(use-package magit
  :bind ("C-x g" . magit-status))
(use-package magit-section)

(use-package org-roam
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/Desktop/roam")
  (org-roam-completion-system 'ivy)
  (org-roam-dailies-directory "daily/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      "%?"
      :target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org"
			 "#+TITLE: ${title}\n")
      :unnarrowed t)
     ("r" "ref" plain
      ""
      :target (file+head "literature/${slug}.org"
			 "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n t" . org-roam-dailies-goto-today)
	 ("C-c n y" . org-roam-dailies-goto-yesterday)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
         :map org-mode-map))

(use-package deft
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Desktop/roam")
  (deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (deft-use-filename-as-title t))

(defun mp/deft-parse-title (file contents)
  ""
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
	(string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(advice-add 'deft-parse-title :override #'mp/deft-parse-title)

;;something's not working with this
;;install manually
(use-package dash)

(use-package transpose-frame)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package multiple-cursors)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package whitespace-mode
  :ensure nil
  :hook ((clojure-mode . whitespace-mode)
	 (markdown-mode . whitespace-mode))
  :init
  (setq
   whitespace-style '(face lines-tail)
   whitespace-line-column 80))

(use-package auto-fill-mode
  :ensure nil
  :hook (org-mode . auto-fill-mode)
  :init
  (setq-default fill-column 80))

(use-package which-key
  :config
  (which-key-mode))

(use-package yasnippet
  :config (yas-global-mode 1)
	   (yas-reload-all))

(use-package yasnippet-snippets)

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Desktop/paradigm/"))
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(defun kill-project-buffers ()
  ""
  (interactive)
  (let ((bufs (buffer-list (selected-frame))))
    (dolist (buf bufs)
      (with-current-buffer buf
	(when (projectile-project-p)
	  (kill-buffer buf))))))

(use-package perspective
  :demand t
  :bind
  ("C-x b"   . persp-ivy-switch-buffer)
  ("C-x C-b" . persp-ibuffer)
  :config
  (persp-mode))

(use-package winner
  :config
  (winner-mode 1))

(use-package ace-window
  :bind
  ("M-o" . ace-window))

(use-package avy
  :bind ("C-:" . avy-goto-char))

(defun js-font-setup ()
  (face-remap-add-relative 'font-lock-variable-name-face
			   :foreground "#1481ba")
  ;; "#ffa400"
  (face-remap-add-relative 'font-lock-function-name-face
			   :foreground "#23ce6b"))
(defun disable-tabs () (setq indent-tabs-mode nil))

(use-package js
  :config
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js-chain-indent 't)
  :hook ((js-mode . eglot-ensure)
	 (js-mode . disable-tabs)
	 (js-mode . electric-pair-mode)
	 (js-mode . js-font-setup)))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode) ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-indent-level 2)
  (setq js-chain-indent nil)
  :hook
  ((typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . company-mode)
   (tsx-ts-mode . company-mode))
  :custom-face
  (eglot-diagnostic-tag-unnecessary-face ((t (:underline (:color "yellow green" :style wave))))))

(use-package zig-mode
  :config
  (setq lsp-zig-zls-executable "zls")
  :hook ((zig-mode . lsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cider

  :bind (("C-c C-o" . (lambda () (interactive)
			(cider-find-and-clear-repl-output t))))
  :hook
  (cider-repl-mode . company-mode)
  (cider-mode . company-mode))
(use-package paredit)

(use-package clojure-mode

  :config (setq clojure-align-forms-automatically t)
  (show-paren-mode 1))

(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)))

(use-package clojure-mode-extra-font-locking)

(use-package rainbow-delimiters)

(load-file "~/.emacs.d/clojure.el")

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u2")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
