(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun any-horizontal-splits-p ()
  "Return t if any windows have been split horizontally."
  (when (member nil (mapcar #'window-full-width-p (window-list))) t))

(defun resize-font (frame)
  (if (any-horizontal-splits-p)
      (set-face-attribute 'default nil :height 120)
    (set-face-attribute 'default nil :height 140)))

(add-hook 'window-buffer-change-functions
	  'resize-font)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/nano-emacs")

;; Welcome message
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(require 'nano-layout)
(require 'nano-theme-dark)
(require 'nano-modeline)
(provide 'nano)

(global-linum-mode 1)
(setq ns-right-command-modifier 'control)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dired-omit-files "^\\.$|~$")

(setq mac-right-command-modifier 'control)

(require 'subr-x)

(defun insert-random-uuid ()
  (interactive)
  (insert (downcase (string-trim (shell-command-to-string "uuidgen")))))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun copy-paragraph ()
  "Copy the current paragraph to clipboard."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (clipboard-kill-ring-save start end)))


(use-package darcula-theme
  :ensure t)

(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :config (tree-sitter-require 'javascript))

(use-package company
  :ensure t)

(use-package counsel
  :ensure t)

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
	     (calc . t)))
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

(use-package org-roam
  :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Desktop/roam")
      (org-roam-completion-system 'ivy)
      (org-roam-capture-templates
       '(("d" "default" plain (function org-roam-capture--get-point)
	  "%?"
	  :file-name "permanent/%<%Y%m%d%H%M%S>-${slug}"
	  :head "#+TITLE: ${title}\n"
	  :unnarrowed t)
	 ("p" "project" plain (function org-roam-capture--get-point)
	  "%?"
	  :file-name "project/%<%Y%m%d%H%M%S>-${slug}"
	  :head "#+TITLE: ${title}\n"
	  :unnarrowed t)
	 ("r" "ref" plain (function org-roam-capture--get-point)
	  ""
	  :file-name "literature/${slug}"
	  :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}\n"
	  :unnarrowed t)))
      :bind (:map org-roam-mode-map
		  (("C-c n l" . org-roam)
		   ("C-c n t" . org-roam-dailies-find-today)
		   ("C-c n f" . org-roam-find-file)
		   ("C-c n j" . org-roam-jump-to-index)
		   ("C-c n b" . org-roam-switch-to-buffer)
		   ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Desktop/roam"))


(use-package darcula-theme
  :ensure t)

(use-package swiper
  :ensure t)

(use-package ido-completing-read+
  :ensure t
  :init
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  (ido-ubiquitous-mode 1)
  :bind
  ("C-x C-b" . ibuffer))

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex))

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-python-command "python3")
  :config
  (elpy-enable))

(use-package neotree
  :ensure t)

;;something's not working with this
;;install manually
(use-package dash
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package transpose-frame
  :ensure t)
(use-package nginx-mode
  :ensure t)
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode))

(use-package multiple-cursors
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package whitespace-mode
  :hook ((clojure-mode . whitespace-mode)
	 (markdown-mode . whitespace-mode))
  :init
  (setq
   whitespace-style '(face lines-tail)
   whitespace-line-column 80))

(use-package auto-fill-mode
  :hook (org-mode . auto-fill-mode)
  :init
  (setq-default fill-column 80))

(use-package julia-mode
  :ensure t)

(use-package which-key
  :ensure t
    :config
    (which-key-mode))

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config (setq lsp-ui-doc-enable nil)
  :commands lsp)

(use-package lsp-ui
  :ensure t)

(defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))

(use-package projectile
  :ensure t
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


;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#fceade")

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
  :hook ((js-mode . lsp)
	 (js-mode . lsp-ui-mode)
	 (js-mode . disable-tabs)
	 (js-mode . js-font-setup)))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  (setq js-chain-indent nil)
  :hook ((typescript-mode . lsp)
	 (typescript-mode . lsp-ui-mode)
	 (typescript-mode . disable-tabs)))

(defun js--multi-line-declaration-indentation ()
  "Helper function for `js--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (forward-sexp-function ; Use Lisp version.
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js--declaration-keyword-re))
        (let ((pt (point)))
          (when (looking-at js--indent-operator-re)
            (goto-char (match-end 0)))
          ;; The "operator" is probably a regexp literal opener.
          (when (nth 3 (syntax-ppss))
            (goto-char pt)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at js--indent-operator-re)
                                   (js--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (js--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js--declaration-keyword-re)
          (goto-char (match-beginning 0))
          (+ js-indent-level (current-column)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cider
  :ensure t
  :bind (("C-c C-o" . (lambda () (interactive)
			       (cider-find-and-clear-repl-output t))))
  :hook
  (cider-repl-mode . company-mode)
  (cider-mode . company-mode))
(use-package paredit
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config (setq clojure-align-forms-automatically t)
	   (show-paren-mode 1))

(use-package aggressive-indent
  :ensure t
  :hook (clojure-mode . aggressive-indent-mode))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(load-file "~/.emacs.d/clojure.el")

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u2")))


;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Generated
;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("6f59df85468a454049cbfd90848d36a2da8d60aa29e8f9cc1c239d52cdac7ab7" "41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" default))
 '(ensime-sem-high-faces
   '((var :foreground "#9876aa" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
			 (:color "#808080"))
     (implicitParams :underline
		     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6")))
 '(nil nil t)
 '(org-roam-capture-templates
   '(("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "permanent/%<%Y%m%d%H%M%S>-${slug}" :head "#+TITLE: ${title}
" :unnarrowed t)
     ("p" "project" plain #'org-roam-capture--get-point "%?" :file-name "project/%<%Y%m%d%H%M%S>-${slug}" :head "#+TITLE: ${title}
" :unnarrowed t)
     ("r" "ref" plain #'org-roam-capture--get-point "" :file-name "literature/${slug}" :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
" :unnarrowed t)) t)
 '(org-roam-completion-system 'ivy t)
 '(org-roam-dailies-capture-templates
   '(("d" "daily" plain #'org-roam-capture--get-point "" :immediate-finish t :file-name "daily/%<%Y-%m-%d>" :head "#+TITLE: %<%Y-%m-%d>")) t)
 '(org-roam-directory "~/Desktop/roam" t)
 '(package-selected-packages
   '(org-roam quote
	      (swiper deft org-roam typescript-mode counsel-jq ido-completing-read+ projectile which-key lsp-ui js-mode jetbrains-darcula-theme lsp-mode json-navigator markdown-mode sql-indent restclient company aggressive-indent-mode aggressive-indent rainbow-delimiters clojure-mode-extra-font-locking clojure-mode paredit cider use-package transpose-frame smex pipenv org nginx-mode neotree multiple-cursors magit ido-ubiquitous idea-darkula-theme go-mode exec-path-from-shell elpy diminish darcula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-type-face ((t (:inherit nano-face-salient :foreground "#9CCC65"))))
 '(org-document-title ((t (:inherit nano-face-faded :foreground "LightSkyBlue1"))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-3))))
 '(org-level-3 ((t (:inherit outline-2)))))
