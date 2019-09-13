(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(global-linum-mode 1)
(setq ns-right-command-modifier 'control)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package company
  :ensure t)

(use-package org
  :hook (org-mode . visual-line-mode)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-switchb))
  :config (org-babel-do-load-languages
	   'org-babel-load-languages
	   '((sh         . t)
	     (js         . t)
	     (emacs-lisp . t)
	     (perl       . t)
	     (scala      . t)
	     (clojure    . t)
	     (python     . t)
	     (ruby       . t)
	     (dot        . t)
	     (css        . t)
	     (plantuml   . t))))


(use-package darcula-theme
  :ensure t)

(use-package ido-ubiquitous
  :ensure t
  :init
  (ido-mode t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cider
  :ensure t
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


(use-package multiple-cursors
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Generated
;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sql-indent restclient company aggressive-indent-mode aggressive-indent rainbow-delimiters clojure-mode-extra-font-locking clojure-mode paredit cider use-package transpose-frame smex pipenv org nginx-mode neotree multiple-cursors magit ido-ubiquitous idea-darkula-theme go-mode exec-path-from-shell elpy diminish darcula-theme)))
 '(safe-local-variable-values
   (quote
    ((cider-clojure-cli-global-options . "-A:server")
     (cider-repl-require-ns-on-set . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
