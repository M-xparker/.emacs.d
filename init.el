
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


(use-package go-mode
  :ensure t
  :init
  (setq exec-path (cons "/usr/local/go/bin" exec-path))
  (add-to-list 'exec-path "/Users/matthewparker/go/bin")
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind
  ("M-." . godef-jump))


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
