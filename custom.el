;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Generated
;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(counsel-describe-function-function 'helpful-callable)
 '(counsel-describe-variable-function 'helpful-variable)
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "6f59df85468a454049cbfd90848d36a2da8d60aa29e8f9cc1c239d52cdac7ab7" "41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/Desktop/roam" t)
 '(deft-recursive t t)
 '(deft-use-filter-string-for-filename t t)
 '(doom-modeline-height 15)
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
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#5B6268")
 '(highlight-tail-colors
   ((("#333a38" "#99bb66" "green")
     . 0)
    (("#2b3d48" "#46D9FF" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c6b")
 '(org-roam-capture-templates
   '(("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "permanent/%<%Y%m%d%H%M%S>-${slug}" :head "#+TITLE: ${title}
" :unnarrowed t)
     ("p" "project" plain #'org-roam-capture--get-point "%?" :file-name "project/%<%Y%m%d%H%M%S>-${slug}" :head "#+TITLE: ${title}
" :unnarrowed t)
     ("r" "ref" plain #'org-roam-capture--get-point "" :file-name "literature/${slug}" :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
" :unnarrowed t)))
 '(org-roam-completion-system 'ivy t)
 '(org-roam-dailies-capture-templates
   '(("d" "daily" plain #'org-roam-capture--get-point "" :immediate-finish t :file-name "daily/%<%Y-%m-%d>" :head "#+TITLE: %<%Y-%m-%d>")))
 '(org-roam-directory "~/Desktop/roam")
 '(package-selected-packages
   '(magit magit-section doom-themes hydra swiper ivy counsel ivy-rich doom-modeline undo-tree lsp-ui dash org-roam quote
	   (swiper deft org-roam typescript-mode counsel-jq projectile which-key lsp-ui js-mode jetbrains-darcula-theme lsp-mode json-navigator markdown-mode sql-indent restclient company aggressive-indent-mode aggressive-indent rainbow-delimiters clojure-mode-extra-font-locking clojure-mode paredit cider use-package transpose-frame smex pipenv org nginx-mode neotree multiple-cursors magit ido-ubiquitous idea-darkula-theme go-mode exec-path-from-shell elpy diminish darcula-theme)))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
