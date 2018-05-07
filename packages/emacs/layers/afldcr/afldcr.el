(require 'pretty-custom)

(custom! after-save-hook '(delete-trailing-whitespace)
  "Nice hooks to run after saving a buffer.")

(custom! counsel-mode t
  "Turn on more advanced ivy-mode functionality!"
  :require counsel)

(custom! custom-enabled-themes '(list 'doom-one-light
				      'doom-one)
  "The `doom-one' themes are _very_ nice."
  :require doom-themes)

(custom! enable-recursive-minibuffers t
  "Recommended by the great abo-abo.")

(custom! evil-mode t
  "Prevent RSI! Turn on `evil-mode' now!"
  :require evil)

(custom! indent-tabs-mode nil
  "Spaces are a bit nicer in LISP/Haskell.")

(custom! inhibit-startup-screen t
  "Please leave me alone Emacs.")

(custom! ivy-mode t
  "Better minibuffer entry."
  :require ivy)

(custom! ivy-use-virtual-buffers t
  "Recommended by the great abo-abo.")

(custom! menu-bar-mode (and (eq system-type 'darwin)
			    (not (display-graphic-p)))
  "Disable extraneous UI features.
Unlike `scroll-bar-mode' and `tool-bar-mode', however,
`menu-bar-mode' is useful in Mac OS.")

(custom! projectile-mode t
  "Allow project-based editing."
  :require projectile)

(custom! projectile-completion-system '(quote ivy)
  "Use ivy for completing Projectile prompts."
  :require ivy)

(custom! scroll-bar-mode nil
  "Disable extraneous UI features.")

(custom! tool-bar-mode nil
  "Disable extraneous UI features.")

;; adoc-mode doesn't add an entry to auto-mode-alist by default
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(custom-set-faces
 ;; PragmataPro is the bet font to have ever been made
 '(default ((t (:height 140 :family "PragmataPro"))))
 ;; Edit in monospace by default
 '(fixed-pitch ((t (:inherit default))))
 ;; I'm not a fan of markdown-face's scaling
 '(markdown-code-face ((t (:background "nil"))))
 '(markdown-inline-code-face ((t (:inherit markdown-pre-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1.0 :width normal))))
 '(markup-meta-hide-face ((t (:inherit markup-meta-face :foreground "gray25" :height 1.0))))
 '(markup-replacement-face ((t (:foreground "plum1"))))
 '(markup-secondary-text-face ((t (:inherit markup-gen-face :foreground "firebrick" :height 1.0))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :weight bold :height 1.0))))
 '(markup-title-1-face ((t (:inherit markup-title-0-face :height 1.0))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :weight bold :height 1.0))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :weight bold :height 1.0))))
 '(markup-title-4-face ((t (:inherit markup-gen-face :slant italic :height 1.0)))))

(provide 'afldcr)
