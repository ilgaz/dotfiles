
;;; packages.el --- use-package and all that  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; USE-PACKAGE BLOCK

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package which-key
  :config
  (which-key-mode))

(use-package base16-theme
  :ensure t)

(use-package helpful
  :defer t)

;; (use-package enlight DASHBOARD FOR LATER
;; lsp-ui for sideline

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
	'((small
	   :default-family "CommitMono"
	   :default-height 95)
	  (regular)
	  (medium
	   :default-weight regular
	   :default-height 150
	   :bold-weight bold)
	  (large
	   :inherit medium
	   :default-height 170)
	  (t
	   :default-family "CommitMono"
	   :default-height 120
	   )))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1)
  )

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "slant")
  (setq centaur-tabs-height 20)
  (setq centaur-tabs-icon-type 'nerd-icons)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-bar 'over)
  :bind
  ("C-c [" . centaur-tabs-backward)
  ("C-c ]" . centaur-tabs-forward))

(use-package goto-line-preview
  :config
  (global-set-key [remap goto-line] 'goto-line-preview)
  (setq goto-line-preview-hl-duration 2.5))

(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  )

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (css-fontify-colors nil)
  :config
  (add-hook 'css-mode-hook #'colorful-mode)
  ;; (add-hook 'css-mode-hook #'colorful-mode) TODO: SET MARKDOWN
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package highlight-thing
  :config
  (add-hook 'lsp-mode-hook #'highlight-thing-mode)
  (setq highlight-thing-all-visible-buffers-p t
	highlight-thing-case-sensitive-p nil
	highlight-thing-delay-seconds 0.2))

(use-package anzu
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package mwim
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning)
  (global-set-key (kbd "C-e") 'mwim-end))

(use-package hydra)

(use-package counsel)

(use-package prescient
  :config
  (prescient-persist-mode 1)
  (setq prescient-history-length 10
	prescient-filter-method 'fuzzy))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode)
  (setq ivy-prescient-retain-classic-highlighting t
	ivy-prescient-enable-filtering t
	ivy-prescient-enable-sorting t)
  )

(use-package ivy
  :config
  (ivy-mode)
  (counsel-mode)
  (setopt ivy-use-virtual-buffers t
	  enable-recursive-minibuffers t
	  search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "%d/%d "
	ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  (keymap-global-set "C-s" 'swiper-isearch)
  (keymap-global-set "M-x" #'counsel-M-x))

(use-package swiper
  :bind (:map isearch-mode-map
              ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :config
  (progn
    (bind-key "M-a" #'swiper-avy swiper-map)))

(use-package telephone-line
  :config
  (telephone-line-mode 1)
  (setq telephone-line-subseparator-faces '())
  (setq telephone-line-height 18
	telephone-line-evil-use-short-tag t))

(use-package literate-calc-mode)

(use-package nerd-icons)

(use-package smartparens
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode lsp-mode emacs-lisp-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package expand-region
  :ensure expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region)
  )

(use-package rainbow-delimiters
  :hook (prog-mode lsp-mode emacs-lisp-mode markdown-mode))

(use-package simpleclip
  :config
  (simpleclip-mode 1))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  )

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package yasnippet
  :config
  (global-set-key (kbd "C-c s") 'yas-insert-snippet))
(use-package yasnippet-snippets)
(use-package ivy-yasnippet
  :config
  (global-set-key [remap yas-insert-snippet] 'ivy-yasnippet))

(use-package string-inflection)

(use-package apheleia
  :config
  (apheleia-global-mode +1)) ;; won't load until first save

(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; dispatchers for corfu + orderless

(defun my/orderless-first-literal (pattern index _total)
  "Match the first component literally."
  (when (= index 0)
    'orderless-literal))

(defun my/orderless-rest-prefix (pattern index _total)
  "Match subsequent components as word PREFIXES."
  (when (> index 0)
    'orderless-prefixes))

(use-package corfu
  :init
  (global-corfu-mode +1)
  ;; extensions
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (corfu-echo-mode)
  (corfu-indexed-mode)
  ;; (corfu-popupinfo-mode)
  (corfu-quick-complete) ;; dunno how this works
  
  :config
  (setq corfu-auto t
	;; corfu-quit-no-match 'separator
	corfu-cycle t
	corfu-quit-at-boundary nil
	corfu-preselect 'first
	corfu-auto-delay 0.0
	corfu-auto-prefix 2
	corfu-preview-current 'insert
	corfu-quit-at-boundary 'separator
	global-corfu-minibuffer (lambda ()
				  (not (or (bound-and-true-p mct--active)
					   (bound-and-true-p vertico--input)
					   (eq (current-local-map) read-passwd-map))))
	corfu-echo-delay 0.1
	corfu-popupinfo-delay 0.25
	corfu-popupinfo-max-width 120
	corfu-popupinfo-min-width 60
	corfu-popupinfo-max-height 60
	corfu-popupinfo-min-height 30
	)


  (add-hook 'corfu-mode-hook
	    (lambda ()
	      (orderless-define-completion-style flexible-cmp
		;; Base matching styles (used when no dispatcher applies):
		(orderless-matching-styles '(orderless-regexp))
		;; Our dispatchers, in order:
		(orderless-style-dispatchers
		 '(my/orderless-first-literal
		   my/orderless-rest-prefix)))
	      ;;	      (orderless-define-completion-style orderless-literal-only
	      ;;		(orderless-style-dispatchers nil)
	      ;;		(orderless-matching-styles '(orderless-literal))
	      ;;		)
	      (setq-local completion-styles '(orderless flexible-cmp basic)
			  completion-category-overrides '((lsp-completion-provider (styles flexible-cmp basic)))
			  completion-category-defaults nil)))
  )

(use-package corfu-prescient
  :config
  (corfu-prescient-mode +1)
  (setq corfu-prescient-enable-sorting t
	corfu-prescient-enable-filtering t)
  )

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package quickrun)

(use-package lsp-mode
  :hook (
	 (lsp-mode . sideline-mode) ;; more languages below
	 )
  :config
  (setq lsp-keymap-prefix "s-l"
	))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  (require 'lsp-pyright)
  :hook ((lsp-mode-hook . python-mode)))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-position 'at-point
	lsp-ui-sideline-enable nil)

  
  :bind
  ("C-h h" . lsp-ui-doc-glance))

(use-package sideline-lsp)

(use-package sideline
  :init
  (setq sideline-backends-left-skip-current-line t    ; don't display on current line (left)
	sideline-backends-right-skip-current-line nil
	sideline-order-left 'up                       ; or 'up
	sideline-format-left "■ %s   "                ; format for left aligment
	sideline-format-right "   %s"                 ; format for right aligment
	sideline-priority 100                         ; overlays' priority
	sideline-display-backend-name t               ; display the backend name
	sideline-backends-right '(sideline-lsp sideline-flycheck)
	sideline-backends-left '()
	sideline-force-display-if-exceeds nil
	sideline-truncate t))

(add-hook 'ts-fold-on-fold-hook #'sideline-render-this)

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :config
  (let ((sideline-font-height 280)
	(sideline-backend-height (truncate (* 280 0.7)))
	)
    (set-face-attribute 'sideline-flycheck-error nil    :family "IBM Plex Sans" :weight 'thin :height sideline-font-height :foreground "firebrick3")
    (set-face-attribute 'sideline-flycheck-warning nil  :family "IBM Plex Sans" :weight 'thin :height sideline-font-height :foreground "goldenrod2")
    (set-face-attribute 'sideline-flycheck-info nil     :family "IBM Plex Sans" :weight 'thin :height sideline-font-height :slant 'italic :foreground "#00b3b3")
    (set-face-attribute 'sideline-backend nil           :family "IBM Plex Sans" :weight 'thin :height sideline-backend-height :width 'semi-expanded :italic t :foreground "#e6e6e6")))

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/.config/emacs-config" "~/Developer/projects")
	projectile-project-root-files-top-down-recurring '(".projectile"))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("D" "~/Downloads/"                "Downloads")
     ("d" "~/Developer"                 "Developer")))
  
  :config
  (dirvish-peek-mode)             ; Preview files in minibuffer
  (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'

  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index))
	dirvish-attributes
	(append
	 ;; The order of these attributes is insignificant, they are always
	 ;; displayed in the same position.
	 '(vc-state subtree-state nerd-icons collapse)
	 ;; Other attributes are displayed in the order they appear in this list.
	 '(git-msg file-modes file-time file-size))
	dired-mouse-drag-files t
	mouse-drag-and-drop-region-cross-program t
	delete-by-moving-to-trash t
	dirvish-use-header-line 'global
	dirvish-mode-line-bar-image-width 0
	dirvish-header-line-height '(15 . 5)
	dirvish-header-line-format
	'(:left (path) :right (free-space))
	dirvish-mode-line-format
	'(:left (sort "|| Properties -> " file-time " " file-size symlink) :right (omit yank index))
	)
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)
  (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c e" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switche2s-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))


(use-package org-bullets
  :hook (org-mode-hook . (lambda () (org-bullets-mode 1))))

(use-package gptel
  :config
  (setq  gptel-backend (gptel-make-openai "Groq"
			 :host "api.groq.com"
			 :endpoint "/openai/v1/chat/completions"
			 :key (auth-source-pick-first-password :host "api.groq.com")
			 :stream t
			 :models '(llama-3.1-70b-versatile
				   llama-3.1-8b-instant
				   llama3-70b-8192
				   llama3-8b-8192
				   gemma-7b-it
				   gemma2-9b-it
				   meta-llama/llama-4-maverick-17b-128e-instruct
				   meta-llama/llama-4-scout-17b-16e-instruct
				   mistral-saba-24b
				   qwen-qwq-32b
				   compound-beta
				   compound-beta-mini))
	 gptel-model   'gemma2-9b-it
	 gptel-default-mode 'org-mode)

  (setopt gptel-prompt-prefix-alist
          `((markdown-mode . ,(concat "me ›  "))
            (org-mode . ,(concat  "me ›  "))
            (text-mode . ,(concat "me ›  ")))
	  gptel-response-prefix-alist
	  '((markdown-mode . "ai  ")
	    (org-mode . "ai  ")
	    (text-mode . "ai  "))

	  gptel-response-separator "\n\n"
	  gptel-confirm-tool-calls 'auto
	  gptel-use-tools t
	  gptel-display-buffer-action '(display-buffer-in-previous-window)
	  gptel-org-branching-context nil
	  gptel-expert-commands t)
  ;;'(gptel-user-name-font ((t (:inherit default :background "gray97" :foreground "selectedTextBackgroundColor" :inverse-video t :weight bold :family "Roboto Slab"))))
  (defface gptel-user-name-font
    '((t :family "Inter"
         :weight semi-bold
         :foreground "selectedTextBackgroundColor"
	 :background "gray97"
         :inverse-video t
         :inherit default))
    "User prompt face")
  (defface gptel-assistant-name-font
    '((t :family "Inter"
         :weight semi-bold
         :foreground "purple"
	 :background "gray97"
         :inverse-video t
         :inherit default))
    "Assistant prompt face")
  (defface gptel-assistant-text
    '((t :family "Inter"
         :weight light
         :foreground "pink"
         :box t
         :inherit default))
    "Assistant prelude face, after the prompt")
  
  ;; Styling our prompts with font locking
  (font-lock-add-keywords
   'org-mode
   `(("^\\(me ›[ ]?\\)"
      (1 '(face gptel-user-name-font line-prefix
                ,(propertize " " 'face 'gptel-user-name-font))))))

  (font-lock-add-keywords
   'org-mode
   `(("^\\(ai \\)"
      (1 '(face gptel-assistant-name-font line-prefix
                ,(propertize " " 'face 'gptel-assistant-name-font)))
      )))
  
  
  :bind
  (("C-c g g" . gptel-menu)
   ("C-c g x" . gptel-abort)))

(use-package aidermacs
  :init
  (setq aidermacs-backend 'vterm)
  :bind (("C-c a" . aidermacs-transient-menu)))

(use-package bufler
  :bind (("C-x C-b" . bufler-list)))

;;; packages.el ends here

