;;; package --- config init.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(setq package-enable-at-startup nil)


(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defvar emacs-config-directory "~/.config/emacs-config")

(let ((default-directory emacs-config-directory))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  
  (require 'beacon)
  (require 'golden-ratio))

(defvar emacs-lib-directory (expand-file-name "lib" emacs-config-directory))
(defvar emacs-themes-directory (expand-file-name "themes" emacs-config-directory))

(add-to-list 'custom-theme-load-path emacs-themes-directory)
(dolist (theme-dir (directory-files emacs-themes-directory t "^[^.]"))
  (when (file-directory-p theme-dir)
    (add-to-list 'custom-theme-load-path theme-dir)))

(load (expand-file-name "packages.el" emacs-config-directory))
(load (expand-file-name "config.el" emacs-config-directory))

(require 'auth-source-pass)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a1c18db2838b593fba371cb2623abd8f7644a7811ac53c6530eebdf8b9a25a8d"
     default))
 '(package-selected-packages
   '(aggressive-indent aidermacs anzu apheleia base16-theme bufler cape
		       centaur-tabs colorful-mode
		       corfu-candidate-overlay corfu-prescient counsel
		       dired dirvish drag-stuff dumb-jump
		       expand-region fontaine forge goto-line-preview
		       helpful highlight-thing hydra ivy-prescient
		       ivy-yasnippet literate-calc-mode lsp-pyright
		       lsp-ui mwim nerd-icons orderless projectile
		       quickrun rainbow-delimiters sideline-flycheck
		       sideline-lsp simpleclip smartparens
		       solaire-mode string-inflection telephone-line
		       ts-fold undo-fu undo-fu-session
		       yasnippet-snippets))
 '(safe-local-variable-values '((flycheck-disabled-checkers emacs-lisp-checkdoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#d3d3d3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 160 :width normal :foundry "nil" :family "CommitMono"))))
 '(fixed-pitch ((t (:family "CommitMono" :height 160))))
 '(flycheck-info ((t (:underline (:color "#00b3b3" :style wave :position nil)))))
 '(lsp-flycheck-info-unnecessary-face ((t (:foreground "gray" :underline (:color "#00b3b3" :style wave :position nil)))) t)
 '(lsp-headerline-breadcrumb-symbols-info-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "#00b3b3" :style wave :position nil)))))
 '(lsp-ui-sideline-global ((t (:height 2.0 :family "Raleway"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch) :height 3))))
 '(org-document-info ((t (:foreground "dark orange" :height 3))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter" :height 2.0 :underline nil))))
 '(org-done ((t (:background "#00ff00" :foreground "black" :box (1 . 1) :weight bold))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "Inter"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-todo ((t (:background "dark salmon" :foreground "black" :box (1 . 1) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch) :height 1.1 :background "gray"))))
 '(variable-pitch ((t (:family "Inter" :height 180 :weight thin)))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; init.el ends here
