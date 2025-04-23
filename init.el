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


(let* ((emacs-config-directory "~/.config/emacs-config"))
  (setq custom-theme-directory "~/.config/emacs-config/themes/")

  (let ((default-directory (expand-file-name "lib" emacs-config-directory)))

    (normal-top-level-add-subdirs-to-load-path)
    (require 'beacon)
    (require 'golden-ratio))

  (load (expand-file-name "packages.el" emacs-config-directory))
  (load (expand-file-name "config.el" emacs-config-directory)))

(require 'auth-source-pass)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aggressive-indent anzu apheleia base16-theme cape centaur-tabs
		       colorful-mode corfu-candidate-overlay
		       corfu-prescient counsel dired dirvish
		       drag-stuff dumb-jump expand-region fontaine
		       goto-line-preview helpful highlight-thing hydra
		       ivy-prescient ivy-yasnippet literate-calc-mode
		       lsp-pyright lsp-ui mwim nerd-icons orderless
		       projectile quickrun rainbow-delimiters
		       sideline-flycheck sideline-lsp simpleclip
		       smartparens solaire-mode string-inflection
		       telephone-line ts-fold undo-fu undo-fu-session
		       yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type graphic)) :family "CommitMono" :height 120)))
 '(flycheck-info ((t (:underline (:color "#00b3b3" :style wave :position nil)))))
 '(lsp-flycheck-info-unnecessary-face ((t (:foreground "gray" :underline (:color "#00b3b3" :style wave :position nil)))) t)
 '(lsp-headerline-breadcrumb-symbols-info-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "#00b3b3" :style wave :position nil)))))
 '(lsp-ui-sideline-global ((t (:height 2.0 :family "Raleway")))))
