;;; config.el --- Configurations  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq backup-directory-alist '((".*" . "~/.cache/emacs-backup-dir")))

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(text-scale-set 3)
(golden-ratio-mode 1)
(beacon-mode 1)
(delete-selection-mode 1)

(load-theme 'cyberpunk t)

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; hydras
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(ivy-mode 1)
;; (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
;; (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
;; (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-done)
;; (define-key ivy-minibuffer-map (kbd "C-M-h") 'ivy-previous-line-and-call)
;; (define-key ivy-minibuffer-map (kbd "C-:") 'ivy-dired)
;; (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
;; (define-key ivy-minibuffer-map (kbd "C-c C-a") 'ivy-read-action)

(defhydra dumb-jump-hydra
  (:color blue :columns 3 :global-map "<f3>")
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

(setq counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"
      ivy-use-selectable-prompt t
      ivy-display-style 'fancy
      ivy-read-action-function #'ivy-hydra-read-action)

(defun ivy-dired ()
  (interactive)
  (if ivy--directory
      (ivy-quit-and-run
	(dired ivy--directory)
	(when (re-search-forward
               (regexp-quote
		(substring ivy--current 0 -1)) nil t)
          (goto-char (match-beginning 0))))
    (user-error
     "Not completing files currently")))
(put 'ivy-dired 'no-counsel-M-x t)

(setq ivy-switch-buffer-faces-alist
      '((emacs-lisp-mode . swiper-match-face-1)
        (dired-mode . ivy-subdir)
        (org-mode . org-level-4)))

(setq counsel-grep-base-command "grep -niE %s %s"
      counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s %s"
      counsel-rg-base-command (split-string
			       "rg --hidden --glob=!.git/ --sort path -M 240 --no-heading --line-number --color never %s")
      counsel-git-grep-cmd-default
      (concat "git --no-pager grep --full-name -n --no-color -i -e '%s' -- './*' "
              (mapconcat (lambda (x) (format "':!*.%s'" x))
                         '("htm" "so" "a" "TTC" "NDS" "png" "md5") " "))
      counsel-fzf-dir-function
      (lambda ()
        (let ((d (locate-dominating-file default-directory ".git")))
          (if (or (null d)
                  (equal (expand-file-name d)
                         (expand-file-name "~/")))
              default-directory
            d)))
      )

(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

(defun auto-string-inflection ()
  "Switching by major-mode."
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(global-set-key (kbd "C-c C-w") 'auto-string-inflection)

(set-frame-font "CommitMono:size=18" nil t)

(global-set-key (kbd "C-c f f") 'hs-toggle-hiding)
(global-set-key (kbd "C-c f h") 'hs-hide-all)
(global-set-key (kbd "C-c f s") 'hs-show-all)

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(let* ((variable-tuple '(:font "Inter"))
       (base-font-color (face-foreground 'default nil 'default))
       (headline `(:inherit default :weight bold :foreground ,base-font-color)))
  
  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Inter" :height 180 :weight thin))))
 '(fixed-pitch ((t ( :family "CommitMono" :height 160)))))


(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch) :height 3))))
 '(org-document-info ((t (:foreground "dark orange" :height 3))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 ;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch) :height 1.1 :background "gray")))))

(message "Finished evaluating config.el")

;;; config.el ends here


