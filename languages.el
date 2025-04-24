;;; package --- Language config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (setq web-mode-enable-auto-indentation nil
	lsp-enable-indentation nil))

(use-package prettier-js
  :hook (web-mode-hook . prettier-js-mode))

(use-package reformatter
  :hook
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

(use-package apheleia
  :ensure t
  :defines (apheleia-formatters apheleia-mode-alist)
  :hook (after-init . apheleia-global-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;;; languages.el ends here
