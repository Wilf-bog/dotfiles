(setq custom-file (make-temp-file "emacs-custom-"))



;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure nil)	; older config had it true.
  (package-native-compile t)
  (warning-minimum-level :emergency))

(setq custom-safe-themes t)

(load-theme 'modus-vivendi-tinted :no-confirm)

(use-package magit
  :ensure t)

(add-hook 'magit-process-find-password-functions
	  'magit-process-password-auth-source)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (dolist (item '(("sh" . "src shell")
		    ("el" . "src emacs-lisp")
		    ("cel" . "src emacs-lisp :tangle .emacs.d/init.el")
		    ("cco" . "src conf :tangle DIR")
		    ("py" . "src python")))
    (add-to-list 'org-structure-template-alist item)))
