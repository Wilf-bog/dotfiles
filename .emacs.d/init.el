(setq custom-file (make-temp-file "emacs-custom-"))



(use-package magit
  :ensure t)

(add-hook 'magit-process-find-password-functions
	  'magit-process-password-auth-source)
