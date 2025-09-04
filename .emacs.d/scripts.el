;; A function to disable all the themes. See:
;; https://emacsredux.com/blog/2025/02/03/clean-unloading-of-emacs-themes/

(defun wilf-disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
