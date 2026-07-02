;;; wilf-cosmic-fullscreen.el --- Better fullscreen toggle for COSMIC/Wayland -*- lexical-binding: t; -*-

(defun my/toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(global-set-key (kbd "<f11>") #'my/toggle-fullscreen)

(provide 'wilf-cosmic-fullscreen)

;;; wilf-cosmic-fullscreen.el ends here
