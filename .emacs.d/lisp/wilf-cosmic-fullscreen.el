(defun my/toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(global-set-key (kbd "<f11>") #'my/toggle-fullscreen)
