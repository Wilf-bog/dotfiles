(setq frame-resize-pixelwise t
      frame-title-format '("%b")	; Name of file as the title of the frame
      use-file-dialog nil
      use-short-answers t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      visible-bell t
      )

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq gc-cons-threshold (* 10 128 1024 1024))
(setq garbage-collection-messages nil)
