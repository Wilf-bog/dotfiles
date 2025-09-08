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

;; (push '(menu-bar-lines . 0) default-frame-alist)
    ;; (push '(tool-bar-lines . 0) default-frame-alist)
    ;; (push '(vertical-scroll-bars . nil) default-frame-alist)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq gc-cons-threshold (* 10 128 1024 1024))
(setq garbage-collection-messages nil)

(require 'package)

;;; Setup Emacs Lisp Package Archives (ELPAs)
;; where to get packages to install
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Configure ELPA priorities
;; Prefer GNU sources and stable versions before development versions from MELPA.
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                        ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                        ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                        ; from melpa
