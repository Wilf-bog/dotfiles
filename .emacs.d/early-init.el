;; Copyright (C) 2025  Frédéric Vachon

;; Author: Frédéric Vachon <vachonfrederic@proton.me>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(setq package-enable-at-startup nil)
