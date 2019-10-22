
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "c:/Users/Filip/AppData/Roaming/.emacs.d/settings")

(require 'packages-setup)
(require 'better-defaults)
(require 'sane-defaults)
(require 'key-bindings)
(require 'theme-settings)

;; Window size on startup
(when window-system (set-frame-size (selected-frame) 180 60))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-strach-message nil)


(setq linum-format " %d ")

;; Highlight current line
(global-hl-line-mode 1)

;; Do not create # or ~ files everywhere
(setq create-lockfiles nil)

;; Line numbers
(global-display-line-numbers-mode t)


;; Show git line diff in margin
(global-git-gutter-mode +1)

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 95
                    :weight 'normal
                    :width 'normal)

;; Faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "default"))))
 '(linum ((t (:foreground "gray"))))
 '(mode-line ((t (:background "black" :foreground "default" :box (:line-width 5 :color "black")))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "white" :box (:line-width 5 :color "black")))))
 '(vertical-border ((t (:background "default" :foreground "black")))))

;; Mode line // Minimal version, Filip edited
;; (setq-default mode-line-format '(" %b%*   %l:%c   %m")

(require 'all-the-icons)
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Obey editorconfig
(editorconfig-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-modeline editorconfig git-gutter-fringe zenburn-theme))))

