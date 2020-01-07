(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; TODO: Load path depending on OS
;(add-to-list 'load-path "c:/Users/Filip/AppData/Roaming/.emacs.d/settings")
(add-to-list 'load-path "~/dev/dotfiles/emacs/settings/")

(require 'packages-setup)
(require 'better-defaults)
(require 'sane-defaults)
(require 'key-bindings)
(require 'theme-settings)

;; Window size on startup
(when window-system (set-frame-size (selected-frame) 180 55))

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
;; TODO: font height should be smaller on windows, 110?
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Doom modeline is awesome and cool
(doom-modeline-mode 1)
(doom-themes-visual-bell-config)

(setq neo-window-fixed-size nil)
(setq neo-smart-open t)

;; Obey editorconfig
(editorconfig-mode 1)


;; pyenv mode to use correct python versions
(pyenv-mode)

;; This magic checks for .python-version file
(defun ssbb-pyenv-hook ()
"Automatically activates pyenv version if .python-version file exists."
(f-traverse-upwards
(lambda (path)
  (let ((pyenv-version-path (f-expand ".python-version" path)))
		(if (f-exists? pyenv-version-path)
				(pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))
(add-hook 'find-file-hook 'ssbb-pyenv-hook)

;; elpy makes python editing smart
(elpy-enable)

;; ido enables fuzzy matching for finding files/folders
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Smex enables ido-mode for meta commands
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-mode t)
 '(package-selected-packages
   '(py-autopep8 smex elpy doom-modeline doom-themes neotree go-mode editorconfig git-gutter-fringe)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

