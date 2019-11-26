(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/"))
                  tls-checktrust t
                  tls-program '("gnutls-cli --x509cafile %t -p %p %h")
                  gnutls-verify-error t)
;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)

;;(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package all-the-icons)
(use-package doom-modeline)
(use-package py-autopep8)
(use-package smex)
(use-package pyenv-mode)
(use-package elpy)
(use-package doom-themes)
(use-package neotree)
(use-package go-mode)
(use-package editorconfig)
(use-package git-gutter-fringe)
(use-package yaml-mode)

(provide 'packages-setup)
