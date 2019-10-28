(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/"))
                  tls-checktrust t
                  tls-program '("gnutls-cli --x509cafile %t -p %p %h")
                  gnutls-verify-error t)
;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)

;;(package-initialize)

(provide 'packages-setup)
