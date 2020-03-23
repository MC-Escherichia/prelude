(package-initialize)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
;;(package-refresh-contents)
(eval-when-compile
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
(require 'use-package-ensure)
(setq use-package-always-ensure t)  

(use-package dash)
(use-package dash-functional)
(use-package s)
(use-package f)
(use-package kv)
(use-package ht)



(provide 'mfc-packages)
