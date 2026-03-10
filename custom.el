(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/adarsh.melethil/miniforge3/")
 '(ignored-local-variable-values '((c-macro-preprocessor . "c++ -x c++ -std=c++20 -E -")))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(docker dockerfile-mode haskell-mode lsp-docker sqlformat terraform-mode))
 '(safe-local-variable-values
   '((setq-default flycheck-disabled-checkers '(python-pylint))
     (eval pyvenv-workon "cards") (pyvenv-workon . "stdlib")
     (sql-postgres-login-params
      '((user :default "postgres") (passowrd "postgres")
        (database :default "postgres") (server :default "localhost")
        (port :default 5432)))
     (pyvenv-workon . "gw3.11_2")))
 '(warning-suppress-log-types '((native-compiler)))
 '(warning-suppress-types
   '((native-compiler) (native-compiler) (defvaralias) (lexical-binding))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
