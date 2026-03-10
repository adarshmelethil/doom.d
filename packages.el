;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! hide-mode-line)
(package! visual-fill-column)
;; (package! org-present)
(package! vala-mode)
(package! lfe-mode)
(package! ponylang-mode)

(package! command-log-mode)
(package! flycheck-pony)
;; (package! pony-snippets)

(package! slime)

(package! nix-mode)

(package! htmlize)
;; (package! oer-reveal-publish)
;; (package! ox-reveal)
;; (package! emacs-reveal
;;   :recipe (:type git
;;            :local-repo ""
;;            :repo "emacs-reveal"))

(package! tree-sitter-langs
  :recipe (:type git
           :repo "emacs-tree-sitter/tree-sitter-langs"))

(package! meson-mode
  :recipe (:type git
           :repo "wentasah/meson-mode"))


(package! emacs-bazel-mode
  :recipe (:host github
           :repo "bazelbuild/emacs-bazel-mode"))



(package! cmake-ide
  :recipe (:host github
           :repo "atilaneves/cmake-ide"))

(package! sqlformat)

(package! groovy-mode)
(package! jenkinsfile-mode
  :recipe (:host github
           :repo "john2x/jenkinsfile-mode"))


(package! terraform-mode)
