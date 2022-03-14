;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-super-agenda)
;; (package! clj-refactor)
(package! google-this)
(package! git-link)
(package! browse-at-remote)
;; (package! explain-pause-mode)

;; replacement/enhancement for dired
(package! dirvish)

;; Org
(package! org-present :recipe
  (:host github
   :repo "rlister/org-present"
   :files ("*.el")))

;; Python
(package! pyvenv)
(package! pydoc :recipe
  (:host github
   :repo "statmobile/pydoc"
   :files ("*.el")))
(package! company-anaconda)
;; (package! pylookup)
(package! pymacs)


;; (package! focus)
;; (package! beacon)

;; (package! live-py-mode)

(package! direnv)
