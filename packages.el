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

;; (package! tree-sitter)
;; (package! tree-sitter-langs)

(package! company-posframe)
(package! mixed-pitch)
(package! org-appear)

(package! websocket)
(package! simple-httpd)
(package! f)

(unpin! org-roam)
(package! org-roam-ui)
(package! org-transclusion)
(package! org-contrib)
(package! org-fancy-priorities)
(package! elpy)
(package! deferred)
(package! epc)
(package! ctable)

(package! buffer-env)
(package! deft)


(package! paradox)
;; (package! fira-code-mode)

(package! elm-mode)
