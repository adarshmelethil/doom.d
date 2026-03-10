;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Set Variables            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
;;
;;

(setq
 ;; META
 user-full-name "Adarsh Melethil"
 user-mail-address "adarshmelethil@gmail.com"
 ;; GLOBALS
 my/work-dir (concat (file-name-as-directory (getenv "HOME")) "work")
 my/org-dir (concat (file-name-as-directory my/work-dir) "org")
 my/src-dir (concat (file-name-as-directory my/work-dir) "src")
 my/scripts-dir (concat (file-name-as-directory my/work-dir) "scripts")
 my/bin-dir (concat (file-name-as-directory my/work-dir) "bin")
 my/conda-root (concat (file-name-as-directory (getenv "HOME")) "miniforge3")
 projectile-project-search-path `(,my/src-dir)

 ;; Line numbers
 global-display-line-numbers-mode 'visual
 display-line-numbers-type 'visual
 display-line-numbers 'visual
 column-number-mode t
 display-line-numbers-major-tick 0
 display-line-numbers-grow-only t

 ;; Parens
 show-paren-style 'mixed

 ;; Fonts
 ;; -*-ProggyCleanTT Nerd Font Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1
 ;; ProggyCleanTT Nerd Font Mono Book
 ;; ProggyCleanTT Nerd Font Mono-normal-normal-normal
 ;; doom-font (font-spec :family "ProggyCleanTT Nerd Font Mono" :size 24 :weight 'light)
 ;; doom-font (font-spec :family "Atkinson Hyperlegible" :size 16)
 ;; doom-font (font-spec :family "3270Medium Nerd Font Mono" :size 12)
 ;; doom-theme 'doom-palenight
 ;; Themes
 doom-theme 'doom-gruvbox
 ;; gotham-theme; doom-outrun-electric


 org-babel-napkin-plantuml-server-url "http://localhost:8080")

(setenv "WORKON_HOME" (concat (file-name-as-directory my/conda-root) "envs"))

;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
(setq plantuml-default-exec-mode 'jar
      org-plantuml-jar-path (expand-file-name "~/work/scripts/plantuml-1.2025.4.jar")
      plantuml-jar-path org-plantuml-jar-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Functions                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (list-directory my/src-dir)
;; (insert-directory my/src-dir "-l")
(require 'seq)
(require 'dash)
;; (file-directory-p my/src-dir)

(defun complement (f)
  (lambda (&rest args)
    (not (apply f args))))

(cl-iter-defun git-projects-under-directory (dir)
  (dolist (sub-dir (seq-filter (complement #'directory-empty-p) (seq-filter #'file-accessible-directory-p (directory-files dir t "^[^.]"))))
    (if (file-exists-p (concat (file-name-as-directory sub-dir) ".git"))
        (iter-yield (file-name-as-directory (abbreviate-file-name sub-dir)))
      (iter-yield-from (git-projects-under-directory sub-dir)))))

(cl-iter-defun iter-filter (func iter)
  (iter-do (val iter)
    (when (apply func (list val))
      (iter-yield val))))

(cl-iter-defun new-projects (&optional root-project-dir)
  (iter-yield-from (iter-filter
                    (complement (-rpartial #'member projectile-known-projects))
                    (git-projects-under-directory (or root-project-dir my/src-dir)))))

(defun add-all-new-projects (project-root)
  (interactive (list (read-directory-name "Add to known projects: " "~/" my/src-dir)))
  (iter-do (p (new-projects))
    (message "Adding project: %s" p)
    (projectile-add-known-project p)
    ))

;; (add-all-new-projects my/src-dir)

(string-match-p directory-files-no-dot-files-regexp "/Users/adarsh.melethil/work/src/..")
(seq-first (directory-files-recursively my/src-dir directory-files-no-dot-files-regexp))
(seq-filter 'file-directory-p (directory-files my/src-dir t directory-files-no-dot-files-regexp))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 3))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
(defun my/projectile-pyvenv-workon-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (message "project: %s" project)
    (if (member project (directory-files (getenv "WORKON_HOME")))
        (progn
          (message "Using conda env '%s'" project)
          (pyvenv-workon project)
          ;; (conda-env-activate project)
          )
      (progn
        (message "No env found with name '%s'" project)
        ;; (pyvenv-deactivate)
        ;; (conda-env-deactivate)
        ))))
(defun my/split-window (pos)
  (cond
   ((string= pos "right")
    (progn
      (split-window-horizontally)
      (evil-window-right 1)))
   ((string= pos "left")
    (split-window-horizontally))
   ((string= pos "up")
    (split-window-vertically))
   ((string= pos "down")
    (progn
      (split-window-vertically)
      (evil-window-down 1)))))
(defun my/vterm-send-escape ()
  (interactive)
  (vterm-send-key "<escape>"))
(defun my/remove-local-before-save-hooks ()
  (interactive)
  (remove-hook 'before-save-hook #'ws-butler-before-save t)
  (remove-hook 'before-save-hook #'format-all-buffer--from-hook t))
(defun my/load-exports-from-script (env-file)
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents env-file)
    (let* ((lines (split-string (buffer-string) "\n" t "\s*"))
           (export-lines (mapcar (lambda (elt) (substring elt 7))
                                 (seq-filter
                                  (apply-partially #'string-prefix-p "export ")
                                  lines)))
           (var-values (mapcar (lambda (elt) (split-string elt "=")) export-lines)))
      (message "%s" var-values)
      (cl-loop for (key . value) in var-values do
               (message "%s -> %s" key (car value))
               (setenv key (car value))))))
(defun my/search-action (x)
  "Search for X."
  (if (doom-region-active-p)
      (progn
        (delete-region (doom-region-beginning) (doom-region-end))
        (insert x))
    (insert x)))
(defun my/spell-check (query)
  (interactive
   (list (if (use-region-p) (doom-thing-at-point-or-region))))
  (require 'request)
  (require 'json)
  (message "-%s-" (doom-region-active-p))
  (let ((ivy-initial-inputs-alist `((t . ,query)))
        (counsel-search-engine 'google))
    (ivy-read "search: " #'counsel-search-function
              :action #'my/search-action
              :dynamic-collection t
              :caller 'my/spell-check)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Python setup             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           projectile setup          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Org Mode Setup           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))
;; (require 'sqlformat)  ; manual install

(map!
 ;; split navigation
 :n "<down>" 'evil-window-down
 :n "<left>" 'evil-window-left
 :n "<up>" 'evil-window-up
 :n "<right>" 'evil-window-right

 ;; split resizing
 :n "+" 'evil-window-increase-width
 :n "_" 'evil-window-decrease-width
 :n "M-=" 'evil-window-increase-height
 :n "M--" 'evil-window-decrease-height
 ;; split creation
 (:leader
  :n "<right>" (lambda () (interactive) (my/split-window "right"))
  :n "<up>" (lambda () (interactive) (my/split-window "up"))
  :n "<left>" (lambda () (interactive) (my/split-window "left"))
  :n "<down>" (lambda () (interactive) (my/split-window "down"))
  :nv "s c" #'my/spell-check
  :nv "F s" #'sqlformat-region

  :n "p A" #'add-all-new-projects

  ;; git-link
  :n "Y g" #'git-link-homepage
  :n "c b" #'browse-at-remote

  :n "r h" #'my/remove-local-before-save-hooks
  :n "l e" #'my/load-exports-from-script)
 (:after org
  :map 'org-mode-map
  :n "M-j" #'org-metadown
  :n "M-k" #'org-metaup)

 (:after vterm-mode
  :map '(vterm-mode-map eshell-mode-map)
  :i "<f13>" 'my/vterm-send-escape)
 )

;; (use-package! sqlformat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Miscellaneous             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             Custom Code             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.doom.d/gateway-core.el")


(defun efs/presentation-setup ()
  (message "efs/presentation-setup")
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

;; This option is more advanced, allows you to scale other faces too
;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
;;                                    (org-verbatim (:height 1.75) org-verbatim)
;;                                    (org-block (:height 1.25) org-block))))

(defun efs/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  ;; (text-scale-mode 0))

  ;; If you use face-remapping-alist, this clears the scaling:
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package! org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")

  (org-image-actual-width nil))


;; (use-package! org-tree-slide
;;   :custom
;;   (org-image-actual-width nil))

;; (use-package! visual-fill-column
;;   :config
;;   (setq-default visual-fill-column-width 110
;;         visual-fill-column-center-text t)
;;   )
;; Configure fill width
;; (defun my/org-present-start ()
;;   ;; Center the presentation and wrap lines
;;   (setq visual-fill-column-width '210
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1)
;;   (visual-line-mode 1))

;; (defun my/org-present-end ()
;;   ;; Stop centering the document
;;   (visual-fill-column-mode 0)
;;   (visual-line-mode 0))

;; Set reusable font name variables
(defvar my/fixed-width-font "JetBrains Mono"
  "The font to use for monospaced (fixed width) text.")

(defvar my/variable-width-font "Iosevka Aile"
  "The font to use for variable-pitch (document) text.")

;; NOTE: These settings might not be ideal for your machine, tweak them as needed!
;; (set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 180)
;; (set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 190)
;; (set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.3)

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  ;; (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face))
  )

;; Make the document title a bit bigger
;; (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
                                        ;(set-face-attribute 'org-block :foreground nil :inherit 'fixed-pitch)
                                        ;(set-face-attribute 'org-table :inherit 'fixed-pitch)
                                        ;(set-face-attribute 'org-formula :inherit 'fixed-pitch)
                                        ;(set-face-attribute 'org-code :inherit '(shadow fixed-pitch))
                                        ;(set-face-attribute 'org-verbatim :inherit '(shadow fixed-pitch))
                                        ;(set-face-attribute 'org-special-keyword :inherit '(font-lock-comment-face fixed-pitch))
                                        ;(set-face-attribute 'org-meta-line :inherit '(font-lock-comment-face fixed-pitch))
                                        ;(set-face-attribute 'org-checkbox :inherit 'fixed-pitch)

;;; Centering Org Documents --------------------------------

;; Configure fill width

;;; Org Present --------------------------------------------

;; Install org-present if needed
;; Register hooks with org-present

;; (defun my/org-present-prepare-slide (buffer-name heading)
;;   ;; Show only top-level headlines
;;   (org-overview)

;;   ;; Unfold the current entry
;;   (org-show-entry)

;;   ;; Show only direct subheadings of the slide but don't expand them
;;   (org-show-children))

;; (defun my/org-present-start ()
;;   ;; Tweak font sizes
;;   (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
;;                                      (header-line (:height 4.0) variable-pitch)
;;                                      (org-document-title (:height 1.75) org-document-title)
;;                                      (org-code (:height 1.55) org-code)
;;                                      (org-verbatim (:height 1.55) org-verbatim)
;;                                      (org-block (:height 1.25) org-block)
;;                                      (org-block-begin-line (:height 0.7) org-block)))

;;   ;; Set a blank header line string to create blank space at the top
;;   (setq header-line-format " ")

;;   ;; Display inline images automatically
;;   (org-display-inline-images)

;;   ;; Center the presentation and wrap lines
;;   ;; (visual-fill-column-mode 1)
;;   ;; (visual-line-mode 1)
;;   )

;; (defun my/org-present-end ()
;;   ;; Reset font customizations
;;   (setq-local face-remapping-alist '((default variable-pitch default)))

;;   ;; Clear the header line string so that it isn't displayed
;;   (setq header-line-format nil)

;;   ;; Stop displaying inline images
;;   (org-remove-inline-images)

;;   ;; Stop centering the document
;;   ;; (visual-fill-column-mode 0)
;;   (visual-line-mode 0))


;; (use-package! org-present
;;   :config
;;   ;; (add-hook 'org-present-mode-hook 'my/org-present-start)
;;   ;; (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
;;   ;; (setq visual-fill-column-width 110
;;   ;;       visual-fill-column-center-text t)

;;   ;; Turn on variable pitch fonts in Org Mode buffers
;;   ;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;;   ;; Register hooks with org-present
;;   (add-hook 'org-present-mode-hook 'my/org-present-start)
;;   (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
;;   (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)
;;   )


;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package! ponylang-mode
  :ensure t
  :init
  (setq ponylang-banner 1)
  :config
  :bind-keymap
  ("<f6>" . ponylang-menu))
;; (setq create-lockfiles nil)
(add-to-list 'load-path (expand-file-name "~/.doom.d"))
;; (autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
;; (add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
;; (add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
;; (add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
;; (add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")

(use-package! nix-mode
  :mode "\\.nix\\'")

;; (add-to-list 'load-path "~/.doom.d/packages/emacs-reveal")
;; (require 'emacs-reveal)
;; (setq tree-sitter-langs-git-dir (straight--repos-dir "tree-sitter-langs"))

(custom-set-variables
 `(conda-anaconda-home ,(expand-file-name "~/miniforge3/")))

;; (conda-mode-line-setup)
(pyvenv-workon "gw3.11")

(use-package! groovy-mode)
(use-package! jenkinsfile-mode)
(use-package! terraform-mode)
