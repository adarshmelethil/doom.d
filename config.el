;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;            Set Variables            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

 ;; Line numbers
 global-display-line-numbers-mode 'visual
 display-line-numbers-type 'visual
 display-line-numbers 'visual
 column-number-mode t
 display-line-numbers-major-tick 0

 ;; Parens
 show-paren-style 'mixed

 ;; Fonts
 ;; *func
 ;; List all fonts and insert into current buffer
 ;; (dolist (font (font-family-list))
 ;;   (insert (format "%s\n" font)))
 doom-font (font-spec :family "Input Serif" :size 12)
 ;; American Typewriter; Give You Glory; Bodoni 72 Oldstyle

 ;; Themes
 doom-theme 'doom-gruvbox
 ;; gotham-theme; doom-outrun-electric
 )

(setenv "WORKON_HOME" (concat (file-name-as-directory my/conda-root) "envs"))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;            General Setup            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (pyvenv-workon project)
      (progn (message "No env found with name '%s'" project) (pyvenv-deactivate)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Python setup           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! conda
  :init
  (setenv "ANACONDA_HOME" my/conda-root)
  :custom
  (conda-anaconda-home my/conda-root)
  (conda-system-gud-pdb-command-name "python -m ipdb"))
(use-package! anaconda-mode
  :hook ((python-mode . anaconda-eldoc-mode)
         (python-mode . anaconda-mode)))
(use-package! pyvenv
  :after anaconda-mode
  :hook (python-mode . pyvenv-mode))
(use-package! company-anaconda
  :after anaconda-mode company
  :hook (python-mode . anaconda-mode)
  :config
  (set-company-backend! 'python-mode '(company-anaconda)))
;; (use-package! pymacs
;;   :config
;;   (pymacs-load "ropemacs" "rope")
;;   (setq ropemacs-enable-shortcuts nil)
;;   (setq ropemacs-local-prefix "C-c C-p"))


(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil))
(use-package! python-mode
  :general
  (:states 'normal
   :keymaps 'python-mode-map
   "<SPC> p *" #'load-ropemacs)
  :custom
  python-shell-completion-native-enable nil
  python-indent-offset 2
  python-shell-interpreter "ipython")

;; (setq-hook! python-mode
;;   )
;; (use-package! pylookup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           projectile setup          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! projectile
  :after pyvenv
  :init
  (setq projectile-project-search-path `((,my/src-dir . 3))
        projectile-auto-discover t)
  (add-hook
   'projectile-after-switch-project-hook
   #'my/projectile-pyvenv-workon-set))

(use-package! dash)
(use-package! undo-fu)

(use-package! explain-pause-mode
  :config
  (explain-pause-mode))

(use-package! evil
  :after undo-fu
  :init
  (setq evil-undo-system 'undo-fu))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Org Mode Setup           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-super-agenda
  :after org-agenda
  :custom
  org-super-agenda-mode '((:name "Today"
                           :time-grid t
                           :scheduled today)
                          (:name "Due today"
                           :deadline today)
                          (:name "Important"
                           :priority "A")
                          (:name "Overdue"
                           :deadline past)
                          (:name "Due soon"
                           :deadline future)
                          (:name "Big Outcomes"
                           :tag "bo"))
  :config
  (org-super-agenda-mode))

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

(after! org
  (setq
   org-todo-keywords
   '((sequence
      "TODO(t)"
      "ON-GOING(p)"
      "STARTED(s)"
      "BLOCKED(w)"
      "|"
      "DONE(d)"
      "KILL(k)"))
   org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("STARTED" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("BLOCKED" . +org-todo-onhold)
     ("ON-GOING" . +org-todo-active))
   org-log-done 'time
   org-use-property-inheritance t))

(use-package! org-present
  :after org
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;              Miscellaneous             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! google-this
  :config
  (google-this-mode +1))
(use-package! ivy
  :custom
  +ivy-buffer-preview t)

(use-package! dirvish)
;; (use-package! focus)
;; (use-package! beacon
;;   :init (beacon-mode 1))

(use-package! direnv)
