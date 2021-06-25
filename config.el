;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Adarsh Melethil"
      user-mail-address "adarshmelethil@gmail.com")

(setq doom-font (font-spec :family "Hack Nerd Font" :size 12))

(setq doom-theme 'doom-outrun-electric)

(setq org-directory "~/org/")
(setq my/base-work-dir "~/work")
;; (setq projectile-project-search-path '(directory-files "~/work/src"))
(defun my/directory-files (folder-name)
  (--map (concat (file-name-as-directory folder-name) it)
         (-filter (lambda (f) (not (string-prefix-p "." f)))
                  (directory-files folder-name))))

(let* ((src-folder (concat (file-name-as-directory my/base-work-dir) "src"))
       (repo-folders (my/directory-files src-folder))
       (group-folders (-flatten (--map (my/directory-files it) repo-folders)))
       )
  (setq projectile-project-search-path group-folders))
;; (--map (directory-files it) ())

(setq display-line-numbers-type 'relative)

(require 'general)
(general-evil-setup)

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-mode '((:name "Today"
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
                                       :tag "bo")))
  :config
  (org-super-agenda-mode))

(defun my-split-window (pos)
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

(general-nmap
    ;; split navigation
    "<down>" 'evil-window-down
    "<left>" 'evil-window-left
    "<up>" 'evil-window-up
    "<right>" 'evil-window-right

    ;; split resizing
    "+" 'evil-window-increase-width
    "_" 'evil-window-decrease-width
    "M-=" 'evil-window-increase-height
    "M--" 'evil-window-decrease-height

    ;; split creation
    "<SPC> <right>" (lambda () (interactive) (my-split-window "right"))
    "<SPC> <up>" (lambda () (interactive) (my-split-window "up"))
    "<SPC> <left>" (lambda () (interactive) (my-split-window "left"))
    "<SPC> <down>" (lambda () (interactive) (my-split-window "down")))

(setq show-paren-style 'mixed)
(set-company-backend! 'python-mode '(company-anaconda))

(with-eval-after-load 'org
  (progn
    (setq org-todo-keywords
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
          ("ON-GOING" . +org-todo-active)))
    (set-face-attribute 'org-level-1 nil :weight 'ultra-light  :height 1.2 :foreground "#ebe8e8" :background "#1f1f1f" )
    ;; (set-face-attribute 'org-level-2 nil :box '(:line-width 1 :color "#0d352c") :weight 'ultra-light :height 1.1 :foreground "#34ace0" :background "#0d352c");"#181835");;"#1f1f3f");;"#0d3028")
    ;; (set-face-attribute 'org-level-3 nil  :weight 'ultra-light :height 1.05 :foreground "#aaa69d" )
    ))
(setq org-log-done 'time)
;; (enables logging)
(setq org-use-property-inheritance t)
(custom-set-faces
  '(org-document-title ((t (:foreground "#fff" :weight bold :height 1.6 :width expanded)))))

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 2))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun my/vterm-send-escape ()
  (interactive)
  (vterm-send-key "<escape>"))

(general-define-key
  :states '(insert)
  :keymaps 'vterm-mode-map
  "<f13>" 'my/vterm-send-escape)

(use-package! google-this
  :config
  (google-this-mode +1))


(setq +ivy-buffer-preview t)

(display-time-mode t)
