;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom install'
;; will do this for you). The `doom!' block below controls what modules are
;; enabled and in what order they will be loaded. Remember to run 'doom refresh'
;; after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom! :input
       :completion
       (company +tng)           ; the ultimate code completion backend
       (ivy +fuzzy +prescient)               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs

       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       ophints           ; highlight the region an operation acts on
       unicode           ; extended unicode support for various languages
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       snippets          ; my elves. They type so I don't have to
       ;; lispy

       :emacs
       dired             ; making dired pretty [functional]
       (ibuffer +icons)           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       vterm             ; another terminals in Emacs

       :checkers
       (spell +aspell) ; tasing you for misspelling mispelling
       syntax

       :tools
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
                                        ;(docker +lsp)
       (eval +overlay)     ; run code, run (also, repls)
       (lookup)           ; helps you navigate your code and documentation
       (lsp +peek)
       (magit +forge)             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;; (pass +auth)              ; password manager for nerds
       prodigy           ; FIXME managing external services & code builders
       ;; rgb               ; creating color strings
       tree-sitter
       ;; taskrunner

       :os
       macos  ; improve compatibility with macOS
       (tty +osx)               ; improve the terminal Emacs experience

       :lang
       (rust +lsp)
       (clojure +lsp)
       (json +lsp)
       ;; (web +lsp) ; the tubes
       common-lisp       ; if you've seen one lisp, you've seen them all
       emacs-lisp        ; drown in parentheses
       hy                ; readability of scheme w/ speed of python
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +pretty
        +roam2
        +present)        ; using org-mode for presentations
       plantuml          ; diagrams for confusing people more
       (python +lsp +conda +pyright +tree-sitter +pytest) ; +cython
       rest              ; Emacs as a REST client
       (sh +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       yaml              ; JSON, but readable
       (lua +lsp)
       zig
       erlang
       elixir
       (go +lsp)

       :email
       ;; (mu4e +gmail +org)

       :app
       ;; calendar

       :config
       (default +bindings +smartparens))
