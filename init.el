;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom install'
;; will do this for you). The `doom!' block below controls what modules are
;; enabled and in what order they will be loaded. Remember to run 'doom refresh'
;; after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom! :input
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company +childframe +tng)           ; the ultimate code completion backend
       (ivy +fuzzy +prescient +childframe +icons)               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode
	      +ascii
	      +github)  ; 🙂

       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ; hydra
       indent-guides     ; highlighted indent columns
       (ligatures
       ; +extra
      ;; +iosevka
         +fira
        )         ; replace bits of code with pretty symbols pretty-code => ligatures
       minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; tabs              ; an tab bar for Emacs
       (treemacs +lsp)          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;; god               ; run Emacs commands without modifier keys
       multiple-cursors  ; editing in many places at once
       ;; parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       vterm             ; another terminals in Emacs

       :checkers
       (syntax +childframe)              ; tasing you for every semicolon you forget  flycheck => syntax 
       (spell +aspell +flyspell +enchant +everywhere +hunspell) ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp)
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
         +dictionary +docsets +offline
        )        ; ...or in Dash docsets locally
       (lsp +peek)
       (magit +forge)             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       (pass +auth)              ; password manager for nerds
       pdf               ; pdf enhancements
       prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       wakatime
       tree-sitter
       taskrunner

        :os
        macos  ; improve compatibility with macOS
       (tty +osx)               ; improve the terminal Emacs experience

       :lang
       (json +lsp)
       (web +lsp) ; the tubes
       clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       hy                ; readability of scheme w/ speed of python
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
	+gnuplot
	+journal
	+noter
	+pretty
	+roam2
        +dragndrop       ; drag & drop files/images into org buffers
        +ipython         ; ipython/jupyter support for babel
        +pomodoro        ; be fruitful with the tomato technique
        +present)        ; using org-mode for presentations
       plantuml          ; diagrams for confusing people more
       (python +lsp +conda) 
       rest              ; Emacs as a REST client
       ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (sh 
         +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       yaml              ; JSON, but readable


       :email
       (mu4e +gmail +org)

       :app
       calendar
       write             ; emacs for writers (fiction, notes, papers, etc.)

       :config
       literate
       (default +bindings +smartparens))
