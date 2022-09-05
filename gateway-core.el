;;; gateway-core.el -*- lexical-binding: t; -*-
(require 'f)
(require 'cl-lib)
(require 'ctable)

(defgroup gateway nil
  "Gateway related helpers."
  :group 'convenience)

(define-minor-mode gateway-mode
  "Toggles global gateway-mode."
  :init-value t
  :global t
  :group 'gateway
  :lighter " Gateway"

  (if gateway-mode
      (message "gateway-basic-mode activated!")
    (message "gateway-basic-mode deactivated!")))
(add-hook 'gateway-mode-hook (lambda () (message "Hook was executed!")))
(add-hook 'gateway-mode-on-hook (lambda () (message "gateway turned on!")))
(add-hook 'gateway-mode-off-hook (lambda () (message "gateway turned off!")))
(defconst gateway-buffer-name "*gateway*")
(defconst gateway-dir "/Users/adarsh.melethil/work/src/github.com/adarsh-emburse/cards-gateway/")
(defconst gateway-emacs-dir (f-join gateway-dir "emacs"))
(defconst gateway-epc-server-script (f-join gateway-emacs-dir "server.py"))
(defvar *gateway-epc* nil)

(defun gateway-buffer (&optional show)
  (let* ((buf-name gateway-buffer-name)
         (buf (get-buffer buf-name)))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create buf-name))
      (cd gateway-dir))
    (when show (pop-to-buffer buf))
    buf))

(defmacro gw-defun (name args &rest body)
  (declare (doc-string 3) (indent 2))
  `(defun ,name ,args
     (interactive)
     (with-current-buffer (gateway-buffer)
       ,@body)))

(gw-defun gateway-buf-clear ()
  (erase-buffer))

(defun gateway-start-epc ()
  (interactive)
  (if *gateway-epc*
      (unless (epc:live-p *gateway-epc*)
        (epc:manager-restart-process *gateway-epc*)
        (message "Start gateway server: %s" *gateway-epc*))
   (progn
     (setq *gateway-epc* (epc:start-epc "~/.doom.d/gateway-server.sh" '()))
     (message "created new gateway server: %s" *gateway-epc*)))
  *gateway-epc*)

(defun gateway-kill-epc ()
  (interactive)
  (if *gateway-epc*
    (or (epc:stop-epc *gateway-epc*) t)
    nil))

(defun gateway-restart-epc ()
  (interactive)
  (progn
    (gateway-kill-epc)
    (gateway-start-epc)))

(defmacro gateway-call (callback func &rest args)
  `(deferred:$
     (epc:call-deferred *gateway-epc* (quote ,func) (quote ,args))
     (deferred:nextc it
       (lambda (result)
         (message "%s(%s) = %s" (quote ,func) (quote ,args) result)))
     (deferred:nextc it ,callback)))

(defun no-op (&rest args) nil)
(defun gateway-call-echo ()
  (interactive)
  (gateway-call #'no-op echo 10 40))

(defun gateway-call-data ()
  (interactive)
  (gateway-call #'no-op data 1 2 3 (:a :b :c)))


(defun gateway-create-model ()
  (interactive)
  (gateway-call #'no-op create_models "connection"))


(map! :leader
      (:prefix "a"

       (:prefix "e"  ;; Environment
        "a" (lambda () (interactive) (pyvenv-workon "cards-gateway")))

       (:prefix "b"  ;; Buffer
        "b" (lambda () (interactive) (gateway-buffer t)))

       (:prefix "s"  ;; Server
        "s" #'gateway-start-epc
        "k" #'gateway-kill-epc
        "r" #'gateway-restart-epc)

       (:prefix "m"  ;; Model
        "c" #'gateway-create-model)

       (:prefix "t"  ;; Test
        "e" #'gateway-call-echo
        "d" #'gateway-call-data
        "h" (lambda () (interactive) (message "Gateway HelloWorld")))))

;; (eval-js-file "/Users/adarsh.melethil/.doom.d/gateway-pkg/out/js/main.js")

;; (eval-js "import 'https://deno.land/x/fuzzy_search@0.3.0/mod-fuzzy.js'")
;; (eval-js "lisp.print('hello')")

;; (ctbl:popup-table-buffer-easy
;;  '((1 2 3 4) (5 6 7 8) (9 10 11 12)))


;; (deferred:$
;;   (deferred:wait 10000) ; 1000msec
;;   (deferred:nextc it
;;     (lambda (x)
;;       (message "Timer sample! : %s msec" x))))


(defun company-simple-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-simple-backend))
    (prefix (when (looking-back "foo\\>")
              (match-string 0)))
    (candidates (when (equal arg "foo")
                  (list "foobar" "foobaz" "foobarbaz")))
    (meta (format "This value is named %s" arg))))
