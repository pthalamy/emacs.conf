;; Default directory for lisp files
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Melpa
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
                         '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Correspondance des parenthèses :
;; Avec ceci, positionnez le curseur sur une parenthèse ouvrante ou
;; une parenthèse fermante, Emacs met en couleur la paire de
;; parenthèses.
(show-paren-mode 1)

;; Afficher les numéros de lignes dans la mode-line (barre du bas de
;; fenêtre) :
(line-number-mode t)
(column-number-mode t)

;; All temp files in same dir
 (setq backup-directory-alist
                  `((".*" . ,temporary-file-directory)))
        (setq auto-save-file-name-transforms
                  `((".*" ,temporary-file-directory t)))

;; Ne pas afficher le message d'accueil
(setq inhibit-startup-message t)

;; Visionner la région (aka sélection) courante :
(transient-mark-mode t)

;; Correction orthographique :
(ispell-change-dictionary "francais")

;; Souligner les mots incorrects en mode LaTeX
(add-hook 'latex-mode-hook 'flyspell-mode)

;; Se limiter à des lignes de 80 caractères dans les modes textes (y
;; compris le mode LaTeX) :
;; cf. http://www-verimag.imag.fr/~moy/emacs/#autofill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Changer le comportement de la selection de fichiers (C-x C-f)
;; (ido-mode 1)

;; Pour une interface graphique un peu dépouillée
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Définir des touches pour se déplacer rapidement :
;; Aller à la parenthèse ouvrante correspondante :
(global-set-key [M-shift-right] 'forward-sexp)
;; Aller à la parenthèse Fermante correspondante :
(global-set-key [M-shift-left] 'backward-sexp)

;; Compiler avec M-f9, recompiler (avec la même commande de
;; compilation) avec f9.
(global-set-key [M-f9]   'compile)
(global-set-key [f9]     'compile-again)
(setq compilation-last-buffer nil)

(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
 (interactive "p")
 (if (and (eq pfx 1)
          compilation-last-buffer)
         (progn
           (set-buffer compilation-last-buffer)
           (revert-buffer t t))
         (call-interactively 'compile)))

;; Ouvrir les .txt en org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Save Emacs session when exiting Emacs
(desktop-save-mode 1)

;; Disable right alt as meta
(setq ns-right-alternate-modifier nil)

;; Bind replace-string to C-x <F1>
(global-set-key (kbd "<f1>") 'replace-string)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20151011.1823")
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
;; This is where your snippets will lie.
(setq yas/root-directory '("~/.emacs.d/elpa/yasnippet-20151011.1823/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(yas-global-mode 1)

;; auto-complete
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'objc-mode)
(auto-complete-mode 1)

;; Java JDK Completion
;; (require 'ajc-java-complete-config)
;; (add-hook 'java-mode-hook 'ajc-java-complete-mode)
;; (add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; Associate .bb files to c-mode
(add-to-list 'auto-mode-alist '("\\.bb\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.bbh\\'" . c-mode))

;; Always show matching parenthesis and brackets C-M-n C-M-p
(show-paren-mode 1)

;; Change window using M-arrow
;; (windmove-default-keybindings 'meta)
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down)

;; C-c left to undo, C-c right to redo window changes
(winner-mode 1)

;; Tabbar change tab
(tabbar-mode 1)
(global-set-key [(super right)] 'tabbar-forward-tab)
(global-set-key [(super left)] 'tabbar-backward-tab)
(global-set-key [(super shift right)] 'tabbar-forward-group)
(global-set-key [(super shift left)] 'tabbar-backward-group)
;;   Sort tabs by name
(defun tabbar-add-tab (tabset object &optional append_ignored)
  (let ((tabs (tabbar-tabs tabset)))
        (if (tabbar-get-tab object tabset)
                tabs
          (let ((tab (tabbar-make-tab object tabset)))
                (tabbar-set-template tabset nil)
                (set tabset (sort (cons tab tabs)
                                                  (lambda (a b) (string< (buffer-name (car a)) (buffer-name (car b))))))))))

;; Get default PATH
(if (not (getenv "TERM_PROGRAM"))
        (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
          (setenv "PATH" path)))

;; Colorful shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#000000" "#d01A4E" "#7E7D7E" "#b58900" "#268bd2" "#d33682" "#2aa198" "#DCDCCC"])
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes (quote ("96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "1c50040ec3b3480b1fec3a0e912cac1eb011c27dd16d087d61e72054685de345" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#d01A4E") (60 . "#cb4b16") (80 . "#b58900") (100 . "#b58900") (120 . "#b58900") (140 . "#7E7D7E") (160 . "#7E7D7E") (180 . "#9FAA9B") (200 . "#9FC59F") (220 . "#859900") (240 . "#31be67") (260 . "#2aa198") (280 . "#268bd2") (300 . "#268bd2") (320 . "#268bd2") (340 . "#00a74e") (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#d33682"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Line numbers to the left
;; (linum-mode 1)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't truncate lines
(setq truncate-lines t)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Trailing whitespace is unnecessary
;; (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
;; (add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; Window manipulation
(global-set-key [(super +)] 'enlarge-window)
(global-set-key [(super -)] 'shrink-window)

;; shell colors again
(require 'ansi-color)

;; Tramp ssh (faster)
(setq tramp-default-method "ssh")

;; coding style / tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-default-style "k&r"
          c-basic-offset 4)

;; No namespace indent in c++
(defun my-c-setup ()
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-default-style "k&r"
          c-basic-offset 4)
  (c-set-offset 'innamespace [0])
  (c-set-offset 'case-label '+))
(add-hook 'c++-mode-hook 'my-c-setup)


;; Cleanup lists
;; (add-to-list 'c-cleanup-list 'brace-else-brace)
;; (add-to-list 'c-cleanup-list 'brace-elseif-brace)
;; (add-to-list 'c-cleanup-list 'brace-catch-brace)
;; (add-to-list 'c-cleanup-list 'empty-defun-btace)
;; (add-to-list 'c-cleanup-list 'defun-close-semi)


(add-hook 'java-mode-hook (lambda ()
                                (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))

(add-hook 'java-mode-hook
                  (lambda ()
                        "Treat Java 1.5 @-style annotations as comments."
                        (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
                        (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; Extended Helm Config
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-mode 1)

;; Prints the decimal value of hex under cursor
(defun what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2 )
        (save-excursion
          (search-backward-regexp "[^0-9A-Fa-fx#]" nil t)
          (forward-char)
          (setq p1 (point) )
          (search-forward-regexp "[^0-9A-Fa-fx#]" nil t)
          (backward-char)
          (setq p2 (point) ) )

        (setq inputStr (buffer-substring-no-properties p1 p2) )

        (let ((case-fold-search nil) )
          (setq tempStr (replace-regexp-in-string "^0x" "" inputStr )) ; C, Perl, …
          (setq tempStr (replace-regexp-in-string "^#x" "" tempStr )) ; elisp …
          (setq tempStr (replace-regexp-in-string "^#" "" tempStr ))  ; CSS …
          )

        (message "Hex %s is %d" tempStr (string-to-number tempStr 16 ) )
        ))

;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory and changes the
prompt to 'name>'."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
        (shell (current-buffer))
        (sleep-for 0 200)
        (delete-region (point-min) (point-max))
        (comint-simple-send (get-buffer-process (current-buffer))
                            (concat "export PS1=\"" name ">\""))
        ))
(global-set-key (kbd "C-c s") 'new-shell)

;; Show  / Hide function body
(require 'hideshow)
(setq hs-minor-mode t)
(global-set-key (kbd "M-s M-s") 'hs-toggle-hiding)
(global-set-key (kbd "M-s M-a") 'hs-hide-all)
(global-set-key (kbd "M-s M-z") 'hs-show-all)

;; Don't open new frame for every file openning
(setq ns-pop-up-frames 'nil)

;; sqlplus path
(setq sql-oracle-program "/opt/oracle/instantclient_11_2/sqlplus")

;; BATTERY MODE !
(display-battery-mode 1)
(display-time-mode 1)
(global-auto-revert-mode 1)

;; subword mode using meta
(global-subword-mode 1)
;; (global-set-key (kbd "M-left") 'subword-backward)
;; (global-set-key (kbd "M-right") 'subword-forward)
;; (global-set-key (kbd "M-DEL") 'subword-backward-kill)

;; Prolog
(add-to-list 'load-path "~/.emacs.d/lisp/")
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist))

;; Projet-GL
(setq auto-mode-alist (cons '("\\.ass$" . asm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.deca$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.decah$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.g4$" . antlr-mode) auto-mode-alist))

;; Linum mode
(global-linum-mode t)
;; Offset the number by two spaces to work around some weird fringe glitch
(setq linum-format "  %d ")

;; Window enlarge-shrink
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)
(global-set-key (kbd "s-{") 'shrink-window-horizontally)

;; Doxy yasnippet
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)
(global-set-key (kbd "C-c d") 'moo-doxygen)

;; Projectile
(projectile-global-mode 1)
(setq projectile-enable-caching t)

;; Guess style
(add-to-list 'load-path "~/.emacs.d/lisp/guess-style/")
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(add-hook 'c-mode-common-hook 'guess-style-guess-all)
