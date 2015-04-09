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
(global-set-key [M-right] 'forward-sexp)
;; Aller à la parenthèse Fermante correspondante :
(global-set-key [M-left] 'backward-sexp)

;; Compiler avec M-f9, recompiler (avec la même commande de
;; compilation) avec f9.
(global-set-key [M-f9]   'compile)
(global-set-key [f9]     'recompile)

;; Ouvrir les .txt en org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Save Emacs session when exiting Emacs
(desktop-save-mode 1)

;; Disable right alt as meta
(setq ns-right-alternate-modifier nil)

;; Bind replace-string to C-x <F1>
(global-set-key (kbd "<f1>") 'replace-string)
(global-set-key [f2] 'shell)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(auto-complete-mode 1)

;; Associate .bb files to c-mode
(add-to-list 'auto-mode-alist '("\\.bb\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.bbh\\'" . c-mode))

;; Always show matching parenthesis and brackets
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

;; WebKit
;(require 'webkit)

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
(linum-mode 1)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't truncate lines
(setq truncate-lines t)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; Window manipulation
(global-set-key [(super +)] 'enlarge-window)
(global-set-key [(super -)] 'shrink-window)

;; shell colors again
(require 'ansi-color)

;; Tramp ssh (faster)
(setq tramp-default-method "ssh")
