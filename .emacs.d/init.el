(let ((path (expand-file-name "~/.emacs.d/lisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))


;;; ### Add MELPA package list ###

;;; You can install many Emacs packages from [MELPA](https://melpa.org)
;;; repository. To add MELPA to the package list add the following to your
;;; `~/.emacs.d/init.el` file


(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stb" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)

;;; This also turns on checking TLS certificates (in both possible
;;; modes) with `tls-program` set to only the first value from the
;;; default value (for more info see [Your Text Editor Is
;;; Malware](https://glyph.twistedmatrix.com/2015/11/editor-malware.html)).

;;; We use major version specific package directory (but only if the
;;; directory already exists, meaning you may be switching between
;;; more than one version of Emacs).

(let ((path (format "~/.emacs.d/elpa-%s" emacs-major-version)))
  (if (file-accessible-directory-p path)
      (setq package-user-dir path)))

;;; Now we are ready to initialize package system.

(package-initialize)

(setq use-package-always-ensure nil)

;;; Now you can list available packages by running `M-x list-packages`.
;;; Mark packages you want to install by pressing `i` and later press `x`
;;; to install all marked packages (the necessary dependencies will be
;;; installed automatically).

;;; Now, load [use-package](https://github.com/jwiegley/use-package/)
;;; package or propose automatic installation if it is not yet
;;; installed.

(unless (require 'use-package nil t)
  (if (not (yes-or-no-p (concat "Refresh packages, install use-package and"
				" other packages used by init file? ")))
      (error "you need to install use-package first")
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t)))

;;; After loading `use-package` we can use it to configure other
;;; packages.


;;; ### Workaround for security vulnerability in Emacs >= 21.1 and < 25.3 ###
;;;
;;;  See [Changes in Emacs 25.3](https://www.gnu.org/software/emacs/news/NEWS.25.3)

(eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))

;;; ### Common settings for programming modes ###

;;; For modes using [company](https://company-mode.github.io/) for tab
;;; completion add

(use-package company
  :init
  (setq company-idle-delay nil  ; avoid auto completion popup, use TAB
				; to show it
	company-tooltip-align-annotations t)
  :hook (after-init . global-company-mode)
  :bind
  (:map prog-mode-map
	("C-i" . company-indent-or-complete-common)
	("C-M-i" . counsel-company)))

;;; For modes that also use Language Server Protocol from
;;; [lsp-mode](https://github.com/emacs-lsp/lsp-mode) add

(use-package lsp-mode
  :commands lsp
  ;; reformat code and add missing (or remove old) imports
  :hook ((before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports))
  :bind (("C-c d" . lsp-describe-thing-at-point)
	 ("C-c e n" . flymake-goto-next-error)
	 ("C-c e p" . flymake-goto-prev-error)
	 ("C-c e r" . lsp-find-references)
	 ("C-c e R" . lsp-rename)
	 ("C-c e i" . lsp-find-implementation)
	 ("C-c e t" . lsp-find-type-definition)))

defun my-go-electric-brace ()
  "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
  (interactive)
  (insert "{")
  (when (looking-back " {")
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (insert "}")
      (indent-according-to-mode))))

(defun my-godoc-package ()
  "Display godoc for given package (with completion)."
  (interactive)
  (godoc (ivy-read "Package: " (go-packages) :require-match t)))

(use-package go-guru
  :after go-mode)

(use-package go-mode
  :init
  (setq go-fontify-function-calls nil)  ; fontifing names of called
					; functions is too much for me
  :bind
  (:map go-mode-map
	("C-c e g" . godoc)
	("C-c P" . my-godoc-package)
	("{" . my-go-electric-brace))
  :hook ((go-mode . lsp)
	 (go-mode . smartparens-mode)))

;; Go/speedbar integration

(eval-after-load 'speedbar
  '(speedbar-add-supported-extension ".go"))


;;; Now, in go buffers you can use `M-.` to jump to the definition of
;;; the identifier at point (use `M-,` to jump back as for normal tags
;;; in Emacs 25.1) and you can also use `C-c C-d` for a short
;;; description of the identifier at point (actually it is constantly
;;; displayed in the mode line by enabled lsp support). You can
;;; use `C-c d` for a longer description of the identifier at point.

;;; For this to work you have to

;;; 1. After adding above to your emacs config file see how to
;;;    [install from MELPA all required packages](#add-melpa-package-list).
;;;    Or just install [go-mode], [go-guru].

;;; 2. Install Go compiler. Under Debian you may install `golang` package
;;;    (but in Debian 10 Buster it is 1.11 compared to the current 1.14,
;;;    so you may consider
;;;    [downloading the current version of Go](https://golang.org/dl/)). Otherwise
;;;    search for the package for your system or
;;;    see [Getting started](https://golang.org/doc/install).

;;; 3. Install [gopls](https://github.com/golang/tools/blob/master/gopls/README.md) with
;;;    (but in your home directory, not inside some Go module)

;;;    ```
;;;    $ GO111MODULE=on go get golang.org/x/tools/gopls@latest
;;;    ```

;;; 4. Install [guru](https://godoc.org/golang.org/x/tools/cmd/guru)
;;;    with

;;;    ```
;;;    $ go get -u golang.org/x/tools/cmd/guru
;;;    ```

;;; 5. Add your `$GOPATH/bin` to your `PATH` environment variable (or
;;;    copy the `gopls` and `guru` executables from
;;;    `$GOPATH/bin` to some directory which is in your `PATH`).

;;; See also
;;; [Go, pls stop breaking my editor - GopherCon SG 2019](https://www.youtube.com/watch?v=gZ7N3HulAb0)
;;; and [Writing Go in Emacs](http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/)
;;; for more info.

;;; [go-guru]: https://melpa.org/#/go-guru


