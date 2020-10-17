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
