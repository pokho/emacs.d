;;; init-windows.el --- Windows-specific configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Windows-specific setup for Emacs configuration

;;; Code:

;; Set up package archives
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/packages/")))

;; Initialize packages
(package-initialize)

;; Set exec-path for Windows executables
(when (eq system-type 'windows-nt)
  ;; Add common Windows paths to exec-path
  (dolist (path '("C:/Program Files/Git/bin"
                  "C:/Program Files/ripgrep"
                  "C:/ProgramData/chocolatey/bin"
                  "C:/Windows/System32"
                  "C:/Windows/SysNative"))
    (when (file-exists-p path)
      (setenv "PATH" (concat path ";" (getenv "PATH")))
      (add-to-list 'exec-path path))))

;; Windows-specific settings
(when (eq system-type 'windows-nt)
  ;; Use Windows-style home directory
  (setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

  ;; Set org directory for Windows
  (setq org-directory (expand-file-name "~/org"))

  ;; Font settings for Windows
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "Consolas-11"))

  ;; Clipboard settings
  (select-enable-clipboard t)
  (select-enable-primary t)

  ;; Shell settings
  (setq shell-file-name "powershell.exe"
        explicit-shell-file-name "powershell.exe"
        shell-command-switch "-noprofile -Command"))

;; Load configuration files
(let ((config-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-exists-p config-dir)
    (dolist (file '("init-gptel.el"
                    "init-org.el"
                    "init-org-roam.el"
                    "init-projectile.el"
                    "init-git.el"
                    "init-utils.el"))
      (let ((full-path (expand-file-name file config-dir)))
        (when (file-exists-p full-path)
          (load full-path))))))

(provide 'init-windows)
;;; init-windows.el ends here