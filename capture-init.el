;;; capture-init.el --- Minimal configuration for org-roam capture -*- lexical-binding: t -*-
;;; Commentary:
;;; This is a minimal Emacs configuration for org-roam capture,
;;; designed to avoid startup warnings and load only what's necessary.

;;; Code:

;; Suppress warnings and messages completely
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      warning-minimum-level :emergency
      byte-compile-warnings nil)

;; Redirect all messages to avoid warnings
(defconst capture--original-message-fn (symbol-function 'message))
(defun capture--silence-messages (&rest args))
(advice-add 'message :override #'capture--silence-messages)

;; Set up package system without loading external configs
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Simple package availability check
(defun capture-ensure-package (package)
  "Ensure PACKAGE is available."
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; Install org-roam if needed
(capture-ensure-package 'org-roam)

;; Load essential packages
(require 'org)
(require 'org-roam)

;; Set directory early so it will be used
(setq org-roam-directory (file-truename "~/org/roam"))
(setq org-roam-database-connector 'sqlite)

;; Define org-roam capture templates
(setq org-roam-capture-templates
      '(("r" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("w" "web" plain
         "* [[%?url][%?title]]\n\n%?selected\n\n%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+source: %^{url}\n#+captured: %U\n\n")
         :unnarrowed t)
        ("p" "pdf" plain
         "* PDF: %?title\n* Source: %?source\n\n%?selected\n\n%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+source: %^{source}\n#+type: PDF\n#+captured: %U\n\n")
         :unnarrowed t)
        ("s" "spreadsheet" plain
         "* Spreadsheet: %?title\n* Source: %?source\n\n%?selected\n\n%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+source: %^{source}\n#+type: Spreadsheet\n#+captured: %U\n\n")
         :unnarrowed t)))

;; Define the universal capture function directly here to avoid loading init-utils
(defun pokho/org-roam-universal-capture (type title source selected-text &optional extra-info)
  "Universal capture function for different source types."
  (interactive)
  (select-frame-set-input-focus (car (frame-list)))
  (let* ((capture-key (cond ((eq type 'web) "w")
                           ((eq type 'pdf) "p")
                           ((eq type 'spreadsheet) "s")
                           (t "r")))
         (info (append extra-info
                       (list :source source
                             :title title
                             :url source
                             :selected selected-text
                             :type type))))
    (org-roam-capture--capture :node (org-roam-node-create :title title)
                                :info info
                                :goto nil
                                :prompt nil
                                :keys capture-key)))

;; Restore message function for capture interaction
(advice-remove 'message #'capture--silence-messages)

;;; capture-init.el ends here