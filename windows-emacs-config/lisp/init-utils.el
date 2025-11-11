;;; init-utils.el --- Utility functions (Windows-compatible) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/no-javascript-please ()
  "Disable JavaScript in certain web modes."
  (setq-local js-indent-level 2))

(defun sanityinc/set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful on Mac OS X and Windows, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[\t\n]*$" ""
                          (shell-command-to-string
                           (if (eq system-type 'windows-nt)
                               "powershell.exe -NoProfile -Command \"echo $env:PATH\""
                             "zsh -l -c 'echo $PATH'")))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Windows-specific improvements
(when (eq system-type 'windows-nt)
  ;; Try to use Git Bash for shell commands when available
  (let ((git-bash-path "C:/Program Files/Git/bin/bash.exe"))
    (when (file-exists-p git-bash-path)
      (setq shell-file-name git-bash-path
            explicit-shell-file-name git-bash-path
            explicit-bash-args '("--noediting" "-i")
            shell-command-switch "-c")))

  ;; Set up exec-path properly on Windows
  (sanityinc/set-exec-path-from-shell-PATH)

  ;; Windows-specific font setup
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                        :font "Consolas"
                        :height 120)))

;; Universal capture functions
(defun pokho/org-roam-capture ()
  "Universal org-roam capture function."
  (interactive)
  (select-frame-set-input-focus (car (frame-list)))
  (org-roam-capture))

(defun pokho/org-roam-universal-capture (type title source selected-text &optional extra-info)
  "Universal capture function for different source types.
TYPE can be 'web, 'pdf, 'spreadsheet, 'text, etc.
TITLE is the capture title.
SOURCE is the URL, file path, or other source identifier.
SELECTED-TEXT is any selected content.
EXTRA-INFO is an optional alist with additional metadata."
  (interactive)
  (select-frame-set-input-focus (car (frame-list)))
  (let* ((capture-key (cond ((eq type 'web) "w")
                           ((eq type 'pdf) "p")
                           ((eq type 'spreadsheet) "s")
                           (t "d")))
         (template (case capture-key
                    ("w" '("web" plain
                           "* [[%?url][%?title]]\n\n%?selected\n\n%?"
                           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                              "#+title: ${title}\n#+source: %^{url}\n#+captured: %U\n\n")
                           :unnarrowed t))
                    ("p" '("pdf" plain
                           "* PDF: %?title\n* Source: %?source\n\n%?selected\n\n%?"
                           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                              "#+title: ${title}\n#+source: %^{source}\n#+type: PDF\n#+captured: %U\n\n")
                           :unnarrowed t))
                    ("s" '("spreadsheet" plain
                           "* Spreadsheet: %?title\n* Source: %?source\n\n%?selected\n\n%?"
                           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                              "#+title: ${title}\n#+source: %^{source}\n#+type: Spreadsheet\n#+captured: %U\n\n")
                           :unnarrowed t))
                    (t '("default" plain
                         "* %?title\n\n%?selected\n\n%?"
                         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+source: %^{source}\n#+captured: %U\n\n")
                         :unnarrowed t))))
         (org-roam-capture-templates
          (append org-roam-capture-templates
                  (list (append template (list :keys capture-key))))))
    (org-roam-capture-
     :node (org-roam-node-create :title (or title (format "%s Capture" (capitalize (symbol-name type)))))
     :info (append extra-info
                   (list :source source :title title :selected selected-text :type type))
     :goto nil
     :prompt nil
     :keys capture-key)))

(defun pokho/org-roam-browser-capture (title url selected-text)
  "Create a new org-roam capture with browser information.
TITLE is the browser tab title, URL is the page URL, and SELECTED-TEXT
is any text selected in the browser."
  (interactive)
  (pokho/org-roam-universal-capture 'web title url selected-text `(:url ,url)))

(defun pokho/org-roam-pdf-capture (title file-path selected-text &optional page)
  "Create a new org-roam capture from PDF.
TITLE is the document title, FILE-PATH is the PDF location,
SELECTED-TEXT is highlighted text, PAGE is optional page number."
  (interactive)
  (pokho/org-roam-universal-capture 'pdf title file-path selected-text
                                   (when page `(:page ,page))))

(defun pokho/org-roam-spreadsheet-capture (title file-path selected-text &optional sheet cell)
  "Create a new org-roam capture from spreadsheet.
TITLE is the document title, FILE-PATH is the file location,
SELECTED-TEXT is cell content, SHEET and CELL are optional identifiers."
  (interactive)
  (pokho/org-roam-universal-capture 'spreadsheet title file-path selected-text
                                   (append (when sheet `(:sheet ,sheet))
                                           (when cell `(:cell ,cell)))))

;; Windows-specific clipboard functions
(defun pokho/windows-get-selected-text ()
  "Get selected text from Windows clipboard."
  (if (eq system-type 'windows-nt)
      (let ((clipboard-text (current-kill 0)))
        (if (string= clipboard-text "") "" clipboard-text))
    ""))

(provide 'init-utils)
;;; init-utils.el ends here