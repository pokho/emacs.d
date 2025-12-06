;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/display-buffer-full-frame (buffer alist)
  "If it's not visible, display buffer full-frame, saving the prior window config.
The saved config will be restored when the window is quit later.
BUFFER and ALIST are as for `display-buffer-full-frame'."
  (let ((initial-window-configuration (current-window-configuration)))
    (or (display-buffer-reuse-window buffer alist)
        (let ((full-window (display-buffer-full-frame buffer alist)))
          (prog1
              full-window
            (set-window-parameter full-window 'sanityinc/previous-config initial-window-configuration))))))

(defun sanityinc/maybe-restore-window-configuration (orig &optional kill window)
  (let* ((window  (or window (selected-window)))
         (to-restore (window-parameter window 'sanityinc/previous-config)))
    (set-window-parameter window 'sanityinc/previous-config nil)
    (funcall orig kill window)
    (when to-restore
      (set-window-configuration to-restore))))

(advice-add 'quit-window :around 'sanityinc/maybe-restore-window-configuration)

(defmacro sanityinc/fullframe-mode (mode)
  "Configure buffers that open in MODE to display in full-frame."
  `(add-to-list 'display-buffer-alist
                (cons (cons 'major-mode ,mode)
                      (list 'sanityinc/display-buffer-full-frame))))

(sanityinc/fullframe-mode 'package-menu-mode)


;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun sanityinc/remove-auto-mode (mode)
  "Remove entries from `auto-mode-alist' that are for `MODE'."
  (setq auto-mode-alist (seq-remove (lambda (x) (eq mode (cdr x))) auto-mode-alist)))

;; Like diminish, but for major modes
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))


;; String utilities missing from core emacs

(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))



;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))



;; Rename the current file

(if (fboundp 'rename-visited-file)
    (defalias 'rename-this-file-and-buffer 'rename-visited-file)
  (defun rename-this-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (unless filename
        (error "Buffer '%s' is not visiting a file!" name))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))


;; Browse current HTML file

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


(defun restart-emacs ()
  "Restart Emacs.
This saves the session, kills the current Emacs process, and starts a new one."
  (interactive)
  (if (daemonp)
      (message "Cannot restart a running Emacs daemon.")
    (let ((emacs-binary (car command-line-args))
          (emacs-args (cdr command-line-args-left)))
      (message "Restarting Emacs with command: setsid %s %s" emacs-binary emacs-args)
      (apply 'start-process "restart-emacs" nil "setsid" (cons emacs-binary emacs-args))
      (kill-emacs))))

(defun pokho/org-roam-capture-from-anywhere ()
  "Create a new org-roam capture, raising the Emacs frame."
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
         (template (pcase capture-key
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
                    (_ '("default" plain
                        "* %?title\n\n%?selected\n\n%?"
                        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                           "#+title: ${title}\n#+source: %^{source}\n#+captured: %U\n\n")
                        :unnarrowed t)))))
    (setq org-roam-capture-templates
          (append org-roam-capture-templates
                  (list (append template (list :keys capture-key)))))
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


;; System-wide org-protocol configuration
(require 'org-protocol)
(require 'server)

;; Ensure server is started for org-protocol to work
(unless (server-running-p)
  (server-start))

;; Add org-protocol handlers for system-wide capture
(add-to-list 'org-protocol-protocol-alist
             '("org-capture" :protocol "org-capture" :function org-protocol-capture))

;; Enhanced org-protocol capture handler that integrates with org-roam
(defun org-protocol-capture-web (info)
  "Handle org-protocol capture for web content via org-roam."
  (let ((title (plist-get info :title))
        (url (plist-get info :url))
        (selected (plist-get info :body)))
    (pokho/org-roam-browser-capture (or title "Web Capture") url (or selected "")))
  nil)

;; Register the enhanced handler
(add-to-list 'org-protocol-protocol-alist
             '("org-roam-web" :protocol "org-roam-web" :function org-protocol-capture-web))

(provide 'init-utils)
;;; init-utils.el ends here