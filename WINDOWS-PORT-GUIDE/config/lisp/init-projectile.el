;;; init-projectile.el --- Use Projectile for navigation within projects (Windows-compatible) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  ;; Windows-specific executable detection
  (when (eq system-type 'windows-nt)
    ;; Try to find ripgrep on Windows
    (let ((rg-paths '("C:/Program Files/ripgrep/rg.exe"
                     "C:/ProgramData/chocolatey/bin/rg.exe"
                     "C:/msys64/mingw64/bin/rg.exe"
                     "%USERPROFILE%/Documents/ripgrep/rg.exe"
                     "rg.exe")))
      (dolist (path rg-paths)
        (when (file-exists-p path)
          (setq-default projectile-generic-command (concat path " --files --hidden -0"))
          (return)))))

  ;; Fallback to original if specific path not found
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  ;; Alternative: use find if available on Windows (Git for Windows provides it)
  (when (and (not (executable-find "rg"))
             (executable-find "find"))
    (setq-default projectile-generic-command "find . -type f -print0"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(provide 'init-projectile)
;;; init-projectile.el ends here
