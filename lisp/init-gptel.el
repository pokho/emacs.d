
(require-package 'gptel)

;; This file configures gptel to work with multiple AI providers.
;;
;; API keys are expected to be in your `~/.authinfo.gpg` file.
;; You need to add entries for each service you want to use.
;;
;; Example entries for `~/.authinfo.gpg`:
;;
;; machine api.openai.com login gptel password <YOUR_OPENAI_API_KEY>
;; machine api.anthropic.com login gptel password <YOUR_ANTHROPIC_API_KEY>
;; machine generativelanguage.googleapis.com login gptel password <YOUR_GOOGLE_API_KEY>
;; machine api.x.ai login gptel password <YOUR_XAI_API_KEY>
;; machine api.moonshot.cn login gptel password <YOUR_MOONSHOT_API_KEY>
;;
;; After adding these, reload your auth source or restart Emacs.

;; Define the different providers in an association list.
(defvar my-gptel-providers
  `((:openai . ,(gptel-make-openai "OpenAI"
                  :key (lambda () (auth-source-pass-get 'machine "api.openai.com" 'login "gptel"))
                  :models '("gpt-4-turbo-preview" "gpt-4" "gpt-3.5-turbo")))
    (:claude . ,(gptel-make-anthropic "Anthropic"
                  :key (lambda () (auth-source-pass-get 'machine "api.anthropic.com" 'login "gptel"))
                  :models '("claude-3-opus-20240229" "claude-3-sonnet-20240229" "claude-2.1")))
    (:gemini . ,(gptel-make-google "Google Gemini"
                :key (lambda () (auth-source-pass-get 'machine "generativelanguage.googleapis.com" 'login "gptel"))
                 :models '("gemini-1.5-pro-latest" "gemini-pro")))
    (:grok . ,(gptel-make-xai "Grok (xAI)"
                :key (lambda () (auth-source-pass-get 'machine "api.x.ai" 'login "gptel"))
                :models '("grok-1.5" "grok-1")))
    (:kimi . ,(gptel-make-openai "Kimi (Moonshot)"
                :key (lambda () (auth-source-pass-get 'machine "api.moonshot.cn" 'login "gptel"))
                :host "api.moonshot.cn/v1"
                :models '("kimi-k2-0711-preview"))))
  "An alist of configured gptel providers. The car is the keyword identifier
and the cdr is the gptel backend object.")

;; Helper function to easily switch between providers.
(defun my-gptel-switch-provider (provider-key)
  "Switch gptel to use the provider associated with PROVIDER-KEY.
PROVIDER-KEY should be one of: :openai, :claude, :gemini, :grok, :kimi."
  (interactive
   (let* ((provider-names (mapcar (lambda (p) (symbol-name (car p))) my-gptel-providers)))
     (list (intern-soft (completing-read "Choose a provider: " provider-names nil t)))))
  (let ((provider-config (assoc provider-key my-gptel-providers)))
    (if provider-config
        (progn
          (setq gptel-backend (cdr provider-config))
          ;; Set to the first (and often best) available model for the provider.
          (setq gptel-model (car (gptel-models gptel-backend)))
          (message "Switched gptel provider to %s (model: %s)" (gptel-name gptel-backend) gptel-model))
      (warn "Provider '%s' not found in `my-gptel-providers`" provider-key))))

;; Set a default provider on startup.
;; You can change :openai to your preferred default.
(add-hook 'after-init-hook (lambda () (my-gptel-switch-provider :kimi)))

(provide 'init-gptel)
