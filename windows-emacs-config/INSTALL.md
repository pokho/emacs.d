# Quick Installation Guide

## One-Command Setup

1. **Extract this configuration** to `C:\Users\YOURNAME\.emacs.d\`

2. **Install dependencies** (run as Administrator in PowerShell):
   ```powershell
   winget install Git.Git
   winget install BurntSushi.ripgrep.MSVC
   winget install SQLite.SQLite
   ```

3. **Test the configuration**:
   ```powershell
   cd C:\Users\YOURNAME\.emacs.d
   .\test-config.ps1
   ```

4. **Set up API keys** in `~/.authinfo.gpg`:
   ```
   machine openrouter.ai login gptel password YOUR_OPENROUTER_API_KEY
   machine api.moonshot.cn login gptel password YOUR_MOONSHOT_API_KEY
   ```

5. **Create org directories**:
   ```powershell
   mkdir ~/org
   mkdir ~/org/roam
   ```

That's it! Start Emacs and your configuration will be loaded automatically.

## Verify Installation

1. Start Emacs
2. `M-x gptel` - Test AI integration
3. `C-c a` - Test org-agenda
4. `M-x org-roam-capture` - Test note capture
5. `C-c p p` - Test projectile

## Quick Capture Setup

Right-click Desktop → New → Shortcut:
- Location: `powershell.exe -ExecutionPolicy Bypass -File "C:\Users\YOURNAME\.emacs.d\browser-capture.ps1"`
- Set shortcut key: `Ctrl+Alt+O`

Now you can capture from any application with `Ctrl+Alt+O`!