# Windows Porting Guide for Emacs Configuration

## Overview

This guide provides step-by-step instructions for migrating your Linux Emacs configuration to Windows 11, preserving all functionality including GPTel AI integration, org-roam universal capture system, and development tools.

## 🎯 What Gets Ported

### Core Features Preserved
- ✅ **GPTel AI Integration** - Multiple providers, 13+ specialized presets
- ✅ **Org-mode GTD System** - Complete productivity workflow
- ✅ **Org-roam** - Zettelkasten note management
- ✅ **Universal Capture System** - Browser/PDF/Spreadsheet capture
- ✅ **Projectile** - Project navigation and management
- ✅ **Magit** - Git integration
- ✅ **All keybindings and workflows**

### Platform-Specific Adaptations
- 🔄 **Paths** - Unix → Windows path conversion
- 🔄 **Shell Scripts** - Bash → PowerShell conversion
- 🔄 **Executable Detection** - Windows tool discovery
- 🔄 **Clipboard** - X11 → Windows clipboard
- 🔄 **Window Management** - xdotool → PowerShell automation

---

## 📋 Porting Checklist

### Phase 1: Environment Setup
- [ ] Install Emacs 28+ for Windows
- [ ] Install PowerShell 7+ (usually pre-installed)
- [ ] Install Git for Windows
- [ ] Install ripgrep (optional but recommended)
- [ ] Install SQLite3 (for org-roam)

### Phase 2: Configuration Transfer
- [ ] Copy configuration files to Windows
- [ ] Update Windows-specific paths
- [ ] Set up org directories
- [ ] Configure API keys
- [ ] Test individual components

### Phase 3: Capture System Setup
- [ ] Deploy PowerShell capture scripts
- [ ] Set up global hotkey (Ctrl+Alt+O)
- [ ] Test browser capture
- [ ] Test PDF capture
- [ ] Test spreadsheet capture

### Phase 4: Validation
- [ ] Run configuration test script
- [ ] Verify all keybindings work
- [ ] Test org-roam database
- [ ] Validate GPTel connectivity
- [ ] Confirm capture system functionality

---

## 🚀 Quick Porting Instructions

### Step 1: Install Prerequisites

Open PowerShell as Administrator:

```powershell
# Core dependencies
winget install --id Git.Git -e --source winget
winget install BurntSushi.ripgrep.MSVC
winget install SQLite.SQLite

# Optional: Chocolatey for additional tools
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
```

### Step 2: Copy Configuration Files

#### Option A: Copy Everything
1. Copy entire `WINDOWS-PORT-GUIDE/` folder to `C:\Users\YOURNAME\.emacs.d/`
2. Rename `config/init-windows.el` to `init.el`
3. Update paths as needed (see Step 3)

#### Option B: Selective Copy
Copy these essential files:

**Core Configuration:**
```
config/init-windows.el          → ~/.emacs.d/init.el
config/lisp/init-gptel.el      → ~/.emacs.d/lisp/init-gptel.el
config/lisp/init-org.el        → ~/.emacs.d/lisp/init-org.el
config/lisp/init-org-roam.el   → ~/.emacs.d/lisp/init-org-roam.el
config/lisp/init-projectile.el → ~/.emacs.d/lisp/init-projectile.el
config/lisp/init-git.el        → ~/.emacs.d/lisp/init-git.el
config/lisp/init-utils.el      → ~/.emacs.d/lisp/init-utils.el
```

**Capture Scripts:**
```
scripts/browser-capture.ps1      → ~/.emacs.d/browser-capture.ps1
scripts/pdf-capture.ps1         → ~/.emacs.d/pdf-capture.ps1
scripts/spreadsheet-capture.ps1 → ~/.emacs.d/spreadsheet-capture.ps1
scripts/org-roam-capture.ps1    → ~/.emacs.d/org-roam-capture.ps1
scripts/test-config.ps1         → ~/.emacs.d/test-config.ps1
```

### Step 3: Update Windows Paths

Edit `~/.emacs.d/init.el` and update these paths:

```elisp
;; Update these paths for your system
(setq org-directory (expand-file-name "~/org"))
(setq org-roam-directory (expand-file-name "~/org/roam"))
```

### Step 4: Set Up Directories

```powershell
# Create org directories
mkdir -p ~/org
mkdir -p ~/org/roam
mkdir -p ~/org/roam/daily
```

### Step 5: Configure API Keys

Create `~/.authinfo.gpg`:

```
machine api.openai.com login gptel password YOUR_OPENAI_API_KEY
machine api.anthropic.com login gptel password YOUR_ANTHROPIC_API_KEY
machine generativelanguage.googleapis.com login gptel password YOUR_GOOGLE_API_KEY
machine api.moonshot.cn login gptel password YOUR_MOONSHOT_API_KEY
machine openrouter.ai login gptel password YOUR_OPENROUTER_API_KEY
machine api.x.ai login gptel password YOUR_XAI_API_KEY
```

**To encrypt**: Open in Emacs and save with encryption (Emacs will prompt for password).

### Step 6: Set Up Universal Capture

1. **Create Desktop Shortcut**:
   - Right-click Desktop → New → Shortcut
   - Location: `powershell.exe -ExecutionPolicy Bypass -File "%USERPROFILE%\.emacs.d\browser-capture.ps1"`
   - Name: "Emacs Capture"

2. **Set Global Hotkey**:
   - Right-click shortcut → Properties
   - Shortcut key: `Ctrl + Alt + O`
   - Run: Minimized

3. **Test Capture**:
   - Select text in Chrome
   - Press `Ctrl + Alt + O`
   - Emacs should open with org-roam capture

### Step 7: Test Configuration

```powershell
cd ~/.emacs.d
.\test-config.ps1
```

---

## 🔧 Detailed Configuration

### Path Mappings

| Linux Path | Windows Equivalent |
|------------|-------------------|
| `~/org/` | `~/org/` |
| `~/org/roam/` | `~/org/roam/` |
| `~/.emacs.d/` | `~/.emacs.d/` |
| `/usr/bin/rg` | `rg.exe` (in PATH) |
| `xdotool` | PowerShell window detection |

### PowerShell Execution Policy

If you see execution policy errors:

```powershell
# Allow scripts to run (as Administrator)
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser

# Or temporarily bypass for testing
Set-ExecutionPolicy Bypass -Scope Process
```

### Emacs Executable Paths

The configuration automatically searches for Emacs in:

```
C:\Program Files\Emacs\emacs\bin\emacs.exe
C:\Program Files\Emacs\emacs\bin\emacsclientw.exe
C:\msys64\mingw64\bin\emacs.exe
emacs.exe (if in PATH)
```

### Tool Detection

The configuration automatically detects Windows tools in:

```
C:\Program Files\Git\bin\
C:\Program Files\ripgrep\
C:\ProgramData\chocolatey\bin\
C:\Windows\System32\
C:\Windows\SysNative\
```

---

## 🎯 GPTel Setup on Windows

### Supported Providers
- **OpenRouter** (recommended) - Multiple models, good free tier
- **Groq** - Fast inference, Llama models
- **Moonshot** - Kimi models, good for long context
- **Gemini** - Google's models
- **OpenAI** - GPT models

### Quick Test

1. Start Emacs
2. `M-x gptel`
3. Select provider: `OpenRouter`
4. Select model: `z-ai/glm-4.6`
5. Start chatting!

### Preset Examples

```elisp
;; Access specialized AI assistants
M-x gptel-prompt-select RET architect RET  ; Systems architecture
M-x gptel-prompt-select RET frontend RET  ; UX specialist
M-x gptel-prompt-select RET security RET  ; Security expert
```

---

## 📚 Org-roam on Windows

### Database Setup

The configuration automatically detects SQLite3 and configures org-roam:

```elisp
(setq org-roam-database-connector 'sqlite)
```

### Migration Steps

1. **Copy existing org files** to `~/org/`
2. **Copy org-roam database** if migrating from Linux:
   - Linux: `~/.emacs.d/org-roam.db`
   - Windows: `~/.emacs.d/org-roam.db` (same location)

### First Run

1. Start Emacs
2. `M-x org-roam-db-build-cache`
3. Test with `M-x org-roam-capture`

---

## 🖥️ Universal Capture System Details

### Browser Capture (Chrome)

**How it works:**
1. PowerShell detects Chrome windows
2. Extracts window title
3. Gets clipboard text
4. Creates org-roam note with metadata

**PowerShell Script:** `browser-capture.ps1`

### PDF Capture

**How it works:**
1. Detects active PDF window
2. Captures selected text
3. Includes page number if available
4. Creates structured note with source info

**PowerShell Script:** `pdf-capture.ps1`

### Spreadsheet Capture

**How it works:**
1. Detects Excel/LibreOffice window
2. Captures selected cells
3. Includes sheet and cell references
4. Creates structured note with data context

**PowerShell Script:** `spreadsheet-capture.ps1`

### Customization

Add new capture types by extending `pokho/org-roam-universal-capture` in `init-utils.el`.

---

## 🔍 Troubleshooting

### Common Issues

#### 1. "Emacs executable not found"
**Solution**:
- Ensure Emacs is installed to default location
- Or update paths in `org-roam-capture.ps1`

#### 2. "PowerShell execution policy" error
**Solution**:
```powershell
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
```

#### 3. "SQLite3 not found"
**Solution**:
```powershell
winget install SQLite.SQLite
```

#### 4. "PDF-Tools installation failed"
**Solution**:
- Install pre-compiled binaries
- Or compile using MSYS2 (see PDF-Tools documentation)

#### 5. "Capture scripts not working"
**Solution**:
- Run `.\test-config.ps1` to diagnose
- Check PowerShell execution policy
- Verify Chrome is running before capture

### Debug Mode

Enable debug logging in Emacs:

```elisp
(setq gptel-log-level 'debug)
(setq org-roam-debug t)
```

### Performance Optimization

Add to `init.el`:

```elisp
;; Increase garbage collection threshold
(setq gc-cons-threshold 100000000)

;; Enable native compilation (Emacs 28+)
(setq comp-deferred-compilation t)

;; Disable unused features for faster startup
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
```

---

## 📁 File Organization

```
WINDOWS-PORT-GUIDE/
├── WINDOWS-PORTING-GUIDE.md    # This guide
├── README-WINDOWS.md           # Detailed documentation
├── INSTALL.md                  # Quick install guide
├── config/                     # Configuration files
│   ├── init-windows.el        # Main init file (Windows)
│   └── lisp/                   # Configuration modules
│       ├── init-gptel.el      # AI integration
│       ├── init-org.el        # Org-mode setup
│       ├── init-org-roam.el   # Note management
│       ├── init-projectile.el # Project navigation
│       ├── init-git.el        # Git integration
│       └── init-utils.el      # Utility functions
├── scripts/                    # PowerShell scripts
│   ├── browser-capture.ps1    # Chrome capture
│   ├── pdf-capture.ps1       # PDF capture
│   ├── spreadsheet-capture.ps1 # Excel capture
│   ├── org-roam-capture.ps1  # Universal capture
│   └── test-config.ps1       # Configuration test
├── docs/                      # Additional documentation
└── tools/                     # Helper tools and scripts
    ├── copy-config.ps1       # Automated copy script
    ├── validate-config.ps1   # Validation script
    └── setup-hotkey.ps1      # Hotkey setup script
```

---

## 🎉 Migration Complete!

Your Linux Emacs configuration has been successfully ported to Windows 11! All original functionality is preserved with Windows-specific optimizations.

### Next Steps

1. **Test Everything**: Run `.\test-config.ps1` to verify setup
2. **Explore Features**: Try GPTel presets and capture system
3. **Customize**: Adjust paths and settings as needed
4. **Extend**: Add your own capture templates and presets

### Need Help?

- Check `docs/TROUBLESHOOTING.md` for common issues
- Run `.\validate-config.ps1` for detailed system check
- Review individual component documentation in `docs/`

Enjoy your fully-featured Emacs setup on Windows! 🚀