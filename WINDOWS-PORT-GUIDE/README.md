# Windows Emacs Configuration Portability Kit

This directory contains everything you need to port your Linux Emacs configuration to Windows 11, preserving all functionality including AI integration, org-roam, and the universal capture system.

## 🚀 Quick Start

### Option 1: One-Click Setup
```powershell
# Run as administrator
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
.\tools\copy-config.ps1
.\tools\setup-hotkey.ps1
.\tools\validate-config.ps1
```

### Option 2: Manual Installation
1. Copy `config/` directory contents to `~/.emacs.d/`
2. Copy `scripts/` directory contents to `~/.emacs.d/`
3. Run `.\tools\validate-config.ps1` to verify setup
4. Follow the detailed instructions in `WINDOWS-PORTING-GUIDE.md`

## 📁 Directory Structure

```
WINDOWS-PORT-GUIDE/
├── WINDOWS-PORTING-GUIDE.md    # Comprehensive porting guide
├── README.md                   # This file
├── config/                     # Configuration files
│   ├── init-windows.el        # Main configuration (Windows version)
│   └── lisp/                   # Configuration modules
│       ├── init-gptel.el      # AI/LLM integration
│       ├── init-org.el        # Org-mode GTD system
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
├── docs/                      # Documentation
│   ├── README-WINDOWS.md      # Detailed setup instructions
│   └── INSTALL.md            # Quick installation guide
└── tools/                     # Automation scripts
    ├── copy-config.ps1       # Automated config copy
    ├── validate-config.ps1   # System validation
    └── setup-hotkey.ps1      # Global hotkey setup
```

## ✨ Features Preserved

### Core Functionality
- ✅ **GPTel AI Integration** - Multiple providers (OpenRouter, Groq, Moonshot, Gemini)
- ✅ **Specialized AI Presets** - 13+ role-based assistants (architect, frontend, security, etc.)
- ✅ **Org-mode GTD** - Complete productivity system with agenda views
- ✅ **Org-roam** - Zettelkasten note management with SQLite backend
- ✅ **Universal Capture System** - System-wide content capture via `Ctrl+Alt+O`
- ✅ **Projectile** - Project navigation with ripgrep integration
- ✅ **Magit** - Full Git integration

### Windows-Specific Optimizations
- 🔄 **Path Handling** - Automatic Windows path resolution
- 🔄 **Tool Detection** - Windows executable discovery
- 🔄 **PowerShell Scripts** - Native Windows automation
- 🔄 **Clipboard Integration** - Windows clipboard support
- 🔄 **Performance** - Optimized for Windows environment

## 🛠️ Requirements

### Must-Have
- **Emacs 28+** for Windows
- **PowerShell 7+** (pre-installed on Windows 11)

### Recommended
- **Git for Windows** - Provides Unix tools and shell
- **Ripgrep** - Fast text search for Projectile
- **SQLite3** - Database backend for org-roam
- **Chrome** - Browser for capture system

### Quick Install (PowerShell Admin)
```powershell
winget install Git.Git
winget install BurntSushi.ripgrep.MSVC
winget install SQLite.SQLite
```

## 📋 Setup Checklist

### Pre-Installation
- [ ] Install prerequisites listed above
- [ ] Install Emacs for Windows
- [ ] Backup existing `~/.emacs.d/` if you have one

### Installation
- [ ] Copy configuration files using `tools\copy-config.ps1`
- [ ] Set up global hotkey using `tools\setup-hotkey.ps1`
- [ ] Validate installation with `tools\validate-config.ps1`

### Configuration
- [ ] Create `~/.authinfo.gpg` with API keys
- [ ] Test GPTel connectivity
- [ ] Verify org-roam database setup
- [ ] Test universal capture system

## 🎯 Key Differences from Linux Version

### Platform Adaptations
| Feature | Linux Version | Windows Version |
|---------|---------------|-----------------|
| Shell Scripts | Bash | PowerShell |
| Path Separators | `/` | `\` (auto-handled) |
| Clipboard | xclip | Windows Clipboard API |
| Window Detection | xdotool | PowerShell window APIs |
| Tool Discovery | Unix paths | Windows PATH detection |
| Package Manager | package.el | package.el (same) |

### What's the Same
- All Elisp configuration logic
- Keybindings and workflows
- Org-mode and org-roam functionality
- GPTel presets and AI integration
- Projectile project management
- Magit Git integration

## 🔧 Customization

### Adding New Capture Types

Edit `config/lisp/init-utils.el` and add to `pokho/org-roam-universal-capture`:

```elisp
((eq type 'my-capture) "m")
 ;; Add template for my-capture
 ("m" '("my-capture" plain
        "* %?title\n\n%?selected\n\n%?"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+type: my-capture\n#+captured: %U\n\n")
        :unnarrowed t))
```

### Adding AI Presets

Edit `config/lisp/init-gptel.el`:

```elisp
(gptel-make-preset 'my-preset
  :system "You are a specialist in..."
  :backend "OpenRouter"
  :model 'your-preferred-model
  :stream t)
```

### Tool Paths

Windows tool paths are auto-detected in these locations:
```
C:\Program Files\Git\bin\
C:\Program Files\ripgrep\
C:\ProgramData\chocolatey\bin\
C:\Windows\System32\
```

## 🆘 Troubleshooting

### Common Issues
1. **"Emacs not found"** - Add Emacs to PATH or update paths in scripts
2. **"PowerShell execution policy"** - Run `Set-ExecutionPolicy RemoteSigned`
3. **"Capture not working"** - Check hotkey setup and script permissions
4. **"SQLite not found"** - Install SQLite3 with `winget install SQLite.SQLite`

### Debug Mode
Enable detailed logging:
```powershell
$env:EMACS_DEBUG = 1
emacs --debug-init
```

### Get Help
- Run `.\tools\validate-config.ps1` for comprehensive system check
- Check `docs/WINDOWS-PORTING-GUIDE.md` for detailed troubleshooting
- Review individual tool documentation

## 🎉 Migration Complete!

Your Linux Emacs configuration has been successfully ported to Windows 11 with full functionality preserved. All your familiar workflows, AI tools, and productivity systems are now available on Windows.

### Next Steps
1. Explore the AI presets with `M-x gptel-prompt-select`
2. Set up your org-roam workflow
3. Test the universal capture system
4. Customize paths and settings as needed

Enjoy your fully-featured Emacs setup on Windows! 🚀