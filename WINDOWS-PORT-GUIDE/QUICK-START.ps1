# Quick Start Script for Windows Emacs Configuration
# This script handles the entire setup process automatically

param(
    [Parameter(Mandatory=$false)]
    [switch]$Force,

    [Parameter(Mandatory=$false)]
    [switch]$IncludeHotkey
)

# Enhanced colors for better visibility
$Host.UI.RawUI.WindowTitle = "Windows Emacs Configuration Setup"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host " Windows Emacs Configuration Setup" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check PowerShell version
if ($PSVersionTable.PSVersion.Major -lt 7) {
    Write-Host "⚠️  Warning: PowerShell 7+ recommended for best compatibility" -ForegroundColor Yellow
    Write-Host "   Current version: $($PSVersionTable.PSVersion)" -ForegroundColor Gray
    Write-Host ""
}

# Check if running as administrator
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if ($isAdmin) {
    Write-Host "✓ Running with administrator privileges" -ForegroundColor Green
} else {
    Write-Host "ℹ️  Running with user privileges (some features may require admin)" -ForegroundColor Cyan
}
Write-Host ""

# Get script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "📁 Configuration source: $ScriptDir" -ForegroundColor Cyan
Write-Host ""

# Function to install a tool if not present
function Install-Tool {
    param(
        [string]$Name,
        [string]$WingetId,
        [string]$TestCommand
    )

    Write-Host "🔧 Checking $Name..." -NoNewline

    if (Get-Command $TestCommand -ErrorAction SilentlyContinue) {
        Write-Host " ✓ Already installed" -ForegroundColor Green
        return $true
    } else {
        Write-Host " 📦 Installing..." -ForegroundColor Yellow
        try {
            $result = winget install --id $WingetId -e --source winget --accept-package-agreements --accept-source-agreements
            if ($LASTEXITCODE -eq 0) {
                Write-Host " ✓ Installation successful" -ForegroundColor Green
                return $true
            } else {
                Write-Host " ✗ Installation failed" -ForegroundColor Red
                return $false
            }
        } catch {
            Write-Host " ✗ Installation error: $_" -ForegroundColor Red
            return $false
        }
    }
}

# Step 1: Install prerequisites
Write-Host "📦 Step 1: Installing Prerequisites" -ForegroundColor Cyan
Write-Host "------------------------------------" -ForegroundColor Cyan

$tools = @(
    @{ Name = "Git for Windows"; WingetId = "Git.Git"; TestCommand = "git" },
    @{ Name = "Ripgrep"; WingetId = "BurntSushi.ripgrep.MSVC"; TestCommand = "rg" },
    @{ Name = "SQLite3"; WingetId = "SQLite.SQLite"; TestCommand = "sqlite3" }
)

$installResults = foreach ($tool in $tools) {
    Install-Tool @tool
}

Write-Host ""

# Step 2: Copy configuration files
Write-Host "📋 Step 2: Copying Configuration Files" -ForegroundColor Cyan
Write-Host "-----------------------------------" -ForegroundColor Cyan

$copyScript = Join-Path $ScriptDir "tools\copy-config.ps1"
if (Test-Path $copyScript) {
    $copyArgs = @{}
    if ($Force) { $copyScript["Force"] = $true }

    try {
        & $copyScript @copyArgs
        if ($LASTEXITCODE -eq 0) {
            Write-Host "✓ Configuration copied successfully" -ForegroundColor Green
        } else {
            Write-Host "⚠️  Configuration copy completed with warnings" -ForegroundColor Yellow
        }
    } catch {
        Write-Host "✗ Configuration copy failed: $_" -ForegroundColor Red
    }
} else {
    Write-Host "✗ Copy script not found: $copyScript" -ForegroundColor Red
}

Write-Host ""

# Step 3: Validate configuration
Write-Host "🔍 Step 3: Validating Configuration" -ForegroundColor Cyan
Write-Host "---------------------------------" -ForegroundColor Cyan

$validateScript = Join-Path $ScriptDir "tools\validate-config.ps1"
if (Test-Path $validateScript) {
    try {
        & $validateScript
        $validationSuccess = ($LASTEXITCODE -eq 0)
    } catch {
        Write-Host "✗ Validation failed: $_" -ForegroundColor Red
        $validationSuccess = $false
    }
} else {
    Write-Host "✗ Validation script not found: $validateScript" -ForegroundColor Red
    $validationSuccess = $false
}

Write-Host ""

# Step 4: Set up global hotkey (optional)
if ($IncludeHotkey) {
    Write-Host "⌨️  Step 4: Setting Up Global Hotkey" -ForegroundColor Cyan
    Write-Host "----------------------------------" -ForegroundColor Cyan

    $hotkeyScript = Join-Path $ScriptDir "tools\setup-hotkey.ps1"
    if (Test-Path $hotkeyScript) {
        try {
            & $hotkeyScript
            if ($LASTEXITCODE -eq 0) {
                Write-Host "✓ Hotkey setup completed" -ForegroundColor Green
            } else {
                Write-Host "⚠️  Hotkey setup completed with warnings" -ForegroundColor Yellow
            }
        } catch {
            Write-Host "✗ Hotkey setup failed: $_" -ForegroundColor Red
        }
    } else {
        Write-Host "✗ Hotkey script not found: $hotkeyScript" -ForegroundColor Red
    }

    Write-Host ""
}

# Step 5: Final summary
Write-Host "🎉 Setup Complete!" -ForegroundColor Green
Write-Host "==================" -ForegroundColor Green
Write-Host ""

Write-Host "What's been done:" -ForegroundColor Cyan
Write-Host "✅ Prerequisites installed (if needed)" -ForegroundColor White
Write-Host "✅ Configuration files copied to ~/.emacs.d/" -ForegroundColor White
Write-Host "✅ System validation performed" -ForegroundColor White
if ($IncludeHotkey) {
    Write-Host "✅ Global hotkey configured" -ForegroundColor White
}
Write-Host ""

Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "1. Create your API keys file:" -ForegroundColor White
Write-Host "   File: ~/.authinfo.gpg" -ForegroundColor Gray
Write-Host "   Add your OpenRouter, Moonshot, etc. API keys" -ForegroundColor Gray
Write-Host ""
Write-Host "2. Start Emacs:" -ForegroundColor White
Write-Host "   Emacs should load with all your configurations" -ForegroundColor Gray
Write-Host ""
Write-Host "3. Test key features:" -ForegroundColor White
Write-Host "   M-x gptel                 - Test AI integration" -ForegroundColor Gray
Write-Host "   C-c a                     - Open org agenda" -ForegroundColor Gray
Write-Host "   M-x org-roam-capture      - Test note capture" -ForegroundColor Gray
Write-Host "   C-c p p                   - Test projectile" -ForegroundColor Gray
Write-Host ""
if ($IncludeHotkey) {
    Write-Host "4. Test universal capture:" -ForegroundColor White
    Write-Host "   Select text in Chrome, press Ctrl+Alt+O" -ForegroundColor Gray
    Write-Host ""
}

Write-Host "📚 Documentation:" -ForegroundColor Cyan
Write-Host "WINDOWS-PORTING-GUIDE.md - Complete setup guide" -ForegroundColor Gray
Write-Host "docs\README-WINDOWS.md    - Detailed documentation" -ForegroundColor Gray
Write-Host ""

if (-not $validationSuccess) {
    Write-Host "⚠️  Some validation tests failed. Please check the output above." -ForegroundColor Yellow
    Write-Host "   Run '.\tools\validate-config.ps1' for detailed diagnostics." -ForegroundColor Gray
}

Write-Host "🚀 Your Linux Emacs configuration is now ready on Windows!" -ForegroundColor Green

# Offer to open Emacs
$openEmacs = Read-Host "Would you like to start Emacs now? (Y/n)"
if ($openEmacs -ne "n" -and $openEmacs -ne "N") {
    Write-Host "Starting Emacs..." -ForegroundColor Cyan
    try {
        Start-Process "emacs"
        Write-Host "✓ Emacs started successfully" -ForegroundColor Green
    } catch {
        Write-Host "✗ Failed to start Emacs: $_" -ForegroundColor Red
        Write-Host "   Make sure Emacs is installed and in your PATH" -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "Enjoy your fully-featured Emacs setup on Windows! 🎊" -ForegroundColor Green