# Global Hotkey Setup Script
# Creates Windows shortcut with global hotkey for universal capture

param(
    [Parameter(Mandatory=$false)]
    [string]$ShortcutName = "Emacs Capture",

    [Parameter(Mandatory=$false)]
    [string]$Hotkey = "Ctrl+Alt+O",

    [Parameter(Mandatory=$false)]
    [string]$TargetDir = "$env:USERPROFILE\.emacs.d"
)

Write-Host "=== Global Hotkey Setup ===" -ForegroundColor Green
Write-Host "Creating shortcut: $ShortcutName" -ForegroundColor Cyan
Write-Host "Hotkey: $Hotkey" -ForegroundColor Cyan
Write-Host ""

# Validate target directory
if (-not (Test-Path $TargetDir)) {
    Write-Host "Error: Target directory not found: $TargetDir" -ForegroundColor Red
    Write-Host "Please ensure the Emacs configuration is properly installed." -ForegroundColor Red
    exit 1
}

# Validate capture script exists
$captureScript = Join-Path $TargetDir "browser-capture.ps1"
if (-not (Test-Path $captureScript)) {
    Write-Host "Error: Capture script not found: $captureScript" -ForegroundColor Red
    exit 1
}

# Get desktop paths
$desktopPath = [Environment]::GetFolderPath("Desktop")
$publicDesktopPath = [Environment]::GetFolderPath("CommonDesktopDirectory")

Write-Host "Desktop locations:" -ForegroundColor Cyan
Write-Host "  Current User: $desktopPath" -ForegroundColor Gray
Write-Host "  All Users: $publicDesktopPath" -ForegroundColor Gray

# Ask user where to create shortcut
Write-Host ""
Write-Host "Where would you like to create the shortcut?" -ForegroundColor Cyan
Write-Host "1. Current User Desktop only"
Write-Host "2. All Users Desktop (requires admin)"
Write-Host "3. Both locations"

$choice = Read-Host "Enter choice (1-3)"

if (-not $choice -or $choice -notin @("1", "2", "3")) {
    $choice = "1"  # Default to user desktop
}

$createUserDesktop = $choice -in @("1", "3")
$createPublicDesktop = $choice -in @("2", "3")

# PowerShell command for the shortcut
$scriptPath = $captureScript
$command = "powershell.exe -ExecutionPolicy Bypass -File `"$scriptPath`""

function Create-CaptureShortcut {
    param(
        [string]$Path,
        [string]$Description
    )

    Write-Host "Creating shortcut: $Description..." -NoNewline

    try {
        # Create shell object
        $shell = New-Object -ComObject WScript.Shell

        # Create shortcut
        $shortcut = $shell.CreateShortcut($Path)
        $shortcut.TargetPath = "powershell.exe"
        $shortcut.Arguments = "-ExecutionPolicy Bypass -File `"$scriptPath`""
        $shortcut.WorkingDirectory = $TargetDir
        $shortcut.Description = "Universal capture system for Emacs org-roam"
        $shortcut.WindowStyle = 7  # Minimized

        # Set hotkey (Windows shortcut format)
        $shortcut.Hotkey = $Hotkey

        # Save shortcut
        $shortcut.Save()

        Write-Host " ✓ DONE" -ForegroundColor Green
        return $true
    } catch {
        Write-Host " ✗ ERROR: $_" -ForegroundColor Red
        return $false
    }
}

$successCount = 0

# Create shortcuts
if ($createUserDesktop) {
    $shortcutPath = Join-Path $desktopPath "$ShortcutName.lnk"
    if (Create-CaptureShortcut -Path $shortcutPath -Description "User Desktop") {
        $successCount++
    }
}

if ($createPublicDesktop) {
    # Check if running as administrator
    $isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

    if (-not $isAdmin) {
        Write-Host "Warning: Creating shortcut in All Users desktop requires administrator privileges" -ForegroundColor Yellow
        Write-Host "Please run this script as Administrator to create public shortcuts" -ForegroundColor Yellow
    } else {
        $shortcutPath = Join-Path $publicDesktopPath "$ShortcutName.lnk"
        if (Create-CaptureShortcut -Path $shortcutPath -Description "All Users Desktop") {
            $successCount++
        }
    }
}

# Summary
Write-Host ""
Write-Host "=== Setup Complete ===" -ForegroundColor Green

if ($successCount -gt 0) {
    Write-Host "Successfully created $successCount shortcut(s)" -ForegroundColor Green
    Write-Host ""
    Write-Host "Usage:" -ForegroundColor Cyan
    Write-Host "1. Select text in any application (Chrome, PDF viewer, Excel)" -ForegroundColor White
    Write-Host "2. Press hotkey: $Hotkey" -ForegroundColor White
    Write-Host "3. Emacs will open with org-roam capture" -ForegroundColor White
    Write-Host ""
    Write-Host "Hotkey will be system-wide and should work from any application." -ForegroundColor Gray
} else {
    Write-Host "No shortcuts were created due to errors." -ForegroundColor Red
}

# Test instructions
Write-Host ""
Write-Host "To test the setup:" -ForegroundColor Cyan
Write-Host "1. Open Chrome and select some text" -ForegroundColor White
Write-Host "2. Press $Hotkey" -ForegroundColor White
Write-Host "3. Emacs should open with capture dialog" -ForegroundColor White
Write-Host ""
Write-Host "If it doesn't work:" -ForegroundColor Yellow
Write-Host "- Check that PowerShell execution policy allows scripts" -ForegroundColor Yellow
Write-Host "- Verify the capture script exists at: $captureScript" -ForegroundColor Yellow
Write-Host "- Try running the capture script manually" -ForegroundColor Yellow