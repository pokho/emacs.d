# Automated Configuration Copy Script
# This script copies the Windows-compatible Emacs configuration to the correct locations

param(
    [Parameter(Mandatory=$false)]
    [string]$TargetDir = "$env:USERPROFILE\.emacs.d",

    [Parameter(Mandatory=$false)]
    [switch]$Force
)

Write-Host "=== Emacs Configuration Copy Script ===" -ForegroundColor Green
Write-Host "Target Directory: $TargetDir" -ForegroundColor Cyan
Write-Host ""

# Function to safely copy file
function Copy-ConfigFile {
    param(
        [string]$Source,
        [string]$Destination,
        [string]$Description
    )

    Write-Host "Copying $Description..." -NoNewline

    # Ensure destination directory exists
    $destDir = Split-Path $Destination -Parent
    if (-not (Test-Path $destDir)) {
        New-Item -ItemType Directory -Path $destDir -Force | Out-Null
    }

    # Check if destination exists
    if ((Test-Path $Destination) -and -not $Force) {
        Write-Host " SKIPPED (already exists)" -ForegroundColor Yellow
        return
    }

    try {
        Copy-Item -Path $Source -Destination $Destination -Force:$Force
        Write-Host " OK" -ForegroundColor Green
    } catch {
        Write-Host " ERROR: $_" -ForegroundColor Red
    }
}

# Get script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ConfigDir = Join-Path $ScriptDir "..\config"
$ScriptsDir = Join-Path $ScriptDir "..\scripts"

# Verify source directory exists
if (-not (Test-Path $ConfigDir)) {
    Write-Host "Error: Source configuration directory not found: $ConfigDir" -ForegroundColor Red
    exit 1
}

# Create target directory if it doesn't exist
if (-not (Test-Path $TargetDir)) {
    Write-Host "Creating target directory: $TargetDir" -ForegroundColor Cyan
    New-Item -ItemType Directory -Path $TargetDir -Force | Out-Null
}

# Copy main init file
$InitSource = Join-Path $ConfigDir "init-windows.el"
$InitDest = Join-Path $TargetDir "init.el"
Copy-ConfigFile -Source $InitSource -Destination $InitDest -Description "Main init file"

# Copy lisp directory
$LispSource = Join-Path $ConfigDir "lisp"
$LispDest = Join-Path $TargetDir "lisp"
Copy-ConfigFile -Source $LispSource -Destination $LispDest -Description "Lisp configuration modules"

# Copy PowerShell scripts
Write-Host "Copying PowerShell capture scripts..." -ForegroundColor Cyan
$scriptFiles = @(
    "browser-capture.ps1",
    "pdf-capture.ps1",
    "spreadsheet-capture.ps1",
    "org-roam-capture.ps1",
    "test-config.ps1"
)

foreach ($script in $scriptFiles) {
    $scriptSource = Join-Path $ScriptsDir $script
    $scriptDest = Join-Path $TargetDir $script
    Copy-ConfigFile -Source $scriptSource -Destination $scriptDest -Description $script
}

# Create org directories
Write-Host "Creating org directories..." -ForegroundColor Cyan
$orgDirs = @(
    (Join-Path $env:USERPROFILE "org"),
    (Join-Path $env:USERPROFILE "org\roam")
)

foreach ($dir in $orgDirs) {
    if (-not (Test-Path $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
        Write-Host "Created: $dir" -ForegroundColor Green
    } else {
        Write-Host "Directory exists: $dir" -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "=== Copy Complete! ===" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Cyan
Write-Host "1. Set up your API keys in ~/.authinfo.gpg"
Write-Host "2. Run: cd $TargetDir; .\test-config.ps1"
Write-Host "3. Start Emacs and test the configuration"
Write-Host ""
Write-Host "For help, see: docs/WINDOWS-PORTING-GUIDE.md" -ForegroundColor Gray