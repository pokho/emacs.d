# Configuration Validation Script
# Comprehensive validation of Windows Emacs configuration

param(
    [Parameter(Mandatory=$false)]
    [string]$ConfigDir = "$env:USERPROFILE\.emacs.d"
)

Write-Host "=== Emacs Configuration Validation ===" -ForegroundColor Green
Write-Host "Config Directory: $ConfigDir" -ForegroundColor Cyan
Write-Host ""

# Validation results
$Results = @{
    Passed = 0
    Failed = 0
    Warnings = 0
}

function Test-Requirement {
    param(
        [string]$Name,
        [scriptblock]$Test,
        [string]$Description = "",
        [switch]$IsWarning
    )

    Write-Host "Testing $Name..." -NoNewline

    try {
        $result = & $Test
        if ($result) {
            Write-Host " ✓ PASS" -ForegroundColor Green
            $script:Results.Passed++
        } else {
            if ($IsWarning) {
                Write-Host " ⚠ WARNING" -ForegroundColor Yellow
                $script:Results.Warnings++
            } else {
                Write-Host " ✗ FAIL" -ForegroundColor Red
                $script:Results.Failed++
            }
        }

        if ($Description) {
            Write-Host "  $Description" -ForegroundColor Gray
        }
    } catch {
        Write-Host " ✗ ERROR: $_" -ForegroundColor Red
        $script:Results.Failed++
    }
}

# Test Core Requirements
Write-Host "=== Core Requirements ===" -ForegroundColor Cyan

Test-Requirement "Emacs Installation" {
    Get-Command "emacs" -ErrorAction SilentlyContinue
} "Emacs must be installed and in PATH"

Test-Requirement "PowerShell 7+" {
    $PSVersionTable.PSVersion.Major -ge 7
} "PowerShell 7+ recommended for best compatibility"

Test-Requirement "Configuration Directory" {
    Test-Path $ConfigDir
} "Emacs configuration directory exists"

Test-Requirement "Main Init File" {
    $initFile = Join-Path $ConfigDir "init.el"
    Test-Path $initFile
} "init.el should exist in config directory"

Test-Requirement "Lisp Directory" {
    $lispDir = Join-Path $ConfigDir "lisp"
    Test-Path $lispDir
} "lisp/ directory should exist"

# Test Configuration Files
Write-Host ""
Write-Host "=== Configuration Files ===" -ForegroundColor Cyan

$configFiles = @(
    "init-gptel.el",
    "init-org.el",
    "init-org-roam.el",
    "init-projectile.el",
    "init-git.el",
    "init-utils.el"
)

foreach ($file in $configFiles) {
    Test-Requirement "Config: $file" {
        $filePath = Join-Path $ConfigDir "lisp\$file"
        Test-Path $filePath
    } "Configuration module $file should exist"
}

# Test PowerShell Scripts
Write-Host ""
Write-Host "=== PowerShell Scripts ===" -ForegroundColor Cyan

$scriptFiles = @(
    "browser-capture.ps1",
    "pdf-capture.ps1",
    "spreadsheet-capture.ps1",
    "org-roam-capture.ps1",
    "test-config.ps1"
)

foreach ($script in $scriptFiles) {
    Test-Requirement "Script: $script" {
        $scriptPath = Join-Path $ConfigDir $script
        Test-Path $scriptPath
    } "Capture script $script should exist"
}

# Test Optional Tools
Write-Host ""
Write-Host "=== Recommended Tools ===" -ForegroundColor Cyan

Test-Requirement "Git for Windows" {
    Get-Command "git" -ErrorAction SilentlyContinue
} "Git integration for Magit" -IsWarning

Test-Requirement "Ripgrep" {
    Get-Command "rg" -ErrorAction SilentlyContinue
} "Fast search for Projectile" -IsWarning

Test-Requirement "SQLite3" {
    Get-Command "sqlite3" -ErrorAction SilentlyContinue
} "Database for org-roam" -IsWarning

# Test Directory Structure
Write-Host ""
Write-Host "=== Directory Structure ===" -ForegroundColor Cyan

Test-Requirement "Org Directory" {
    Test-Path (Join-Path $env:USERPROFILE "org")
} "org/ directory for org-mode files"

Test-Requirement "Org-roam Directory" {
    Test-Path (Join-Path $env:USERPROFILE "org\roam")
} "org/roam/ directory for org-roam notes"

# Test Configuration Syntax
Write-Host ""
Write-Host "=== Configuration Syntax ===" -ForegroundColor Cyan

Test-Requirement "Init File Syntax" {
    $initFile = Join-Path $ConfigDir "init.el"
    $content = Get-Content $initFile -Raw
    $content -match "\(provide\s+['\"]init-windows"
} "Main init file should be valid Elisp"

Test-Requirement "No Unix Paths" {
    $initFile = Join-Path $ConfigDir "init.el"
    $content = Get-Content $initFile -Raw
    # Check for obvious Unix paths that should be Windows paths
    -not ($content -match "/home/[^"']*\.emacs\.d")
} "Should not contain hardcoded Unix paths"

# Test API Keys Setup
Write-Host ""
Write-Host "=== API Keys Configuration ===" -ForegroundColor Cyan

Test-Requirement "Authinfo File Exists" {
    $authFile = Join-Path $env:USERPROFILE ".authinfo.gpg"
    Test-Path $authFile
} "Create ~/.authinfo.gpg with your API keys" -IsWarning

# Advanced Tests
Write-Host ""
Write-Host "=== Advanced Validation ===" -ForegroundColor Cyan

Test-Requirement "Emacs Can Start" {
    try {
        # Test if Emacs can start and load config (non-blocking)
        $process = Start-Process "emacs" -ArgumentList "--batch", "--eval", "(progn (load-file \"$ConfigDir\init.el\") (message \"Config loaded successfully\"))" -NoNewWindow -Wait -PassThru
        $process.ExitCode -eq 0
    } catch {
        $false
    }
} "Emacs should be able to load the configuration" -IsWarning

# Summary
Write-Host ""
Write-Host "=== Validation Summary ===" -ForegroundColor Cyan

$totalTests = $Results.Passed + $Results.Failed + $Results.Warnings

Write-Host "Total Tests: $totalTests" -ForegroundColor White
Write-Host "Passed: $($Results.Passed)" -ForegroundColor Green
Write-Host "Failed: $($Results.Failed)" -ForegroundColor Red
Write-Host "Warnings: $($Results.Warnings)" -ForegroundColor Yellow

if ($Results.Failed -eq 0) {
    Write-Host ""
    Write-Host "🎉 All critical tests passed!" -ForegroundColor Green

    if ($Results.Warnings -eq 0) {
        Write-Host "✨ Perfect configuration setup!" -ForegroundColor Green
    } else {
        Write-Host "⚠️  Some optional components missing (see warnings above)" -ForegroundColor Yellow
    }
} else {
    Write-Host ""
    Write-Host "❌ Some critical issues found. Please fix before using the configuration." -ForegroundColor Red
}

Write-Host ""
Write-Host "Recommendations:" -ForegroundColor Cyan

if (-not (Get-Command "git" -ErrorAction SilentlyContinue)) {
    Write-Host "- Install Git for Windows: winget install Git.Git" -ForegroundColor White
}

if (-not (Get-Command "rg" -ErrorAction SilentlyContinue)) {
    Write-Host "- Install ripgrep: winget install BurntSushi.ripgrep.MSVC" -ForegroundColor White
}

if (-not (Get-Command "sqlite3" -ErrorAction SilentlyContinue)) {
    Write-Host "- Install SQLite3: winget install SQLite.SQLite" -ForegroundColor White
}

$authFile = Join-Path $env:USERPROFILE ".authinfo.gpg"
if (-not (Test-Path $authFile)) {
    Write-Host "- Create ~/.authinfo.gpg with your API keys" -ForegroundColor White
}

Write-Host ""
Write-Host "For detailed setup instructions, see: docs/WINDOWS-PORTING-GUIDE.md" -ForegroundColor Gray

# Exit with appropriate code
exit ($Results.Failed -gt 0 ? 1 : 0)