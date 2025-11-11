# Configuration test script for Windows Emacs setup
# This script verifies that all required components are properly installed

Write-Host "=== Windows Emacs Configuration Test ===" -ForegroundColor Green
Write-Host ""

# Test function
function Test-Component {
    param(
        [string]$Name,
        [string]$Executable,
        [string]$TestCommand = "--version"
    )

    Write-Host "Testing $Name..." -NoNewline

    try {
        if (Get-Command $Executable -ErrorAction SilentlyContinue) {
            $result = & $Executable $TestCommand 2>&1
            if ($LASTEXITCODE -eq 0) {
                Write-Host " OK" -ForegroundColor Green
                return $true
            } else {
                Write-Host " FAILED" -ForegroundColor Red
                Write-Host "  Error: $result" -ForegroundColor Yellow
                return $false
            }
        } else {
            Write-Host " NOT FOUND" -ForegroundColor Red
            return $false
        }
    } catch {
        Write-Host " ERROR" -ForegroundColor Red
        Write-Host "  Exception: $_" -ForegroundColor Yellow
        return $false
    }
}

# Test Emacs installation
Write-Host "=== Core Components ==="
$emacsOk = Test-Component "Emacs" "emacs" "--version"

# Test PowerShell
Write-Host "Testing PowerShell..." -NoNewline
$psVersion = $PSVersionTable.PSVersion
if ($psVersion.Major -ge 7) {
    Write-Host " OK (v$($psVersion.Major).$($psVersion.Minor))" -ForegroundColor Green
} else {
    Write-Host " WARNING (v$($psVersion.Major).$($psVersion.Minor) - PowerShell 7+ recommended)" -ForegroundColor Yellow
}

# Test optional but recommended tools
Write-Host ""
Write-Host "=== Recommended Tools ==="
$gitOk = Test-Component "Git" "git" "--version"
$rgOk = Test-Component "Ripgrep" "rg" "--version"
$sqliteOk = Test-Component "SQLite3" "sqlite3" "--version"

# Test configuration files
Write-Host ""
Write-Host "=== Configuration Files ==="

$configFiles = @(
    "init.el",
    "lisp\init-gptel.el",
    "lisp\init-org.el",
    "lisp\init-org-roam.el",
    "lisp\init-projectile.el",
    "lisp\init-git.el",
    "lisp\init-utils.el"
)

foreach ($file in $configFiles) {
    $fullPath = Join-Path $PSScriptRoot $file
    Write-Host "Checking $file..." -NoNewline
    if (Test-Path $fullPath) {
        Write-Host " OK" -ForegroundColor Green
    } else {
        Write-Host " MISSING" -ForegroundColor Red
    }
}

# Test PowerShell scripts
Write-Host ""
Write-Host "=== PowerShell Scripts ==="

$scriptFiles = @(
    "browser-capture.ps1",
    "pdf-capture.ps1",
    "spreadsheet-capture.ps1",
    "org-roam-capture.ps1"
)

foreach ($script in $scriptFiles) {
    $fullPath = Join-Path $PSScriptRoot $script
    Write-Host "Checking $script..." -NoNewline
    if (Test-Path $fullPath) {
        Write-Host " OK" -ForegroundColor Green
    } else {
        Write-Host " MISSING" -ForegroundColor Red
    }
}

# Test directory structure
Write-Host ""
Write-Host "=== Directory Structure ==="

$userOrgPath = Join-Path $env:USERPROFILE "org"
Write-Host "Checking ~/org directory..." -NoNewline
if (Test-Path $userOrgPath) {
    Write-Host " OK" -ForegroundColor Green
} else {
    Write-Host " MISSING (run: mkdir ~/org)" -ForegroundColor Yellow
}

$userRoamPath = Join-Path $env:USERPROFILE "org\roam"
Write-Host "Checking ~/org/roam directory..." -NoNewline
if (Test-Path $userRoamPath) {
    Write-Host " OK" -ForegroundColor Green
} else {
    Write-Host " MISSING (run: mkdir ~/org/roam)" -ForegroundColor Yellow
}

# Summary
Write-Host ""
Write-Host "=== Summary ===" -ForegroundColor Cyan

$issues = @()
if (-not $emacsOk) { $issues += "Emacs not found or not working" }
if (-not $gitOk) { $issues += "Git not installed" }
if (-not $rgOk) { $issues += "Ripgrep not installed (Projectile performance)" }
if (-not $sqliteOk) { $issues += "SQLite3 not installed (org-roam database)" }

if ($issues.Count -eq 0) {
    Write-Host "All tests passed! Your configuration should work correctly." -ForegroundColor Green
} else {
    Write-Host "Issues found:" -ForegroundColor Red
    foreach ($issue in $issues) {
        Write-Host "  - $issue" -ForegroundColor Yellow
    }

    Write-Host ""
    Write-Host "Please run these commands to install missing components:" -ForegroundColor Cyan
    if (-not $gitOk) { Write-Host "  winget install Git.Git" -ForegroundColor White }
    if (-not $rgOk) { Write-Host "  winget install BurntSushi.ripgrep.MSVC" -ForegroundColor White }
    if (-not $sqliteOk) { Write-Host "  winget install SQLite.SQLite" -ForegroundColor White }
}

Write-Host ""
Write-Host "Configuration test completed." -ForegroundColor Cyan