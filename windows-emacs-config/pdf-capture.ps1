# PDF capture script for org-roam (Windows PowerShell version)
# Usage: .\pdf-capture.ps1 "TITLE" "FILE_PATH" "SELECTED_TEXT" [PAGE]

param(
    [string]$Title = "",
    [string]$FilePath = "",
    [string]$SelectedText = "",
    [string]$Page = ""
)

# If not enough arguments, try to get current window info
if (-not $Title -or -not $FilePath) {
    try {
        # Get the active window title using PowerShell
        $activeWindow = (Get-Process | Where-Object {$_.MainWindowTitle -ne ""} | Sort-Object -Property CPU -Descending | Select-Object -First 1).MainWindowTitle

        if ($activeWindow -and $activeWindow -match "\.pdf") {
            $Title = $activeWindow
            # File path extraction would need more complex logic or PDF reader integration
        }
    } catch {
        # Window detection failed
    }
}

# Default title if still empty
if (-not $Title) {
    $Title = "PDF Capture $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
}

# Prepare extra info
$extraInfo = ""
if ($Page) {
    $extraInfo = "page=$Page"
}

# Launch Emacs with org-roam capture
& "$PSScriptRoot\org-roam-capture.ps1" -Type "pdf" -Title $Title -Source $FilePath -SelectedText $SelectedText -ExtraInfo $extraInfo