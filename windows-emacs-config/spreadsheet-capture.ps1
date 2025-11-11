# Spreadsheet capture script for org-roam (Windows PowerShell version)
# Usage: .\spreadsheet-capture.ps1 "TITLE" "FILE_PATH" "SELECTED_TEXT" [SHEET] [CELL]

param(
    [string]$Title = "",
    [string]$FilePath = "",
    [string]$SelectedText = "",
    [string]$Sheet = "",
    [string]$Cell = ""
)

# If not enough arguments, try to get current window info
if (-not $Title -or -not $FilePath) {
    try {
        # Get the active window title using PowerShell
        $activeWindow = (Get-Process | Where-Object {$_.MainWindowTitle -ne ""} | Sort-Object -Property CPU -Descending | Select-Object -First 1).MainWindowTitle

        if ($activeWindow -and ($activeWindow -match "Excel" -or $activeWindow -match "Calc" -or $activeWindow -match "\.xlsx?" -or $activeWindow -match "\.ods")) {
            $Title = $activeWindow
            # File path extraction would need Excel/COM integration
        }
    } catch {
        # Window detection failed
    }
}

# Default title if still empty
if (-not $Title) {
    $Title = "Spreadsheet Capture $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
}

# Prepare extra info
$extraInfo = ""
if ($Sheet) {
    $extraInfo = "sheet=$Sheet"
    if ($Cell) {
        $extraInfo = "$extraInfo,cell=$Cell"
    }
} elseif ($Cell) {
    $extraInfo = "cell=$Cell"
}

# Launch Emacs with org-roam capture
& "$PSScriptRoot\org-roam-capture.ps1" -Type "spreadsheet" -Title $Title -Source $FilePath -SelectedText $SelectedText -ExtraInfo $extraInfo