# Browser capture script for org-roam (Windows PowerShell version)
# This script can get current browser tab information and launch org-roam capture

param(
    [string]$Title = "",
    [string]$Url = "",
    [string]$SelectedText = ""
)

# Function to get Chrome tab info using PowerShell
function Get-ChromeInfo {
    try {
        # Try to get Chrome tab via COM automation
        $chrome = Get-Process chrome -ErrorAction SilentlyContinue | Where-Object {$_.MainWindowTitle -ne ""} | Select-Object -First 1

        if ($chrome) {
            $chromeTitle = $chrome.MainWindowTitle
            # Chrome tab title format: "Page Title - Google Chrome"
            $titleOnly = $chromeTitle -replace ' - Google Chrome$', ''
            return @{
                Title = $titleOnly
                Url = ""  # Would need Chrome extension or more complex automation
            }
        }
    } catch {
        # Fallback if COM automation fails
    }

    return @{
        Title = "Chrome Tab"
        Url = ""
    }
}

# Function to get selected text from clipboard
function Get-SelectedText {
    try {
        # Get text from Windows clipboard
        $clipboardText = Get-Clipboard
        if ($clipboardText) {
            return $clipboardText.Trim()
        }
    } catch {
        # Clipboard access failed
    }
    return ""
}

# Main execution
if (-not $Title -or -not $Url) {
    # Try to get browser info automatically
    $browserInfo = Get-ChromeInfo

    if ($browserInfo.Title -ne "Chrome Tab") {
        $Title = $browserInfo.Title
        $Url = $browserInfo.Url
    }

    if (-not $SelectedText) {
        $SelectedText = Get-SelectedText
    }
}

# Default values if still empty
if (-not $Title) {
    $Title = "Web Capture $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
}

# Convert to PowerShell-friendly string for passing to other script
$escapedTitle = $Title -replace '"', '\"'
$escapedUrl = $Url -replace '"', '\"'
$escapedSelectedText = $SelectedText -replace '"', '\"'

# Launch Emacs with org-roam capture
& "$PSScriptRoot\org-roam-capture.ps1" -Type "web" -Title $escapedTitle -Source $escapedUrl -SelectedText $escapedSelectedText