# Miniature Office - Easy Installer for Windows
# Run this script in PowerShell to install and setup the application

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "  Miniature Office - Easy Installer" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

# Check if Python 3 is installed
$pythonCmd = Get-Command python -ErrorAction SilentlyContinue
if (-not $pythonCmd) {
    $pythonCmd = Get-Command python3 -ErrorAction SilentlyContinue
}

if (-not $pythonCmd) {
    Write-Host "❌ Python 3 is not installed!" -ForegroundColor Red
    Write-Host "Please install Python 3.9 or higher from:"
    Write-Host "  https://www.python.org/downloads/"
    Write-Host ""
    Write-Host "Make sure to check 'Add Python to PATH' during installation!"
    exit 1
}

# Get Python executable
$python = $pythonCmd.Source

# Check Python version
$pythonVersion = & $python --version 2>&1
Write-Host "✓ Found $pythonVersion" -ForegroundColor Green

# Check if pip is installed
$pipCheck = & $python -m pip --version 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ pip is not installed!" -ForegroundColor Red
    Write-Host "Please reinstall Python and ensure pip is installed"
    exit 1
}

Write-Host "✓ Found pip" -ForegroundColor Green

# Install dependencies
Write-Host ""
Write-Host "📦 Installing dependencies..." -ForegroundColor Yellow
& $python -m pip install -r requirements.txt --quiet

if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Dependencies installed successfully" -ForegroundColor Green
} else {
    Write-Host "❌ Failed to install dependencies" -ForegroundColor Red
    exit 1
}

# Create a convenient startup script
Write-Host ""
Write-Host "🚀 Creating startup script..." -ForegroundColor Yellow

$startScript = @"
@echo off
echo Starting Miniature Office...
echo Open your browser to: http://localhost:5000
echo.
echo Press Ctrl+C to stop the server
echo.

python run.py
pause
"@

$startScript | Out-File -FilePath "start.bat" -Encoding ASCII

Write-Host "✓ Created start.bat" -ForegroundColor Green

# Create desktop shortcut (optional)
$createShortcut = Read-Host "Would you like to create a Desktop shortcut? (y/n)"
if ($createShortcut -eq "y" -or $createShortcut -eq "Y") {
    $WshShell = New-Object -ComObject WScript.Shell
    $Shortcut = $WshShell.CreateShortcut("$env:USERPROFILE\Desktop\Miniature Office.lnk")
    $Shortcut.TargetPath = "$PWD\start.bat"
    $Shortcut.WorkingDirectory = "$PWD"
    $Shortcut.IconLocation = "$python,0"
    $Shortcut.Description = "Start Miniature Office"
    $Shortcut.Save()
    Write-Host "✓ Created desktop shortcut" -ForegroundColor Green
}

# Installation complete
Write-Host ""
Write-Host "======================================" -ForegroundColor Cyan
Write-Host "✅ Installation Complete!" -ForegroundColor Green
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "To start the application:"
Write-Host "  • Double-click: start.bat"
Write-Host "  • Or run: python run.py"
Write-Host ""
Write-Host "Then open your browser to: http://localhost:5000"
Write-Host ""
Write-Host "Press any key to exit..."
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
