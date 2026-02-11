@echo off
REM Miniature Office - Quick Start for Windows
REM This is a template - the installer will create a customized version

echo Starting Miniature Office...
echo.
echo Open your browser to: http://localhost:5000
echo.
echo Press Ctrl+C to stop the server
echo.

python run.py
if errorlevel 1 (
    echo.
    echo Error: Python not found or failed to start
    echo Please run install.ps1 first, or ensure Python is installed
    echo.
    pause
)
