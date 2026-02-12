#!/bin/bash
echo "Testing PowerShell Floor 18..."
echo '{"method":"get_info","params":{}}' | timeout 10 pwsh -NoProfile -File department_floor.ps1 2>&1 | head -30
