#!/bin/bash
# Miniature Office - Easy Installer for macOS and Linux
# Run this script to install and setup the application

set -e

echo "======================================"
echo "  Miniature Office - Easy Installer"
echo "======================================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if Python 3 is installed
if ! command -v python3 &> /dev/null; then
    echo -e "${RED}❌ Python 3 is not installed!${NC}"
    echo "Please install Python 3.9 or higher:"
    echo "  - macOS: brew install python3"
    echo "  - Ubuntu/Debian: sudo apt install python3 python3-pip"
    echo "  - Fedora: sudo dnf install python3 python3-pip"
    exit 1
fi

# Check Python version
PYTHON_VERSION=$(python3 --version | cut -d' ' -f2)
echo -e "${GREEN}✓${NC} Found Python $PYTHON_VERSION"

# Check if pip is installed
if ! command -v pip3 &> /dev/null; then
    echo -e "${RED}❌ pip3 is not installed!${NC}"
    echo "Please install pip3 for Python package management"
    exit 1
fi

echo -e "${GREEN}✓${NC} Found pip3"

# Install dependencies
echo ""
echo "📦 Installing dependencies..."
pip3 install -r requirements.txt --quiet

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Dependencies installed successfully"
else
    echo -e "${RED}❌ Failed to install dependencies${NC}"
    exit 1
fi

# Create a convenient startup script
echo ""
echo "🚀 Creating startup script..."

cat > start.sh << 'EOF'
#!/bin/bash
# Miniature Office - Quick Start Script

echo "Starting Miniature Office..."
echo "Open your browser to: http://localhost:5000"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

python3 run.py
EOF

chmod +x start.sh

# Create macOS double-clickable launcher
if [[ "$OSTYPE" == "darwin"* ]]; then
    cat > start.command << 'EOF'
#!/bin/bash
cd "$(dirname "$0")"
./start.sh
EOF
    chmod +x start.command
    echo -e "${GREEN}✓${NC} Created start.command (double-click to start on macOS)"
fi

echo -e "${GREEN}✓${NC} Created start.sh"

# Installation complete
echo ""
echo "======================================"
echo -e "${GREEN}✅ Installation Complete!${NC}"
echo "======================================"
echo ""
echo "To start the application:"
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "  • Double-click: start.command"
    echo "  • Or run: ./start.sh"
else
    echo "  • Run: ./start.sh"
fi
echo "  • Or run: python3 run.py"
echo ""
echo "Then open your browser to: http://localhost:5000"
echo ""
