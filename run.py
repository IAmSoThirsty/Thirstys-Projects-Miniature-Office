#!/usr/bin/env python3
"""
Miniature Office - Cognitive IDE
Main entry point
"""
import sys
import os

# Add the project root to Python path
project_root = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, project_root)

from src.server.app import run_server

if __name__ == '__main__':
    run_server(host='0.0.0.0', port=5000, debug=False)
