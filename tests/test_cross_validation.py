#!/usr/bin/env python3
"""
Test runner for Fortran2018 cross-validation
Integrates with existing test framework
"""

import os
import sys
import subprocess
from pathlib import Path

def ensure_dependencies():
    """Ensure required dependencies are available"""
    try:
        import requests
        return True
    except ImportError:
        print("Installing requests dependency...")
        subprocess.run([sys.executable, "-m", "pip", "install", "requests"], 
                      check=True)
        return True

def build_fortran2018():
    """Build Fortran2018 grammar if needed"""
    os.chdir(Path(__file__).parent)
    
    print("Building Fortran2018 grammar...")
    result = subprocess.run(["make", "Fortran2018"], capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"Build failed: {result.stderr}")
        return False
        
    return True

def run_cross_validation():
    """Run the cross-validation test suite"""
    if not ensure_dependencies():
        return False
        
    if not build_fortran2018():
        return False
    
    # Run cross-validation
    tools_dir = Path(__file__).parent / "tools"
    cross_validate_script = tools_dir / "cross_validate.py"
    
    print("Running cross-validation against kaby76/fortran examples...")
    result = subprocess.run([sys.executable, str(cross_validate_script)], 
                          cwd=str(Path(__file__).parent))
    
    return result.returncode == 0

if __name__ == "__main__":
    success = run_cross_validation()
    sys.exit(0 if success else 1)