#!/usr/bin/env python3
"""
Dependency setup manager for the kaby76 extraction pipeline.
Downloads and sets up tritext, .NET SDK, and other required tools locally.
"""

import os
import subprocess
import shutil
import tempfile
import urllib.request
import tarfile
import zipfile
from pathlib import Path


class DependencyManager:
    """Manages external dependencies for the extraction pipeline."""
    
    def __init__(self, target_dir):
        """Initialize dependency manager with target directory."""
        self.target_dir = target_dir
        self.deps_dir = os.path.join(target_dir, 'validation', 'dependencies')
        self.bin_dir = os.path.join(self.deps_dir, 'bin')
        
        # Ensure directories exist
        os.makedirs(self.deps_dir, exist_ok=True)
        os.makedirs(self.bin_dir, exist_ok=True)
    
    def setup_all_dependencies(self):
        """Set up all required dependencies for extraction."""
        print("Setting up dependencies for kaby76 extraction pipeline...")
        
        results = {
            'dotnet': self.setup_dotnet(),
            'tritext': self.setup_tritext(),
            'trash_tools': self.setup_trash_tools()
        }
        
        # Update PATH for this session
        self._update_path()
        
        return results
    
    def setup_dotnet(self):
        """Download and install .NET SDK locally."""
        print("Setting up .NET SDK...")
        
        dotnet_dir = os.path.join(self.deps_dir, 'dotnet')
        dotnet_exe = os.path.join(dotnet_dir, 'dotnet')
        
        if os.path.exists(dotnet_exe):
            print("✓ .NET SDK already installed")
            return {'success': True, 'path': dotnet_exe}
        
        try:
            # Download .NET SDK for Linux x64
            dotnet_url = "https://download.visualstudio.microsoft.com/download/pr/cd0d0a4d-2a6a-4d0d-b42e-dfd3b880e222/008a93f83aba6d1acf75ded3d2cfba24/dotnet-sdk-8.0.403-linux-x64.tar.gz"
            
            print(f"Downloading .NET SDK from {dotnet_url}")
            with tempfile.NamedTemporaryFile(suffix='.tar.gz', delete=False) as tmp:
                urllib.request.urlretrieve(dotnet_url, tmp.name)
                
                # Extract to deps directory
                os.makedirs(dotnet_dir, exist_ok=True)
                with tarfile.open(tmp.name, 'r:gz') as tar:
                    tar.extractall(dotnet_dir)
                
                os.unlink(tmp.name)
            
            # Make executable
            os.chmod(dotnet_exe, 0o755)
            
            # Test installation
            result = subprocess.run([dotnet_exe, '--version'], 
                                  capture_output=True, text=True)
            if result.returncode == 0:
                print(f"✓ .NET SDK installed successfully: {result.stdout.strip()}")
                return {'success': True, 'path': dotnet_exe, 'version': result.stdout.strip()}
            else:
                return {'success': False, 'error': 'Installation verification failed'}
                
        except Exception as e:
            return {'success': False, 'error': str(e)}
    
    def setup_tritext(self):
        """Download and build tritext from kaby76/Trash repository."""
        print("Setting up tritext...")
        
        tritext_exe = os.path.join(self.bin_dir, 'tritext')
        
        if os.path.exists(tritext_exe):
            print("✓ tritext already installed")
            return {'success': True, 'path': tritext_exe}
        
        try:
            # Clone the Trash repository to get tritext source
            trash_dir = os.path.join(self.deps_dir, 'trash')
            if not os.path.exists(trash_dir):
                print("Cloning kaby76/Trash repository for tritext...")
                subprocess.run([
                    'git', 'clone', 
                    'https://github.com/kaby76/Trash.git',
                    trash_dir
                ], check=True, capture_output=True)
            
            # Build tritext using dotnet
            tritext_src = os.path.join(trash_dir, 'src', 'tritext')
            if not os.path.exists(tritext_src):
                return {'success': False, 'error': 'tritext source not found in Trash repo'}
            
            dotnet_exe = os.path.join(self.deps_dir, 'dotnet', 'dotnet')
            if not os.path.exists(dotnet_exe):
                return {'success': False, 'error': '.NET SDK not found - install it first'}
            
            print("Building tritext...")
            # Build the project
            subprocess.run([
                dotnet_exe, 'build', '--configuration', 'Release'
            ], cwd=tritext_src, check=True, capture_output=True)
            
            # Find the built executable
            tritext_built = None
            for root, dirs, files in os.walk(os.path.join(tritext_src, 'bin', 'Release')):
                if 'tritext' in files:
                    tritext_built = os.path.join(root, 'tritext')
                    break
                elif 'tritext.exe' in files:
                    tritext_built = os.path.join(root, 'tritext.exe')
                    break
            
            if tritext_built and os.path.exists(tritext_built):
                # Copy to bin directory
                shutil.copy2(tritext_built, tritext_exe)
                os.chmod(tritext_exe, 0o755)
                
                print("✓ tritext built and installed successfully")
                return {'success': True, 'path': tritext_exe}
            else:
                return {'success': False, 'error': 'Built tritext executable not found'}
                
        except subprocess.CalledProcessError as e:
            return {'success': False, 'error': f'Build failed: {e}'}
        except Exception as e:
            return {'success': False, 'error': str(e)}
    
    def setup_trash_tools(self):
        """Set up other required tools from kaby76/Trash (trparse, trquery, trtext)."""
        print("Setting up Trash tools (trparse, trquery, trtext)...")
        
        tools = ['trparse', 'trquery', 'trtext']
        results = {}
        
        trash_dir = os.path.join(self.deps_dir, 'trash')
        dotnet_exe = os.path.join(self.deps_dir, 'dotnet', 'dotnet')
        
        if not os.path.exists(dotnet_exe):
            return {'success': False, 'error': '.NET SDK not found - install it first'}
        
        try:
            for tool in tools:
                tool_exe = os.path.join(self.bin_dir, tool)
                
                if os.path.exists(tool_exe):
                    print(f"✓ {tool} already installed")
                    results[tool] = {'success': True, 'path': tool_exe}
                    continue
                
                # Build the tool
                tool_src = os.path.join(trash_dir, 'src', tool)
                if not os.path.exists(tool_src):
                    print(f"⚠ {tool} source not found, skipping")
                    results[tool] = {'success': False, 'error': 'source not found'}
                    continue
                
                print(f"Building {tool}...")
                subprocess.run([
                    dotnet_exe, 'build', '--configuration', 'Release'
                ], cwd=tool_src, check=True, capture_output=True)
                
                # Find and copy the built executable
                tool_built = None
                for root, dirs, files in os.walk(os.path.join(tool_src, 'bin', 'Release')):
                    if tool in files:
                        tool_built = os.path.join(root, tool)
                        break
                    elif f'{tool}.exe' in files:
                        tool_built = os.path.join(root, f'{tool}.exe')
                        break
                
                if tool_built and os.path.exists(tool_built):
                    shutil.copy2(tool_built, tool_exe)
                    os.chmod(tool_exe, 0o755)
                    print(f"✓ {tool} built and installed successfully")
                    results[tool] = {'success': True, 'path': tool_exe}
                else:
                    print(f"⚠ {tool} executable not found after build")
                    results[tool] = {'success': False, 'error': 'executable not found'}
            
            return {'success': True, 'tools': results}
            
        except subprocess.CalledProcessError as e:
            return {'success': False, 'error': f'Build failed: {e}'}
        except Exception as e:
            return {'success': False, 'error': str(e)}
    
    def _update_path(self):
        """Update PATH to include our bin directory."""
        current_path = os.environ.get('PATH', '')
        if self.bin_dir not in current_path:
            os.environ['PATH'] = f"{self.bin_dir}:{current_path}"
        
        # Also add dotnet to path
        dotnet_dir = os.path.join(self.deps_dir, 'dotnet')
        if dotnet_dir not in current_path:
            os.environ['PATH'] = f"{dotnet_dir}:{os.environ['PATH']}"
    
    def verify_dependencies(self):
        """Verify that all dependencies are working."""
        print("Verifying dependencies...")
        
        results = {}
        
        # Check .NET
        dotnet_exe = os.path.join(self.deps_dir, 'dotnet', 'dotnet')
        if os.path.exists(dotnet_exe):
            try:
                result = subprocess.run([dotnet_exe, '--version'], 
                                      capture_output=True, text=True)
                results['dotnet'] = result.returncode == 0
                if results['dotnet']:
                    print(f"✓ .NET SDK: {result.stdout.strip()}")
            except:
                results['dotnet'] = False
        else:
            results['dotnet'] = False
        
        # Check tritext
        tritext_exe = os.path.join(self.bin_dir, 'tritext')
        if os.path.exists(tritext_exe):
            try:
                result = subprocess.run([tritext_exe, '--help'], 
                                      capture_output=True, text=True)
                results['tritext'] = result.returncode == 0
                if results['tritext']:
                    print("✓ tritext: Available")
            except:
                results['tritext'] = False
        else:
            results['tritext'] = False
        
        # Check trash tools
        for tool in ['trparse', 'trquery', 'trtext']:
            tool_exe = os.path.join(self.bin_dir, tool)
            if os.path.exists(tool_exe):
                try:
                    result = subprocess.run([tool_exe, '--help'], 
                                          capture_output=True, text=True)
                    results[tool] = result.returncode == 0
                    if results[tool]:
                        print(f"✓ {tool}: Available")
                except:
                    results[tool] = False
            else:
                results[tool] = False
        
        return results
    
    def get_environment_vars(self):
        """Get environment variables needed for extraction."""
        return {
            'PATH': f"{self.bin_dir}:{os.path.join(self.deps_dir, 'dotnet')}:{os.environ.get('PATH', '')}",
            'DOTNET_ROOT': os.path.join(self.deps_dir, 'dotnet')
        }


if __name__ == '__main__':
    import sys
    
    if len(sys.argv) != 2:
        print("Usage: python3 setup_dependencies.py <target_directory>")
        sys.exit(1)
    
    target_dir = sys.argv[1]
    manager = DependencyManager(target_dir)
    
    print("Setting up extraction dependencies...")
    results = manager.setup_all_dependencies()
    
    print("\nVerifying installation...")
    verification = manager.verify_dependencies()
    
    print("\nSetup Summary:")
    for dep, status in verification.items():
        status_text = "✓ OK" if status else "✗ FAILED"
        print(f"  {dep}: {status_text}")
    
    if all(verification.values()):
        print("\n✓ All dependencies set up successfully!")
        print(f"Dependencies installed in: {manager.deps_dir}")
    else:
        print("\n⚠ Some dependencies failed to install")
        print("Check the setup logs above for details")
        sys.exit(1)