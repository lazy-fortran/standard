#!/usr/bin/env python3
"""
Automated downloader for reference Fortran grammars.
Downloads the pre-generated g4 files from kaby76/fortran repository.
"""

import os
import urllib.request
from pathlib import Path


class ReferenceGrammarDownloader:
    """Downloads reference Fortran grammar files."""
    
    BASE_URL = "https://raw.githubusercontent.com/kaby76/fortran/main"
    
    def __init__(self, target_dir):
        """Initialize downloader with target directory."""
        self.target_dir = target_dir
        self.auto_gen_dir = os.path.join(target_dir, 'validation', 'auto-generated')
        self.reference_dir = os.path.join(self.auto_gen_dir, 'fortran-reference')
        
    def download(self):
        """Download reference grammar files."""
        # Create directories if they don't exist
        os.makedirs(self.reference_dir, exist_ok=True)
        
        files_to_download = [
            ('FortranLexer.g4', 'FortranLexer.g4'),
            ('FortranParser.g4', 'FortranParser.g4'),
        ]
        
        print(f"Downloading reference grammars to: {self.reference_dir}")
        
        for remote_file, local_file in files_to_download:
            url = f"{self.BASE_URL}/{remote_file}"
            local_path = os.path.join(self.reference_dir, local_file)
            
            print(f"  Downloading {remote_file}...")
            try:
                urllib.request.urlretrieve(url, local_path)
                file_size = os.path.getsize(local_path)
                print(f"    ✓ Downloaded {local_file} ({file_size:,} bytes)")
            except Exception as e:
                print(f"    ✗ Failed to download {remote_file}: {e}")
                return False
        
        return True
    
    def verify_files(self):
        """Verify that all expected files exist."""
        expected_files = [
            os.path.join(self.reference_dir, 'FortranLexer.g4'),
            os.path.join(self.reference_dir, 'FortranParser.g4'),
        ]
        
        all_present = True
        for filepath in expected_files:
            if os.path.exists(filepath):
                size = os.path.getsize(filepath)
                print(f"  ✓ {os.path.basename(filepath)}: {size:,} bytes")
            else:
                print(f"  ✗ {os.path.basename(filepath)}: MISSING")
                all_present = False
        
        return all_present


if __name__ == '__main__':
    # Simple CLI interface for standalone usage
    import sys
    
    if len(sys.argv) != 2:
        print("Usage: python3 download_reference_grammars.py <target_directory>")
        sys.exit(1)
    
    target_dir = sys.argv[1]
    downloader = ReferenceGrammarDownloader(target_dir)
    
    print("=== Downloading Fortran Reference Grammars ===")
    print(f"Source: kaby76/fortran repository")
    print()
    
    if downloader.download():
        print()
        print("=== Verifying Downloaded Files ===")
        if downloader.verify_files():
            print()
            print("✓ All reference grammar files downloaded successfully!")
            print(f"Location: {downloader.reference_dir}")
        else:
            print()
            print("✗ Some files are missing")
            sys.exit(1)
    else:
        print()
        print("✗ Download failed")
        sys.exit(1)