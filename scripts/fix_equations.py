import os
import re
from pathlib import Path

def fix_equation_spacing(content):
    """Fix spacing around $$ equations in markdown content."""
    # Split content into lines
    lines = content.split('\n')
    new_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i].strip()
        
        # Check for $$ equation start
        if line.startswith('$$'):
            # Add blank line before if needed
            if new_lines and new_lines[-1].strip():
                new_lines.append('')
            
            # Add the equation line
            new_lines.append(line)
            
            # Find equation end
            while i < len(lines) and not lines[i].strip().endswith('$$'):
                i += 1
                new_lines.append(lines[i].strip())
            
            # Add blank line after if needed
            if i + 1 < len(lines) and lines[i + 1].strip():
                new_lines.append('')
        else:
            new_lines.append(lines[i])
        i += 1
    
    return '\n'.join(new_lines)

def process_markdown_files(content_dir):
    """Process all markdown files in content directory."""
    content_path = Path(content_dir)
    modified_files = []
    
    for md_file in content_path.rglob('*.md'):
        try:
            with open(md_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Check if file contains $$ equations
            if '$$' in content:
                new_content = fix_equation_spacing(content)
                
                # If content was modified
                if new_content != content:
                    with open(md_file, 'w', encoding='utf-8') as f:
                        f.write(new_content)
                    modified_files.append(md_file)
                    print(f"Fixed equation spacing in: {md_file}")
        except Exception as e:
            print(f"Error processing {md_file}: {e}")
    
    return modified_files

def main():
    content_dir = "content"
    print("Checking markdown files for equation formatting...")
    modified_files = process_markdown_files(content_dir)
    
    if modified_files:
        print("\nModified files:")
        for file in modified_files:
            print(f"- {file}")
    else:
        print("\nNo files needed modification.")

if __name__ == "__main__":
    main() 