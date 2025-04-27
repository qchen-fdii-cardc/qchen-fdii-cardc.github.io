#[cfg(windows)]
fn main() {
    use std::io::Write;
    let mut res = winres::WindowsResource::new();
    
    // 设置图标
    res.set_icon("icons/app.ico");
    
    // 设置清单文件
    let manifest = r#"<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
    <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
        <security>
            <requestedPrivileges>
                <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
            </requestedPrivileges>
        </security>
    </trustInfo>
</assembly>"#;
    
    let manifest_path = "target/app.manifest";
    std::fs::create_dir_all("target").unwrap();
    let mut manifest_file = std::fs::File::create(manifest_path).unwrap();
    manifest_file.write_all(manifest.as_bytes()).unwrap();
    
    res.set_manifest_file(manifest_path);
    
    // 设置应用程序版本信息
    res.set_version_info(winres::VersionInfo::PRODUCTVERSION, 0x00010000);
    res.set_version_info(winres::VersionInfo::FILEVERSION, 0x00010000);
    res.set("ProductName", "Hugo Post Manifesto");
    res.set("FileDescription", "Hugo Post Manifesto");
    res.set("LegalCopyright", "Copyright (c) 2024");
    
    if let Err(e) = res.compile() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

#[cfg(not(windows))]
fn main() {} 