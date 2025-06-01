use rayon::prelude::*;
use std::time::Instant;
use walkdir::WalkDir;

#[derive(Debug, Clone)]
struct RustFileInfo {
    path: std::path::PathBuf,
    lines: usize,
    size: u64,
}

/// 检查是否为Rust文件
fn is_rust_file(entry: &walkdir::DirEntry) -> bool {
    entry
        .path()
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext == "rs")
        .unwrap_or(false)
}

/// 从文件内容和大小创建RustFileInfo（核心逻辑）
fn create_rust_file_info(
    path: std::path::PathBuf,
    content: String,
    file_size: u64,
) -> RustFileInfo {
    let line_count = content.lines().count();

    RustFileInfo {
        path,
        lines: line_count,
        size: file_size,
    }
}

/// 同步获取Rust文件信息
fn get_rust_file_info_sync(entry: &walkdir::DirEntry) -> RustFileInfo {
    let path = entry.path();
    let content = std::fs::read_to_string(path).unwrap_or_default();
    let file_size = entry.metadata().map(|m| m.len()).unwrap_or(0);

    create_rust_file_info(path.to_path_buf(), content, file_size)
}

/// 异步获取Rust文件信息
async fn get_rust_file_info_async(path: std::path::PathBuf) -> RustFileInfo {
    let content = tokio::fs::read_to_string(&path).await.unwrap_or_default();
    let file_size = tokio::fs::metadata(&path)
        .await
        .map(|m| m.len())
        .unwrap_or(0);

    create_rust_file_info(path, content, file_size)
}

fn main() {
    println!("=== Rust 高效并行文件遍历示例 ===\n");

    // 使用正确的D盘根目录路径
    let target_dir = "D:\\";

    // 方法1: 使用 walkdir + rayon 进行并行处理
    println!("方法1: 同步并行遍历 (walkdir + rayon)");
    println!("遍历目录: {}", target_dir);
    parallel_walk_sync(target_dir);

    println!("\n{}\n", "=".repeat(50));

    // 方法2: 使用 tokio 进行异步并行处理
    println!("方法2: 异步并行遍历 (tokio)");
    println!("遍历目录: {}", target_dir);
    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(parallel_walk_async(target_dir));

    println!("\n{}\n", "=".repeat(50));

    // 方法3: 高级示例 - 查找特定类型文件
    println!("方法3: 高级并行处理 - 查找所有Rust文件");
    println!("遍历目录: {}", target_dir);
    advanced_parallel_processing(target_dir);

    // 方法4：更加实用的方式
    println!("方法4：更加实用的方式");
    println!("遍历目录: {}", target_dir);
    seasoned_walk(target_dir);    
}

fn seasoned_walk(dir: &str){
    let start = Instant::now();
    let rust_file_info: Vec<_> = WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|entry| is_rust_file(entry))
        .collect::<Vec<_>>()
        .par_iter()
        .map(|entry| get_rust_file_info_sync(entry))
        .collect();

    println!("收集到 {} 个文件/目录", rust_file_info.len());
    
    // 统计结果
    let total_lines: usize = rust_file_info.iter().map(|f| f.lines).sum();
    let total_size: u64 = rust_file_info.iter().map(|f| f.size).sum();
    
    println!("处理完成:");
    println!("  Rust文件数: {}", rust_file_info.len());
    println!("  总代码行数: {}", total_lines);
    println!("  总大小: {} bytes", total_size);
    println!("用时: {:?}", start.elapsed());
    
    // 显示最大的10个Rust文件
    println!("\n最大的10个Rust文件:");
    let mut large_files: Vec<_> = rust_file_info.iter().collect();
    large_files.sort_by(|a, b| b.size.cmp(&a.size));
    
    large_files.iter().take(10).for_each(|f| {
        let path_str = f.path.to_string_lossy();
        println!("  🦀 {} ({} bytes, {} 行)", path_str, f.size, f.lines);
    });

}

/// 使用 walkdir + rayon 进行同步并行遍历
/// 这是最常用和高效的方式
fn parallel_walk_sync(dir: &str) {
    let start = Instant::now();
    // 使用 WalkDir 收集所有路径，限制深度避免遍历过深
    let entries: Vec<_> = WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .collect();

    println!("收集到 {} 个文件/目录", entries.len());
    println!("用时: {:?}", start.elapsed());

    let start = Instant::now();
    // 使用 rayon 并行处理：过滤Rust文件并获取信息
    let rust_file_info: Vec<_> = entries
        .par_iter()
        .filter(|entry| is_rust_file(entry))
        .map(|entry| get_rust_file_info_sync(entry))
        .collect();

    // 统计结果
    let total_lines: usize = rust_file_info.iter().map(|f| f.lines).sum();
    let total_size: u64 = rust_file_info.iter().map(|f| f.size).sum();

    println!("处理完成:");
    println!("  Rust文件数: {}", rust_file_info.len());
    println!("  总代码行数: {}", total_lines);
    println!("  总大小: {} bytes", total_size);
    println!("  用时: {:?}", start.elapsed());

    // 显示最大的10个Rust文件
    println!("\n最大的10个Rust文件:");
    let mut large_files: Vec<_> = rust_file_info.iter().collect();
    large_files.sort_by(|a, b| b.size.cmp(&a.size));

    large_files.iter().take(10).for_each(|f| {
        let path_str = f.path.to_string_lossy();
        println!("  🦀 {} ({} bytes, {} 行)", path_str, f.size, f.lines);
    });
}

/// 使用 tokio 进行异步并行遍历
/// 适用于 I/O 密集型操作
async fn parallel_walk_async(dir: &str) {
    let start = Instant::now();
    // 首先同步收集所有路径（因为 walkdir 不是异步的），限制深度
    let entries: Vec<_> = WalkDir::new(dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .collect();

    println!("收集到 {} 个文件/目录", entries.len());
    println!("用时: {:?}", start.elapsed());

    let start = Instant::now();

    // 使用 tokio 异步并行处理：过滤Rust文件并获取信息
    let tasks: Vec<_> = entries
        .into_iter()
        .filter(|entry| is_rust_file(entry))
        .map(|entry| {
            let path = entry.path().to_path_buf();
            tokio::spawn(async move { get_rust_file_info_async(path).await })
        })
        .collect();

    // 等待所有任务完成
    let rust_file_info: Vec<RustFileInfo> = futures::future::join_all(tasks)
        .await
        .into_iter()
        .filter_map(|r| r.ok())
        .collect();

    // 统计结果
    let total_lines: usize = rust_file_info.iter().map(|f| f.lines).sum();
    let total_size: u64 = rust_file_info.iter().map(|f| f.size).sum();

    println!("处理完成:");
    println!("  Rust文件数: {}", rust_file_info.len());
    println!("  总代码行数: {}", total_lines);
    println!("  总大小: {} bytes", total_size);
    println!("  用时: {:?}", start.elapsed());

    // 显示最大的10个Rust文件
    println!("\n最大的10个Rust文件:");
    let mut large_files: Vec<_> = rust_file_info.iter().collect();
    large_files.sort_by(|a, b| b.size.cmp(&a.size));

    large_files.iter().take(10).for_each(|f| {
        let path_str = f.path.to_string_lossy();
        println!("  🦀 {} ({} bytes, {} 行)", path_str, f.size, f.lines);
    });
}

/// 更高级的示例：并行处理特定类型的文件
fn advanced_parallel_processing(target_dir: &str) {
    let start = Instant::now();
    // 在WalkDir阶段就过滤出Rust文件
    let rust_files: Vec<_> = WalkDir::new(target_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|entry| is_rust_file(entry))
        .collect();

    println!("收集到 {} 个 Rust 文件", rust_files.len());
    println!("用时: {:?}", start.elapsed());

    let start = Instant::now();

    // 使用 rayon 并行处理Rust文件信息
    let file_info: Vec<_> = rust_files
        .par_iter()
        .map(|entry| get_rust_file_info_sync(entry))
        .collect();

    let total_lines: usize = file_info.iter().map(|f| f.lines).sum();
    let total_size: u64 = file_info.iter().map(|f| f.size).sum();

    println!("处理完成:");
    println!("  Rust文件数: {}", file_info.len());
    println!("  总代码行数: {}", total_lines);
    println!("  总大小: {} bytes", total_size);
    println!("  处理用时: {:?}", start.elapsed());

    // 显示最大的10个Rust文件
    println!("\n最大的10个Rust文件:");
    let mut large_rust_files: Vec<_> = file_info.iter().collect();
    large_rust_files.sort_by(|a, b| b.size.cmp(&a.size));

    large_rust_files.iter().take(10).for_each(|f| {
        let path_str = f.path.to_string_lossy();
        println!("  🦀 {} ({} bytes, {} 行)", path_str, f.size, f.lines);
    });
}
