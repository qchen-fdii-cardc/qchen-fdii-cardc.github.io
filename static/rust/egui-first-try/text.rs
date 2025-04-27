pub const WINDOW_TITLE: &str = "GitHub Pages 文章发布清单";
pub const HEADING: &str = "GitHub Pages 文章发布清单";
pub const RESET_BUTTON: &str = "重置所有选项";

pub const CHECKLIST_ITEMS: &[&str] = &[
    "更新本地库",
    "新建一个分支来工作",
    "撰写文章,包括代码和图形",
    "中途多次提交commit和把本地代码push到服务器",
    "把文章的draft标志设置为false",
    "commit所有工作",
    "把分支切换到主分支",
    "合并工作分支",
    "push代码库,使得github action能够渲染新撰写的文章",
];

// 获取所有需要包含在字体中的字符
// pub fn get_all_chars() -> String {
//     let mut chars = String::new();
//     
//     // 添加标题
//     chars.push_str(WINDOW_TITLE);
//     chars.push_str(HEADING);
//     chars.push_str(RESET_BUTTON);
//     
//     // 添加所有清单项
//     for item in CHECKLIST_ITEMS {
//         chars.push_str(item);
//     }
//     
//     // 去重并排序
//     let mut chars: Vec<char> = chars.chars().collect();
//     chars.sort();
//     chars.dedup();
//     
//     chars.into_iter().collect()
// } 