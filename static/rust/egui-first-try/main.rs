#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // 在 release 模式下使用 windows subsystem

use eframe::egui;
use egui::IconData;
mod text;

#[cfg(debug_assertions)]
fn log(msg: &str) {
    println!("{}", msg);
}

#[cfg(not(debug_assertions))]
fn log(_msg: &str) {}

struct PostChecklist {
    steps: Vec<(String, bool)>,
    current_step: usize, // 当前可操作的步骤
}

impl Default for PostChecklist {
    fn default() -> Self {
        Self {
            steps: text::CHECKLIST_ITEMS
                .iter()
                .map(|&item| (item.to_string(), false))
                .collect(),
            current_step: 0,
        }
    }
}

impl PostChecklist {
    fn update_current_step(&mut self) {
        // 找到第一个未完成的步骤
        self.current_step = self
            .steps
            .iter()
            .position(|(_, checked)| !checked)
            .unwrap_or(self.steps.len());
    }

    // fn uncheck_following_steps(&mut self, from_step: usize) {
    //     // 取消从指定步骤开始的所有后续步骤
    //     for (_, checked) in self.steps.iter_mut().skip(from_step) {
    //         *checked = false;
    //     }
    //     // 更新当前步骤
    //     self.update_current_step();
    // }
}

impl eframe::App for PostChecklist {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading(text::HEADING);
            ui.separator();

            let mut step_changed = false;
            let mut uncheck_from = None;

            for i in 0..self.steps.len() {
                let can_check = i <= self.current_step;
                let step_text = self.steps[i].0.clone();
                let mut checked = self.steps[i].1;

                if can_check {
                    if ui.checkbox(&mut checked, step_text.as_str()).changed() {
                        self.steps[i].1 = checked;
                        step_changed = true;
                        if !checked {
                            uncheck_from = Some(i + 1);
                        }
                    }
                } else {
                    ui.add_enabled(false, egui::Checkbox::new(&mut checked, step_text.as_str()));
                }
            }

            // 处理状态变化
            if let Some(from_step) = uncheck_from {
                for i in from_step..self.steps.len() {
                    self.steps[i].1 = false;
                }
            }
            if step_changed {
                self.update_current_step();
            }

            ui.separator();
            if ui.button(text::RESET_BUTTON).clicked() {
                for step in &mut self.steps {
                    step.1 = false;
                }
                self.current_step = 0;
            }
        });
    }
}

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([600.0, 500.0])
            .with_resizable(false) // 设置窗口不可调整大小
            .with_title(text::WINDOW_TITLE)
            .with_always_on_top()
            .with_icon(load_icon()),
        ..Default::default()
    };

    eframe::run_native(
        text::WINDOW_TITLE,
        options,
        Box::new(|cc| {
            // 设置中文字体
            let mut fonts = egui::FontDefinitions::default();

            // 添加自定义字体
            fonts.font_data.insert(
                "custom_font".to_owned(),
                egui::FontData::from_static(include_bytes!("../fonts/custom_font.ttf")),
            );

            // 将字体添加到所有字体族
            fonts
                .families
                .get_mut(&egui::FontFamily::Proportional)
                .unwrap()
                .insert(0, "custom_font".to_owned());

            fonts
                .families
                .get_mut(&egui::FontFamily::Monospace)
                .unwrap()
                .insert(0, "custom_font".to_owned());

            // 应用字体设置
            cc.egui_ctx.set_fonts(fonts);

            // 设置界面缩放
            cc.egui_ctx.set_pixels_per_point(1.5);

            Box::new(PostChecklist::default())
        }),
    )
}

fn load_icon() -> IconData {
    log("尝试加载嵌入的图标");

    // 嵌入图标文件
    let icon_bytes = include_bytes!("../icons/app.ico");

    match image::load_from_memory(icon_bytes) {
        Ok(image) => {
            log("成功加载嵌入的图标");
            // 将图像调整为32x32大小，这是Windows图标的标准尺寸
            let image = image.resize_exact(32, 32, image::imageops::FilterType::Lanczos3);
            let rgba = image.to_rgba8();
            let (width, height) = rgba.dimensions();
            log(&format!("图标尺寸: {}x{}", width, height));

            IconData {
                rgba: rgba.into_raw(),
                width: width as _,
                height: height as _,
            }
        }
        Err(e) => {
            log(&format!("错误：无法加载嵌入的图标: {}", e));
            default_icon()
        }
    }
}

fn default_icon() -> IconData {
    log("使用默认图标");
    // 创建一个16x16的默认图标，使用蓝色而不是白色
    let size: u32 = 16;
    let color = vec![0u8, 102, 204, 255];
    let mut rgba = Vec::with_capacity((size * size * 4) as usize);
    for _ in 0..((size * size) as usize) {
        rgba.extend_from_slice(&color);
    }
    IconData {
        rgba,
        width: size,
        height: size,
    }
}
