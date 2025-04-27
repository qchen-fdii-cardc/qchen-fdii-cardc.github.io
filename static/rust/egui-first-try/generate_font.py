import os
import re
import subprocess
import sys
from pathlib import Path


def extract_chinese_chars():
    """从 text.rs 文件中提取所有中文字符"""
    try:
        # 获取项目根目录
        project_root = Path(__file__).parent.parent
        text_rs_path = project_root / "src" / "text.rs"

        # 读取 text.rs 文件
        with open(text_rs_path, "r", encoding="utf-8") as f:
            content = f.read()

        # 使用正则表达式提取所有中文字符
        chinese_chars = re.findall(r'[\u4e00-\u9fff]', content)

        # 去重并排序
        chinese_chars = sorted(set(chinese_chars))

        # 将字符列表转换为字符串
        chars_str = "".join(chinese_chars)

        print(f"成功读取 text.rs，提取到 {len(chinese_chars)} 个中文字符")
        return chars_str

    except Exception as e:
        print(f"错误：{e}")
        sys.exit(1)


def create_font_subset(chars):
    """创建字体子集"""
    project_root = Path(__file__).parent.parent
    font_path = project_root / "fonts" / "霞鹜文楷.ttf"
    output_path = project_root / "fonts" / "custom_font.ttf"

    # 检查源字体文件是否存在
    if not font_path.exists():
        print(f"错误：字体文件不存在：{font_path}")
        sys.exit(1)

    print("开始创建字体子集...")
    try:
        # 使用 pyftsubset 创建字体子集
        subprocess.run([
            "pyftsubset",
            str(font_path),
            f"--text={chars}",
            "--output-file=" + str(output_path),
            "--no-hinting",
            "--desubroutinize",
            "--no-recommended-glyphs",
            "--layout-features=*",
            "--glyph-names",
            "--symbol-cmap",
            "--legacy-cmap",
            "--notdef-glyph",
            "--notdef-outline",
            "--recommended-glyphs",
            "--name-IDs=*",
            "--name-legacy",
            "--name-languages=*"
        ], check=True)

        print(f"字体子集创建成功：{output_path}")

    except Exception as e:
        print(f"创建字体子集失败：{e}")
        sys.exit(1)


def main():
    print("开始处理...")

    # 提取中文字符
    chars = extract_chinese_chars()

    # 创建字体子集
    create_font_subset(chars)

    print("处理完成！")


if __name__ == "__main__":
    main()
