from PIL import Image, ImageDraw, ImageFont
from pathlib import Path
import struct
import io


def create_icon_for_size(size):
    """为特定尺寸创建优化的图标"""
    # 创建一个新的图像
    image = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    draw = ImageDraw.Draw(image)

    # 计算边距，确保在小尺寸时也有足够的空间
    margin = max(2, size // 10)  # 至少2像素的边距，或者10%的边距
    circle_bbox = [margin, margin, size - margin, size - margin]

    # 绘制背景圆形
    draw.ellipse(circle_bbox, fill=(52, 152, 219))

    # 根据尺寸调整字体大小
    font_size = int(size * 0.6)  # 字体占整个图标的60%
    try:
        font = ImageFont.truetype("arial.ttf", font_size)
    except:
        font = ImageFont.load_default()
        font_size = int(size * 0.4)  # 默认字体需要更小一些

    # 绘制文字 "H"
    text = "H"
    text_bbox = draw.textbbox((0, 0), text, font=font)
    text_width = text_bbox[2] - text_bbox[0]
    text_height = text_bbox[3] - text_bbox[1]

    # 将H向下移动一点，为波浪号腾出空间
    x = (size - text_width) // 2
    y = (size - text_height) // 2 + text_height // 6  # 向下移动一点

    draw.text((x, y), text, fill=(255, 255, 255), font=font)

    # 添加波浪号
    wave_font_size = int(font_size * 0.5)  # 波浪号大小为H的一半
    try:
        wave_font = ImageFont.truetype("arial.ttf", wave_font_size)
    except:
        wave_font = ImageFont.load_default()
        wave_font_size = int(size * 0.2)

    wave = "~"
    wave_bbox = draw.textbbox((0, 0), wave, font=wave_font)
    wave_width = wave_bbox[2] - wave_bbox[0]
    wave_height = wave_bbox[3] - wave_bbox[1]

    wave_x = (size - wave_width) // 2
    wave_y = y - text_height // 2  # 将波浪号放在H上方

    draw.text((wave_x, wave_y), wave, fill=(255, 255, 255), font=wave_font)

    # 对小尺寸图标进行锐化
    if size <= 32:
        from PIL import ImageEnhance
        enhancer = ImageEnhance.Sharpness(image)
        image = enhancer.enhance(1.5)

    return image


def save_ico(images, sizes, output_path):
    """保存为 ICO 文件"""
    # ICO 文件头
    header = struct.pack('<HHH', 0, 1, len(sizes))

    # 目录项
    directory = b''
    offset = 6 + len(sizes) * 16  # 6 是文件头大小，16 是每个目录项的大小

    # 图像数据
    image_data = b''

    for i, (img, size) in enumerate(zip(images, sizes)):
        # 将图像转换为 PNG 格式
        png_data = io.BytesIO()
        img.save(png_data, format='PNG')
        png_bytes = png_data.getvalue()

        # 计算 PNG 数据大小
        png_size = len(png_bytes)

        # 如果尺寸大于 255，在目录项中使用 0
        width = size if size <= 255 else 0
        height = size if size <= 255 else 0

        # 添加目录项
        directory += struct.pack('<BBBBHHII',
                                 width,  # 宽度
                                 height,  # 高度
                                 0,     # 颜色数（0 表示 256 色）
                                 0,     # 保留
                                 1,     # 颜色平面数
                                 32,    # 每像素位数
                                 png_size,  # 图像数据大小
                                 offset   # 图像数据偏移
                                 )

        # 添加图像数据
        image_data += png_bytes
        offset += png_size

    # 写入文件
    with open(output_path, 'wb') as f:
        f.write(header)
        f.write(directory)
        f.write(image_data)


def main():
    print("开始生成图标...")

    # Windows需要的所有图标尺寸
    sizes = [16, 20, 24, 32, 40, 48, 64, 96, 128, 256, 512]
    images = []

    # 为每个尺寸创建优化的图标
    for size in sizes:
        print(f"生成 {size}x{size} 图标...")
        image = create_icon_for_size(size)
        images.append(image)

    # 确保输出目录存在
    output_dir = Path(__file__).parent.parent / "icons"
    output_dir.mkdir(exist_ok=True)
    output_path = output_dir / "app.ico"

    # 保存为ICO文件
    print("保存ICO文件...")
    save_ico(images, sizes, output_path)

    print(f"图标文件已生成：{output_path}")


if __name__ == "__main__":
    main()
