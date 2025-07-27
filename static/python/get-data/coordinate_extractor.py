#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Coordinate Extractor Application
A PySide6 application for extracting coordinate data from images
"""

import sys
import csv
import numpy as np
from pathlib import Path
from PySide6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout,
                               QHBoxLayout, QPushButton, QLabel, QFileDialog,
                               QGroupBox, QMessageBox, QTableWidget, QTableWidgetItem, QHeaderView, QInputDialog)
from PySide6.QtCore import Qt, QPoint, QRect
from PySide6.QtGui import QPixmap, QPainter, QPen, QColor, QCursor, QAction


class ImageLabel(QLabel):
    """
    自定义的图片显示标签，专门负责UI事件处理

    设计原则（关注点分离）：
    - UI事件捕获：鼠标点击、移动、键盘按键的原始事件捕获
    - 坐标提取：从UI事件中提取位置信息
    - 快捷键管理：X/Y/C键切换模式，Enter键采样点
    - 事件委托：将处理后的位置信息和操作指令传递给ImageViewer业务层

    UI事件处理职责：
    - mousePressEvent: 捕获点击并传递像素位置给业务层采样
    - mouseMoveEvent: 捕获移动并传递像素位置给业务层更新显示
    - keyPressEvent: 处理快捷键并调用业务层对应操作

    不负责：
    - 业务逻辑处理（如具体的采样、校准逻辑）
    - 数据存储和管理
    - 坐标转换计算（但会调用业务层的转换方法）
    - 放大镜、信息显示等业务UI更新
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self.parent_viewer = parent
        # 启用鼠标追踪，即使没有按下鼠标也能跟踪鼠标移动（用于实时放大镜）
        self.setMouseTracking(True)

    def mousePressEvent(self, event):
        """
        鼠标按下事件处理（UI层）

        职责：
        - 捕获鼠标点击事件
        - 获取点击位置坐标
        - 将位置信息传递给业务层进行采样处理
        """
        if not self.parent_viewer:
            return

        # 获取点击位置并转换为图片像素坐标
        click_pos = event.position().toPoint()
        pixmap_pos = self.parent_viewer.convert_widget_to_pixmap_position(
            click_pos)

        # 将有效的像素位置传递给业务层处理
        if pixmap_pos:
            self.parent_viewer.sample_point_at_position(pixmap_pos)

    def mouseMoveEvent(self, event):
        """
        鼠标移动事件处理（UI层）

        职责：
        - 捕获鼠标移动事件
        - 获取鼠标位置坐标
        - 将位置信息传递给业务层进行实时反馈更新
        """
        if not self.parent_viewer:
            return

        # 获取鼠标位置并转换为图片像素坐标
        move_pos = event.position().toPoint()
        pixmap_pos = self.parent_viewer.convert_widget_to_pixmap_position(
            move_pos)

        # 将有效的像素位置传递给业务层更新显示
        if pixmap_pos:
            self.parent_viewer.update_cursor_tracking_at_position(pixmap_pos)

    def keyPressEvent(self, event):
        """
        键盘按键事件处理（UI层）

        快捷键说明：
        - X: 切换到X轴校准模式
        - Y: 切换到Y轴校准模式  
        - C: 切换到曲线提取模式
        - Enter/Return: 在当前鼠标位置采样点
        """
        if not self.parent_viewer:
            super().keyPressEvent(event)
            return

        key = event.key()

        # 处理模式切换快捷键
        if key == Qt.Key_X:
            self.parent_viewer.switch_to_mode("x_axis")
        elif key == Qt.Key_Y:
            self.parent_viewer.switch_to_mode("y_axis")
        elif key == Qt.Key_C:
            self.parent_viewer.switch_to_mode("curve")
        elif key == Qt.Key_Return or key == Qt.Key_Enter or key == Qt.Key_S:
            # 获取当前鼠标位置并进行采样
            current_pos = self._get_current_mouse_position()
            if current_pos:
                self.parent_viewer.sample_point_at_position(current_pos)
        else:
            super().keyPressEvent(event)

    def _get_current_mouse_position(self):
        """获取当前鼠标在图片中的像素位置（UI层方法）"""
        if not self.parent_viewer or not self.parent_viewer.pixmap:
            return None

        # 获取鼠标的全局位置并转换为控件相对位置
        global_pos = QCursor.pos()
        widget_pos = self.mapFromGlobal(global_pos)

        # 转换为图片像素坐标
        return self.parent_viewer.convert_widget_to_pixmap_position(widget_pos)


class ImageViewer(QWidget):
    """
    图片查看和业务逻辑处理组件

    设计原则（关注点分离）：
    - 业务逻辑：坐标采样、校准点管理、模式切换的核心逻辑
    - 数据管理：校准点和曲线点的存储、转换、计算
    - 坐标转换：像素坐标与实际坐标的转换算法
    - 显示更新：放大镜、信息显示等业务相关的UI更新

    核心功能：
    1. 显示图片并支持缩放适应
    2. 提供三种交互模式：X轴校准、Y轴校准、曲线提取
    3. 实时放大镜显示鼠标周围区域
    4. 坐标转换：将像素坐标转换为实际坐标

    数据存储：
    - x_calibration_points: X轴校准点 [(像素值, 实际值), ...]
    - y_calibration_points: Y轴校准点 [(像素值, 实际值), ...]  
    - curve_points: 曲线点 [(像素x, 像素y), ...]

    不负责：
    - 原始UI事件处理（由ImageLabel负责）
    - 快捷键判断（由ImageLabel负责）
    """

    def __init__(self):
        super().__init__()
        self.pixmap = None                    # 原始图片数据
        self.current_mode = "curve"           # 当前交互模式
        self.x_calibration_points = []       # X轴校准点列表
        self.y_calibration_points = []       # Y轴校准点列表
        self.curve_points = []               # 曲线点列表
        self.curve_table = None              # 曲线点表格（由主窗口设置）
        self.main_window = None              # 主窗口引用（由主窗口设置）

        self.setup_ui()

    def setup_ui(self):
        """设置用户界面布局"""
        layout = QVBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)  # 移除边距避免重叠
        layout.setSpacing(0)  # 移除间距

        # 主图片显示区域 - 增大最小尺寸并优化样式
        self.image_label = ImageLabel(self)
        self.image_label.setMinimumSize(800, 500)  # 增大图片显示区域
        self.image_label.setStyleSheet("""
            border: 2px solid #34495e;
            border-radius: 4px;
            background-color: white;
        """)
        self.image_label.setAlignment(Qt.AlignCenter)
        self.image_label.setText("请加载图片")

        # 放大镜显示区域
        self.zoom_label = QLabel()
        # 设置合适的放大镜尺寸
        self.zoom_label.setFixedSize(250, 250)
        self.zoom_label.setStyleSheet("""
            border: 1px solid #7f8c8d;
            background-color: #ecf0f1;
            border-radius: 4px;
        """)
        self.zoom_label.setText("放大区域")
        self.zoom_label.setAlignment(Qt.AlignCenter)

        # 只添加图片标签，放大镜由主窗口管理
        layout.addWidget(self.image_label)

        self.setLayout(layout)

        # 设置图片标签可以接收键盘焦点（用于键盘快捷键）
        self.image_label.setFocusPolicy(Qt.StrongFocus)

    def load_image(self, image_path):
        """Load image from path"""
        self.pixmap = QPixmap(image_path)
        if not self.pixmap.isNull():
            self.update_display()
            self.update_status(f"图片已加载: {Path(image_path).name}")
            self.image_label.setFocus()
        else:
            self.update_status("图片加载失败")

    def update_display(self):
        """Update the image display"""
        if self.pixmap:
            scaled_pixmap = self.pixmap.scaled(
                self.image_label.size(),
                Qt.KeepAspectRatio,
                Qt.SmoothTransformation
            )
            self.image_label.setPixmap(scaled_pixmap)

    def get_current_mouse_position(self):
        """获取当前鼠标位置在图片中的像素坐标（业务层方法）"""
        if not self.image_label:
            return None

        global_pos = QCursor.pos()
        widget_pos = self.image_label.mapFromGlobal(global_pos)
        return self.convert_widget_to_pixmap_position(widget_pos)

    def update_status(self, message):
        """更新状态栏信息"""
        if self.main_window and hasattr(self.main_window, 'status_bar'):
            self.main_window.status_bar.showMessage(message)

    def add_calibration_point(self, axis, pixmap_pos):
        """
        添加校准点，用于建立像素坐标与实际坐标的对应关系

        Args:
            axis: "x" 或 "y"，表示校准哪个轴
            pixmap_pos: 在图片中的像素位置
        """
        axis_name = "X轴" if axis == "x" else "Y轴"
        pixel_val = pixmap_pos.x() if axis == "x" else pixmap_pos.y()

        # 弹出对话框让用户输入该像素位置对应的实际坐标值
        actual_val, ok = QInputDialog.getDouble(
            self, f"输入{axis_name}实际值",
            f"像素{axis_name}={pixel_val}, 请输入对应的实际{axis_name}值:",
            pixel_val, -1e10, 1e10, 6
        )

        if ok:
            # 将校准点添加到相应的校准数据列表中
            if axis == "x":
                self.x_calibration_points.append((pixel_val, actual_val))
                self.update_status(
                    f"X轴校准点 {len(self.x_calibration_points)} 已添加: 像素X={pixel_val}, 实际X={actual_val}")
                # 更新校准表格显示
                if hasattr(self.main_window, 'update_x_calibration_table'):
                    self.main_window.update_x_calibration_table()
            else:
                self.y_calibration_points.append((pixel_val, actual_val))
                self.update_status(
                    f"Y轴校准点 {len(self.y_calibration_points)} 已添加: 像素Y={pixel_val}, 实际Y={actual_val}")
                # 更新校准表格显示
                if hasattr(self.main_window, 'update_y_calibration_table'):
                    self.main_window.update_y_calibration_table()

    def switch_to_mode(self, mode):
        """
        切换操作模式（业务层方法）

        Args:
            mode: "x_axis", "y_axis", "curve"
        """
        self.set_mode(mode)
        # 同步更新主窗口的按钮状态
        if self.main_window:
            self.main_window.set_mode(mode)

    def convert_widget_to_pixmap_position(self, widget_pos):
        """
        将控件坐标转换为原始图片的像素坐标（业务层方法）

        这个方法从UI层的get_pixmap_position重构而来，专注于坐标转换逻辑

        Args:
            widget_pos: QLabel中的点击位置

        Returns:
            QPoint: 原始图片中的像素坐标，如果位置无效则返回None
        """
        if not self.pixmap:
            return None

        label_rect = self.image_label.rect()
        scaled_pixmap = self.image_label.pixmap()
        if not scaled_pixmap:
            return None

        # 计算缩放后图片在QLabel中的实际位置（居中显示时的偏移量）
        x_offset = (label_rect.width() - scaled_pixmap.width()) // 2
        y_offset = (label_rect.height() - scaled_pixmap.height()) // 2

        # 计算点击位置在缩放图片中的相对坐标
        scaled_x = widget_pos.x() - x_offset
        scaled_y = widget_pos.y() - y_offset

        # 检查点击位置是否在缩放图片的有效区域内
        if (0 <= scaled_x < scaled_pixmap.width() and
                0 <= scaled_y < scaled_pixmap.height()):
            # 计算缩放比例，将缩放坐标转换回原始图片坐标
            scale_x = self.pixmap.width() / scaled_pixmap.width()
            scale_y = self.pixmap.height() / scaled_pixmap.height()

            original_x = int(scaled_x * scale_x)
            original_y = int(scaled_y * scale_y)

            return QPoint(original_x, original_y)
        return None

    def update_cursor_tracking_at_position(self, pixmap_pos):
        """
        在指定像素位置更新光标跟踪和实时反馈显示（业务层方法）

        Args:
            pixmap_pos: QPoint对象，图片中的像素位置
        """
        self.update_zoom_area(pixmap_pos)
        self.update_info(pixmap_pos)

    def sample_point_at_position(self, position):
        """
        在指定位置采样点（业务核心方法）

        根据当前模式决定具体的采样行为：
        - X轴校准模式：添加X轴校准点
        - Y轴校准模式：添加Y轴校准点  
        - 曲线提取模式：添加曲线点

        Args:
            position: QPoint对象，图片中的像素位置
        """
        if self.current_mode == "x_axis":
            self.add_calibration_point("x", position)
        elif self.current_mode == "y_axis":
            self.add_calibration_point("y", position)
        elif self.current_mode == "curve":
            self.add_curve_point(position)

    def update_zoom_area(self, pos):
        """
        更新放大镜区域显示

        在鼠标位置周围创建一个放大视图，帮助用户精确定位点击位置
        """
        if not self.pixmap:
            return

        # 定义放大区域的大小（像素）
        zoom_size = 50
        x, y = pos.x(), pos.y()

        # 从原始图片中提取鼠标周围的区域
        zoom_rect = QRect(x - zoom_size//2, y - zoom_size //
                          2, zoom_size, zoom_size)
        # 确保放大区域不超出图片边界
        zoom_rect = zoom_rect.intersected(self.pixmap.rect())

        if zoom_rect.isValid():
            # 复制放大区域的图片内容
            zoom_pixmap = self.pixmap.copy(zoom_rect)

            # 将放大区域缩放到放大镜显示区域的大小，使用IgnoreAspectRatio确保完全填充
            scaled_zoom = zoom_pixmap.scaled(
                self.zoom_label.size(),
                Qt.IgnoreAspectRatio,  # 忽略宽高比，完全填充显示区域
                Qt.SmoothTransformation
            )

            # 在放大视图的中心绘制十字准线，指示精确位置
            center_x = scaled_zoom.width() // 2
            center_y = scaled_zoom.height() // 2

            painter = QPainter(scaled_zoom)
            painter.setPen(QPen(QColor(255, 0, 0), 2))  # 红色，2像素宽
            # 绘制水平线
            painter.drawLine(center_x - 10, center_y, center_x + 10, center_y)
            # 绘制垂直线
            painter.drawLine(center_x, center_y - 10, center_x, center_y + 10)
            painter.end()

            self.zoom_label.setPixmap(scaled_zoom)
        else:
            # 如果无法创建有效的放大区域，清空放大镜显示
            self.zoom_label.setPixmap(QPixmap())

    def update_info(self, pos):
        """Update information display"""
        info_text = f"位置: ({pos.x()}, {pos.y()})"

        if self.current_mode == "x_axis":
            info_text += f" | X轴校准点: {len(self.x_calibration_points)}"
        elif self.current_mode == "y_axis":
            info_text += f" | Y轴校准点: {len(self.y_calibration_points)}"
        elif self.current_mode == "curve":
            info_text += f" | 曲线点: {len(self.curve_points)} | 点击或按回车键/S键添加点"

        self.update_status(info_text)

    def add_curve_point(self, pos):
        """Add curve point"""
        self.curve_points.append((pos.x(), pos.y()))
        self.update_status(
            f"曲线点 {len(self.curve_points)} 已添加: ({pos.x()}, {pos.y()})")

        if self.curve_table:
            self.update_curve_table()

    def update_curve_table(self):
        """更新曲线点数据表格，包含像素坐标和转换后的实际坐标"""
        if not self.curve_table:
            return

        self.curve_table.setRowCount(len(self.curve_points))

        for i, (pixel_x, pixel_y) in enumerate(self.curve_points):
            # 填充像素坐标到表格
            self.curve_table.setItem(i, 0, QTableWidgetItem(str(pixel_x)))
            self.curve_table.setItem(i, 1, QTableWidgetItem(str(pixel_y)))

            # 初始化转换坐标为像素坐标（如果没有校准数据）
            transformed_x = pixel_x
            transformed_y = pixel_y

            # 尝试获取校准数据进行坐标转换
            if hasattr(self.main_window, 'get_calibration_data'):
                x_calibration = self.main_window.get_calibration_data(
                    self.main_window.x_calibration_table)
                y_calibration = self.main_window.get_calibration_data(
                    self.main_window.y_calibration_table)

                # 如果X轴和Y轴校准数据都存在，进行坐标转换
                if x_calibration and y_calibration:
                    transformed_coords = self._transform_single_point(
                        pixel_x, pixel_y, x_calibration, y_calibration)
                    if transformed_coords:
                        transformed_x, transformed_y = transformed_coords

            # 填充转换后的坐标到表格（保留2位小数）
            self.curve_table.setItem(
                i, 2, QTableWidgetItem(f"{transformed_x:.2f}"))
            self.curve_table.setItem(
                i, 3, QTableWidgetItem(f"{transformed_y:.2f}"))

    def _transform_single_point(self, pixel_x, pixel_y, x_calibration, y_calibration):
        """
        使用最小二乘法将单个像素点转换为实际坐标

        Args:
            pixel_x, pixel_y: 像素坐标
            x_calibration: X轴校准数据 [(像素值, 实际值), ...]
            y_calibration: Y轴校准数据 [(像素值, 实际值), ...]

        Returns:
            (transformed_x, transformed_y) 或 None（如果校准数据不足）
        """
        try:
            # 提取像素坐标和实际坐标
            x_pixels = [p[0] for p in x_calibration]
            y_pixels = [p[0] for p in y_calibration]
            x_actuals = [p[1] for p in x_calibration]
            y_actuals = [p[1] for p in y_calibration]

            # 检查校准点数量是否足够（至少需要2个点才能进行线性拟合）
            if len(x_pixels) >= 2 and len(y_pixels) >= 2:
                # 使用最小二乘法拟合X轴线性变换：actual_x = k_x * pixel_x + b_x
                A_x = np.vstack([x_pixels, np.ones(len(x_pixels))]).T
                k_x, b_x = np.linalg.lstsq(A_x, x_actuals, rcond=None)[0]
                transformed_x = k_x * pixel_x + b_x

                # 使用最小二乘法拟合Y轴线性变换：actual_y = k_y * pixel_y + b_y
                A_y = np.vstack([y_pixels, np.ones(len(y_pixels))]).T
                k_y, b_y = np.linalg.lstsq(A_y, y_actuals, rcond=None)[0]
                transformed_y = k_y * pixel_y + b_y

                return (transformed_x, transformed_y)
        except Exception:
            # 如果转换过程中出现任何错误，返回None
            pass

        return None

    def clear_points(self):
        """Clear all points"""
        self.x_calibration_points.clear()
        self.y_calibration_points.clear()
        self.curve_points.clear()
        self.update_status("所有点已清除")

        # 更新曲线表格显示
        if self.curve_table:
            self.update_curve_table()

    def set_mode(self, mode):
        """Set the current interaction mode"""
        self.current_mode = mode
        mode_names = {
            "x_axis": "X轴校准",
            "y_axis": "Y轴校准",
            "curve": "曲线提取"
        }
        self.update_status(f"模式: {mode_names.get(mode, mode)}")

        # Set cursor based on mode
        if mode in ["x_axis", "y_axis"]:
            self.image_label.setCursor(QCursor(Qt.CrossCursor))
        else:
            self.image_label.setCursor(QCursor(Qt.ArrowCursor))

        # Ensure keyboard focus is set to image label
        self.image_label.setFocus()


class CoordinateExtractorApp(QMainWindow):
    """Main application window"""

    def __init__(self):
        super().__init__()
        self.setWindowTitle("坐标提取器")

        # 设置更合理的初始窗口大小和最小尺寸
        self.setGeometry(100, 100, 1400, 900)  # 增大初始窗口
        self.setMinimumSize(1200, 700)  # 设置最小窗口尺寸确保可用性

        # 设置窗口图标和样式优化
        self.setStyleSheet("""
            QMainWindow {
                background-color: #ffffff;
            }
            QGroupBox {
                font-size: 10pt;
                border: 1px solid #dee2e6;
                margin-top: 12px;
                padding-top: 8px;
                border-radius: 4px;
            }
            QGroupBox::title {
                subcontrol-origin: margin;
                left: 10px;
                padding: 0 8px 0 8px;
                color: #495057;
                font-weight: bold;
                background-color: white;
            }
            QPushButton {
                background-color: #e1e1e1;
                border: 1px solid #cccccc;
                color: #333333;
                padding: 8px 12px;
                font-size: 9pt;
                min-height: 18px;
                text-align: center;
                border-radius: 3px;
            }
            QPushButton:hover {
                background-color: #d4d4d4;
                border-color: #b8b8b8;
            }
            QPushButton:pressed {
                background-color: #c8c8c8;
                border-color: #adadad;
            }
        """)

        # Initialize components
        self.image_viewer = ImageViewer()
        self.image_viewer.main_window = self  # Set reference to main window

        # 创建状态栏
        self.status_bar = self.statusBar()
        self.status_bar.showMessage("信息: 请选择模式并加载图片")

        self.setup_ui()

        # Connect curve table to image viewer
        self.image_viewer.curve_table = self.curve_table

        # 设置默认模式为曲线提取（程序的主要功能）
        self.set_mode("curve")

    def setup_ui(self):
        """Setup the user interface"""
        # 创建菜单栏
        self.create_menu_bar()

        # 创建工具栏
        self.create_tool_bar()

        central_widget = QWidget()
        self.setCentralWidget(central_widget)

        # 主布局：水平布局，左侧图片+校准区域，右侧曲线数据
        main_layout = QHBoxLayout()
        main_layout.setContentsMargins(10, 10, 10, 10)
        main_layout.setSpacing(15)

        # 左侧：图片和校准区域
        left_layout = QVBoxLayout()
        left_layout.setSpacing(10)

        # 图片显示区域
        left_layout.addWidget(self.image_viewer.image_label, 1)

        # 底部：放大镜 + X校准 + Y校准 (横排)
        bottom_layout = QHBoxLayout()
        bottom_layout.setSpacing(15)
        bottom_layout.setContentsMargins(0, 10, 0, 0)  # 增加顶部边距

        # 放大镜
        bottom_layout.addWidget(self.image_viewer.zoom_label)

        # X校准表格
        x_cal_group = self.create_x_calibration_group()
        x_cal_group.setMaximumWidth(250)
        x_cal_group.setMinimumHeight(200)  # 设置最小高度
        x_cal_group.setMaximumHeight(300)  # 设置最大高度
        bottom_layout.addWidget(x_cal_group)

        # Y校准表格
        y_cal_group = self.create_y_calibration_group()
        y_cal_group.setMaximumWidth(250)
        y_cal_group.setMinimumHeight(200)  # 设置最小高度
        y_cal_group.setMaximumHeight(300)  # 设置最大高度
        bottom_layout.addWidget(y_cal_group)

        bottom_layout.addStretch()  # 添加弹性空间

        left_layout.addLayout(bottom_layout)

        main_layout.addLayout(left_layout, 3)  # 左侧占3份空间

        # 右侧：曲线数据面板（独立一栏）
        curve_group = self.create_curve_group()
        curve_group.setMinimumWidth(400)
        curve_group.setMaximumWidth(500)
        main_layout.addWidget(curve_group, 2)  # 右侧占2份空间

        central_widget.setLayout(main_layout)

    def create_tool_bar(self):
        """创建工具栏"""
        toolbar = self.addToolBar('工具栏')
        toolbar.setToolButtonStyle(Qt.ToolButtonIconOnly)  # 只显示图标

        # 加载图片
        load_action = QAction(self)
        load_action.setIcon(self.style().standardIcon(
            self.style().StandardPixmap.SP_DirOpenIcon))
        load_action.setToolTip('加载图片文件')
        load_action.setStatusTip('加载图片文件')
        load_action.triggered.connect(self.load_image)
        toolbar.addAction(load_action)

        toolbar.addSeparator()

        # 保存CSV
        save_action = QAction(self)
        save_action.setIcon(self.style().standardIcon(
            self.style().StandardPixmap.SP_DialogSaveButton))
        save_action.setToolTip('保存CSV文件')
        save_action.setStatusTip('保存提取的数据到CSV文件')
        save_action.triggered.connect(self.save_csv)
        toolbar.addAction(save_action)

        toolbar.addSeparator()

        # X轴校准模式
        x_axis_action = QAction(self)
        x_axis_action.setIcon(self.style().standardIcon(
            self.style().StandardPixmap.SP_ArrowRight))
        x_axis_action.setToolTip('X轴校准模式')
        x_axis_action.setStatusTip('切换到X轴校准模式')
        x_axis_action.setCheckable(True)  # 设置为可选中状态
        x_axis_action.triggered.connect(lambda: self.set_mode("x_axis"))
        toolbar.addAction(x_axis_action)
        self.x_axis_toolbar_action = x_axis_action

        # Y轴校准模式
        y_axis_action = QAction(self)
        y_axis_action.setIcon(self.style().standardIcon(
            self.style().StandardPixmap.SP_ArrowUp))
        y_axis_action.setToolTip('Y轴校准模式')
        y_axis_action.setStatusTip('切换到Y轴校准模式')
        y_axis_action.setCheckable(True)  # 设置为可选中状态
        y_axis_action.triggered.connect(lambda: self.set_mode("y_axis"))
        toolbar.addAction(y_axis_action)
        self.y_axis_toolbar_action = y_axis_action

        # 曲线提取模式
        curve_action = QAction(self)
        curve_action.setIcon(self.style().standardIcon(
            self.style().StandardPixmap.SP_DialogYesButton))
        curve_action.setToolTip('曲线提取模式')
        curve_action.setStatusTip('切换到曲线提取模式')
        curve_action.setCheckable(True)  # 设置为可选中状态
        curve_action.triggered.connect(lambda: self.set_mode("curve"))
        toolbar.addAction(curve_action)
        self.curve_toolbar_action = curve_action

        toolbar.addSeparator()

        # 清除所有点
        clear_action = QAction(self)
        clear_action.setIcon(self.style().standardIcon(
            self.style().StandardPixmap.SP_DialogResetButton))
        clear_action.setToolTip('清除所有点')
        clear_action.setStatusTip('清除所有校准点和曲线点')
        clear_action.triggered.connect(self.clear_points)
        toolbar.addAction(clear_action)

    def create_menu_bar(self):
        """创建菜单栏"""
        menubar = self.menuBar()

        # 文件菜单
        file_menu = menubar.addMenu('文件(&F)')

        # 加载图片
        load_action = QAction('加载图片(&O)', self)
        load_action.setShortcut('Ctrl+O')
        load_action.setStatusTip('加载图片文件')
        load_action.triggered.connect(self.load_image)
        file_menu.addAction(load_action)

        file_menu.addSeparator()

        # 保存CSV
        save_action = QAction('保存CSV(&S)', self)
        save_action.setShortcut('Ctrl+S')
        save_action.setStatusTip('保存提取的数据到CSV文件')
        save_action.triggered.connect(self.save_csv)
        file_menu.addAction(save_action)

        file_menu.addSeparator()

        # 退出
        exit_action = QAction('退出(&X)', self)
        exit_action.setShortcut('Ctrl+Q')
        exit_action.setStatusTip('退出应用程序')
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)

        # 模式菜单
        mode_menu = menubar.addMenu('模式(&M)')

        # X轴校准模式
        x_axis_action = QAction('X轴校准(&X)', self)
        x_axis_action.setShortcut('X')
        x_axis_action.setStatusTip('切换到X轴校准模式')
        x_axis_action.triggered.connect(lambda: self.set_mode("x_axis"))
        mode_menu.addAction(x_axis_action)
        self.x_axis_action = x_axis_action

        # Y轴校准模式
        y_axis_action = QAction('Y轴校准(&Y)', self)
        y_axis_action.setShortcut('Y')
        y_axis_action.setStatusTip('切换到Y轴校准模式')
        y_axis_action.triggered.connect(lambda: self.set_mode("y_axis"))
        mode_menu.addAction(y_axis_action)
        self.y_axis_action = y_axis_action

        # 曲线提取模式
        curve_action = QAction('曲线提取(&C)', self)
        curve_action.setShortcut('C')
        curve_action.setStatusTip('切换到曲线提取模式')
        curve_action.triggered.connect(lambda: self.set_mode("curve"))
        mode_menu.addAction(curve_action)
        self.curve_action = curve_action

        mode_menu.addSeparator()

        # 清除所有点
        clear_action = QAction('清除所有点(&R)', self)
        clear_action.setShortcut('Ctrl+R')
        clear_action.setStatusTip('清除所有校准点和曲线点')
        clear_action.triggered.connect(self.clear_points)
        mode_menu.addAction(clear_action)

    def create_x_calibration_group(self):
        """Create X calibration group"""
        x_cal_group = QGroupBox("X轴校准")
        x_cal_layout = QVBoxLayout()
        x_cal_layout.setContentsMargins(10, 15, 10, 10)
        x_cal_layout.setSpacing(8)

        # X轴校准表格
        self.x_calibration_table = self.create_calibration_table([
                                                                 "像素X", "实际X"])
        self.x_calibration_table.setMinimumHeight(80)
        self.x_calibration_table.setMaximumHeight(120)
        x_cal_layout.addWidget(self.x_calibration_table)

        # X轴校准控制按钮
        x_cal_btn_layout = QHBoxLayout()
        x_cal_btn_layout.setContentsMargins(0, 6, 0, 0)
        x_cal_btn_layout.setSpacing(5)

        self.delete_x_cal_btn = QPushButton("删除选中")
        self.delete_x_cal_btn.clicked.connect(
            self.delete_selected_x_calibration)
        self.delete_x_cal_btn.setStyleSheet(
            "font-size: 8pt; padding: 4px 8px;")
        x_cal_btn_layout.addWidget(self.delete_x_cal_btn)
        x_cal_btn_layout.addStretch()

        x_cal_layout.addLayout(x_cal_btn_layout)
        x_cal_group.setLayout(x_cal_layout)
        return x_cal_group

    def create_y_calibration_group(self):
        """Create Y calibration group"""
        y_cal_group = QGroupBox("Y轴校准")
        y_cal_layout = QVBoxLayout()
        y_cal_layout.setContentsMargins(10, 15, 10, 10)
        y_cal_layout.setSpacing(8)

        # Y轴校准表格
        self.y_calibration_table = self.create_calibration_table([
                                                                 "像素Y", "实际Y"])
        self.y_calibration_table.setMinimumHeight(80)
        self.y_calibration_table.setMaximumHeight(120)
        y_cal_layout.addWidget(self.y_calibration_table)

        # Y轴校准控制按钮
        y_cal_btn_layout = QHBoxLayout()
        y_cal_btn_layout.setContentsMargins(0, 6, 0, 0)
        y_cal_btn_layout.setSpacing(5)

        self.delete_y_cal_btn = QPushButton("删除选中")
        self.delete_y_cal_btn.clicked.connect(
            self.delete_selected_y_calibration)
        self.delete_y_cal_btn.setStyleSheet(
            "font-size: 8pt; padding: 4px 8px;")
        y_cal_btn_layout.addWidget(self.delete_y_cal_btn)
        y_cal_btn_layout.addStretch()

        y_cal_layout.addLayout(y_cal_btn_layout)
        y_cal_group.setLayout(y_cal_layout)
        return y_cal_group

    def create_calibration_table(self, headers):
        """Create a calibration table with given headers"""
        table = QTableWidget()
        table.setColumnCount(2)
        table.setHorizontalHeaderLabels(headers)
        table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        table.setEditTriggers(QTableWidget.DoubleClicked |
                              QTableWidget.SelectedClicked)

        # 优化校准表格的高度设置 - 确保内容可见且不会过大
        table.setMinimumHeight(80)
        table.setMaximumHeight(120)
        table.setSizeAdjustPolicy(QTableWidget.AdjustToContents)
        table.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        table.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        table.setStyleSheet("""
            QTableWidget {
                border: 1px solid #dee2e6;
                background-color: white;
                gridline-color: #e0e0e0;
                font-size: 9pt;
                selection-background-color: #0078d4;
                selection-color: white;
            }
            QTableWidget::item {
                padding: 4px 6px;
                border-bottom: 1px solid #e0e0e0;
                min-height: 20px;
            }
            QTableWidget::item:selected {
                background-color: #0078d4;
                color: white;
            }
            QHeaderView::section {
                background-color: #f5f5f5;
                color: #333333;
                padding: 4px 6px;
                border: 1px solid #dee2e6;
                font-size: 9pt;
                min-height: 24px;
                font-weight: normal;
            }
            QScrollBar:vertical {
                width: 12px;
                background-color: #f8f9fa;
            }
        """)

        # 连接表格内容变化信号，当用户编辑校准数据时自动更新曲线点转换坐标
        table.itemChanged.connect(self.on_calibration_table_changed)

        return table

    def on_calibration_table_changed(self, item):
        """
        处理校准表格内容变化事件

        当用户编辑校准表格中的数值时，自动更新相应的校准数据并重新计算曲线点的转换坐标

        Args:
            item: 被修改的表格项
        """
        if not item:
            return

        table = item.tableWidget()
        if not table:
            return

        try:
            # 验证输入数据的有效性
            if item.column() == 0:  # 像素值列
                pixel_val = int(item.text())
            else:  # 实际值列
                actual_val = float(item.text())

            # 确定是X轴还是Y轴校准表格
            if table == self.x_calibration_table:
                # 更新X轴校准点数据
                self.sync_calibration_data_from_table(self.x_calibration_table,
                                                      self.image_viewer.x_calibration_points)
                self.status_bar.showMessage("X轴校准数据已更新")
            elif table == self.y_calibration_table:
                # 更新Y轴校准点数据
                self.sync_calibration_data_from_table(self.y_calibration_table,
                                                      self.image_viewer.y_calibration_points)
                self.status_bar.showMessage("Y轴校准数据已更新")

            # 校准数据变化后，更新曲线点表格中的转换坐标
            self.image_viewer.update_curve_table()

        except ValueError:
            # 如果输入的不是有效的数字，恢复原值
            self.status_bar.showMessage("校准数据输入无效，请输入正确的数字")
            # 重新加载表格数据以恢复原值
            if table == self.x_calibration_table:
                self.update_x_calibration_table()
            else:
                self.update_y_calibration_table()

    def sync_calibration_data_from_table(self, table_widget, calibration_points):
        """
        从表格同步校准数据到内存中的校准点列表

        Args:
            table_widget: 校准表格控件
            calibration_points: 要更新的校准点数据列表
        """
        calibration_points.clear()
        for row in range(table_widget.rowCount()):
            pixel_item = table_widget.item(row, 0)
            actual_item = table_widget.item(row, 1)
            if pixel_item and actual_item:
                try:
                    pixel_val = int(pixel_item.text())
                    actual_val = float(actual_item.text())
                    calibration_points.append((pixel_val, actual_val))
                except ValueError:
                    # 跳过无效的条目
                    continue

    def create_curve_group(self):
        """Create curve points group"""
        curve_group = QGroupBox("曲线点数据")
        curve_layout = QVBoxLayout()
        curve_layout.setContentsMargins(10, 15, 10, 10)
        curve_layout.setSpacing(8)

        # Create table for curve points - 优化表格高度，充分利用右侧空间
        self.curve_table = QTableWidget()
        self.curve_table.setColumnCount(4)
        self.curve_table.setHorizontalHeaderLabels(
            ["像素X", "像素Y", "转换X", "转换Y"])
        self.curve_table.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)

        # 设置表格高度，充分利用右侧空间
        self.curve_table.setMinimumHeight(400)
        self.curve_table.setSizeAdjustPolicy(QTableWidget.AdjustToContents)
        self.curve_table.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.curve_table.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        # 优化表格样式，确保良好的可读性
        self.curve_table.setStyleSheet("""
            QTableWidget {
                border: 1px solid #dee2e6;
                background-color: white;
                selection-background-color: #0078d4;
                selection-color: white;
                gridline-color: #e0e0e0;
                font-size: 9pt;
                alternate-background-color: #f8f9fa;
            }
            QTableWidget::item {
                padding: 4px 6px;
                border-bottom: 1px solid #e0e0e0;
                min-height: 22px;
            }
            QTableWidget::item:selected {
                background-color: #0078d4;
                color: white;
            }
            QHeaderView::section {
                background-color: #f5f5f5;
                color: #333333;
                padding: 6px 4px;
                border: 1px solid #dee2e6;
                font-size: 9pt;
                min-height: 26px;
                font-weight: normal;
            }
            QScrollBar:vertical {
                width: 12px;
                background-color: #f8f9fa;
                border: 1px solid #dee2e6;
            }
            QScrollBar::handle:vertical {
                background-color: #c0c0c0;
                border-radius: 4px;
                min-height: 20px;
            }
            QScrollBar::handle:vertical:hover {
                background-color: #a0a0a0;
            }
        """)

        # 启用交替行颜色以提高可读性
        self.curve_table.setAlternatingRowColors(True)

        curve_layout.addWidget(self.curve_table)

        # 表格控制按钮 - 优化布局确保不与表格重叠
        table_btn_layout = QHBoxLayout()
        table_btn_layout.setContentsMargins(0, 8, 0, 0)  # 增加上边距
        table_btn_layout.setSpacing(8)

        self.delete_point_btn = QPushButton("删除选中点")
        self.delete_point_btn.clicked.connect(self.delete_selected_point)
        table_btn_layout.addWidget(self.delete_point_btn)

        self.move_up_btn = QPushButton("上移")
        self.move_up_btn.clicked.connect(self.move_point_up)
        table_btn_layout.addWidget(self.move_up_btn)

        self.move_down_btn = QPushButton("下移")
        self.move_down_btn.clicked.connect(self.move_point_down)
        table_btn_layout.addWidget(self.move_down_btn)

        # 添加弹性空间确保按钮左对齐
        table_btn_layout.addStretch()

        curve_layout.addLayout(table_btn_layout)
        curve_group.setLayout(curve_layout)
        return curve_group

    def load_image(self):
        """Load image from file dialog"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "选择图片", "", "图片文件 (*.png *.jpg *.jpeg *.bmp)"
        )
        if file_path:
            self.image_viewer.load_image(file_path)

    def set_mode(self, mode):
        """Set the interaction mode"""
        self.image_viewer.set_mode(mode)

        # 更新菜单项文本来表示当前模式
        self.x_axis_action.setText("X轴校准(&X)")
        self.y_axis_action.setText("Y轴校准(&Y)")
        self.curve_action.setText("曲线提取(&C)")

        # 重置所有工具栏按钮的选中状态
        self.x_axis_toolbar_action.setChecked(False)
        self.y_axis_toolbar_action.setChecked(False)
        self.curve_toolbar_action.setChecked(False)

        # 重置工具栏按钮工具提示
        self.x_axis_toolbar_action.setToolTip("X轴校准模式")
        self.y_axis_toolbar_action.setToolTip("Y轴校准模式")
        self.curve_toolbar_action.setToolTip("曲线提取模式")

        # 根据当前模式设置菜单项和工具栏按钮状态
        if mode == "x_axis":
            self.x_axis_action.setText("● X轴校准(&X)")
            self.x_axis_toolbar_action.setChecked(True)
            self.x_axis_toolbar_action.setToolTip("● X轴校准模式 (当前激活)")
        elif mode == "y_axis":
            self.y_axis_action.setText("● Y轴校准(&Y)")
            self.y_axis_toolbar_action.setChecked(True)
            self.y_axis_toolbar_action.setToolTip("● Y轴校准模式 (当前激活)")
        elif mode == "curve":
            self.curve_action.setText("● 曲线提取(&C)")
            self.curve_toolbar_action.setChecked(True)
            self.curve_toolbar_action.setToolTip("● 曲线提取模式 (当前激活)")

    def clear_points(self):
        """Clear all points"""
        self.image_viewer.clear_points()
        self.x_calibration_table.setRowCount(0)
        self.y_calibration_table.setRowCount(0)
        self.curve_table.setRowCount(0)  # 同时清除曲线表格显示
        # 在状态栏显示清除结果
        self.status_bar.showMessage("所有点已清除")

    def get_calibration_data(self, table_widget):
        """Extract calibration data from table widget"""
        calibration = []
        for row in range(table_widget.rowCount()):
            pixel_item = table_widget.item(row, 0)
            actual_item = table_widget.item(row, 1)
            if pixel_item and actual_item:
                try:
                    pixel_val = int(pixel_item.text())
                    actual_val = float(actual_item.text())
                    calibration.append((pixel_val, actual_val))
                except ValueError:
                    pass  # Ignore invalid entries
        return calibration

    def update_calibration_table(self, table_widget, calibration_points):
        """Update calibration table from data"""
        # 临时断开信号连接，避免在更新表格时触发itemChanged信号
        try:
            table_widget.itemChanged.disconnect(
                self.on_calibration_table_changed)
        except TypeError:
            # 如果信号没有连接，忽略错误
            pass

        table_widget.setRowCount(len(calibration_points))
        for i, (pixel_val, actual_val) in enumerate(calibration_points):
            table_widget.setItem(i, 0, QTableWidgetItem(str(pixel_val)))
            table_widget.setItem(i, 1, QTableWidgetItem(str(actual_val)))

        # 重新连接信号
        table_widget.itemChanged.connect(self.on_calibration_table_changed)

    def update_x_calibration_table(self):
        """Update X calibration table from image viewer data"""
        self.update_calibration_table(
            self.x_calibration_table, self.image_viewer.x_calibration_points)
        # 校准点变化后，同时更新曲线点表格中的转换坐标
        self.image_viewer.update_curve_table()

    def update_y_calibration_table(self):
        """Update Y calibration table from image viewer data"""
        self.update_calibration_table(
            self.y_calibration_table, self.image_viewer.y_calibration_points)
        # 校准点变化后，同时更新曲线点表格中的转换坐标
        self.image_viewer.update_curve_table()

    def delete_calibration_point(self, table_widget, calibration_points, point_type):
        """
        删除选中的校准点

        Args:
            table_widget: 校准表格控件
            calibration_points: 校准点数据列表
            point_type: 校准点类型（用于显示消息）
        """
        current_row = table_widget.currentRow()
        if current_row >= 0 and current_row < len(calibration_points):
            # 从数据列表中删除选中的校准点
            del calibration_points[current_row]
            # 更新表格显示
            self.update_calibration_table(table_widget, calibration_points)
            # 校准点删除后，同时更新曲线点表格中的转换坐标
            self.image_viewer.update_curve_table()
            self.status_bar.showMessage(
                f"已删除{point_type}校准点 {current_row + 1}")

    def delete_selected_x_calibration(self):
        """删除选中的X轴校准点"""
        self.delete_calibration_point(
            self.x_calibration_table, self.image_viewer.x_calibration_points, "X轴")

    def delete_selected_y_calibration(self):
        """删除选中的Y轴校准点"""
        self.delete_calibration_point(
            self.y_calibration_table, self.image_viewer.y_calibration_points, "Y轴")

    def delete_selected_point(self):
        """删除选中的曲线点"""
        current_row = self.curve_table.currentRow()
        if current_row >= 0 and current_row < len(self.image_viewer.curve_points):
            del self.image_viewer.curve_points[current_row]
            self.image_viewer.update_curve_table()
            self.status_bar.showMessage(f"已删除曲线点 {current_row + 1}")

    def move_point_up(self):
        """将选中的曲线点上移一位"""
        current_row = self.curve_table.currentRow()
        if current_row > 0 and current_row < len(self.image_viewer.curve_points):
            # 与上一个点交换位置
            points = self.image_viewer.curve_points
            points[current_row], points[current_row - 1] = \
                points[current_row - 1], points[current_row]
            self.image_viewer.update_curve_table()
            # 保持选中状态在移动后的位置
            self.curve_table.setCurrentCell(current_row - 1, 0)

    def move_point_down(self):
        """将选中的曲线点下移一位"""
        current_row = self.curve_table.currentRow()
        if current_row >= 0 and current_row < len(self.image_viewer.curve_points) - 1:
            # 与下一个点交换位置
            points = self.image_viewer.curve_points
            points[current_row], points[current_row + 1] = \
                points[current_row + 1], points[current_row]
            self.image_viewer.update_curve_table()
            # 保持选中状态在移动后的位置
            self.curve_table.setCurrentCell(current_row + 1, 0)

    def save_csv(self):
        """
        保存提取的数据到CSV文件

        会生成三个文件：
        1. 主数据文件：包含所有曲线点的像素坐标和转换后的实际坐标
        2. X轴校准文件：包含X轴的校准数据
        3. Y轴校准文件：包含Y轴的校准数据
        """
        # 检查是否有曲线点数据
        if not self.image_viewer.curve_points:
            QMessageBox.warning(self, "警告", "没有曲线点数据")
            return

        # 获取校准数据
        x_calibration = self.get_calibration_data(self.x_calibration_table)
        y_calibration = self.get_calibration_data(self.y_calibration_table)

        # 检查校准数据是否完整
        if not x_calibration or not y_calibration:
            QMessageBox.warning(self, "警告", "请完成X轴和Y轴校准")
            return

        # 使用校准数据转换曲线点坐标
        transformed_points = self.transform_points(
            self.image_viewer.curve_points, x_calibration, y_calibration
        )

        # 选择保存位置
        file_path, _ = QFileDialog.getSaveFileName(
            self, "保存CSV文件", "", "CSV文件 (*.csv)"
        )

        if file_path:
            # 生成三个相关文件的路径
            base_path = Path(file_path)
            base_name = base_path.stem
            base_dir = base_path.parent

            main_file = file_path
            x_cal_file = base_dir / f"{base_name}-X.csv"
            y_cal_file = base_dir / f"{base_name}-Y.csv"

            # 保存三个文件
            self.save_to_csv(transformed_points, main_file)
            self.save_calibration_csv(x_calibration, x_cal_file, "X")
            self.save_calibration_csv(y_calibration, y_cal_file, "Y")

            self.status_bar.showMessage(
                f"成功保存数据到: {Path(main_file).name}, {Path(x_cal_file).name}, {Path(y_cal_file).name}")

    def save_calibration_csv(self, calibration_data, file_path, axis_name):
        """Save calibration data to CSV file"""
        with open(file_path, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow(
                [f'pixel_{axis_name.lower()}', f'actual_{axis_name.lower()}'])
            for pixel_val, actual_val in calibration_data:
                writer.writerow([pixel_val, actual_val])

    def transform_points(self, curve_points, x_calibration, y_calibration):
        """
        使用最小二乘法将所有曲线点的像素坐标转换为实际坐标

        工作原理：
        1. 根据校准点建立像素坐标到实际坐标的线性变换关系
        2. 使用最小二乘法拟合直线：actual = k * pixel + b
        3. 将所有曲线点应用这个变换关系

        Args:
            curve_points: 曲线点像素坐标列表 [(pixel_x, pixel_y), ...]
            x_calibration: X轴校准数据 [(像素值, 实际值), ...]
            y_calibration: Y轴校准数据 [(像素值, 实际值), ...]

        Returns:
            变换后的点列表 [(pixel_x, pixel_y, actual_x, actual_y), ...]
        """
        # 提取校准点的像素坐标和实际坐标
        x_pixels = [p[0] for p in x_calibration]
        y_pixels = [p[0] for p in y_calibration]

        def fit_linear_transformation(pixel_coords, actual_coords):
            """
            拟合线性变换关系：actual = k * pixel + b

            如果校准点少于2个，返回恒等变换（不进行坐标转换）
            """
            if len(pixel_coords) < 2:
                return lambda x: x  # 恒等函数，不进行转换

            # 构建最小二乘法的系数矩阵 A = [pixel_coords, ones]
            # 求解方程组：A * [k, b]^T = actual_coords
            A = np.vstack([pixel_coords, np.ones(len(pixel_coords))]).T
            k, b = np.linalg.lstsq(A, actual_coords, rcond=None)[0]
            return lambda x: k * x + b

        # 创建X轴和Y轴的坐标变换函数
        transform_x = fit_linear_transformation(
            x_pixels, [p[1] for p in x_calibration])
        transform_y = fit_linear_transformation(
            y_pixels, [p[1] for p in y_calibration])

        # 对所有曲线点应用坐标变换
        transformed = []
        for pixel_x, pixel_y in curve_points:
            x_val = transform_x(pixel_x)
            y_val = transform_y(pixel_y)
            transformed.append((pixel_x, pixel_y, x_val, y_val))

        return transformed

    def save_to_csv(self, points, file_path):
        """Save points to CSV file"""
        with open(file_path, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow(['px', 'py', 'x', 'y'])
            for px, py, x, y in points:
                writer.writerow([px, py, x, y])

        # 在状态栏显示保存结果
        self.status_bar.showMessage(
            f"已保存 {len(points)} 个数据点到: {Path(file_path).name}")


def main():
    app = QApplication(sys.argv)
    window = CoordinateExtractorApp()
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
