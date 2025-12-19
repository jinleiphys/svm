# -*- coding: utf-8 -*-
"""
样式系统 - 提供现代化的UI主题

包含浅色和深色主题，使用设计令牌(Design Tokens)确保一致性
"""

from typing import Dict, Any

# =============================================================================
# 设计令牌 - 颜色系统
# =============================================================================

COLORS_LIGHT = {
    # 主色调
    'primary': '#2563eb',           # 蓝色主色
    'primary_hover': '#1d4ed8',     # 悬停状态
    'primary_active': '#1e40af',    # 按下状态
    'primary_light': '#dbeafe',     # 浅色背景

    # 背景色
    'background': '#f8fafc',        # 页面背景
    'surface': '#ffffff',           # 卡片/面板背景
    'surface_hover': '#f1f5f9',     # 悬停背景

    # 边框
    'border': '#e2e8f0',            # 默认边框
    'border_hover': '#cbd5e1',      # 悬停边框
    'border_focus': '#2563eb',      # 焦点边框

    # 文本
    'text_primary': '#1e293b',      # 主要文本
    'text_secondary': '#64748b',    # 次要文本
    'text_disabled': '#94a3b8',     # 禁用文本
    'text_inverse': '#ffffff',      # 反色文本

    # 语义色
    'success': '#10b981',           # 成功/运行
    'success_light': '#d1fae5',
    'warning': '#f59e0b',           # 警告
    'warning_light': '#fef3c7',
    'error': '#ef4444',             # 错误/停止
    'error_light': '#fee2e2',
    'info': '#3b82f6',              # 信息
    'info_light': '#dbeafe',
}

COLORS_DARK = {
    # 主色调
    'primary': '#3b82f6',
    'primary_hover': '#60a5fa',
    'primary_active': '#2563eb',
    'primary_light': '#1e3a5f',

    # 背景色
    'background': '#0f172a',
    'surface': '#1e293b',
    'surface_hover': '#334155',

    # 边框
    'border': '#334155',
    'border_hover': '#475569',
    'border_focus': '#3b82f6',

    # 文本
    'text_primary': '#f1f5f9',
    'text_secondary': '#94a3b8',
    'text_disabled': '#64748b',
    'text_inverse': '#0f172a',

    # 语义色
    'success': '#34d399',
    'success_light': '#064e3b',
    'warning': '#fbbf24',
    'warning_light': '#78350f',
    'error': '#f87171',
    'error_light': '#7f1d1d',
    'info': '#60a5fa',
    'info_light': '#1e3a5f',
}

# =============================================================================
# 设计令牌 - 间距系统 (基于8px网格)
# =============================================================================

SPACING = {
    'xs': '4px',
    'sm': '8px',
    'md': '12px',
    'lg': '16px',
    'xl': '24px',
    'xxl': '32px',
}

# =============================================================================
# 设计令牌 - 圆角
# =============================================================================

RADIUS = {
    'sm': '4px',
    'md': '6px',
    'lg': '8px',
    'xl': '12px',
    'full': '9999px',
}

# =============================================================================
# 设计令牌 - 字体大小
# =============================================================================

FONT_SIZE = {
    'xs': '11px',
    'sm': '12px',
    'md': '13px',
    'lg': '14px',
    'xl': '16px',
    '2xl': '18px',
    '3xl': '24px',
}


def get_stylesheet(theme: str = 'light') -> str:
    """
    生成完整的样式表

    Args:
        theme: 主题名称 ('light' 或 'dark')

    Returns:
        CSS样式表字符串
    """
    colors = COLORS_LIGHT if theme == 'light' else COLORS_DARK

    return f"""
    /* =========================================== */
    /* 全局样式                                     */
    /* =========================================== */

    QMainWindow, QWidget {{
        background-color: {colors['background']};
        color: {colors['text_primary']};
        font-size: {FONT_SIZE['md']};
    }}

    /* =========================================== */
    /* 按钮样式                                     */
    /* =========================================== */

    QPushButton {{
        background-color: {colors['surface']};
        color: {colors['text_primary']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['md']};
        padding: {SPACING['sm']} {SPACING['lg']};
        font-size: {FONT_SIZE['md']};
        font-weight: 500;
        min-height: 32px;
    }}

    QPushButton:hover {{
        background-color: {colors['surface_hover']};
        border-color: {colors['border_hover']};
    }}

    QPushButton:pressed {{
        background-color: {colors['primary_light']};
    }}

    QPushButton:disabled {{
        background-color: {colors['surface']};
        color: {colors['text_disabled']};
        border-color: {colors['border']};
    }}

    /* 主要按钮 */
    QPushButton#primaryButton {{
        background-color: {colors['primary']};
        color: {colors['text_inverse']};
        border: none;
    }}

    QPushButton#primaryButton:hover {{
        background-color: {colors['primary_hover']};
    }}

    QPushButton#primaryButton:pressed {{
        background-color: {colors['primary_active']};
    }}

    /* 运行按钮 */
    QPushButton#runButton {{
        background-color: {colors['success']};
        color: white;
        border: none;
        font-weight: 600;
    }}

    QPushButton#runButton:hover {{
        background-color: #059669;
    }}

    /* 停止按钮 */
    QPushButton#stopButton {{
        background-color: {colors['error']};
        color: white;
        border: none;
    }}

    QPushButton#stopButton:hover {{
        background-color: #dc2626;
    }}

    /* =========================================== */
    /* 输入框样式                                   */
    /* =========================================== */

    QLineEdit, QSpinBox, QDoubleSpinBox {{
        background-color: {colors['surface']};
        color: {colors['text_primary']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['md']};
        padding: {SPACING['sm']} {SPACING['md']};
        font-size: {FONT_SIZE['md']};
        min-height: 28px;
    }}

    QLineEdit:focus, QSpinBox:focus, QDoubleSpinBox:focus {{
        border-color: {colors['border_focus']};
        outline: none;
    }}

    QLineEdit:disabled, QSpinBox:disabled, QDoubleSpinBox:disabled {{
        background-color: {colors['surface_hover']};
        color: {colors['text_disabled']};
    }}

    /* =========================================== */
    /* 下拉框样式                                   */
    /* =========================================== */

    QComboBox {{
        background-color: {colors['surface']};
        color: {colors['text_primary']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['md']};
        padding: {SPACING['sm']} {SPACING['md']};
        font-size: {FONT_SIZE['md']};
        min-height: 28px;
    }}

    QComboBox:hover {{
        border-color: {colors['border_hover']};
    }}

    QComboBox:focus {{
        border-color: {colors['border_focus']};
    }}

    QComboBox::drop-down {{
        border: none;
        width: 24px;
    }}

    QComboBox QAbstractItemView {{
        background-color: {colors['surface']};
        color: {colors['text_primary']};
        border: 1px solid {colors['border']};
        border-radius: {RADIUS['md']};
        selection-background-color: {colors['primary_light']};
    }}

    /* =========================================== */
    /* 分组框样式                                   */
    /* =========================================== */

    QGroupBox {{
        background-color: {colors['surface']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['lg']};
        margin-top: 20px;
        padding: {SPACING['lg']};
        font-weight: 600;
    }}

    QGroupBox::title {{
        subcontrol-origin: margin;
        subcontrol-position: top left;
        left: {SPACING['lg']};
        padding: 0 {SPACING['sm']};
        background-color: {colors['surface']};
        color: {colors['text_primary']};
    }}

    /* =========================================== */
    /* 标签页样式                                   */
    /* =========================================== */

    QTabWidget::pane {{
        background-color: {colors['surface']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['lg']};
        margin-top: -1px;
    }}

    QTabBar::tab {{
        background-color: {colors['surface_hover']};
        color: {colors['text_secondary']};
        border: 1.5px solid {colors['border']};
        border-bottom: none;
        border-top-left-radius: {RADIUS['md']};
        border-top-right-radius: {RADIUS['md']};
        padding: {SPACING['sm']} {SPACING['xl']};
        margin-right: 2px;
        font-weight: 500;
    }}

    QTabBar::tab:selected {{
        background-color: {colors['surface']};
        color: {colors['primary']};
        border-bottom: 2px solid {colors['primary']};
    }}

    QTabBar::tab:hover:!selected {{
        background-color: {colors['surface']};
    }}

    /* =========================================== */
    /* 文本编辑器样式                               */
    /* =========================================== */

    QTextEdit, QPlainTextEdit {{
        background-color: {colors['surface']};
        color: {colors['text_primary']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['md']};
        padding: {SPACING['md']};
        font-family: "SF Mono", Monaco, "Cascadia Code", Consolas, monospace;
        font-size: {FONT_SIZE['md']};
    }}

    QTextEdit:focus, QPlainTextEdit:focus {{
        border-color: {colors['border_focus']};
    }}

    /* =========================================== */
    /* 滚动条样式                                   */
    /* =========================================== */

    QScrollBar:vertical {{
        background-color: {colors['background']};
        width: 12px;
        margin: 0;
    }}

    QScrollBar::handle:vertical {{
        background-color: {colors['border']};
        border-radius: 6px;
        min-height: 30px;
        margin: 2px;
    }}

    QScrollBar::handle:vertical:hover {{
        background-color: {colors['border_hover']};
    }}

    QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
        height: 0;
    }}

    QScrollBar:horizontal {{
        background-color: {colors['background']};
        height: 12px;
        margin: 0;
    }}

    QScrollBar::handle:horizontal {{
        background-color: {colors['border']};
        border-radius: 6px;
        min-width: 30px;
        margin: 2px;
    }}

    /* =========================================== */
    /* 标签样式                                     */
    /* =========================================== */

    QLabel {{
        color: {colors['text_primary']};
        font-size: {FONT_SIZE['md']};
    }}

    QLabel#headerLabel {{
        font-size: {FONT_SIZE['xl']};
        font-weight: 600;
        color: {colors['text_primary']};
    }}

    QLabel#subHeaderLabel {{
        font-size: {FONT_SIZE['lg']};
        font-weight: 500;
        color: {colors['text_secondary']};
    }}

    QLabel#descriptionLabel {{
        font-size: {FONT_SIZE['sm']};
        color: {colors['text_secondary']};
    }}

    /* =========================================== */
    /* 分割线样式                                   */
    /* =========================================== */

    QSplitter::handle {{
        background-color: {colors['border']};
    }}

    QSplitter::handle:horizontal {{
        width: 2px;
    }}

    QSplitter::handle:vertical {{
        height: 2px;
    }}

    QSplitter::handle:hover {{
        background-color: {colors['primary']};
    }}

    /* =========================================== */
    /* 工具栏样式                                   */
    /* =========================================== */

    QToolBar {{
        background-color: {colors['surface']};
        border: none;
        border-bottom: 1px solid {colors['border']};
        padding: {SPACING['sm']};
        spacing: {SPACING['sm']};
    }}

    QToolButton {{
        background-color: transparent;
        border: none;
        border-radius: {RADIUS['md']};
        padding: {SPACING['sm']};
    }}

    QToolButton:hover {{
        background-color: {colors['surface_hover']};
    }}

    QToolButton:pressed {{
        background-color: {colors['primary_light']};
    }}

    /* =========================================== */
    /* 状态栏样式                                   */
    /* =========================================== */

    QStatusBar {{
        background-color: {colors['surface']};
        border-top: 1px solid {colors['border']};
        padding: {SPACING['xs']} {SPACING['md']};
    }}

    /* =========================================== */
    /* 菜单样式                                     */
    /* =========================================== */

    QMenuBar {{
        background-color: {colors['surface']};
        border-bottom: 1px solid {colors['border']};
        padding: {SPACING['xs']};
    }}

    QMenuBar::item {{
        background-color: transparent;
        padding: {SPACING['sm']} {SPACING['md']};
        border-radius: {RADIUS['sm']};
    }}

    QMenuBar::item:selected {{
        background-color: {colors['surface_hover']};
    }}

    QMenu {{
        background-color: {colors['surface']};
        border: 1px solid {colors['border']};
        border-radius: {RADIUS['md']};
        padding: {SPACING['xs']};
    }}

    QMenu::item {{
        padding: {SPACING['sm']} {SPACING['xl']};
        border-radius: {RADIUS['sm']};
    }}

    QMenu::item:selected {{
        background-color: {colors['primary_light']};
    }}

    /* =========================================== */
    /* 进度条样式                                   */
    /* =========================================== */

    QProgressBar {{
        background-color: {colors['surface_hover']};
        border: none;
        border-radius: {RADIUS['full']};
        height: 8px;
        text-align: center;
    }}

    QProgressBar::chunk {{
        background-color: {colors['primary']};
        border-radius: {RADIUS['full']};
    }}

    /* =========================================== */
    /* 向导步骤指示器样式                           */
    /* =========================================== */

    QFrame#stepIndicator {{
        background-color: {colors['surface']};
        border: 1.5px solid {colors['border']};
        border-radius: {RADIUS['lg']};
        padding: {SPACING['lg']};
    }}

    QFrame#stepComplete {{
        background-color: {colors['success_light']};
        border-color: {colors['success']};
    }}

    QFrame#stepCurrent {{
        background-color: {colors['primary_light']};
        border-color: {colors['primary']};
    }}
    """


def apply_modern_style(widget, theme: str = 'light'):
    """
    应用现代化样式到指定部件

    Args:
        widget: 要应用样式的QWidget
        theme: 主题名称
    """
    stylesheet = get_stylesheet(theme)
    widget.setStyleSheet(stylesheet)
