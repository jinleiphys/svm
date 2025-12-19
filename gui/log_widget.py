# -*- coding: utf-8 -*-
"""
日志输出组件 - 显示程序运行输出

提供彩色日志显示，支持不同消息类型:
- 普通输出 (白色)
- 信息 (蓝色)
- 警告 (橙色)
- 错误 (红色)
- 成功 (绿色)
"""

from PySide6.QtWidgets import QTextEdit, QWidget, QVBoxLayout
from PySide6.QtGui import QFont, QTextCursor, QColor
from PySide6.QtCore import Qt


class LogWidget(QWidget):
    """
    日志输出组件

    显示计算过程的实时输出，支持彩色消息
    """

    # 消息类型对应的颜色
    COLORS = {
        'output': '#1e293b',   # 默认输出 - 深灰
        'info': '#3b82f6',     # 信息 - 蓝色
        'warning': '#f59e0b',  # 警告 - 橙色
        'error': '#ef4444',    # 错误 - 红色
        'success': '#10b981',  # 成功 - 绿色
    }

    def __init__(self, parent=None):
        super().__init__(parent)

        self._setup_ui()

    def _setup_ui(self):
        """设置UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # 文本显示区域
        self.text_edit = QTextEdit()
        self.text_edit.setReadOnly(True)

        # 设置等宽字体
        font = QFont()
        font.setFamily("SF Mono, Monaco, Cascadia Code, Consolas, monospace")
        font.setPointSize(12)
        self.text_edit.setFont(font)

        # 样式
        self.text_edit.setStyleSheet("""
            QTextEdit {
                background-color: #f8fafc;
                color: #1e293b;
                border: 1px solid #e2e8f0;
                border-radius: 6px;
                padding: 8px;
            }
        """)

        layout.addWidget(self.text_edit)

    def _append_colored(self, text: str, color: str):
        """
        追加彩色文本

        Args:
            text: 要追加的文本
            color: 颜色代码
        """
        cursor = self.text_edit.textCursor()
        cursor.movePosition(QTextCursor.MoveOperation.End)

        # 设置颜色
        format = cursor.charFormat()
        format.setForeground(QColor(color))
        cursor.setCharFormat(format)

        # 插入文本
        cursor.insertText(text + '\n')

        # 滚动到底部
        self.text_edit.setTextCursor(cursor)
        self.text_edit.ensureCursorVisible()

    def log_output(self, text: str):
        """记录普通输出"""
        self._append_colored(text, self.COLORS['output'])

    def log_info(self, text: str):
        """记录信息消息"""
        self._append_colored(f"[INFO] {text}", self.COLORS['info'])

    def log_warning(self, text: str):
        """记录警告消息"""
        self._append_colored(f"[WARNING] {text}", self.COLORS['warning'])

    def log_error(self, text: str):
        """记录错误消息"""
        self._append_colored(f"[ERROR] {text}", self.COLORS['error'])

    def log_success(self, text: str):
        """记录成功消息"""
        self._append_colored(f"[SUCCESS] {text}", self.COLORS['success'])

    def clear(self):
        """清空日志"""
        self.text_edit.clear()

    def get_text(self) -> str:
        """获取所有文本"""
        return self.text_edit.toPlainText()
