#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
SVM GUI - 少体系统随机变分法计算图形界面
主入口文件

基于PySide6开发，提供友好的用户界面用于配置和运行SVM计算
"""

import sys
import os

from PySide6.QtWidgets import QApplication, QSplashScreen
from PySide6.QtGui import QPixmap, QFont
from PySide6.QtCore import Qt, QTimer

from main_window import MainWindow
from styles import apply_modern_style


def main():
    """主函数 - 启动GUI应用"""
    # 创建应用实例
    app = QApplication(sys.argv)
    app.setApplicationName("SVM Calculator")
    app.setApplicationVersion("1.0.0")
    app.setOrganizationName("Few-Body Physics")

    # 设置默认字体
    font = QFont()
    font.setFamily("-apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica, Arial")
    font.setPointSize(13)
    app.setFont(font)

    # 创建主窗口
    window = MainWindow()

    # 应用现代化样式
    apply_modern_style(window, theme='light')

    # 显示窗口
    window.show()

    # 运行应用
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
