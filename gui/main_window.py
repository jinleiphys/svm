# -*- coding: utf-8 -*-
"""
主窗口 - SVM计算GUI的核心窗口

包含:
- 左侧: 输入向导/文本编辑器
- 右侧: 输出日志/结果显示
- 工具栏: 运行、停止、保存等操作
"""

import os
from pathlib import Path

from PySide6.QtWidgets import (
    QMainWindow, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QTabWidget, QToolBar, QStatusBar, QFileDialog, QMessageBox,
    QPushButton, QLabel, QFrame, QMenuBar, QMenu
)
from PySide6.QtCore import Qt, QSize, Signal
from PySide6.QtGui import QAction, QKeySequence

from input_panel import InputPanel
from log_widget import LogWidget
from runner import SVMRunner


class MainWindow(QMainWindow):
    """
    SVM GUI 主窗口

    布局结构:
    ┌─────────────────────────────────────────────┐
    │  菜单栏                                      │
    ├─────────────────────────────────────────────┤
    │  工具栏 (新建/打开/保存 | 运行/停止)         │
    ├────────────────────┬────────────────────────┤
    │                    │                        │
    │   输入面板 (40%)   │    输出面板 (60%)      │
    │                    │                        │
    │  ┌──────────────┐  │   ┌──────────────────┐ │
    │  │ 向导 / 编辑器│  │   │ 日志输出         │ │
    │  └──────────────┘  │   └──────────────────┘ │
    │                    │                        │
    ├────────────────────┴────────────────────────┤
    │  状态栏                                      │
    └─────────────────────────────────────────────┘
    """

    def __init__(self, parent=None):
        super().__init__(parent)

        # 窗口基本设置
        self.setWindowTitle("SVM Calculator - 少体系统随机变分法")
        self.setMinimumSize(1200, 800)
        self.resize(1400, 900)

        # 当前工作目录
        self.working_dir = Path.cwd()
        self.current_file = None

        # 初始化运行器
        self.runner = SVMRunner()

        # 创建UI组件
        self._create_menu_bar()
        self._create_toolbar()
        self._create_central_widget()
        self._create_status_bar()

        # 连接信号
        self._connect_signals()

        # 初始状态
        self._update_status("就绪")

    def _create_menu_bar(self):
        """创建菜单栏"""
        menubar = self.menuBar()

        # === 文件菜单 ===
        file_menu = menubar.addMenu("文件(&F)")

        # 新建
        new_action = QAction("新建(&N)", self)
        new_action.setShortcut(QKeySequence.StandardKey.New)
        new_action.triggered.connect(self._on_new)
        file_menu.addAction(new_action)

        # 打开
        open_action = QAction("打开(&O)...", self)
        open_action.setShortcut(QKeySequence.StandardKey.Open)
        open_action.triggered.connect(self._on_open)
        file_menu.addAction(open_action)

        file_menu.addSeparator()

        # 保存
        save_action = QAction("保存(&S)", self)
        save_action.setShortcut(QKeySequence.StandardKey.Save)
        save_action.triggered.connect(self._on_save)
        file_menu.addAction(save_action)

        # 另存为
        save_as_action = QAction("另存为(&A)...", self)
        save_as_action.setShortcut(QKeySequence("Ctrl+Shift+S"))
        save_as_action.triggered.connect(self._on_save_as)
        file_menu.addAction(save_as_action)

        file_menu.addSeparator()

        # 设置工作目录
        set_dir_action = QAction("设置工作目录...", self)
        set_dir_action.triggered.connect(self._on_set_working_dir)
        file_menu.addAction(set_dir_action)

        file_menu.addSeparator()

        # 退出
        quit_action = QAction("退出(&Q)", self)
        quit_action.setShortcut(QKeySequence.StandardKey.Quit)
        quit_action.triggered.connect(self.close)
        file_menu.addAction(quit_action)

        # === 运行菜单 ===
        run_menu = menubar.addMenu("运行(&R)")

        self.run_action = QAction("运行计算(&R)", self)
        self.run_action.setShortcut(QKeySequence("Ctrl+R"))
        self.run_action.triggered.connect(self._on_run)
        run_menu.addAction(self.run_action)

        self.stop_action = QAction("停止(&S)", self)
        self.stop_action.setShortcut(QKeySequence("Ctrl+."))
        self.stop_action.triggered.connect(self._on_stop)
        self.stop_action.setEnabled(False)
        run_menu.addAction(self.stop_action)

        # === 帮助菜单 ===
        help_menu = menubar.addMenu("帮助(&H)")

        about_action = QAction("关于(&A)", self)
        about_action.triggered.connect(self._on_about)
        help_menu.addAction(about_action)

    def _create_toolbar(self):
        """创建工具栏"""
        toolbar = QToolBar("主工具栏")
        toolbar.setMovable(False)
        toolbar.setIconSize(QSize(24, 24))
        self.addToolBar(toolbar)

        # 新建按钮
        new_btn = QPushButton("新建")
        new_btn.clicked.connect(self._on_new)
        toolbar.addWidget(new_btn)

        # 打开按钮
        open_btn = QPushButton("打开")
        open_btn.clicked.connect(self._on_open)
        toolbar.addWidget(open_btn)

        # 保存按钮
        save_btn = QPushButton("保存")
        save_btn.clicked.connect(self._on_save)
        toolbar.addWidget(save_btn)

        toolbar.addSeparator()

        # 运行按钮
        self.run_btn = QPushButton("▶ 运行计算")
        self.run_btn.setObjectName("runButton")
        self.run_btn.setMinimumWidth(120)
        self.run_btn.clicked.connect(self._on_run)
        toolbar.addWidget(self.run_btn)

        # 停止按钮
        self.stop_btn = QPushButton("■ 停止")
        self.stop_btn.setObjectName("stopButton")
        self.stop_btn.setEnabled(False)
        self.stop_btn.clicked.connect(self._on_stop)
        toolbar.addWidget(self.stop_btn)

        # 添加弹性空间
        spacer = QWidget()
        spacer.setSizePolicy(spacer.sizePolicy().horizontalPolicy(),
                            spacer.sizePolicy().verticalPolicy())
        spacer.setMinimumWidth(20)
        toolbar.addWidget(spacer)

        # 工作目录显示
        self.dir_label = QLabel()
        self.dir_label.setObjectName("descriptionLabel")
        self._update_dir_label()
        toolbar.addWidget(self.dir_label)

    def _create_central_widget(self):
        """创建中央部件 - 分割面板"""
        # 主分割器
        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.setChildrenCollapsible(False)

        # === 左侧: 输入面板 ===
        self.input_panel = InputPanel()
        splitter.addWidget(self.input_panel)

        # === 右侧: 输出面板 ===
        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(0, 0, 0, 0)

        # 输出标签页
        self.output_tabs = QTabWidget()

        # 日志输出标签
        self.log_widget = LogWidget()
        self.output_tabs.addTab(self.log_widget, "输出日志")

        right_layout.addWidget(self.output_tabs)
        splitter.addWidget(right_panel)

        # 设置初始比例 (40% : 60%)
        splitter.setSizes([560, 840])

        self.setCentralWidget(splitter)

    def _create_status_bar(self):
        """创建状态栏"""
        self.status_bar = QStatusBar()
        self.setStatusBar(self.status_bar)

        # 状态标签
        self.status_label = QLabel("就绪")
        self.status_bar.addWidget(self.status_label)

        # 弹性空间
        self.status_bar.addPermanentWidget(QLabel(""))

        # 当前文件标签
        self.file_label = QLabel("未保存")
        self.status_bar.addPermanentWidget(self.file_label)

    def _connect_signals(self):
        """连接信号和槽"""
        # 运行器信号
        self.runner.started.connect(self._on_runner_started)
        self.runner.output_received.connect(self._on_runner_output)
        self.runner.error_received.connect(self._on_runner_error)
        self.runner.finished.connect(self._on_runner_finished)

        # 输入面板信号
        self.input_panel.input_changed.connect(self._on_input_changed)

    def _update_status(self, message: str):
        """更新状态栏消息"""
        self.status_label.setText(message)

    def _update_dir_label(self):
        """更新工作目录标签"""
        dir_str = str(self.working_dir)
        if len(dir_str) > 50:
            dir_str = "..." + dir_str[-47:]
        self.dir_label.setText(f"工作目录: {dir_str}")

    # =========================================================================
    # 菜单和工具栏操作
    # =========================================================================

    def _on_new(self):
        """新建文件"""
        if self._check_save():
            self.input_panel.clear()
            self.log_widget.clear()
            self.current_file = None
            self.file_label.setText("未保存")
            self._update_status("新建文件")

    def _on_open(self):
        """打开文件"""
        if not self._check_save():
            return

        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "打开输入文件",
            str(self.working_dir),
            "输入文件 (*.inp);;所有文件 (*)"
        )

        if file_path:
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                self.input_panel.set_text(content)
                self.current_file = Path(file_path)
                self.working_dir = self.current_file.parent
                self._update_dir_label()
                self.file_label.setText(self.current_file.name)
                self._update_status(f"已打开: {self.current_file.name}")
            except Exception as e:
                QMessageBox.critical(self, "错误", f"无法打开文件:\n{e}")

    def _on_save(self):
        """保存文件"""
        if self.current_file:
            self._save_to_file(self.current_file)
        else:
            self._on_save_as()

    def _on_save_as(self):
        """另存为"""
        file_path, _ = QFileDialog.getSaveFileName(
            self,
            "保存输入文件",
            str(self.working_dir / "fbs.inp"),
            "输入文件 (*.inp);;所有文件 (*)"
        )

        if file_path:
            self._save_to_file(Path(file_path))

    def _save_to_file(self, file_path: Path):
        """保存到指定文件"""
        try:
            content = self.input_panel.get_text()
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(content)
            self.current_file = file_path
            self.working_dir = file_path.parent
            self._update_dir_label()
            self.file_label.setText(file_path.name)
            self._update_status(f"已保存: {file_path.name}")
        except Exception as e:
            QMessageBox.critical(self, "错误", f"无法保存文件:\n{e}")

    def _on_set_working_dir(self):
        """设置工作目录"""
        dir_path = QFileDialog.getExistingDirectory(
            self,
            "选择工作目录",
            str(self.working_dir)
        )

        if dir_path:
            self.working_dir = Path(dir_path)
            self._update_dir_label()
            self._update_status(f"工作目录已更改")

    def _on_run(self):
        """运行计算"""
        # 检查是否有fbs可执行文件
        fbs_path = self._find_fbs_executable()
        if not fbs_path:
            QMessageBox.warning(
                self,
                "未找到可执行文件",
                "未找到fbs可执行文件。\n"
                "请确保已编译程序，或设置正确的工作目录。"
            )
            return

        # 保存输入文件
        input_files = self.input_panel.generate_input_files()
        if not input_files:
            QMessageBox.warning(self, "错误", "无法生成输入文件")
            return

        # 写入文件
        try:
            for filename, content in input_files.items():
                file_path = self.working_dir / filename
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                self.log_widget.log_info(f"已写入: {file_path}")
        except Exception as e:
            QMessageBox.critical(self, "错误", f"写入输入文件失败:\n{e}")
            return

        # 清空日志
        self.log_widget.clear()
        self.log_widget.log_info("开始SVM计算...")
        self.log_widget.log_info(f"工作目录: {self.working_dir}")
        self.log_widget.log_info(f"可执行文件: {fbs_path}")
        self.log_widget.log_info("-" * 50)

        # 运行计算
        self.runner.run(str(fbs_path), str(self.working_dir))

    def _on_stop(self):
        """停止计算"""
        self.runner.stop()
        self.log_widget.log_warning("用户中止计算")

    def _find_fbs_executable(self) -> Path | None:
        """查找fbs可执行文件"""
        # 检查可能的位置
        candidates = [
            self.working_dir / "fbs",
            self.working_dir.parent / "fbs",
            self.working_dir / ".." / "fbs",
            Path.cwd() / "fbs",
            Path.cwd().parent / "fbs",
        ]

        for path in candidates:
            path = path.resolve()
            if path.exists() and path.is_file():
                return path

        return None

    def _on_about(self):
        """显示关于对话框"""
        QMessageBox.about(
            self,
            "关于 SVM Calculator",
            "<h2>SVM Calculator</h2>"
            "<p>少体系统随机变分法计算程序</p>"
            "<p>版本: 1.0.0</p>"
            "<hr>"
            "<p><b>功能:</b></p>"
            "<ul>"
            "<li>配置少体系统参数</li>"
            "<li>设置粒子质量、电荷、自旋</li>"
            "<li>选择相互作用势</li>"
            "<li>运行SVM优化计算</li>"
            "</ul>"
            "<hr>"
            "<p><b>参考文献:</b></p>"
            "<p>K. Varga, Y. Suzuki, Phys. Rev. C52 (1995) 2885</p>"
        )

    # =========================================================================
    # 运行器事件处理
    # =========================================================================

    def _on_runner_started(self):
        """运行器启动"""
        self.run_btn.setEnabled(False)
        self.run_action.setEnabled(False)
        self.stop_btn.setEnabled(True)
        self.stop_action.setEnabled(True)
        self._update_status("正在计算...")

    def _on_runner_output(self, text: str):
        """接收运行器输出"""
        self.log_widget.log_output(text)

    def _on_runner_error(self, text: str):
        """接收运行器错误"""
        self.log_widget.log_error(text)

    def _on_runner_finished(self, exit_code: int):
        """运行器完成"""
        self.run_btn.setEnabled(True)
        self.run_action.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self.stop_action.setEnabled(False)

        if exit_code == 0:
            self.log_widget.log_success("计算完成!")
            self._update_status("计算完成")
        else:
            self.log_widget.log_error(f"计算失败 (退出码: {exit_code})")
            self._update_status("计算失败")

    def _on_input_changed(self):
        """输入内容变化"""
        if self.current_file:
            self.file_label.setText(f"{self.current_file.name} *")

    def _check_save(self) -> bool:
        """检查是否需要保存"""
        # 简化处理,直接返回True
        return True

    def closeEvent(self, event):
        """关闭事件"""
        # 停止运行中的计算
        if self.runner.is_running():
            self.runner.stop()
        event.accept()
