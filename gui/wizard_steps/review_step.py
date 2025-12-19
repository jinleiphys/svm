# -*- coding: utf-8 -*-
"""
预览步骤 - 显示生成的输入文件预览

在提交计算前，让用户检查生成的输入文件内容
"""

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGroupBox,
    QTextEdit, QLabel, QPushButton, QTabWidget
)
from PySide6.QtCore import Signal
from PySide6.QtGui import QFont


class ReviewStep(QGroupBox):
    """
    预览步骤

    显示向导生成的输入文件内容
    """

    def __init__(self, parent=None):
        super().__init__("步骤 4: 预览输入文件", parent)

        self._setup_ui()

    def _setup_ui(self):
        """设置UI布局"""
        layout = QVBoxLayout(self)
        layout.setSpacing(12)

        # 说明文字
        desc = QLabel(
            "预览将要生成的输入文件。点击\"生成输入文件\"按钮后，"
            "这些内容将写入工作目录。"
        )
        desc.setObjectName("descriptionLabel")
        desc.setWordWrap(True)
        layout.addWidget(desc)

        # 标签页显示两个文件
        self.preview_tabs = QTabWidget()

        # fbs.inp 预览
        self.fbs_preview = QTextEdit()
        self.fbs_preview.setReadOnly(True)
        self.fbs_preview.setFont(QFont("SF Mono", 11))
        self.fbs_preview.setPlaceholderText("fbs.inp 预览将在此显示...")
        self.preview_tabs.addTab(self.fbs_preview, "fbs.inp")

        # pot.inp 预览
        self.pot_preview = QTextEdit()
        self.pot_preview.setReadOnly(True)
        self.pot_preview.setFont(QFont("SF Mono", 11))
        self.pot_preview.setPlaceholderText("pot.inp 预览将在此显示...")
        self.preview_tabs.addTab(self.pot_preview, "pot.inp")

        layout.addWidget(self.preview_tabs)

        # 文件格式说明
        format_label = QLabel(
            "<b>文件格式说明:</b><br>"
            "<code>fbs.inp</code>: 主输入文件，包含粒子配置和计算参数<br>"
            "<code>pot.inp</code>: 势能输入文件，定义相互作用势"
        )
        format_label.setObjectName("descriptionLabel")
        layout.addWidget(format_label)

    def set_preview(self, fbs_content: str, pot_content: str):
        """
        设置预览内容

        Args:
            fbs_content: fbs.inp 文件内容
            pot_content: pot.inp 文件内容
        """
        # 添加注释说明
        fbs_annotated = self._annotate_fbs(fbs_content)
        pot_annotated = self._annotate_pot(pot_content)

        self.fbs_preview.setText(fbs_annotated)
        self.pot_preview.setText(pot_annotated)

    def _annotate_fbs(self, content: str) -> str:
        """为fbs.inp添加行注释"""
        lines = content.strip().split('\n')
        annotations = [
            "# 粒子数",
            "# 各粒子质量 (amu)",
            "# 各粒子电荷 (e)",
            "# 统计类型 (1=费米子, 2=玻色子)",
            "# 自旋配置",
            "# 同位旋标记",
            "# 同位旋配置",
            "# 势能参数",
            "# ico, 初始基函数数, 目标基函数数",
            "# 高斯宽度范围 (最小, 最大)",
        ]

        result = []
        for i, line in enumerate(lines):
            if i < len(annotations):
                result.append(f"{line:40s} {annotations[i]}")
            else:
                result.append(line)

        return '\n'.join(result)

    def _annotate_pot(self, content: str) -> str:
        """为pot.inp添加行注释"""
        lines = content.strip().split('\n')
        annotations = [
            "# 势能类型 (1=中心力, 2=张量, 3=自旋-轨道)",
            "# 势能选项参数",
        ]

        result = []
        for i, line in enumerate(lines):
            if i < len(annotations):
                result.append(f"{line:40s} {annotations[i]}")
            else:
                result.append(line)

        return '\n'.join(result)
