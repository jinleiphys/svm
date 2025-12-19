# -*- coding: utf-8 -*-
"""
输入面板 - 包含向导和文本编辑器两种模式

提供两种输入方式:
1. 向导模式: 分步骤引导用户配置参数
2. 编辑器模式: 直接编辑输入文件文本
"""

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTabWidget,
    QTextEdit, QLabel, QFrame, QPushButton, QScrollArea,
    QGroupBox, QFormLayout, QSpinBox, QDoubleSpinBox,
    QComboBox, QLineEdit, QGridLayout, QSizePolicy
)
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont

from wizard_steps.particle_step import ParticleStep
from wizard_steps.potential_step import PotentialStep
from wizard_steps.svm_step import SVMStep
from wizard_steps.review_step import ReviewStep


class InputPanel(QWidget):
    """
    输入面板 - 包含向导和文本编辑器

    信号:
        input_changed: 输入内容发生变化时发出
    """

    input_changed = Signal()

    def __init__(self, parent=None):
        super().__init__(parent)

        self._setup_ui()

    def _setup_ui(self):
        """设置UI"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # 创建标签页
        self.tabs = QTabWidget()

        # === 向导标签页 ===
        wizard_widget = self._create_wizard_tab()
        self.tabs.addTab(wizard_widget, "向导模式")

        # === 文本编辑器标签页 ===
        editor_widget = self._create_editor_tab()
        self.tabs.addTab(editor_widget, "文本编辑器")

        layout.addWidget(self.tabs)

    def _create_wizard_tab(self) -> QWidget:
        """创建向导标签页"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        layout.setSpacing(16)

        # 标题
        title = QLabel("SVM计算配置向导")
        title.setObjectName("headerLabel")
        layout.addWidget(title)

        subtitle = QLabel("按照步骤配置少体系统参数")
        subtitle.setObjectName("subHeaderLabel")
        layout.addWidget(subtitle)

        # 滚动区域
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)

        scroll_content = QWidget()
        scroll_layout = QVBoxLayout(scroll_content)
        scroll_layout.setSpacing(24)

        # === 步骤1: 粒子配置 ===
        self.particle_step = ParticleStep()
        self.particle_step.data_changed.connect(self._on_data_changed)
        scroll_layout.addWidget(self.particle_step)

        # === 步骤2: 势能配置 ===
        self.potential_step = PotentialStep()
        self.potential_step.data_changed.connect(self._on_data_changed)
        scroll_layout.addWidget(self.potential_step)

        # === 步骤3: SVM参数 ===
        self.svm_step = SVMStep()
        self.svm_step.data_changed.connect(self._on_data_changed)
        scroll_layout.addWidget(self.svm_step)

        # === 步骤4: 预览 ===
        self.review_step = ReviewStep()
        scroll_layout.addWidget(self.review_step)

        # 添加弹性空间
        scroll_layout.addStretch()

        scroll.setWidget(scroll_content)
        layout.addWidget(scroll)

        # 底部按钮区域
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        generate_btn = QPushButton("生成输入文件")
        generate_btn.setObjectName("primaryButton")
        generate_btn.clicked.connect(self._on_generate)
        button_layout.addWidget(generate_btn)

        layout.addLayout(button_layout)

        return widget

    def _create_editor_tab(self) -> QWidget:
        """创建文本编辑器标签页"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # 标题
        title = QLabel("输入文件编辑器")
        title.setObjectName("headerLabel")
        layout.addWidget(title)

        subtitle = QLabel("直接编辑fbs.inp和pot.inp文件内容")
        subtitle.setObjectName("subHeaderLabel")
        layout.addWidget(subtitle)

        # fbs.inp 编辑器
        fbs_group = QGroupBox("fbs.inp - 主输入文件")
        fbs_layout = QVBoxLayout(fbs_group)

        self.fbs_editor = QTextEdit()
        self.fbs_editor.setFont(QFont("SF Mono", 12))
        self.fbs_editor.setPlaceholderText("# fbs.inp 内容...")
        self.fbs_editor.textChanged.connect(self._on_text_changed)
        fbs_layout.addWidget(self.fbs_editor)

        layout.addWidget(fbs_group)

        # pot.inp 编辑器
        pot_group = QGroupBox("pot.inp - 势能输入文件")
        pot_layout = QVBoxLayout(pot_group)

        self.pot_editor = QTextEdit()
        self.pot_editor.setFont(QFont("SF Mono", 12))
        self.pot_editor.setPlaceholderText("# pot.inp 内容...")
        self.pot_editor.textChanged.connect(self._on_text_changed)
        pot_layout.addWidget(self.pot_editor)

        layout.addWidget(pot_group)

        return widget

    def _on_data_changed(self):
        """向导数据变化时更新预览"""
        self._update_preview()
        self.input_changed.emit()

    def _on_text_changed(self):
        """文本内容变化"""
        self.input_changed.emit()

    def _on_generate(self):
        """生成输入文件并更新编辑器"""
        input_files = self.generate_input_files()
        if input_files:
            self.fbs_editor.setText(input_files.get('fbs.inp', ''))
            self.pot_editor.setText(input_files.get('pot.inp', ''))
            # 切换到编辑器标签页
            self.tabs.setCurrentIndex(1)

    def _update_preview(self):
        """更新预览"""
        input_files = self.generate_input_files()
        if input_files:
            self.review_step.set_preview(
                input_files.get('fbs.inp', ''),
                input_files.get('pot.inp', '')
            )

    def generate_input_files(self) -> dict:
        """
        根据向导配置生成输入文件

        Returns:
            dict: {'fbs.inp': content, 'pot.inp': content}
        """
        # 获取粒子配置
        particle_data = self.particle_step.get_data()
        potential_data = self.potential_step.get_data()
        svm_data = self.svm_step.get_data()

        # 生成fbs.inp
        fbs_lines = []

        # 第1行: 粒子数
        fbs_lines.append(str(particle_data['num_particles']))

        # 第2行: 质量 (逗号分隔)
        masses = particle_data['masses'][:particle_data['num_particles']]
        fbs_lines.append(','.join(f"{m:.1f}" for m in masses))

        # 第3行: 电荷 (逗号分隔)
        charges = particle_data['charges'][:particle_data['num_particles']]
        fbs_lines.append(','.join(str(int(c)) for c in charges))

        # 第4行: 统计类型 (1=费米子, 2=玻色子)
        fbs_lines.append(str(particle_data['statistics']))

        # 第5行: 自旋配置
        spin_cfg = particle_data['spin_config']
        fbs_lines.append(spin_cfg)

        # 第6行: 同位旋配置标记
        fbs_lines.append("1")

        # 第7行: 同位旋配置
        isospin_cfg = particle_data['isospin_config']
        fbs_lines.append(isospin_cfg)

        # 第8行: 势能参数
        pot_params = potential_data['potential_params']
        fbs_lines.append(pot_params)

        # 第9行: ico, 初始基, 目标基
        fbs_lines.append(f"{svm_data['ico']},{svm_data['initial_basis']},{svm_data['target_basis']}")

        # 第10行: SVM参数
        fbs_lines.append(f"{svm_data['param1']},{svm_data['param2']}")

        fbs_content = '\n'.join(fbs_lines) + '\n'

        # 生成pot.inp
        pot_lines = []
        pot_lines.append(str(potential_data['potential_type']))
        pot_lines.append(potential_data['potential_options'])
        pot_content = '\n'.join(pot_lines) + '\n'

        return {
            'fbs.inp': fbs_content,
            'pot.inp': pot_content
        }

    def get_text(self) -> str:
        """获取fbs.inp内容"""
        return self.fbs_editor.toPlainText()

    def set_text(self, content: str):
        """设置fbs.inp内容"""
        self.fbs_editor.setText(content)

    def clear(self):
        """清空所有内容"""
        self.fbs_editor.clear()
        self.pot_editor.clear()
        self.particle_step.reset()
        self.potential_step.reset()
        self.svm_step.reset()
