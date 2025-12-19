# -*- coding: utf-8 -*-
"""
势能配置步骤 - 配置核子间相互作用势

包括:
- 势能类型选择
- 势能参数设置
- 势能选项
"""

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGroupBox, QFormLayout,
    QSpinBox, QDoubleSpinBox, QComboBox, QLabel, QGridLayout,
    QFrame, QLineEdit, QTextEdit
)
from PySide6.QtCore import Signal


class PotentialStep(QGroupBox):
    """
    势能配置步骤

    信号:
        data_changed: 数据变化时发出
    """

    data_changed = Signal()

    # 势能类型
    POTENTIAL_TYPES = {
        '中心力势 (Central)': 1,
        '张量势 (Tensor)': 2,
        '自旋-轨道势 (Spin-Orbit)': 3,
    }

    # 预设势能参数
    POTENTIAL_PRESETS = {
        '自定义': None,
        'Minnesota势 (标准)': {
            'type': 1,
            'options': '0,4',
            'params': '41.47,-16981,2,1',
        },
        'Minnesota势 (弱相互作用)': {
            'type': 1,
            'options': '0,4',
            'params': '30.0,-12000,2,1',
        },
        'Volkov势': {
            'type': 1,
            'options': '0,3',
            'params': '60.65,-61.14,1.8,1.0',
        },
    }

    def __init__(self, parent=None):
        super().__init__("步骤 2: 相互作用势配置", parent)

        self._setup_ui()
        self._connect_signals()

    def _setup_ui(self):
        """设置UI布局"""
        # 主布局
        layout = QVBoxLayout(self)
        layout.setSpacing(12)
        layout.setContentsMargins(16, 20, 16, 16)

        # === 说明文字 ===
        desc = QLabel(
            "配置核子间的相互作用势。势能决定了系统的动力学行为和基态能量。"
        )
        desc.setObjectName("descriptionLabel")
        desc.setWordWrap(True)
        layout.addWidget(desc)

        # === 预设选择区域 ===
        preset_group = QGroupBox("快速选择")
        preset_layout = QVBoxLayout(preset_group)
        preset_layout.setContentsMargins(12, 16, 12, 12)

        # 预设下拉框
        preset_row = QHBoxLayout()
        preset_label = QLabel("势能预设:")
        preset_label.setFixedWidth(80)
        self.preset_combo = QComboBox()
        self.preset_combo.addItems(self.POTENTIAL_PRESETS.keys())
        self.preset_combo.setToolTip(
            "选择预设的势能参数:\n"
            "• Minnesota势: 核物理中常用的核子-核子势\n"
            "• Volkov势: 简单的高斯型势能"
        )
        preset_row.addWidget(preset_label)
        preset_row.addWidget(self.preset_combo, 1)
        preset_layout.addLayout(preset_row)

        # 预设说明
        preset_desc = QLabel(
            "💡 提示: 选择预设可快速填充常用势能参数，也可选择\"自定义\"手动输入。"
        )
        preset_desc.setObjectName("descriptionLabel")
        preset_desc.setWordWrap(True)
        preset_layout.addWidget(preset_desc)

        layout.addWidget(preset_group)

        # === 势能类型区域 ===
        type_group = QGroupBox("势能类型")
        type_layout = QVBoxLayout(type_group)
        type_layout.setContentsMargins(12, 16, 12, 12)

        type_row = QHBoxLayout()
        type_label = QLabel("类型:")
        type_label.setFixedWidth(80)
        self.type_combo = QComboBox()
        self.type_combo.addItems(self.POTENTIAL_TYPES.keys())
        self.type_combo.setToolTip(
            "选择核子间相互作用势的类型:\n"
            "• 中心力势: 只依赖于粒子间距离 V(r)\n"
            "• 张量势: 包含自旋-自旋耦合项\n"
            "• 自旋-轨道势: 包含自旋-轨道耦合项"
        )
        type_row.addWidget(type_label)
        type_row.addWidget(self.type_combo, 1)
        type_layout.addLayout(type_row)

        layout.addWidget(type_group)

        # === 势能参数区域 ===
        params_group = QGroupBox("势能参数")
        params_layout = QVBoxLayout(params_group)
        params_layout.setContentsMargins(12, 16, 12, 12)
        params_layout.setSpacing(16)

        # 势能选项
        options_row = QHBoxLayout()
        options_label = QLabel("势能选项:")
        options_label.setFixedWidth(80)
        self.options_edit = QLineEdit()
        self.options_edit.setText("0,4")
        self.options_edit.setPlaceholderText("例: 0,4")
        self.options_edit.setToolTip(
            "势能选项参数 (逗号分隔)\n\n"
            "格式: ipcon, no\n"
            "• ipcon: 势能表示方式 (0=解析, 1=插值)\n"
            "• no: 交换算符数目 (1-4)\n\n"
            "例: 0,4 表示使用解析形式，包含4种交换算符"
        )
        options_row.addWidget(options_label)
        options_row.addWidget(self.options_edit, 1)
        params_layout.addLayout(options_row)

        # 选项说明
        options_desc = QLabel(
            "📝 ipcon=0 解析形式, ipcon=1 数值插值; no=交换算符数(1=W, 2=W+M, 3=W+M+B, 4=W+M+B+H)"
        )
        options_desc.setObjectName("descriptionLabel")
        options_desc.setWordWrap(True)
        params_layout.addWidget(options_desc)

        # 分隔线
        line1 = QFrame()
        line1.setFrameShape(QFrame.Shape.HLine)
        line1.setFrameShadow(QFrame.Shadow.Sunken)
        params_layout.addWidget(line1)

        # 势能参数
        params_row = QHBoxLayout()
        params_label = QLabel("势能强度:")
        params_label.setFixedWidth(80)
        self.params_edit = QLineEdit()
        self.params_edit.setText("41.47,-16981,2,1")
        self.params_edit.setPlaceholderText("例: V0,V1,μ0,μ1")
        self.params_edit.setToolTip(
            "势能强度和范围参数 (逗号分隔)\n\n"
            "Minnesota势格式: V_R, V_S, κ_R, κ_S\n"
            "• V_R: 排斥势强度 (MeV·fm³)\n"
            "• V_S: 吸引势强度 (MeV·fm³)\n"
            "• κ_R: 排斥势范围参数 (fm⁻²)\n"
            "• κ_S: 吸引势范围参数 (fm⁻²)\n\n"
            "例: 41.47,-16981,2,1"
        )
        params_row.addWidget(params_label)
        params_row.addWidget(self.params_edit, 1)
        params_layout.addLayout(params_row)

        # 参数说明
        params_desc = QLabel(
            "📝 Minnesota势: V_R=41.47, V_S=-16981 (MeV·fm³), κ_R=2, κ_S=1 (fm⁻²)"
        )
        params_desc.setObjectName("descriptionLabel")
        params_desc.setWordWrap(True)
        params_layout.addWidget(params_desc)

        layout.addWidget(params_group)

        # === 参数说明区域 ===
        info_group = QGroupBox("势能公式参考")
        info_layout = QVBoxLayout(info_group)
        info_layout.setContentsMargins(12, 16, 12, 12)

        info_text = QLabel()
        info_text.setWordWrap(True)
        info_text.setText(
            "<b>Minnesota势 (核物理常用):</b><br>"
            "V(r) = V_R·exp(-κ_R·r²) + V_S·exp(-κ_S·r²)<br><br>"
            "<b>交换算符说明:</b><br>"
            "• <b>W</b> (Wigner): 中心力，单位算符<br>"
            "• <b>M</b> (Majorana): 空间交换 P_r<br>"
            "• <b>B</b> (Bartlett): 自旋交换 P_σ<br>"
            "• <b>H</b> (Heisenberg): 自旋-同位旋交换 P_σP_τ"
        )
        info_layout.addWidget(info_text)

        layout.addWidget(info_group)

        # 添加弹性空间
        layout.addStretch()

    def _connect_signals(self):
        """连接信号"""
        self.preset_combo.currentTextChanged.connect(self._on_preset_changed)
        self.type_combo.currentIndexChanged.connect(self._emit_changed)
        self.options_edit.textChanged.connect(self._emit_changed)
        self.params_edit.textChanged.connect(self._emit_changed)

    def _on_preset_changed(self, preset_name: str):
        """预设变化"""
        preset = self.POTENTIAL_PRESETS.get(preset_name)
        if preset:
            # 查找对应的类型索引
            for name, value in self.POTENTIAL_TYPES.items():
                if value == preset['type']:
                    self.type_combo.setCurrentText(name)
                    break
            self.options_edit.setText(preset['options'])
            self.params_edit.setText(preset['params'])

        self._emit_changed()

    def _emit_changed(self):
        """发出数据变化信号"""
        self.data_changed.emit()

    def get_data(self) -> dict:
        """获取配置数据"""
        type_name = self.type_combo.currentText()
        return {
            'potential_type': self.POTENTIAL_TYPES.get(type_name, 1),
            'potential_options': self.options_edit.text(),
            'potential_params': self.params_edit.text(),
        }

    def reset(self):
        """重置为默认值"""
        self.preset_combo.setCurrentIndex(0)
        self.type_combo.setCurrentIndex(0)
        self.options_edit.setText("0,4")
        self.params_edit.setText("41.47,-16981,2,1")
