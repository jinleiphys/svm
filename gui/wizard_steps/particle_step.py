# -*- coding: utf-8 -*-
"""
粒子配置步骤 - 配置少体系统中的粒子参数

包括:
- 粒子数量
- 各粒子的质量
- 各粒子的电荷
- 统计类型 (费米子/玻色子)
- 自旋和同位旋配置
"""

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGroupBox, QFormLayout,
    QSpinBox, QDoubleSpinBox, QComboBox, QLabel, QGridLayout,
    QFrame, QLineEdit, QScrollArea
)
from PySide6.QtCore import Signal


class ParticleStep(QGroupBox):
    """
    粒子配置步骤

    信号:
        data_changed: 数据变化时发出
    """

    data_changed = Signal()

    # 预设系统配置
    PRESETS = {
        '自定义': None,
        '3体系统 (三核子)': {
            'num_particles': 3,
            'masses': [1.0, 1.0, 1.0],
            'charges': [1, 0, 0],
            'statistics': 1,
            'spin_config': '1.,1,1,2,2,2,2',
            'isospin_config': '1.,1,2,1,2,1,2',
        },
        '4体系统 (α粒子)': {
            'num_particles': 4,
            'masses': [1.0, 1.0, 1.0, 1.0],
            'charges': [1, 1, 0, 0],
            'statistics': 1,
            'spin_config': '1.,1,1,1,2,2,2,2',
            'isospin_config': '1.,1,1,2,2,1,2,1,2',
        },
        '6体系统 (6Li)': {
            'num_particles': 6,
            'masses': [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
            'charges': [0, 0, 0, 0, 0, 0],
            'statistics': 1,
            'spin_config': '1.,1,1,2,2,2,2',
            'isospin_config': '1.,1,2,1,2,1,2',
        },
    }

    def __init__(self, parent=None):
        super().__init__("步骤 1: 粒子系统配置", parent)

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
            "配置少体系统中的粒子参数。可以选择预设配置，或手动设置每个粒子的属性。"
        )
        desc.setObjectName("descriptionLabel")
        desc.setWordWrap(True)
        layout.addWidget(desc)

        # === 预设选择区域 ===
        preset_group = QGroupBox("快速选择")
        preset_layout = QVBoxLayout(preset_group)
        preset_layout.setContentsMargins(12, 16, 12, 12)

        preset_row = QHBoxLayout()
        preset_label = QLabel("预设系统:")
        preset_label.setFixedWidth(80)
        self.preset_combo = QComboBox()
        self.preset_combo.addItems(self.PRESETS.keys())
        self.preset_combo.setToolTip(
            "选择预设的粒子系统:\n"
            "• 3体系统: 如氚核(³H)或氦-3(³He)\n"
            "• 4体系统: 如α粒子(⁴He)\n"
            "• 6体系统: 如锂-6(⁶Li)"
        )
        preset_row.addWidget(preset_label)
        preset_row.addWidget(self.preset_combo, 1)
        preset_layout.addLayout(preset_row)

        # 预设说明
        preset_desc = QLabel(
            "💡 提示: 选择预设可快速填充常用系统参数，也可选择\"自定义\"手动配置。"
        )
        preset_desc.setObjectName("descriptionLabel")
        preset_desc.setWordWrap(True)
        preset_layout.addWidget(preset_desc)

        layout.addWidget(preset_group)

        # === 基本参数区域 ===
        basic_group = QGroupBox("基本参数")
        basic_layout = QVBoxLayout(basic_group)
        basic_layout.setContentsMargins(12, 16, 12, 12)
        basic_layout.setSpacing(12)

        # 粒子数
        num_row = QHBoxLayout()
        num_label = QLabel("粒子数:")
        num_label.setFixedWidth(80)
        self.num_particles_spin = QSpinBox()
        self.num_particles_spin.setRange(2, 10)
        self.num_particles_spin.setValue(6)
        self.num_particles_spin.setFixedWidth(100)
        self.num_particles_spin.setToolTip("系统中的粒子总数 (2-10)")
        num_row.addWidget(num_label)
        num_row.addWidget(self.num_particles_spin)
        num_row.addStretch()
        basic_layout.addLayout(num_row)

        # 统计类型
        stat_row = QHBoxLayout()
        stat_label = QLabel("统计类型:")
        stat_label.setFixedWidth(80)
        self.statistics_combo = QComboBox()
        self.statistics_combo.addItems(["费米子 (Fermi-Dirac)", "玻色子 (Bose-Einstein)"])
        self.statistics_combo.setToolTip(
            "粒子遵循的量子统计规律:\n"
            "• 费米子: 遵循泡利不相容原理，如核子\n"
            "• 玻色子: 可以占据相同量子态，如π介子"
        )
        stat_row.addWidget(stat_label)
        stat_row.addWidget(self.statistics_combo, 1)
        basic_layout.addLayout(stat_row)

        # 说明
        stat_desc = QLabel(
            "📝 核子(质子、中子)是费米子，需要反对称化波函数"
        )
        stat_desc.setObjectName("descriptionLabel")
        basic_layout.addWidget(stat_desc)

        layout.addWidget(basic_group)

        # === 质量和电荷区域 ===
        mass_group = QGroupBox("粒子质量和电荷")
        mass_layout = QVBoxLayout(mass_group)
        mass_layout.setContentsMargins(12, 16, 12, 12)

        # 质量输入
        mass_row = QHBoxLayout()
        mass_label = QLabel("质量 (amu):")
        mass_label.setFixedWidth(80)
        mass_row.addWidget(mass_label)

        self.mass_spins = []
        for i in range(6):  # 默认显示6个
            spin = QDoubleSpinBox()
            spin.setRange(0.1, 300.0)
            spin.setValue(1.0)
            spin.setDecimals(2)
            spin.setSingleStep(0.1)
            spin.setFixedWidth(70)
            spin.setToolTip(f"粒子 {i+1} 的质量 (原子质量单位)\n1.0 = 核子质量")
            self.mass_spins.append(spin)
            mass_row.addWidget(spin)

        mass_row.addStretch()
        mass_layout.addLayout(mass_row)

        # 电荷输入
        charge_row = QHBoxLayout()
        charge_label = QLabel("电荷 (e):")
        charge_label.setFixedWidth(80)
        charge_row.addWidget(charge_label)

        self.charge_spins = []
        for i in range(6):
            spin = QSpinBox()
            spin.setRange(-10, 10)
            spin.setValue(0)
            spin.setFixedWidth(70)
            spin.setToolTip(f"粒子 {i+1} 的电荷\n质子=1, 中子=0")
            self.charge_spins.append(spin)
            charge_row.addWidget(spin)

        charge_row.addStretch()
        mass_layout.addLayout(charge_row)

        # 说明
        mass_desc = QLabel(
            "📝 核子质量≈1 amu; 质子电荷=+1, 中子电荷=0"
        )
        mass_desc.setObjectName("descriptionLabel")
        mass_layout.addWidget(mass_desc)

        layout.addWidget(mass_group)

        # === 自旋和同位旋配置区域 ===
        spin_group = QGroupBox("自旋和同位旋配置")
        spin_layout = QVBoxLayout(spin_group)
        spin_layout.setContentsMargins(12, 16, 12, 12)
        spin_layout.setSpacing(12)

        # 自旋配置
        spin_row = QHBoxLayout()
        spin_label = QLabel("自旋配置:")
        spin_label.setFixedWidth(80)
        self.spin_config_edit = QLineEdit()
        self.spin_config_edit.setText("1.,1,1,2,2,2,2")
        self.spin_config_edit.setPlaceholderText("例: 1.,1,1,2,2,2,2")
        self.spin_config_edit.setToolTip(
            "自旋配置字符串\n\n"
            "格式: 归一化因子, 自旋投影序列\n"
            "• 1 = 自旋向上 (↑)\n"
            "• 2 = 自旋向下 (↓)\n\n"
            "例: 1.,1,1,2,2,2,2\n"
            "表示前3个自旋向上，后3个自旋向下"
        )
        spin_row.addWidget(spin_label)
        spin_row.addWidget(self.spin_config_edit, 1)
        spin_layout.addLayout(spin_row)

        spin_desc = QLabel(
            "📝 格式: 系数,s₁,s₂,... (1=↑上, 2=↓下)"
        )
        spin_desc.setObjectName("descriptionLabel")
        spin_layout.addWidget(spin_desc)

        # 同位旋配置
        isospin_row = QHBoxLayout()
        isospin_label = QLabel("同位旋配置:")
        isospin_label.setFixedWidth(80)
        self.isospin_config_edit = QLineEdit()
        self.isospin_config_edit.setText("1.,1,2,1,2,1,2")
        self.isospin_config_edit.setPlaceholderText("例: 1.,1,2,1,2,1,2")
        self.isospin_config_edit.setToolTip(
            "同位旋配置字符串\n\n"
            "格式: 归一化因子, 同位旋投影序列\n"
            "• 1 = 质子 (p)\n"
            "• 2 = 中子 (n)\n\n"
            "例: 1.,1,2,1,2,1,2\n"
            "表示质子和中子交替排列"
        )
        isospin_row.addWidget(isospin_label)
        isospin_row.addWidget(self.isospin_config_edit, 1)
        spin_layout.addLayout(isospin_row)

        isospin_desc = QLabel(
            "📝 格式: 系数,t₁,t₂,... (1=质子p, 2=中子n)"
        )
        isospin_desc.setObjectName("descriptionLabel")
        spin_layout.addWidget(isospin_desc)

        layout.addWidget(spin_group)

        # 添加弹性空间
        layout.addStretch()

    def _connect_signals(self):
        """连接信号"""
        self.preset_combo.currentTextChanged.connect(self._on_preset_changed)
        self.num_particles_spin.valueChanged.connect(self._on_num_particles_changed)
        self.statistics_combo.currentIndexChanged.connect(self._emit_changed)

        for spin in self.mass_spins:
            spin.valueChanged.connect(self._emit_changed)
        for spin in self.charge_spins:
            spin.valueChanged.connect(self._emit_changed)

        self.spin_config_edit.textChanged.connect(self._emit_changed)
        self.isospin_config_edit.textChanged.connect(self._emit_changed)

    def _on_preset_changed(self, preset_name: str):
        """预设变化"""
        preset = self.PRESETS.get(preset_name)
        if preset:
            self.num_particles_spin.setValue(preset['num_particles'])
            for i, mass in enumerate(preset['masses']):
                if i < len(self.mass_spins):
                    self.mass_spins[i].setValue(mass)
            for i, charge in enumerate(preset['charges']):
                if i < len(self.charge_spins):
                    self.charge_spins[i].setValue(charge)
            self.statistics_combo.setCurrentIndex(preset['statistics'] - 1)
            self.spin_config_edit.setText(preset['spin_config'])
            self.isospin_config_edit.setText(preset['isospin_config'])

        self._emit_changed()

    def _on_num_particles_changed(self, value: int):
        """粒子数变化"""
        self._update_particle_visibility()
        self._emit_changed()

    def _update_particle_visibility(self):
        """更新粒子输入框的可见性"""
        n = self.num_particles_spin.value()
        for i in range(6):
            enabled = i < n
            self.mass_spins[i].setEnabled(enabled)
            self.charge_spins[i].setEnabled(enabled)

    def _emit_changed(self):
        """发出数据变化信号"""
        self.data_changed.emit()

    def get_data(self) -> dict:
        """获取配置数据"""
        n = self.num_particles_spin.value()
        return {
            'num_particles': n,
            'masses': [self.mass_spins[i].value() for i in range(min(n, 6))],
            'charges': [self.charge_spins[i].value() for i in range(min(n, 6))],
            'statistics': self.statistics_combo.currentIndex() + 1,
            'spin_config': self.spin_config_edit.text(),
            'isospin_config': self.isospin_config_edit.text(),
        }

    def reset(self):
        """重置为默认值"""
        self.preset_combo.setCurrentIndex(0)
        self.num_particles_spin.setValue(6)
        self.statistics_combo.setCurrentIndex(0)
        for spin in self.mass_spins:
            spin.setValue(1.0)
        for spin in self.charge_spins:
            spin.setValue(0)
        self.spin_config_edit.setText("1.,1,1,2,2,2,2")
        self.isospin_config_edit.setText("1.,1,2,1,2,1,2")
