# -*- coding: utf-8 -*-
"""
SVM参数配置步骤 - 配置随机变分法的计算参数

包括:
- 计算模式选择
- 基函数数量
- 优化参数
"""

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGroupBox, QFormLayout,
    QSpinBox, QDoubleSpinBox, QComboBox, QLabel, QGridLayout,
    QFrame, QSlider
)
from PySide6.QtCore import Signal, Qt


class SVMStep(QGroupBox):
    """
    SVM参数配置步骤

    信号:
        data_changed: 数据变化时发出
    """

    data_changed = Signal()

    # 计算模式
    CALCULATION_MODES = {
        '固定基优化 (推荐新手)': 2,
        '自由优化': 1,
        '快速验证': 3,
    }

    def __init__(self, parent=None):
        super().__init__("步骤 3: SVM计算参数", parent)

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
            "配置随机变分法(SVM)的计算参数。这些参数控制基函数的生成和优化过程。"
        )
        desc.setObjectName("descriptionLabel")
        desc.setWordWrap(True)
        layout.addWidget(desc)

        # === 计算模式区域 ===
        mode_group = QGroupBox("计算模式")
        mode_layout = QVBoxLayout(mode_group)
        mode_layout.setContentsMargins(12, 16, 12, 12)
        mode_layout.setSpacing(12)

        mode_row = QHBoxLayout()
        mode_label = QLabel("模式:")
        mode_label.setFixedWidth(80)
        self.mode_combo = QComboBox()
        self.mode_combo.addItems(self.CALCULATION_MODES.keys())
        self.mode_combo.setToolTip(
            "选择计算模式:\n\n"
            "• 固定基优化 (ico=2): 逐步添加基函数并优化\n"
            "  推荐新手使用，稳定可靠\n\n"
            "• 自由优化 (ico=1): 同时优化所有参数\n"
            "  更灵活但可能不稳定\n\n"
            "• 快速验证 (ico=3): 快速检查输入是否正确"
        )
        mode_row.addWidget(mode_label)
        mode_row.addWidget(self.mode_combo, 1)
        mode_layout.addLayout(mode_row)

        mode_desc = QLabel(
            "📝 推荐使用\"固定基优化\"模式，逐步构建变分基组"
        )
        mode_desc.setObjectName("descriptionLabel")
        mode_layout.addWidget(mode_desc)

        layout.addWidget(mode_group)

        # === 基函数参数区域 ===
        basis_group = QGroupBox("基函数配置")
        basis_layout = QVBoxLayout(basis_group)
        basis_layout.setContentsMargins(12, 16, 12, 12)
        basis_layout.setSpacing(12)

        # 初始基函数数
        initial_row = QHBoxLayout()
        initial_label = QLabel("初始基数:")
        initial_label.setFixedWidth(80)
        self.initial_basis_spin = QSpinBox()
        self.initial_basis_spin.setRange(1, 100)
        self.initial_basis_spin.setValue(10)
        self.initial_basis_spin.setFixedWidth(100)
        self.initial_basis_spin.setToolTip("开始优化时的基函数数量")
        initial_row.addWidget(initial_label)
        initial_row.addWidget(self.initial_basis_spin)
        initial_row.addStretch()
        basis_layout.addLayout(initial_row)

        # 目标基函数数
        target_row = QHBoxLayout()
        target_label = QLabel("目标基数:")
        target_label.setFixedWidth(80)
        self.target_basis_spin = QSpinBox()
        self.target_basis_spin.setRange(1, 200)
        self.target_basis_spin.setValue(30)
        self.target_basis_spin.setFixedWidth(100)
        self.target_basis_spin.setToolTip(
            "最终基函数数量\n"
            "更多基函数 = 更高精度，但计算时间更长"
        )
        target_row.addWidget(target_label)
        target_row.addWidget(self.target_basis_spin)
        target_row.addStretch()
        basis_layout.addLayout(target_row)

        # 基函数数量滑块
        slider_row = QHBoxLayout()
        slider_label = QLabel("快速设置:")
        slider_label.setFixedWidth(80)
        self.basis_slider = QSlider(Qt.Orientation.Horizontal)
        self.basis_slider.setRange(10, 100)
        self.basis_slider.setValue(30)
        self.basis_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.basis_slider.setTickInterval(10)
        self.basis_label = QLabel("30")
        self.basis_label.setFixedWidth(30)
        slider_row.addWidget(slider_label)
        slider_row.addWidget(self.basis_slider, 1)
        slider_row.addWidget(self.basis_label)
        basis_layout.addLayout(slider_row)

        basis_desc = QLabel(
            "📝 建议: 小系统30-50，中等系统50-100，大系统100-200"
        )
        basis_desc.setObjectName("descriptionLabel")
        basis_layout.addWidget(basis_desc)

        layout.addWidget(basis_group)

        # === 优化参数区域 ===
        opt_group = QGroupBox("高斯宽度参数")
        opt_layout = QVBoxLayout(opt_group)
        opt_layout.setContentsMargins(12, 16, 12, 12)
        opt_layout.setSpacing(12)

        # 最小宽度
        min_row = QHBoxLayout()
        min_label = QLabel("最小宽度:")
        min_label.setFixedWidth(80)
        self.param1_spin = QDoubleSpinBox()
        self.param1_spin.setRange(0.001, 1.0)
        self.param1_spin.setValue(0.01)
        self.param1_spin.setDecimals(3)
        self.param1_spin.setSingleStep(0.001)
        self.param1_spin.setFixedWidth(100)
        self.param1_spin.setToolTip("高斯基函数的最小宽度参数 (fm⁻²)")
        min_row.addWidget(min_label)
        min_row.addWidget(self.param1_spin)
        min_row.addStretch()
        opt_layout.addLayout(min_row)

        # 最大宽度
        max_row = QHBoxLayout()
        max_label = QLabel("最大宽度:")
        max_label.setFixedWidth(80)
        self.param2_spin = QDoubleSpinBox()
        self.param2_spin.setRange(1.0, 100.0)
        self.param2_spin.setValue(15.0)
        self.param2_spin.setDecimals(1)
        self.param2_spin.setSingleStep(1.0)
        self.param2_spin.setFixedWidth(100)
        self.param2_spin.setToolTip("高斯基函数的最大宽度参数 (fm⁻²)")
        max_row.addWidget(max_label)
        max_row.addWidget(self.param2_spin)
        max_row.addStretch()
        opt_layout.addLayout(max_row)

        opt_desc = QLabel(
            "📝 宽度范围决定高斯基的空间扩展，影响收敛性"
        )
        opt_desc.setObjectName("descriptionLabel")
        opt_layout.addWidget(opt_desc)

        layout.addWidget(opt_group)

        # === 计算预估区域 ===
        info_group = QGroupBox("计算预估")
        info_layout = QVBoxLayout(info_group)
        info_layout.setContentsMargins(12, 16, 12, 12)

        self.estimate_label = QLabel()
        self.estimate_label.setWordWrap(True)
        self._update_estimate()
        info_layout.addWidget(self.estimate_label)

        layout.addWidget(info_group)

        # 添加弹性空间
        layout.addStretch()

    def _connect_signals(self):
        """连接信号"""
        self.mode_combo.currentIndexChanged.connect(self._emit_changed)
        self.initial_basis_spin.valueChanged.connect(self._on_basis_changed)
        self.target_basis_spin.valueChanged.connect(self._on_basis_changed)
        self.basis_slider.valueChanged.connect(self._on_slider_changed)
        self.param1_spin.valueChanged.connect(self._emit_changed)
        self.param2_spin.valueChanged.connect(self._emit_changed)

    def _on_slider_changed(self, value: int):
        """滑块变化"""
        self.target_basis_spin.setValue(value)
        self.basis_label.setText(str(value))

    def _on_basis_changed(self):
        """基函数数变化"""
        self._update_estimate()
        self._emit_changed()

    def _update_estimate(self):
        """更新计算预估"""
        n = self.target_basis_spin.value()
        # 粗略估计计算时间 (基于之前的性能测试)
        # 30个基函数约2秒
        time_estimate = (n / 30) ** 2 * 2

        if time_estimate < 1:
            time_str = "< 1 秒"
        elif time_estimate < 60:
            time_str = f"约 {time_estimate:.0f} 秒"
        else:
            time_str = f"约 {time_estimate/60:.1f} 分钟"

        self.estimate_label.setText(
            f"<b>基函数数:</b> {n}<br>"
            f"<b>矩阵维度:</b> {n} × {n}<br>"
            f"<b>预计时间:</b> {time_str}<br><br>"
            f"💡 建议先用较少基函数(20-30)验证，再逐步增加。"
        )

    def _emit_changed(self):
        """发出数据变化信号"""
        self.data_changed.emit()

    def get_data(self) -> dict:
        """获取配置数据"""
        mode_name = self.mode_combo.currentText()
        return {
            'ico': self.CALCULATION_MODES.get(mode_name, 2),
            'initial_basis': self.initial_basis_spin.value(),
            'target_basis': self.target_basis_spin.value(),
            'param1': self.param1_spin.value(),
            'param2': self.param2_spin.value(),
        }

    def reset(self):
        """重置为默认值"""
        self.mode_combo.setCurrentIndex(0)
        self.initial_basis_spin.setValue(10)
        self.target_basis_spin.setValue(30)
        self.basis_slider.setValue(30)
        self.param1_spin.setValue(0.01)
        self.param2_spin.setValue(15.0)
