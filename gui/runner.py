# -*- coding: utf-8 -*-
"""
SVM运行器 - 执行SVM计算程序

使用QProcess在后台运行fbs可执行文件，
实时捕获输出并发送到GUI显示
"""

from PySide6.QtCore import QObject, QProcess, Signal
from PySide6.QtWidgets import QApplication


class SVMRunner(QObject):
    """
    SVM计算运行器

    在后台执行fbs程序，实时传递输出

    信号:
        started: 计算开始时发出
        output_received: 收到标准输出时发出
        error_received: 收到错误输出时发出
        finished: 计算完成时发出 (带退出码)
    """

    started = Signal()
    output_received = Signal(str)
    error_received = Signal(str)
    finished = Signal(int)

    def __init__(self, parent=None):
        super().__init__(parent)

        self.process = None

    def run(self, executable: str, working_dir: str):
        """
        运行SVM计算

        Args:
            executable: fbs可执行文件路径
            working_dir: 工作目录
        """
        if self.process is not None and self.process.state() != QProcess.ProcessState.NotRunning:
            return

        # 创建进程
        self.process = QProcess(self)
        self.process.setWorkingDirectory(working_dir)

        # 连接信号
        self.process.readyReadStandardOutput.connect(self._on_stdout)
        self.process.readyReadStandardError.connect(self._on_stderr)
        self.process.finished.connect(self._on_finished)
        self.process.started.connect(self._on_started)

        # 设置环境变量 (优化OpenMP和OpenBLAS线程)
        env = self.process.processEnvironment()
        env.insert("OMP_NUM_THREADS", "2")
        env.insert("OPENBLAS_NUM_THREADS", "1")
        self.process.setProcessEnvironment(env)

        # 启动进程
        self.process.start(executable, [])

    def stop(self):
        """停止计算"""
        if self.process and self.process.state() != QProcess.ProcessState.NotRunning:
            self.process.terminate()
            # 等待进程结束，如果3秒后还没结束则强制终止
            if not self.process.waitForFinished(3000):
                self.process.kill()

    def is_running(self) -> bool:
        """检查是否正在运行"""
        return self.process is not None and self.process.state() != QProcess.ProcessState.NotRunning

    def _on_started(self):
        """进程启动"""
        self.started.emit()

    def _on_stdout(self):
        """处理标准输出"""
        if self.process:
            data = self.process.readAllStandardOutput()
            text = bytes(data).decode('utf-8', errors='replace')
            # 按行分割并发送
            for line in text.splitlines():
                if line.strip():
                    self.output_received.emit(line)
            # 处理事件以保持UI响应
            QApplication.processEvents()

    def _on_stderr(self):
        """处理错误输出"""
        if self.process:
            data = self.process.readAllStandardError()
            text = bytes(data).decode('utf-8', errors='replace')
            for line in text.splitlines():
                if line.strip():
                    self.error_received.emit(line)
            QApplication.processEvents()

    def _on_finished(self, exit_code: int, exit_status):
        """进程结束"""
        self.finished.emit(exit_code)
