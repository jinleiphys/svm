#!/bin/bash
#===============================================================================
# setup_gui.sh - SVM GUI 安装脚本
#
# 功能:
#   1. 检测/创建 Conda 环境
#   2. 安装依赖包 (PySide6, numpy)
#   3. 生成 run_gui.sh 启动脚本
#
# 用法:
#   ./setup_gui.sh              # 创建名为 svm_gui 的 conda 环境
#   ./setup_gui.sh myenv        # 使用指定的环境名
#
#===============================================================================

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GUI_DIR="${SCRIPT_DIR}/gui"

# 环境名称 (可通过参数指定)
ENV_NAME="${1:-svm_gui}"

echo -e "${BLUE}"
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║                                                              ║"
echo "║   SVM GUI 安装程序                                           ║"
echo "║   少体系统随机变分法计算图形界面                               ║"
echo "║                                                              ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

#-------------------------------------------------------------------------------
# 检查 Conda
#-------------------------------------------------------------------------------
echo -e "${YELLOW}[1/4] 检查 Conda 环境...${NC}"

# 查找 conda
CONDA_CMD=""
if command -v conda &> /dev/null; then
    CONDA_CMD="conda"
    echo -e "  ${GREEN}✓ 找到 conda${NC}"
elif command -v mamba &> /dev/null; then
    CONDA_CMD="mamba"
    echo -e "  ${GREEN}✓ 找到 mamba (更快的conda替代)${NC}"
else
    echo -e "  ${RED}✗ 未找到 Conda${NC}"
    echo ""
    echo "请先安装 Conda:"
    echo "  推荐: https://docs.anaconda.com/miniconda/"
    echo ""
    echo "  macOS (Homebrew):"
    echo "    brew install miniconda"
    echo ""
    echo "  或下载安装脚本:"
    echo "    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-arm64.sh"
    echo "    bash Miniconda3-latest-MacOSX-arm64.sh"
    exit 1
fi

# 初始化 conda (确保 conda activate 可用)
eval "$(conda shell.bash hook)" 2>/dev/null || true

#-------------------------------------------------------------------------------
# 创建/更新 Conda 环境
#-------------------------------------------------------------------------------
echo -e "${YELLOW}[2/4] 配置 Conda 环境: ${ENV_NAME}...${NC}"

# 检查环境是否已存在
if conda env list | grep -q "^${ENV_NAME} "; then
    echo -e "  ${CYAN}环境 '${ENV_NAME}' 已存在，将更新依赖...${NC}"
    conda activate "${ENV_NAME}"
else
    echo -e "  创建新环境 '${ENV_NAME}'..."
    conda create -n "${ENV_NAME}" python=3.11 -y -q
    conda activate "${ENV_NAME}"
    echo -e "  ${GREEN}✓ 环境已创建${NC}"
fi

#-------------------------------------------------------------------------------
# 安装依赖
#-------------------------------------------------------------------------------
echo -e "${YELLOW}[3/4] 安装依赖包...${NC}"

# 创建/更新 requirements.txt
cat > "${GUI_DIR}/requirements.txt" << 'EOF'
PySide6>=6.5.0
numpy>=1.24.0
EOF

# 使用 pip 安装 (PySide6 在 conda-forge 中可能版本较旧)
pip install -r "${GUI_DIR}/requirements.txt" -q

echo -e "  ${GREEN}✓ 依赖安装完成${NC}"
echo -e "  已安装: PySide6, numpy"

#-------------------------------------------------------------------------------
# 生成启动脚本
#-------------------------------------------------------------------------------
echo -e "${YELLOW}[4/4] 生成启动脚本...${NC}"

RUN_SCRIPT="${SCRIPT_DIR}/run_gui.sh"

cat > "$RUN_SCRIPT" << EOF
#!/bin/bash
#===============================================================================
# run_gui.sh - SVM GUI 启动脚本 (自动生成)
#
# 生成时间: $(date)
# Conda 环境: ${ENV_NAME}
#===============================================================================

SCRIPT_DIR="\$(cd "\$(dirname "\${BASH_SOURCE[0]}")" && pwd)"

# 初始化 conda
eval "\$(conda shell.bash hook)" 2>/dev/null

# 激活环境
conda activate "${ENV_NAME}"

if [ \$? -ne 0 ]; then
    echo "错误: 无法激活 conda 环境 '${ENV_NAME}'"
    echo "请运行 ./setup_gui.sh 重新安装"
    exit 1
fi

# 运行 GUI
cd "\${SCRIPT_DIR}/gui"
python main.py "\$@"
EOF

chmod +x "$RUN_SCRIPT"
echo -e "  ${GREEN}✓ 启动脚本已生成: run_gui.sh${NC}"

#-------------------------------------------------------------------------------
# 完成
#-------------------------------------------------------------------------------
echo ""
echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║                    安装完成!                                  ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "Conda 环境: ${CYAN}${ENV_NAME}${NC}"
echo ""
echo "启动 GUI:"
echo -e "  ${BLUE}./run_gui.sh${NC}"
echo ""
echo "或手动运行:"
echo -e "  ${BLUE}conda activate ${ENV_NAME}${NC}"
echo -e "  ${BLUE}cd gui && python main.py${NC}"
echo ""

# 检查 fbs 是否已编译
if [ -f "${SCRIPT_DIR}/fbs" ]; then
    echo -e "${GREEN}✓ 已检测到 fbs 可执行文件${NC}"
else
    echo -e "${YELLOW}⚠ 未检测到 fbs 可执行文件${NC}"
    echo "  请先编译 SVM 程序:"
    echo "    ./setup.sh"
    echo "    make"
fi
echo ""
