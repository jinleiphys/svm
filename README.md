# FBS: Few-Body System Solver

基于随机变分方法 (Stochastic Variational Method, SVM) 的少体量子系统求解器。

## 目录

- [简介](#简介)
- [物理背景](#物理背景)
- [图形界面 (GUI)](#图形界面-gui)
- [安装](#安装)
- [使用方法](#使用方法)
- [输入文件格式](#输入文件格式)
- [输出文件](#输出文件)
- [代码结构](#代码结构)
- [示例](#示例)
- [参考文献](#参考文献)

---

## 简介

本程序使用随机变分方法 (SVM) 求解少体量子力学问题。该方法基于关联高斯基函数展开，适用于具有中心力相互作用且轨道角动量 L=0 的系统。

### 主要特性

- **模块化设计**：代码分为12个独立模块，结构清晰
- **LAPACK 支持**：可选使用 LAPACK/OpenBLAS 加速矩阵运算
- **灵活的势能定义**：支持解析形式和数值插值两种方式
- **多种优化模式**：预设基、逐步优化、细化优化
- **费米子和玻色子**：支持全同粒子的对称化/反对称化

### 适用问题

- 原子核少体问题（氘核、氚核、α粒子等）
- 少体原子系统（氦原子、锂原子等）
- 奇异强子系统
- 超核物理
- 冷原子物理中的少体问题

---

## 物理背景

### 随机变分方法 (SVM)

SVM 是一种求解少体薛定谔方程的变分方法。波函数展开为关联高斯基函数的线性组合：

$$\Psi = \sum_{i=1}^{K} c_i \phi_i$$

其中每个基函数 $\phi_i$ 是关联高斯函数：

$$\phi_i = \mathcal{A} \left[ \exp\left(-\sum_{j<k} A_{jk}^{(i)} (\mathbf{r}_j - \mathbf{r}_k)^2 \right) \chi_{spin} \chi_{isospin} \right]$$

这里：
- $\mathcal{A}$ 是反对称化算符（费米子）或对称化算符（玻色子）
- $A_{jk}^{(i)}$ 是非线性变分参数
- $\chi_{spin}$ 和 $\chi_{isospin}$ 是自旋和同位旋波函数

### Jacobi 坐标

为了分离质心运动，程序使用 Jacobi 坐标系：

对于 N 个粒子：
- $\boldsymbol{\xi}_1 = \mathbf{r}_2 - \mathbf{r}_1$
- $\boldsymbol{\xi}_2 = \mathbf{r}_3 - \frac{m_1\mathbf{r}_1 + m_2\mathbf{r}_2}{m_1 + m_2}$
- ...
- $\boldsymbol{\xi}_{N-1}$ = 第 N 个粒子相对于前 N-1 个粒子质心的位置
- $\boldsymbol{\xi}_N$ = 系统质心

### 哈密顿量

程序求解的哈密顿量形式：

```math
H = T + V = -\sum_{i=1}^{N-1} \frac{\hbar^2}{2\mu_i} \nabla_{\xi_i}^2 + \sum_{i<j} V_{ij}(|\mathbf{r}_i - \mathbf{r}_j|)
```

两体势能支持以下交换算符：
- **Wigner (W)**：中心力，单位算符
- **Majorana (M)**：空间交换算符 $P_{ij}^r$
- **Bartlett (B)**：自旋交换算符 $P_{ij}^\sigma$
- **Heisenberg (H)**：自旋-同位旋交换算符 $P_{ij}^\sigma P_{ij}^\tau$

---

## 图形界面 (GUI)

本项目提供了用户友好的图形界面，方便配置和运行 SVM 计算。

### GUI 安装

```bash
# 安装 GUI (需要 Conda)
./setup_gui.sh

# 或指定环境名
./setup_gui.sh my_svm_env
```

### 启动 GUI

```bash
./run_gui.sh
```

### GUI 功能

- **向导模式**: 分步骤引导配置粒子、势能和计算参数
- **预设系统**: 内置常用系统配置（3体、4体、6体等）
- **实时预览**: 自动生成输入文件并预览
- **一键运行**: 直接在界面中运行计算并查看输出
- **中文界面**: 全中文注释和提示

### GUI 截图

```
┌─────────────────────────────────────────────────┐
│  SVM Calculator - 少体系统随机变分法              │
├─────────────────────────────────────────────────┤
│  [新建] [打开] [保存]  |  [▶ 运行计算] [■ 停止]  │
├────────────────────┬────────────────────────────┤
│                    │                            │
│   向导模式         │      输出日志              │
│                    │                            │
│  步骤1: 粒子配置   │  [INFO] 开始SVM计算...     │
│  步骤2: 势能配置   │  Basis size: 1  E: 5.79    │
│  步骤3: SVM参数    │  Basis size: 2  E: 4.58    │
│  步骤4: 预览       │  ...                       │
│                    │                            │
│  [生成输入文件]    │  [SUCCESS] 计算完成!       │
│                    │                            │
└────────────────────┴────────────────────────────┘
```

---

## 安装

### 系统要求

- Fortran 编译器：gfortran (推荐) 或 ifort
- LAPACK/BLAS 库（可选，但强烈推荐）
- Make 构建工具

### 编译

```bash
# 克隆仓库
git clone https://github.com/jinleiphys/svm.git
cd svm

# 使用 LAPACK 编译（默认，推荐）
make

# 或使用 OpenBLAS
make openblas

# 或不使用外部库（纯内置实现）
make nolapack

# 调试模式
make DEBUG=1

# 清理编译文件
make clean
```

### macOS 用户

在 macOS 上，程序自动使用 Accelerate 框架，无需额外安装 LAPACK。

### Linux 用户

需要安装 LAPACK：

```bash
# Ubuntu/Debian
sudo apt-get install liblapack-dev libblas-dev

# CentOS/RHEL
sudo yum install lapack-devel blas-devel

# 或使用 OpenBLAS
sudo apt-get install libopenblas-dev
```

---

## 使用方法

### 基本用法

```bash
# 进入示例目录
cd example1

# 运行程序
../fbs
```

### 运行模式

程序支持三种运行模式，由 `fbs.inp` 文件中的 `ico` 参数控制：

| ico 值 | 模式 | 说明 |
|--------|------|------|
| 1 | 预设基计算 | 使用 `fbs.res` 中已有的基函数，仅计算矩阵元并对角化 |
| 2 | 逐步优化 | 从小基组开始，逐个添加优化的基函数 |
| 3 | 细化优化 | 对已有基组进行细化，逐个替换为更优的基函数 |

### 工作流程

1. **准备输入文件**
   - `fbs.inp`：系统参数（粒子数、质量、自旋等）
   - `pot.inp`：势能参数

2. **运行程序**
   ```bash
   ./fbs
   ```

3. **检查输出**
   - 屏幕输出：优化进度和能量
   - `fbs.res`：优化后的基函数参数
   - `ener.dat`：能量本征值

---

## 输入文件格式

### fbs.inp - 系统参数文件

```
npar                           # 粒子数
m1 m2 m3 ... m_npar            # 各粒子质量
z1 z2 z3 ... z_npar            # 各粒子电荷
nisc                           # 同位旋组态数
c1 iso1_1 iso1_2 ... iso1_npar # 第1个同位旋组态：系数 + 各粒子同位旋z分量
c2 iso2_1 iso2_2 ... iso2_npar # 第2个同位旋组态
...
nspc                           # 自旋组态数
c1 sp1_1 sp1_2 ... sp1_npar    # 第1个自旋组态：系数 + 各粒子自旋z分量
c2 sp2_1 sp2_2 ... sp2_npar    # 第2个自旋组态
...
hbar2_over_m  irand  ico  ibf  # 动能系数, 随机种子, 运行模式, 统计类型
mm0  kk0  mnb                  # 优化周期数, 随机试验数, 目标基组大小
bmin  bmax                     # 参数生成范围
```

#### 参数说明

| 参数 | 说明 | 典型值 |
|------|------|--------|
| `npar` | 粒子数 | 2-6 |
| `m_i` | 第 i 个粒子的质量 | 以选定单位为准 |
| `z_i` | 第 i 个粒子的电荷 | 0 或整数 |
| `nisc` | 同位旋组态数 | 1-4 |
| `nspc` | 自旋组态数 | 1-4 |
| `hbar2_over_m` | $\hbar^2/m$ 动能系数 | 取决于单位 |
| `irand` | 随机数种子 | 任意正整数 |
| `ico` | 运行模式 | 1, 2, 或 3 |
| `ibf` | 统计类型 | 1=费米子, 2=玻色子 |
| `mm0` | 优化循环次数 | 5-20 |
| `kk0` | 每参数随机试验数 | 50-200 |
| `mnb` | 目标基组大小 | 50-400 |
| `bmin`, `bmax` | 参数范围 | 取决于系统尺度 |

### pot.inp - 势能参数文件

```
ipcon                          # 势能表示方式: 1=插值, 2=解析
npt  no                        # 势能项数, 算符数
v1 a1 b1 n1                    # 第1项: 强度, 高斯指数, 线性项, r的幂次
v2 a2 b2 n2                    # 第2项
...
```

#### 势能形式

当 `ipcon = 2` 时，势能形式为：

$$V(r) = \sum_{i=1}^{npt} v_i \cdot r^{n_i} \cdot \exp(-a_i r^2 + b_i r)$$

| 参数 | 说明 |
|------|------|
| `ipcon` | 1=数值插值, 2=解析形式 |
| `npt` | 势能项的数目 |
| `no` | 交换算符数目 (1=仅Wigner, 2-4=包含Majorana等) |
| `v_i` | 势能强度 |
| `a_i` | 高斯衰减指数 |
| `b_i` | 线性项系数 |
| `n_i` | r 的幂次 |

#### 势能示例

**高斯势**：$V(r) = V_0 \exp(-\alpha r^2)$
```
2
1  1
V0  alpha  0.0  0
```

**Yukawa 势**（需要 ipcon=1 数值积分）：$V(r) = V_0 \exp(-\mu r)/r$

**Minnesota 势**（原子核物理常用）：
```
2
3  4
200.0   1.487  0.0  0    # Wigner
-178.0  0.639  0.0  0
-91.85  0.465  0.0  0
...                      # Majorana, Bartlett, Heisenberg
```

---

## 输出文件

### fbs.res - 基函数参数文件

```
nbas_total  nbas_index         # 总基函数数, 当前优化索引
A(1,1,1) A(1,1,2) ... A(1,1,npar) A(1,2,1) ... A(1,npar,npar)  # 第1个基函数的A矩阵
A(2,1,1) A(2,1,2) ... A(2,npar,npar)                           # 第2个基函数
...
```

A 矩阵定义了基函数的形状：$\phi_i \propto \exp(-\mathbf{x}^T A_i \mathbf{x})$

### ener.dat - 能量文件

```
Dimension:   K              # 基组大小
E(1) =   -X.XXXXXXXXXXXX    # 基态能量
E(2) =   -X.XXXXXXXXXXXX    # 第一激发态能量
E(3) =   -X.XXXXXXXXXXXX    # 第二激发态能量
```

### 屏幕输出

程序运行时会输出：
- 系统信息（粒子数、运行模式等）
- 有效排列数
- 优化进度（基组大小和当前能量）
- 最终结果

---

## 代码结构

```
svm/
├── Makefile                    # 构建系统
├── README.md                   # 本文档
├── setup.sh                    # Fortran 编译配置脚本
├── setup_gui.sh                # GUI 安装脚本
├── run_gui.sh                  # GUI 启动脚本 (自动生成)
├── .gitignore                  # Git 忽略文件
│
├── src/                        # 现代 Fortran 源代码
│   ├── constants_mod.f90       # 物理和数学常量
│   ├── parameters_mod.f90      # 全局参数和共享变量
│   ├── random_mod.f90          # 随机数生成器
│   ├── linear_algebra_mod.f90  # 线性代数（支持 LAPACK）
│   ├── special_functions_mod.f90  # 特殊函数
│   ├── coordinate_transform_mod.f90  # Jacobi 坐标变换
│   ├── permutation_mod.f90     # 排列和对称化
│   ├── potential_mod.f90       # 两体势能
│   ├── matrix_elements_mod.f90 # 矩阵元计算
│   ├── diagonalization_mod.f90 # 广义本征值问题
│   ├── io_mod.f90              # 输入输出
│   ├── svm_mod.f90             # SVM 优化算法
│   └── main.f90                # 主程序
│
├── gui/                        # 图形界面 (PySide6)
│   ├── main.py                 # GUI 入口
│   ├── main_window.py          # 主窗口
│   ├── input_panel.py          # 输入面板
│   ├── log_widget.py           # 日志组件
│   ├── runner.py               # 计算运行器
│   ├── styles.py               # 样式系统
│   ├── requirements.txt        # Python 依赖
│   └── wizard_steps/           # 向导步骤
│       ├── particle_step.py    # 粒子配置
│       ├── potential_step.py   # 势能配置
│       ├── svm_step.py         # SVM参数
│       └── review_step.py      # 预览
│
├── obj/                        # 编译生成的目标文件（自动创建）
│
└── example1-6/                 # 示例目录
    ├── fbs.inp                 # 系统参数
    ├── pot.inp                 # 势能参数
    └── fbs.res                 # 预计算的基函数（部分示例）
```

### 模块依赖关系

```
constants_mod
    │
    ├── parameters_mod
    │       │
    │       ├── random_mod
    │       │
    │       ├── linear_algebra_mod ──────────────────┐
    │       │       │                                │
    │       │       └── special_functions_mod        │
    │       │               │                        │
    │       │               └── coordinate_transform_mod
    │       │                       │
    │       ├── permutation_mod     │
    │       │                       │
    │       └── potential_mod ──────┤
    │               │               │
    │               └── matrix_elements_mod
    │                       │
    │                       └── diagonalization_mod
    │                               │
    │                               └── io_mod
    │                                       │
    │                                       └── svm_mod
    │                                               │
    └───────────────────────────────────────────────┴── main
```

### 各模块功能

| 模块 | 功能 | 关键子程序/函数 |
|------|------|----------------|
| `constants_mod` | 定义常量 | `PI`, `MNPAR`, `MNBAS`, `dp` |
| `parameters_mod` | 全局变量 | `init_parameters()` |
| `random_mod` | 随机数 | `ran2()`, `init_random()` |
| `linear_algebra_mod` | 矩阵运算 | `solve_geneig()`, `choldc()`, `vdet()`, `vinv()` |
| `special_functions_mod` | 特殊函数 | `hermite()`, `erfc0()`, `potmat()`, `zerus()` |
| `coordinate_transform_mod` | 坐标变换 | `compute_jacobi_transform()`, `trcorr()` |
| `permutation_mod` | 对称化 | `compute_spin_isospin_elements()`, `permut()` |
| `potential_mod` | 势能 | `poten()`, `pot_function()`, `potval()` |
| `matrix_elements_mod` | 矩阵元 | `compute_matrix_element()`, `vkin_ene()`, `vpot_ene()` |
| `diagonalization_mod` | 本征值 | `diagonalize()`, `eigval_lowest()` |
| `io_mod` | 输入输出 | `read_input_data()`, `write_basis_file()` |
| `svm_mod` | SVM 优化 | `svm_step_by_step()`, `svm_refinement()` |

---

## 示例

### Example 1-3: 基本测试

这些示例用于测试程序的基本功能。

### Example 4: 6 粒子系统

```bash
cd example4
cat fbs.inp
```

```
6                              # 6个粒子
1.0 1.0 1.0 1.0 1.0 1.0        # 质量
0 0 0 0 0 0                    # 电荷（中性）
1                              # 1个同位旋组态
1.0  1 -1 1 -1 1 -1            # 同位旋 z 分量
1                              # 1个自旋组态
1.0  1 -1 1 -1 1 -1            # 自旋 z 分量
41.47 12345 3 1                # hbar^2/m, 种子, 模式=细化, 费米子
5 100 100                      # 优化参数
0.1 5.0                        # 参数范围
```

运行：
```bash
../fbs
```

### Example 5: 带自旋组态

这个例子展示了多自旋组态的使用。

### Example 6: 自定义势能

演示如何定义自定义的两体势能。

---

## 高级用法

### 修改势能函数

如果需要使用 `ipcon=1`（数值插值）模式下的自定义势能，可以修改 `src/potential_mod.f90` 中的 `pot_function` 函数：

```fortran
function pot_function(r) result(v)
    implicit none
    real(dp), intent(in) :: r
    real(dp) :: v

    ! 示例：Woods-Saxon 势
    real(dp), parameter :: V0 = -50.0_dp   ! MeV
    real(dp), parameter :: R0 = 1.2_dp     ! fm
    real(dp), parameter :: a = 0.5_dp      ! fm

    v = V0 / (1.0_dp + exp((r - R0) / a))

end function pot_function
```

修改后重新编译：
```bash
make clean && make
```

### 调整数组大小

如果需要处理更多粒子或更大基组，修改 `src/constants_mod.f90`：

```fortran
! 最大粒子数
integer, parameter :: MNPAR = 8    ! 默认 6

! 最大基组大小
integer, parameter :: MNBAS = 800  ! 默认 400
```

### 使用 Intel Fortran 编译器

```bash
make FC=ifort
```

---

## 性能优化建议

1. **使用 LAPACK**：默认启用，显著提升对角化速度

2. **合理设置优化参数**：
   - `kk0 = 100-200`：每个参数的随机试验数
   - `mm0 = 5-10`：优化循环数
   - 增大这些值可以得到更好的变分结果，但计算时间增加

3. **基组大小**：
   - 小系统（3体）：50-100
   - 中等系统（4-5体）：100-200
   - 大系统（6体）：200-400

4. **并行化**：目前版本为串行，可以通过 OpenMP 或 MPI 扩展

---

## 故障排除

### 编译错误

**问题**：找不到 LAPACK
```
ld: library not found for -llapack
```

**解决**：
```bash
# 安装 LAPACK
sudo apt-get install liblapack-dev  # Linux

# 或使用无 LAPACK 模式
make nolapack
```

### 运行错误

**问题**：`Error: Singular matrix in ludcmp`

**原因**：基函数线性相关

**解决**：
- 检查参数范围 `bmin`, `bmax`
- 减小基组大小
- 使用不同的随机种子

**问题**：能量不收敛

**解决**：
- 增加 `kk0` 和 `mm0`
- 调整参数范围
- 检查势能参数

---

## 参考文献

1. K. Varga, Y. Suzuki, "Precise solution of few-body problems with the stochastic variational method on a correlated Gaussian basis", **Phys. Rev. C 52** (1995) 2885. [DOI:10.1103/PhysRevC.52.2885](https://doi.org/10.1103/PhysRevC.52.2885)

2. Y. Suzuki, K. Varga, "Stochastic Variational Approach to Quantum-Mechanical Few-Body Problems", **Lecture Notes in Physics**, Vol. 54, Springer (1998). [ISBN: 978-3-540-65152-9](https://www.springer.com/gp/book/9783540651529)

3. K. Varga, Y. Suzuki, "Solution of few-body problems with the stochastic variational method: I. Central forces", **Computer Physics Communications 106** (1997) 157-168.

---

## 许可证

本项目基于原始 SVM 代码开发，仅供学术研究使用。

## 致谢

原始算法和代码由 K. Varga 和 Y. Suzuki 开发。现代 Fortran 版本重构由 Claude 辅助完成。

---

## 联系方式

如有问题或建议，请在 GitHub 上提交 Issue：
https://github.com/jinleiphys/svm/issues
