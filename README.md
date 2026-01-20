# DKD-Predict 糖尿病肾病智能诊断系统

## 简介

DKD-Predict 是基于 XGBoost 机器学习算法开发的糖尿病肾病（DKD）辅助诊断系统。本系统面向糖尿病合并慢性肾脏病（CKD）的患者群体，帮助临床医生判断其是否为 DKD。

## 环境要求

### R 版本
- R >= 4.0.0

### 必需的 R 包

```r
# 安装必需的包
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "tidyverse", "DT"))

# 安装特定版本的 mlr3 相关包（与模型训练环境一致）
install.packages('remotes')
remotes::install_version("mlr3", version = "0.16.1")
remotes::install_version("mlr3learners", version = "0.5.6")
remotes::install_version("paradox", version = "0.11.1")
remotes::install_version('xgboost', version = '1.7.5.1')
```

## 文件结构

```
web_app/
├── app.R                    # Shiny 应用主文件
├── README.md                # 本说明文档
├── data/
│   ├── 10-Model-xgboost-all-model.rds    # XGBoost 模型文件
│   ├── 11-scale-miss_value.rdata          # 归一化参数和缺失值填补值
│   └── ...
├── plans/
│   └── shiny-app-design.md  # 设计文档
└── R/
    └── example.R            # 示例代码
```

## 启动应用

### 方法 1：在 RStudio 中运行

1. 打开 RStudio
2. 打开 `app.R` 文件
3. 点击右上角的 "Run App" 按钮

### 方法 2：在 R 控制台中运行

```r
# 设置工作目录
setwd("path/to/web_app")

# 运行应用
shiny::runApp()
```

### 方法 3：指定端口运行

```r
shiny::runApp(port = 3838, host = "0.0.0.0")
```

## 功能说明

### 1. 欢迎页面
- 系统介绍
- 快速导航到各功能模块

### 2. 批量预测
- 上传 CSV 文件进行批量预测
- 支持下载预测模板
- 预测结果可视化展示
- 支持下载预测结果

### 3. 单条预测
- 手动输入患者临床指标
- 实时显示预测结果
- 提供风险等级判断和临床建议

### 4. 使用说明
- 输入变量详细说明
- 结果解读指南

## 输入变量说明

### 连续变量（12个）

| 变量名 | 中文名称 | 单位 |
|--------|----------|------|
| age | 年龄 | 岁 |
| alb | 白蛋白 | g/L |
| cr | 肌酐 | μmol/L |
| cysc | 胱抑素C | mg/L |
| egfr_ckd_epi | eGFR (CKD-EPI) | mL/min/1.73m² |
| glu | 空腹血糖 | mmol/L |
| hb | 血红蛋白 | g/L |
| hba1c | 糖化血红蛋白 | % |
| sbp | 收缩压 | mmHg |
| tcho | 总胆固醇 | mmol/L |
| uacr | 尿白蛋白肌酐比 | mg/g |
| bun | 尿素氮 | mmol/L |

### 分类变量（5个）

| 变量名 | 中文名称 | 编码 |
|--------|----------|------|
| GENDER | 性别 | 0=女, 1=男 |
| dia_per | 糖尿病周围神经病变 | 0=无, 1=有 |
| dia_ret | 糖尿病视网膜病变 | 0=无, 1=有 |
| nep_syndrome | 肾病综合征 | 0=无, 1=有 |
| urbc | 尿红细胞异常 | 0=正常, 1=异常 |

## 模型参数

- **最佳分类阈值**: 0.179 (17.9%)
- **正类标签**: 1 (DKD)

## 结果解读

| 概率范围 | 风险等级 | 建议 |
|----------|----------|------|
| < 17.9% | 低风险 | 非DKD可能性较大，考虑排查其他肾脏疾病 |
| ≥ 17.9% | 高风险 | DKD可能性较大，建议进一步检查或肾活检 |

## 注意事项

1. **仅供临床参考**：本系统预测结果仅供辅助参考，不能替代医生的专业诊断。
2. **数据完整性**：批量预测时请确保 CSV 文件包含所有 17 个必需变量。
3. **模型版本**：请确保使用与模型训练环境一致的 R 包版本。

## 故障排除

### 问题：模型加载失败
```
Error: cannot unserialize ALTREP object...
```
**解决方案**：确保安装了正确版本的 `xgboost` (1.7.5.1)

### 问题：预测时出错
```
Error: 缺少以下变量...
```
**解决方案**：检查上传的 CSV 文件是否包含所有必需的变量列

## 版本信息

- **应用版本**: v1.0
- **创建日期**: 2026-01-19

## 免责声明

本系统仅供临床辅助参考，最终诊断需结合肾活检及其他临床资料综合判断。使用本系统产生的任何后果由使用者自行承担。
