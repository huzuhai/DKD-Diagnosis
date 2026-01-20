# =============================================================================
# 初始化 renv 环境 - 用于包环境管理
# =============================================================================
# 运行此脚本以初始化项目的 R 包环境
# 这将创建 renv.lock 文件，记录所有依赖包的版本

# 首先安装 renv（如果尚未安装）
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# 初始化 renv 项目
renv::init()

# 安装项目依赖的包
packages <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "mlr3",
  "mlr3learners",
  "tidyverse",
  "DT",
  "xgboost"
)

# 安装缺失的包
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# 创建 renv.lock 快照，记录当前包环境
renv::snapshot()

cat("\n=== renv 环境初始化完成 ===\n")
cat("已创建 renv.lock 文件，记录了所有依赖包版本\n")
cat("部署时请确保包含以下文件/文件夹：\n")
cat("  - renv.lock\n")
cat("  - renv/\n")
cat("  - .Rprofile\n")
