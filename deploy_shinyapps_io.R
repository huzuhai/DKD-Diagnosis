# =============================================================================
# 部署到 shinyapps.io 的脚本
# =============================================================================
# 使用 rsconnect 包部署应用到 shinyapps.io

# 安装 rsconnect（如果尚未安装）
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

library(rsconnect)

# =============================================================================
# 步骤 1: 配置 shinyapps.io 账户
# =============================================================================
# 首次部署前，需要配置您的 shinyapps.io 账户
# 登录 https://www.shinyapps.io/，进入 Account -> Tokens
# 获取您的 token 和 secret

# 取消下面注释并填入您的账户信息
# rsconnect::setAccountInfo(
#   name = "your-account-name",     # 您的 shinyapps.io 用户名
#   token = "your-token",           # 您的 token
#   secret = "your-secret"          # 您的 secret
# )

# =============================================================================
# 步骤 2: 部署应用
# =============================================================================

# 定义应用名称
app_name <- "DKD-Predict"

# 定义要上传的文件
app_files <- c(
  "app.R",                                   # 主应用文件
  "data/10-Model-xgboost-all-model.rds",     # 模型文件
  "data/11-scale-miss_value.rdata"           # 缺失值和归一化参数
)

# 部署应用
cat("正在部署应用到 shinyapps.io...\n")

rsconnect::deployApp(
  appDir = getwd(),                     # 应用目录
  appName = app_name,                   # 应用名称
  appFiles = app_files,                 # 要上传的文件
  appTitle = "DKD-Predict 糖尿病肾病智能诊断系统",
  forceUpdate = TRUE,                   # 强制更新
  launch.browser = TRUE                 # 部署完成后打开浏览器
)

cat("\n=== 部署完成 ===\n")
cat("应用已部署到: https://your-account.shinyapps.io/", app_name, "\n", sep = "")
