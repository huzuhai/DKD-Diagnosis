# =============================================================================
# DKD-Predict 糖尿病肾病智能诊断系统
# 基于 XGBoost 机器学习模型的 DKD 辅助诊断 Shiny 应用
# =============================================================================

# 加载必要的包
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(mlr3)
library(mlr3learners)
library(tidyverse)
library(DT)

# =============================================================================
# 0. 多语言支持 - 翻译字典
# =============================================================================

# 中文翻译
i18n_zh <- list(
  # 应用标题
  app_title = "DKD-Predict 糖尿病肾病智能诊断系统",
  app_subtitle = "基于机器学习的糖尿病肾病智能辅助诊断系统",
  
  # 菜单
  menu_welcome = "欢迎页面",
  menu_batch = "批量预测",
  menu_single = "单条预测",
  menu_help = "使用说明",
  
  # 侧边栏
  version = "版本",
  threshold = "阈值",
  language = "语言",
  
  # 欢迎页面
  welcome_intro = "本系统基于 XGBoost 机器学习算法开发，旨在帮助临床医生快速判断糖尿病合并慢性肾脏病（CKD）患者是否为糖尿病肾病（DKD）。",
  batch_predict = "批量预测",
  batch_desc = "上传 CSV 文件，一次性预测多位患者的 DKD 风险概率。",
  start_batch = "开始批量预测",
  single_predict = "单条预测",
  single_desc = "手动输入单个患者的临床指标，获取个性化预测结果。",
  start_single = "开始单条预测",
  help_title = "使用说明",
  help_desc = "了解系统功能、输入变量说明及结果解读指南。",
  view_help = "查看说明",
  model_metrics = "模型性能指标",
  best_threshold = "最佳分类阈值",
  disclaimer = "免责声明：本系统仅供临床辅助参考，不能替代医生的专业诊断。",
  
  # 批量预测页面
  upload_data = "上传患者数据",
  select_csv = "选择 CSV 文件",
  browse = "浏览...",
  no_file = "未选择文件",
  file_req = "文件要求：CSV格式，包含以下17个变量列：",
  download_template = "下载模板文件",
  data_preview = "上传数据预览",
  please_upload = "请上传CSV文件",
  loaded_records = "已加载",
  records = "条记录",
  variables = "个变量",
  predict_results = "预测结果",
  start_predict = "开始预测",
  download_results = "下载预测结果",
  total_records = "共",
  
  # 单条预测页面
  basic_info = "基本信息",
  renal_function = "肾功能指标",
  metabolic = "代谢指标",
  hematology = "血液学及生命体征",
  complications = "并发症及其他",
  predict_result = "预测结果",
  click_predict = "点击上方按钮开始预测",
  dkd_risk = "DKD 风险概率",
  input_summary = "输入数据汇总",
  
  # 结果
  high_risk = "高风险 - DKD可能性大",
  low_risk = "低风险 - 非DKD可能性大",
  clinical_advice = "临床建议",
  high_risk_advice = "该患者 DKD 可能性较高，建议：",
  high_advice_1 = "进一步完善相关检查",
  high_advice_2 = "考虑肾活检以明确诊断",
  high_advice_3 = "积极控制血糖、血压",
  high_advice_4 = "定期监测肾功能变化",
  low_risk_advice = "该患者 DKD 可能性较低，考虑非糖尿病肾病可能，建议：",
  low_advice_1 = "排查其他肾脏疾病病因",
  low_advice_2 = "必要时行肾活检明确诊断",
  low_advice_3 = "继续监测肾功能和尿蛋白",
  
  # 帮助页面
  system_intro = "系统介绍",
  system_intro_text1 = "DKD-Predict 是基于 XGBoost 机器学习算法开发的糖尿病肾病（DKD）辅助诊断系统。",
  system_intro_text2 = "本系统面向糖尿病合并慢性肾脏病（CKD）的患者群体，帮助临床医生判断其是否为 DKD。",
  var_explanation = "输入变量说明",
  continuous_vars = "连续变量（13个）",
  categorical_vars_title = "分类变量说明",
  binary_vars = "二分类变量（4个）",
  result_interpretation = "结果解读",
  predict_prob = "预测概率：",
  prob_desc = "模型输出的 DKD 可能性概率（0-100%）",
  classification_threshold = "分类阈值：",
  low_risk_desc = "：概率 < ",
  low_risk_meaning = "%，非DKD可能性较大",
  high_risk_desc = "：概率 ≥ ",
  high_risk_meaning = "%，DKD可能性较大",
  note = "注意：本系统预测结果仅供临床参考，最终诊断需结合肾活检及其他临床资料综合判断。",
  
  # 通知消息
  file_success = "文件上传成功！",
  file_error = "文件读取错误：",
  predict_complete = "预测完成！共处理",
  predict_error = "预测错误：",
  missing_vars = "警告：缺少以下变量：",
  processing = "正在预测...",
  preprocessing = "数据预处理中...",
  model_predicting = "模型预测中...",
  generating_results = "生成结果...",
  done = "完成！",
  single_predict_complete = "预测完成！",
  
  # 变量标签
  age = "年龄 (岁)",
  alb = "白蛋白 (g/L)",
  cr = "肌酐 (μmol/L)",
  cysc = "胱抑素C (mg/L)",
  egfr_ckd_epi = "eGFR-CKD-EPI (mL/min/1.73m²)",
  glu = "空腹血糖 (mmol/L)",
  hb = "血红蛋白 (g/L)",
  hba1c = "糖化血红蛋白 (%)",
  sbp = "收缩压 (mmHg)",
  tcho = "总胆固醇 (mmol/L)",
  uacr = "尿白蛋白肌酐比 (mg/g)",
  bun = "尿素氮 (mmol/L)",
  GENDER = "性别",
  dia_per = "糖尿病周围神经病变",
  dia_ret = "糖尿病视网膜病变",
  nep_syndrome = "肾病综合征",
  urbc = "尿红细胞 (/HP)",
  
  # 选项
  female = "女",
  male = "男",
  no = "无",
  yes = "有",
  normal = "正常",
  abnormal = "异常",
  
  # 表格列名
  col_id = "序号",
  col_prob = "DKD概率",
  col_risk = "风险等级",
  col_var = "变量",
  col_value = "数值"
)

# 英文翻译
i18n_en <- list(
  # 应用标题
  app_title = "DKD-Predict: Diabetic Kidney Disease Diagnosis System",
  app_subtitle = "AI-Powered Diabetic Kidney Disease Diagnostic Assistant",
  
  # 菜单
  menu_welcome = "Welcome",
  menu_batch = "Batch Prediction",
  menu_single = "Single Prediction",
  menu_help = "User Guide",
  
  # 侧边栏
  version = "Version",
  threshold = "Threshold",
  language = "Language",
  
  # 欢迎页面
  welcome_intro = "This system is developed based on XGBoost machine learning algorithm to help clinicians quickly determine whether diabetic patients with chronic kidney disease (CKD) have diabetic kidney disease (DKD).",
  batch_predict = "Batch Prediction",
  batch_desc = "Upload a CSV file to predict DKD risk for multiple patients at once.",
  start_batch = "Start Batch Prediction",
  single_predict = "Single Prediction",
  single_desc = "Manually enter clinical indicators for a single patient to get personalized prediction.",
  start_single = "Start Single Prediction",
  help_title = "User Guide",
  help_desc = "Learn about system features, input variables, and result interpretation.",
  view_help = "View Guide",
  model_metrics = "Model Performance Metrics",
  best_threshold = "Optimal Classification Threshold",
  disclaimer = "Disclaimer: This system is for clinical reference only and cannot replace professional medical diagnosis.",
  
  # 批量预测页面
  upload_data = "Upload Patient Data",
  select_csv = "Select CSV File",
  browse = "Browse...",
  no_file = "No file selected",
  file_req = "File requirements: CSV format with the following 17 variable columns:",
  download_template = "Download Template",
  data_preview = "Data Preview",
  please_upload = "Please upload a CSV file",
  loaded_records = "Loaded",
  records = "records",
  variables = "variables",
  predict_results = "Prediction Results",
  start_predict = "Start Prediction",
  download_results = "Download Results",
  total_records = "Total",
  
  # 单条预测页面
  basic_info = "Basic Information",
  renal_function = "Renal Function",
  metabolic = "Metabolic Indicators",
  hematology = "Hematology & Vital Signs",
  complications = "Complications & Others",
  predict_result = "Prediction Result",
  click_predict = "Click the button above to start prediction",
  dkd_risk = "DKD Risk Probability",
  input_summary = "Input Data Summary",
  
  # 结果
  high_risk = "High Risk - DKD Likely",
  low_risk = "Low Risk - Non-DKD Likely",
  clinical_advice = "Clinical Recommendations",
  high_risk_advice = "High probability of DKD. Recommendations:",
  high_advice_1 = "Further comprehensive examinations",
  high_advice_2 = "Consider renal biopsy for definitive diagnosis",
  high_advice_3 = "Actively control blood glucose and blood pressure",
  high_advice_4 = "Regular monitoring of renal function",
  low_risk_advice = "Low probability of DKD, consider non-diabetic kidney disease. Recommendations:",
  low_advice_1 = "Investigate other causes of kidney disease",
  low_advice_2 = "Consider renal biopsy if necessary",
  low_advice_3 = "Continue monitoring renal function and proteinuria",
  
  # 帮助页面
  system_intro = "System Introduction",
  system_intro_text1 = "DKD-Predict is a diabetic kidney disease (DKD) diagnostic assistant developed using XGBoost machine learning algorithm.",
  system_intro_text2 = "This system targets diabetic patients with chronic kidney disease (CKD) to help clinicians determine whether they have DKD.",
  var_explanation = "Input Variables",
  continuous_vars = "Continuous Variables (13)",
  categorical_vars_title = "Categorical Variables",
  binary_vars = "Binary Variables (4)",
  result_interpretation = "Result Interpretation",
  predict_prob = "Prediction Probability:",
  prob_desc = "DKD probability output by the model (0-100%)",
  classification_threshold = "Classification Threshold:",
  low_risk_desc = ": Probability < ",
  low_risk_meaning = "%, Non-DKD likely",
  high_risk_desc = ": Probability ≥ ",
  high_risk_meaning = "%, DKD likely",
  note = "Note: Prediction results are for clinical reference only. Final diagnosis should be based on renal biopsy and other clinical data.",
  
  # 通知消息
  file_success = "File uploaded successfully!",
  file_error = "File reading error: ",
  predict_complete = "Prediction complete! Processed ",
  predict_error = "Prediction error: ",
  missing_vars = "Warning: Missing variables: ",
  processing = "Processing...",
  preprocessing = "Preprocessing data...",
  model_predicting = "Model predicting...",
  generating_results = "Generating results...",
  done = "Done!",
  single_predict_complete = "Prediction complete!",
  
  # 变量标签
  age = "Age (years)",
  alb = "Albumin (g/L)",
  cr = "Creatinine (μmol/L)",
  cysc = "Cystatin C (mg/L)",
  egfr_ckd_epi = "eGFR-CKD-EPI (mL/min/1.73m²)",
  glu = "Fasting Glucose (mmol/L)",
  hb = "Hemoglobin (g/L)",
  hba1c = "HbA1c (%)",
  sbp = "Systolic BP (mmHg)",
  tcho = "Total Cholesterol (mmol/L)",
  uacr = "UACR (mg/g)",
  bun = "BUN (mmol/L)",
  GENDER = "Gender",
  dia_per = "Diabetic Peripheral Neuropathy",
  dia_ret = "Diabetic Retinopathy",
  nep_syndrome = "Nephrotic Syndrome",
  urbc = "Urine RBC (/HP)",
  
  # 选项
  female = "Female",
  male = "Male",
  no = "No",
  yes = "Yes",
  normal = "Normal",
  abnormal = "Abnormal",
  
  # 表格列名
  col_id = "ID",
  col_prob = "DKD Probability",
  col_risk = "Risk Level",
  col_var = "Variable",
  col_value = "Value"
)

# 获取翻译文本的函数
get_text <- function(key, lang = "zh") {
  if (lang == "en") {
    return(i18n_en[[key]])
  } else {
    return(i18n_zh[[key]])
  }
}

# =============================================================================
# 1. 加载模型和预处理参数
# =============================================================================

# 加载 XGBoost 模型
lrn_xgboost <- readRDS("./data/10-Model-xgboost-all-model.rds")

# 加载归一化参数和缺失值填补值
load("./data/11-scale-miss_value.rdata")

# 提取归一化参数 (假设 tran_scale 对象存在于加载的数据中)
# 如果变量名不同，请根据实际情况修改
scale_select <- tran_scale$state$scale
center_select <- tran_scale$state$center

# 定义特征变量
selected_vars <- c("age", "alb", "cr", "cysc", "egfr_ckd_epi", "glu", "hb", 
                   "hba1c", "sbp", "tcho", "uacr", "GENDER", "dia_per", 
                   "dia_ret", "nep_syndrome", "urbc", "bun")

# 连续变量（需要归一化）- 排除分类变量 GENDER, dia_per, dia_ret, nep_syndrome
continuous_vars <- c("age", "alb", "cr", "cysc", "egfr_ckd_epi", "glu", "hb",
                     "hba1c", "sbp", "tcho", "uacr", "bun", "urbc")

# 分类变量（0/1编码）
categorical_vars <- c("GENDER", "dia_per", "dia_ret", "nep_syndrome")

# 最佳阈值
THRESHOLD <- 0.179087966680527

# 变量中文名称映射
var_labels <- c(
  age = "年龄 (岁)",
  alb = "白蛋白 (g/L)",
  cr = "肌酐 (μmol/L)",
  cysc = "胱抑素C (mg/L)",
  egfr_ckd_epi = "eGFR-CKD-EPI (mL/min/1.73m²)",
  glu = "空腹血糖 (mmol/L)",
  hb = "血红蛋白 (g/L)",
  hba1c = "糖化血红蛋白 (%)",
  sbp = "收缩压 (mmHg)",
  tcho = "总胆固醇 (mmol/L)",
  uacr = "尿白蛋白肌酐比 (mg/g)",
  bun = "尿素氮 (mmol/L)",
  GENDER = "性别",
  dia_per = "糖尿病周围神经病变",
  dia_ret = "糖尿病视网膜病变",
  nep_syndrome = "肾病综合征",
  urbc = "尿红细胞 (/HP)"
)

# =============================================================================
# 2. 数据处理函数
# =============================================================================

# 缺失值填补函数
impute_missing <- function(df, imputed_values, vars) {
  for (var in vars) {
    if (var %in% names(df) && var %in% names(imputed_values)) {
      df[[var]][is.na(df[[var]])] <- imputed_values[var]
    }
  }
  return(df)
}

# 归一化函数
scale_function <- function(df, vars_, center, scal) {
  for (var in vars_) {
    if (var %in% names(df) && var %in% names(center) && var %in% names(scal)) {
      df[[var]] <- (df[[var]] - center[var]) / scal[var]
    }
  }
  return(df)
}

# 完整预处理流程
preprocess_data <- function(df) {
  # 确保数据框包含所有必需变量
  missing_vars <- setdiff(selected_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("缺少以下变量:", paste(missing_vars, collapse = ", ")))
  }
  
  # 只保留需要的变量
  df <- df[, selected_vars, drop = FALSE]
  
  # 转换为数值类型
  for (var in continuous_vars) {
    df[[var]] <- as.numeric(df[[var]])
  }
  for (var in categorical_vars) {
    df[[var]] <- as.integer(df[[var]])
  }
  
  # 填补缺失值（仅对连续变量）
  df <- impute_missing(df, imputed_miss_value, continuous_vars)
  
  # 归一化（仅对连续变量）
  df <- scale_function(df, continuous_vars, center_select, scale_select)
  
  return(df)
}

# 预测函数
predict_dkd <- function(model, new_data) {
  # 确保数据是 data.frame
  new_data <- as.data.frame(new_data)
  
  # 使用 mlr3 模型预测
  pred <- model$predict_newdata(new_data)
  
  # 获取正类概率
  prob <- pred$prob[, "1"]
  
  return(prob)
}

# 判断风险等级
get_risk_level <- function(prob, threshold = THRESHOLD) {
  ifelse(prob >= threshold, "高风险 - DKD可能性大", "低风险 - 非DKD可能性大")
}

# 获取风险颜色
get_risk_color <- function(prob, threshold = THRESHOLD) {
  ifelse(prob >= threshold, "red", "green")
}

# =============================================================================
# 3. UI 界面设计
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # 标题栏
  dashboardHeader(
    title = "DKD-Predict 糖尿病肾病智能诊断系统",
    titleWidth = 350
  ),
  
  # 侧边栏菜单
  dashboardSidebar(
    width = 250,
    sidebarMenuOutput("sidebar_menu"),
    br(),
    # 语言选择
    div(style = "padding: 15px;",
        radioButtons("lang_select",
                     label = div(icon("globe"), " Language / 语言"),
                     choices = c("中文" = "zh", "English" = "en"),
                     selected = "zh",
                     inline = TRUE),
        hr(),
        p(style = "color: #b8c7ce; font-size: 12px;",
          "版本 / Version: v1.0"),
        p(style = "color: #b8c7ce; font-size: 12px;",
          paste("阈值 / Threshold:", round(THRESHOLD, 4)))
    )
  ),
  
  # 主体内容
  dashboardBody(
    # 自定义CSS样式和JavaScript
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box-header { background-color: #3c8dbc; color: white; }
        .info-box { min-height: 90px; }
        .result-high { color: #dd4b39; font-weight: bold; font-size: 18px; }
        .result-low { color: #00a65a; font-weight: bold; font-size: 18px; }
        .welcome-title { font-size: 36px; color: #3c8dbc; margin-bottom: 20px; }
        .welcome-subtitle { font-size: 18px; color: #666; margin-bottom: 30px; }
        .feature-box { padding: 20px; margin: 10px; background: white;
                       border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); }
        .gauge-container { text-align: center; padding: 20px; }
        .probability-display { font-size: 48px; font-weight: bold; }
      ")),
      # JavaScript for dynamic title update
      tags$script(HTML("
        Shiny.addCustomMessageHandler('updateTitle', function(title) {
          $('.logo').text(title);
          $('title').text(title);
        });
      "))
    ),
    
    tabItems(
      # =========== 欢迎页面 ===========
      tabItem(
        tabName = "welcome",
        uiOutput("welcome_ui")
      ),
      
      # =========== 批量预测页面 ===========
      tabItem(
        tabName = "batch",
        uiOutput("batch_ui")
      ),
      
      # =========== 单条预测页面 ===========
      tabItem(
        tabName = "single",
        uiOutput("single_ui")
      ),
      
      # =========== 使用说明页面 ===========
      tabItem(
        tabName = "help",
        uiOutput("help_ui")
      )
    ) # tabItems 结束
  ) # dashboardBody 结束
) # dashboardPage 结束

# =============================================================================
# 4. Server 逻辑
# =============================================================================

server <- function(input, output, session) {
  
  # 存储上传的数据和预测结果
  uploaded_data <- reactiveVal(NULL)
  prediction_results <- reactiveVal(NULL)
  
  # =========== 导航按钮事件 ===========
  observeEvent(input$go_batch, {
    updateTabItems(session, "tabs", "batch")
  })
  
  observeEvent(input$go_single, {
    updateTabItems(session, "tabs", "single")
  })
  
  observeEvent(input$go_help, {
    updateTabItems(session, "tabs", "help")
  })
  
  # =========== 动态渲染页面 - 多语言支持 ===========
  
  # 获取当前语言
  current_lang <- reactive({
    input$lang_select
  })
  
  # 监听语言变化并更新标题
  observeEvent(input$lang_select, {
    lang <- input$lang_select
    title <- get_text("app_title", lang)
    session$sendCustomMessage("updateTitle", title)
  })
  
  # 动态渲染侧边栏菜单
  output$sidebar_menu <- renderMenu({
    lang <- current_lang()
    sidebarMenu(
      id = "tabs",
      menuItem(get_text("menu_welcome", lang), tabName = "welcome", icon = icon("home")),
      menuItem(get_text("menu_batch", lang), tabName = "batch", icon = icon("file-csv")),
      menuItem(get_text("menu_single", lang), tabName = "single", icon = icon("user")),
      menuItem(get_text("menu_help", lang), tabName = "help", icon = icon("question-circle"))
    )
  })
  
  # 欢迎页面
  output$welcome_ui <- renderUI({
    lang <- current_lang()
    
    tagList(
      fluidRow(
        column(12, align = "center",
               div(style = "padding: 50px;",
                   h1(class = "welcome-title", 
                      icon("stethoscope"), " DKD-Predict"),
                   h3(class = "welcome-subtitle", get_text("app_subtitle", lang)),
                   hr(),
                   p(style = "font-size: 16px; max-width: 800px; margin: auto;",
                     get_text("welcome_intro", lang)),
                   br()
               )
        )
      ),
      fluidRow(
        column(4,
               div(class = "feature-box",
                   h4(icon("file-csv", style = "color: #3c8dbc;"), " ", get_text("batch_predict", lang)),
                   p(get_text("batch_desc", lang)),
                   actionButton("go_batch", get_text("start_batch", lang), 
                                class = "btn-primary", width = "100%")
               )
        ),
        column(4,
               div(class = "feature-box",
                   h4(icon("user", style = "color: #00a65a;"), " ", get_text("single_predict", lang)),
                   p(get_text("single_desc", lang)),
                   actionButton("go_single", get_text("start_single", lang), 
                                class = "btn-success", width = "100%")
               )
        ),
        column(4,
               div(class = "feature-box",
                   h4(icon("book", style = "color: #f39c12;"), " ", get_text("help_title", lang)),
                   p(get_text("help_desc", lang)),
                   actionButton("go_help", get_text("view_help", lang), 
                                class = "btn-warning", width = "100%")
               )
        )
      ),
      br(),
      fluidRow(
        column(12, align = "center",
               div(style = "padding: 20px; background: #fff; border-radius: 5px; margin-top: 20px;",
                   h4(get_text("model_metrics", lang)),
                   p(get_text("best_threshold", lang), ": ", strong(round(THRESHOLD, 4))),
                   p(style = "color: #999; font-size: 12px;", get_text("disclaimer", lang))
               )
        )
      )
    )
  })
  
  # =========== 批量预测功能 ===========
  
  # 批量预测页面
  output$batch_ui <- renderUI({
    lang <- current_lang()
    
    tagList(
      fluidRow(
        box(
          title = get_text("upload_data", lang), status = "primary", solidHeader = TRUE,
          width = 4,
          fileInput("file_upload", get_text("select_csv", lang),
                    accept = c("text/csv", ".csv"),
                    buttonLabel = get_text("browse", lang),
                    placeholder = get_text("no_file", lang)),
          hr(),
          p(style = "font-size: 12px; color: #666;", get_text("file_req", lang)),
          p(style = "font-size: 11px; color: #999;", paste(selected_vars, collapse = ", ")),
          hr(),
          downloadButton("download_template", get_text("download_template", lang), 
                         class = "btn-info btn-block")
        ),
        box(
          title = get_text("data_preview", lang), status = "info", solidHeader = TRUE,
          width = 8,
          DTOutput("preview_table"),
          verbatimTextOutput("upload_status")
        )
      ),
      fluidRow(
        box(
          title = get_text("predict_results", lang), status = "success", solidHeader = TRUE,
          width = 12,
          actionButton("btn_batch_predict", get_text("start_predict", lang), 
                       class = "btn-success btn-lg", icon = icon("play")),
          downloadButton("download_results", get_text("download_results", lang), 
                         class = "btn-primary"),
          hr(),
          DTOutput("results_table")
        )
      )
    )
  })
  
  # 处理文件上传
  observeEvent(input$file_upload, {
    req(input$file_upload)
    lang <- current_lang()
    
    tryCatch({
      # 读取CSV文件 - 尝试多种编码
      df <- tryCatch({
        # 首先尝试 UTF-8 编码
        read.csv(input$file_upload$datapath, stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8", check.names = FALSE)
      }, error = function(e1) {
        tryCatch({
          # 尝试 GBK 编码 (中文 Windows 默认)
          read.csv(input$file_upload$datapath, stringsAsFactors = FALSE,
                   fileEncoding = "GBK", check.names = FALSE)
        }, error = function(e2) {
          tryCatch({
            # 尝试 GB2312 编码
            read.csv(input$file_upload$datapath, stringsAsFactors = FALSE,
                     fileEncoding = "GB2312", check.names = FALSE)
          }, error = function(e3) {
            # 最后尝试系统默认编码
            read.csv(input$file_upload$datapath, stringsAsFactors = FALSE,
                     check.names = FALSE)
          })
        })
      })
      
      # 检查必需的变量
      missing_vars <- setdiff(selected_vars, names(df))
      
      if (length(missing_vars) > 0) {
        showNotification(
          paste(get_text("missing_vars", lang), paste(missing_vars, collapse = ", ")),
          type = "warning",
          duration = 10
        )
      }
      
      uploaded_data(df)
      
      showNotification(get_text("file_success", lang), type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste(get_text("file_error", lang), e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # 显示上传数据预览
  output$preview_table <- renderDT({
    req(uploaded_data())
    lang <- current_lang()
    
    # 根据语言选择 DataTables 语言设置
    dt_lang <- if (lang == "en") {
      list()  # 英文使用默认设置
    } else {
      list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Chinese.json")
    }
    
    # 根据语言构建 caption
    caption <- if (lang == "en") {
      paste(get_text("total_records", lang), nrow(uploaded_data()), get_text("records", lang))
    } else {
      paste(get_text("total_records", lang), nrow(uploaded_data()), get_text("records", lang))
    }
    
    datatable(
      uploaded_data(),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        language = dt_lang
      ),
      caption = caption
    )
  })
  
  # 上传状态
  output$upload_status <- renderText({
    lang <- current_lang()
    
    if (is.null(uploaded_data())) {
      get_text("please_upload", lang)
    } else {
      if (lang == "en") {
        paste(get_text("loaded_records", lang), nrow(uploaded_data()), 
              get_text("records", lang), ",", ncol(uploaded_data()), 
              get_text("variables", lang))
      } else {
        paste(get_text("loaded_records", lang), nrow(uploaded_data()), 
              get_text("records", lang), "，", ncol(uploaded_data()), 
              get_text("variables", lang))
      }
    }
  })
  
  # 批量预测按钮事件
  observeEvent(input$btn_batch_predict, {
    req(uploaded_data())
    lang <- current_lang()
    
    tryCatch({
      withProgress(message = get_text("processing", lang), value = 0, {
        # 获取上传的数据
        df <- uploaded_data()
        
        incProgress(0.2, detail = get_text("preprocessing", lang))
        
        # 预处理数据
        df_processed <- preprocess_data(df)
        
        incProgress(0.5, detail = get_text("model_predicting", lang))
        
        # 执行预测
        probs <- predict_dkd(lrn_xgboost, df_processed)
        
        incProgress(0.8, detail = get_text("generating_results", lang))
        
        # 创建结果数据框 - 根据语言设置列名
        if (lang == "en") {
          results <- data.frame(
            ID = 1:nrow(df),
            `DKD Probability` = round(probs * 100, 2),
            `Risk Level` = ifelse(probs >= THRESHOLD, "High Risk - DKD Likely", "Low Risk - Non-DKD Likely"),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        } else {
          results <- data.frame(
            序号 = 1:nrow(df),
            DKD概率 = round(probs * 100, 2),
            风险等级 = get_risk_level(probs),
            stringsAsFactors = FALSE
          )
        }
        
        # 合并原始数据
        results <- cbind(results, df[, selected_vars])
        
        prediction_results(results)
        
        incProgress(1, detail = get_text("done", lang))
      })
      
      showNotification(
        paste(get_text("predict_complete", lang), nrow(uploaded_data()), get_text("records", lang)),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste(get_text("predict_error", lang), e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # 显示预测结果表格
  output$results_table <- renderDT({
    req(prediction_results())
    lang <- current_lang()
    
    # 根据语言选择 DataTables 语言设置
    dt_lang <- if (lang == "en") {
      list()  # 英文使用默认设置
    } else {
      list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Chinese.json")
    }
    
    # 根据语言设置 caption
    caption <- if (lang == "en") {
      "Prediction Results"
    } else {
      "预测结果"
    }
    
    # 获取结果数据
    results <- prediction_results()
    
    # 根据语言选择风险等级列名和值
    risk_col <- if (lang == "en") "Risk Level" else "风险等级"
    prob_col <- if (lang == "en") "DKD Probability" else "DKD概率"
    high_risk_val <- if (lang == "en") "High Risk - DKD Likely" else "高风险 - DKD可能性大"
    low_risk_val <- if (lang == "en") "Low Risk - Non-DKD Likely" else "低风险 - 非DKD可能性大"
    
    dt <- datatable(
      results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = dt_lang
      ),
      caption = caption
    )
    
    # 只在列存在时应用格式化
    if (risk_col %in% names(results)) {
      dt <- dt %>%
        formatStyle(
          risk_col,
          backgroundColor = styleEqual(
            c(high_risk_val, low_risk_val),
            c("#ffcccc", "#ccffcc")
          )
        )
    }
    
    if (prob_col %in% names(results)) {
      dt <- dt %>%
        formatStyle(
          prob_col,
          background = styleColorBar(c(0, 100), "lightblue"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    }
    
    dt
  })
  
  # 下载模板文件
  output$download_template <- downloadHandler(
    filename = function() {
      "DKD_prediction_template.csv"
    },
    content = function(file) {
      # 创建模板数据框
      template <- data.frame(matrix(ncol = length(selected_vars), nrow = 2))
      names(template) <- selected_vars
      
      # 添加示例数据
      template[1, ] <- c(60, 35, 100, 1.2, 60, 7, 120, 7, 130, 5, 300, 1, 0, 0, 0, 0, 7)
      template[2, ] <- c(55, 38, 80, 1.0, 75, 6, 130, 6.5, 125, 4.5, 150, 0, 1, 1, 0, 0, 6)
      
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # 下载预测结果
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("DKD_prediction_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(prediction_results())
      write.csv(prediction_results(), file, row.names = FALSE)
    }
  )
  
  # =========== 单条预测页面 ===========
  
  output$single_ui <- renderUI({
    lang <- current_lang()
    
    # 根据语言获取选项标签
    gender_choices <- if(lang == "en") c("Female" = 0, "Male" = 1) else c("女" = 0, "男" = 1)
    yesno_choices <- if(lang == "en") c("No" = 0, "Yes" = 1) else c("无" = 0, "有" = 1)
    
    tagList(
      fluidRow(
        # 左侧：输入表单
        column(6,
               box(
                 title = get_text("basic_info", lang), status = "primary", solidHeader = TRUE,
                 width = 12, collapsible = TRUE,
                 fluidRow(
                   column(6, numericInput("inp_age", get_text("age", lang), value = 60, min = 18, max = 100)),
                   column(6, selectInput("inp_GENDER", get_text("GENDER", lang), choices = gender_choices, selected = 1))
                 )
               ),
               box(
                 title = get_text("renal_function", lang), status = "warning", solidHeader = TRUE,
                 width = 12, collapsible = TRUE,
                 fluidRow(
                   column(6, numericInput("inp_cr", get_text("cr", lang), value = 100, min = 0)),
                   column(6, numericInput("inp_cysc", get_text("cysc", lang), value = 1.2, min = 0, step = 0.1))
                 ),
                 fluidRow(
                   column(6, numericInput("inp_egfr_ckd_epi", get_text("egfr_ckd_epi", lang), value = 60, min = 0)),
                   column(6, numericInput("inp_bun", get_text("bun", lang), value = 7, min = 0, step = 0.1))
                 ),
                 numericInput("inp_uacr", get_text("uacr", lang), value = 300, min = 0, width = "50%")
               ),
               box(
                 title = get_text("metabolic", lang), status = "info", solidHeader = TRUE,
                 width = 12, collapsible = TRUE,
                 fluidRow(
                   column(6, numericInput("inp_glu", get_text("glu", lang), value = 7, min = 0, step = 0.1)),
                   column(6, numericInput("inp_hba1c", get_text("hba1c", lang), value = 7, min = 0, step = 0.1))
                 ),
                 fluidRow(
                   column(6, numericInput("inp_tcho", get_text("tcho", lang), value = 5, min = 0, step = 0.1)),
                   column(6, numericInput("inp_alb", get_text("alb", lang), value = 35, min = 0, step = 0.1))
                 )
               ),
               box(
                 title = get_text("hematology", lang), status = "success", solidHeader = TRUE,
                 width = 12, collapsible = TRUE,
                 fluidRow(
                   column(6, numericInput("inp_hb", get_text("hb", lang), value = 120, min = 0)),
                   column(6, numericInput("inp_sbp", get_text("sbp", lang), value = 130, min = 0))
                 )
               ),
               box(
                 title = get_text("complications", lang), status = "danger", solidHeader = TRUE,
                 width = 12, collapsible = TRUE,
                 fluidRow(
                   column(6, selectInput("inp_dia_per", get_text("dia_per", lang), choices = yesno_choices)),
                   column(6, selectInput("inp_dia_ret", get_text("dia_ret", lang), choices = yesno_choices))
                 ),
                 fluidRow(
                   column(6, selectInput("inp_nep_syndrome", get_text("nep_syndrome", lang), choices = yesno_choices)),
                   column(6, numericInput("inp_urbc", get_text("urbc", lang), value = 0, min = 0, step = 1))
                 )
               )
        ),
        # 右侧：预测结果
        column(6,
               box(
                 title = get_text("predict_result", lang), status = "primary", solidHeader = TRUE,
                 width = 12,
                 div(style = "text-align: center; padding: 20px;",
                     actionButton("btn_single_predict", get_text("start_predict", lang), 
                                  class = "btn-primary btn-lg", icon = icon("calculator"), width = "200px"),
                     hr(),
                     h4(get_text("dkd_risk", lang)),
                     div(class = "gauge-container", uiOutput("single_result_display")),
                     hr(),
                     uiOutput("single_risk_level"),
                     uiOutput("single_recommendation")
                 )
               ),
               box(
                 title = get_text("input_summary", lang), status = "info", solidHeader = TRUE,
                 width = 12, collapsible = TRUE, collapsed = TRUE,
                 tableOutput("input_summary")
               )
        )
      )
    )
  })
  
  # =========== 帮助页面 ===========
  
  output$help_ui <- renderUI({
    lang <- current_lang()
    
    tagList(
      fluidRow(
        box(
          title = get_text("system_intro", lang), status = "primary", solidHeader = TRUE,
          width = 12,
          p(get_text("system_intro_text1", lang)),
          p(get_text("system_intro_text2", lang))
        )
      ),
      fluidRow(
        box(
          title = get_text("var_explanation", lang), status = "info", solidHeader = TRUE,
          width = 6,
          h5(get_text("continuous_vars", lang)),
          tags$ul(
            tags$li(paste0(get_text("age", lang))),
            tags$li(paste0(get_text("alb", lang))),
            tags$li(paste0(get_text("cr", lang))),
            tags$li(paste0(get_text("cysc", lang))),
            tags$li(paste0(get_text("egfr_ckd_epi", lang))),
            tags$li(paste0(get_text("glu", lang))),
            tags$li(paste0(get_text("hb", lang))),
            tags$li(paste0(get_text("hba1c", lang))),
            tags$li(paste0(get_text("sbp", lang))),
            tags$li(paste0(get_text("tcho", lang))),
            tags$li(paste0(get_text("uacr", lang))),
            tags$li(paste0(get_text("bun", lang))),
            tags$li(paste0(get_text("urbc", lang)))
          )
        ),
        box(
          title = get_text("categorical_vars_title", lang), status = "warning", solidHeader = TRUE,
          width = 6,
          h5(get_text("binary_vars", lang)),
          tags$ul(
            tags$li(paste0(get_text("GENDER", lang), ": 0=", get_text("female", lang), ", 1=", get_text("male", lang))),
            tags$li(paste0(get_text("dia_per", lang), ": 0=", get_text("no", lang), ", 1=", get_text("yes", lang))),
            tags$li(paste0(get_text("dia_ret", lang), ": 0=", get_text("no", lang), ", 1=", get_text("yes", lang))),
            tags$li(paste0(get_text("nep_syndrome", lang), ": 0=", get_text("no", lang), ", 1=", get_text("yes", lang)))
          )
        )
      ),
      fluidRow(
        box(
          title = get_text("result_interpretation", lang), status = "success", solidHeader = TRUE,
          width = 12,
          p(strong(get_text("predict_prob", lang)), get_text("prob_desc", lang)),
          p(strong(get_text("classification_threshold", lang)), paste0(round(THRESHOLD * 100, 2), "%")),
          tags$ul(
            tags$li(span(style = "color: green;", strong(if(lang=="en") "Low Risk" else "低风险")), 
                    paste0(get_text("low_risk_desc", lang), round(THRESHOLD * 100, 2), get_text("low_risk_meaning", lang))),
            tags$li(span(style = "color: red;", strong(if(lang=="en") "High Risk" else "高风险")), 
                    paste0(get_text("high_risk_desc", lang), round(THRESHOLD * 100, 2), get_text("high_risk_meaning", lang)))
          ),
          hr(),
          p(style = "color: #999;", get_text("note", lang))
        )
      )
    )
  })
  
  # =========== 单条预测功能 ===========
  
  # 收集单条输入数据
  get_single_input <- reactive({
    data.frame(
      age = as.numeric(input$inp_age),
      alb = as.numeric(input$inp_alb),
      cr = as.numeric(input$inp_cr),
      cysc = as.numeric(input$inp_cysc),
      egfr_ckd_epi = as.numeric(input$inp_egfr_ckd_epi),
      glu = as.numeric(input$inp_glu),
      hb = as.numeric(input$inp_hb),
      hba1c = as.numeric(input$inp_hba1c),
      sbp = as.numeric(input$inp_sbp),
      tcho = as.numeric(input$inp_tcho),
      uacr = as.numeric(input$inp_uacr),
      GENDER = as.integer(input$inp_GENDER),
      dia_per = as.integer(input$inp_dia_per),
      dia_ret = as.integer(input$inp_dia_ret),
      nep_syndrome = as.integer(input$inp_nep_syndrome),
      urbc = as.numeric(input$inp_urbc),
      bun = as.numeric(input$inp_bun),
      stringsAsFactors = FALSE
    )
  })
  
  # 单条预测结果存储
  single_result <- reactiveVal(NULL)
  
  # 单条预测按钮事件
  observeEvent(input$btn_single_predict, {
    lang <- current_lang()
    
    tryCatch({
      # 获取输入数据
      df <- get_single_input()
      
      # 预处理
      df_processed <- preprocess_data(df)
      
      # 预测
      prob <- predict_dkd(lrn_xgboost, df_processed)
      
      single_result(prob)
      
      showNotification(get_text("single_predict_complete", lang), type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste(get_text("predict_error", lang), e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # 显示单条预测结果 - 概率
  output$single_result_display <- renderUI({
    lang <- current_lang()
    
    if (is.null(single_result())) {
      div(
        style = "color: #999; font-size: 24px;",
        icon("question-circle"),
        p(get_text("click_predict", lang))
      )
    } else {
      prob <- single_result()
      color <- get_risk_color(prob)
      
      div(
        class = "probability-display",
        style = paste0("color: ", color, ";"),
        paste0(round(prob * 100, 2), "%")
      )
    }
  })
  
  # 显示风险等级
  output$single_risk_level <- renderUI({
    req(single_result())
    lang <- current_lang()
    prob <- single_result()
    risk_level <- get_text(ifelse(prob >= THRESHOLD, "high_risk", "low_risk"), lang)
    color <- get_risk_color(prob)
    
    div(
      style = paste0("font-size: 20px; font-weight: bold; color: ", color, ";"),
      icon(ifelse(prob >= THRESHOLD, "exclamation-triangle", "check-circle")),
      risk_level
    )
  })
  
  # 显示建议
  output$single_recommendation <- renderUI({
    req(single_result())
    lang <- current_lang()
    prob <- single_result()
    
    if (prob >= THRESHOLD) {
      div(
        style = "margin-top: 20px; padding: 15px; background: #fff3cd; border-radius: 5px;",
        h5(icon("lightbulb"), " ", get_text("clinical_advice", lang)),
        p(get_text("high_risk_advice", lang)),
        tags$ul(
          tags$li(get_text("high_advice_1", lang)),
          tags$li(get_text("high_advice_2", lang)),
          tags$li(get_text("high_advice_3", lang)),
          tags$li(get_text("high_advice_4", lang))
        )
      )
    } else {
      div(
        style = "margin-top: 20px; padding: 15px; background: #d4edda; border-radius: 5px;",
        h5(icon("info-circle"), " ", get_text("clinical_advice", lang)),
        p(get_text("low_risk_advice", lang)),
        tags$ul(
          tags$li(get_text("low_advice_1", lang)),
          tags$li(get_text("low_advice_2", lang)),
          tags$li(get_text("low_advice_3", lang))
        )
      )
    }
  })
  
  # 显示输入数据汇总
  output$input_summary <- renderTable({
    lang <- current_lang()
    df <- get_single_input()
    
    # 获取变量标签
    var_names <- sapply(names(df), function(x) get_text(x, lang))
    
    summary_df <- data.frame(
      col1 = var_names,
      col2 = as.character(unlist(df[1, ])),
      stringsAsFactors = FALSE
    )
    names(summary_df) <- c(get_text("col_var", lang), get_text("col_value", lang))
    
    summary_df
  })
  
} # server 函数结束

# =============================================================================
# 5. 运行应用
# =============================================================================

shinyApp(ui = ui, server = server)
