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
                     "hba1c", "sbp", "tcho", "uacr", "bun")

# 分类变量（0/1编码）
categorical_vars <- c("GENDER", "dia_per", "dia_ret", "nep_syndrome", "urbc")

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
  urbc = "尿红细胞异常"
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
    sidebarMenu(
      id = "tabs",
      menuItem("欢迎页面", tabName = "welcome", icon = icon("home")),
      menuItem("批量预测", tabName = "batch", icon = icon("file-csv")),
      menuItem("单条预测", tabName = "single", icon = icon("user")),
      menuItem("使用说明", tabName = "help", icon = icon("question-circle"))
    ),
    br(),
    div(style = "padding: 15px;",
        p(style = "color: #b8c7ce; font-size: 12px;",
          "版本: v1.0"),
        p(style = "color: #b8c7ce; font-size: 12px;",
          paste("阈值:", round(THRESHOLD, 4)))
    )
  ),
  
  # 主体内容
  dashboardBody(
    # 自定义CSS样式
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
      "))
    ),
    
    tabItems(
      # =========== 欢迎页面 ===========
      tabItem(
        tabName = "welcome",
        fluidRow(
          column(12, align = "center",
                 div(style = "padding: 50px;",
                     h1(class = "welcome-title", 
                        icon("stethoscope"), " DKD-Predict"),
                     h3(class = "welcome-subtitle",
                        "基于机器学习的糖尿病肾病智能辅助诊断系统"),
                     hr(),
                     p(style = "font-size: 16px; max-width: 800px; margin: auto;",
                       "本系统基于 XGBoost 机器学习算法开发，旨在帮助临床医生快速判断",
                       "糖尿病合并慢性肾脏病（CKD）患者是否为糖尿病肾病（DKD）。"),
                     br()
                 )
          )
        ),
        fluidRow(
          column(4,
                 div(class = "feature-box",
                     h4(icon("file-csv", style = "color: #3c8dbc;"), " 批量预测"),
                     p("上传 CSV 文件，一次性预测多位患者的 DKD 风险概率。"),
                     actionButton("go_batch", "开始批量预测", 
                                  class = "btn-primary", width = "100%")
                 )
          ),
          column(4,
                 div(class = "feature-box",
                     h4(icon("user", style = "color: #00a65a;"), " 单条预测"),
                     p("手动输入单个患者的临床指标，获取个性化预测结果。"),
                     actionButton("go_single", "开始单条预测", 
                                  class = "btn-success", width = "100%")
                 )
          ),
          column(4,
                 div(class = "feature-box",
                     h4(icon("book", style = "color: #f39c12;"), " 使用说明"),
                     p("了解系统功能、输入变量说明及结果解读指南。"),
                     actionButton("go_help", "查看说明", 
                                  class = "btn-warning", width = "100%")
                 )
          )
        ),
        br(),
        fluidRow(
          column(12, align = "center",
                 div(style = "padding: 20px; background: #fff; border-radius: 5px; margin-top: 20px;",
                     h4("模型性能指标"),
                     p("最佳分类阈值: ", strong(round(THRESHOLD, 4))),
                     p(style = "color: #999; font-size: 12px;",
                       "免责声明：本系统仅供临床辅助参考，不能替代医生的专业诊断。")
                 )
          )
        )
      ),
      
      # =========== 批量预测页面 ===========
      tabItem(
        tabName = "batch",
        fluidRow(
          box(
            title = "上传患者数据", status = "primary", solidHeader = TRUE,
            width = 4,
            fileInput("file_upload", "选择 CSV 文件",
                      accept = c("text/csv", ".csv"),
                      buttonLabel = "浏览...",
                      placeholder = "未选择文件"),
            hr(),
            p(style = "font-size: 12px; color: #666;",
              "文件要求：CSV格式，包含以下17个变量列："),
            p(style = "font-size: 11px; color: #999;",
              paste(selected_vars, collapse = ", ")),
            hr(),
            downloadButton("download_template", "下载模板文件", 
                           class = "btn-info btn-block")
          ),
          box(
            title = "上传数据预览", status = "info", solidHeader = TRUE,
            width = 8,
            DTOutput("preview_table"),
            verbatimTextOutput("upload_status")
          )
        ),
        fluidRow(
          box(
            title = "预测结果", status = "success", solidHeader = TRUE,
            width = 12,
            actionButton("btn_batch_predict", "开始预测", 
                         class = "btn-success btn-lg", icon = icon("play")),
            downloadButton("download_results", "下载预测结果", 
                           class = "btn-primary"),
            hr(),
            DTOutput("results_table")
          )
        )
      ),
      
      # =========== 单条预测页面 ===========
      tabItem(
        tabName = "single",
        fluidRow(
          # 左侧：输入表单
          column(6,
                 box(
                   title = "基本信息", status = "primary", solidHeader = TRUE,
                   width = 12, collapsible = TRUE,
                   fluidRow(
                     column(6, numericInput("inp_age", var_labels["age"], 
                                            value = 60, min = 18, max = 100)),
                     column(6, selectInput("inp_GENDER", var_labels["GENDER"],
                                           choices = c("女" = 0, "男" = 1), 
                                           selected = 1))
                   )
                 ),
                 box(
                   title = "肾功能指标", status = "warning", solidHeader = TRUE,
                   width = 12, collapsible = TRUE,
                   fluidRow(
                     column(6, numericInput("inp_cr", var_labels["cr"], 
                                            value = 100, min = 0)),
                     column(6, numericInput("inp_cysc", var_labels["cysc"], 
                                            value = 1.2, min = 0, step = 0.1))
                   ),
                   fluidRow(
                     column(6, numericInput("inp_egfr_ckd_epi", var_labels["egfr_ckd_epi"], 
                                            value = 60, min = 0)),
                     column(6, numericInput("inp_bun", var_labels["bun"], 
                                            value = 7, min = 0, step = 0.1))
                   ),
                   numericInput("inp_uacr", var_labels["uacr"], 
                                value = 300, min = 0, width = "50%")
                 ),
                 box(
                   title = "代谢指标", status = "info", solidHeader = TRUE,
                   width = 12, collapsible = TRUE,
                   fluidRow(
                     column(6, numericInput("inp_glu", var_labels["glu"], 
                                            value = 7, min = 0, step = 0.1)),
                     column(6, numericInput("inp_hba1c", var_labels["hba1c"], 
                                            value = 7, min = 0, step = 0.1))
                   ),
                   fluidRow(
                     column(6, numericInput("inp_tcho", var_labels["tcho"], 
                                            value = 5, min = 0, step = 0.1)),
                     column(6, numericInput("inp_alb", var_labels["alb"], 
                                            value = 35, min = 0, step = 0.1))
                   )
                 ),
                 box(
                   title = "血液学及生命体征", status = "success", solidHeader = TRUE,
                   width = 12, collapsible = TRUE,
                   fluidRow(
                     column(6, numericInput("inp_hb", var_labels["hb"], 
                                            value = 120, min = 0)),
                     column(6, numericInput("inp_sbp", var_labels["sbp"], 
                                            value = 130, min = 0))
                   )
                 ),
                 box(
                   title = "并发症及其他", status = "danger", solidHeader = TRUE,
                   width = 12, collapsible = TRUE,
                   fluidRow(
                     column(6, selectInput("inp_dia_per", var_labels["dia_per"],
                                           choices = c("无" = 0, "有" = 1))),
                     column(6, selectInput("inp_dia_ret", var_labels["dia_ret"],
                                           choices = c("无" = 0, "有" = 1)))
                   ),
                   fluidRow(
                     column(6, selectInput("inp_nep_syndrome", var_labels["nep_syndrome"],
                                           choices = c("无" = 0, "有" = 1))),
                     column(6, selectInput("inp_urbc", var_labels["urbc"],
                                           choices = c("正常" = 0, "异常" = 1)))
                   )
                 )
          ),
          # 右侧：预测结果
          column(6,
                 box(
                   title = "预测结果", status = "primary", solidHeader = TRUE,
                   width = 12,
                   div(style = "text-align: center; padding: 20px;",
                       actionButton("btn_single_predict", "开始预测", 
                                    class = "btn-primary btn-lg", 
                                    icon = icon("calculator"),
                                    width = "200px"),
                       hr(),
                       h4("DKD 风险概率"),
                       div(class = "gauge-container",
                           uiOutput("single_result_display")
                       ),
                       hr(),
                       uiOutput("single_risk_level"),
                       uiOutput("single_recommendation")
                   )
                 ),
                 box(
                   title = "输入数据汇总", status = "info", solidHeader = TRUE,
                   width = 12, collapsible = TRUE, collapsed = TRUE,
                   tableOutput("input_summary")
                 )
          )
        )
      ),
      
      # =========== 使用说明页面 ===========
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            title = "系统介绍", status = "primary", solidHeader = TRUE,
            width = 12,
            p("DKD-Predict 是基于 XGBoost 机器学习算法开发的糖尿病肾病（DKD）辅助诊断系统。"),
            p("本系统面向糖尿病合并慢性肾脏病（CKD）的患者群体，帮助临床医生判断其是否为 DKD。")
          )
        ),
        fluidRow(
          box(
            title = "输入变量说明", status = "info", solidHeader = TRUE,
            width = 6,
            h5("连续变量（12个）"),
            tags$ul(
              tags$li("年龄 (age): 患者年龄，单位：岁"),
              tags$li("白蛋白 (alb): 血清白蛋白，单位：g/L"),
              tags$li("肌酐 (cr): 血肌酐，单位：μmol/L"),
              tags$li("胱抑素C (cysc): 血清胱抑素C，单位：mg/L"),
              tags$li("eGFR (egfr_ckd_epi): CKD-EPI公式计算，单位：mL/min/1.73m²"),
              tags$li("空腹血糖 (glu): 单位：mmol/L"),
              tags$li("血红蛋白 (hb): 单位：g/L"),
              tags$li("糖化血红蛋白 (hba1c): 单位：%"),
              tags$li("收缩压 (sbp): 单位：mmHg"),
              tags$li("总胆固醇 (tcho): 单位：mmol/L"),
              tags$li("UACR (uacr): 尿白蛋白肌酐比，单位：mg/g"),
              tags$li("尿素氮 (bun): 单位：mmol/L")
            )
          ),
          box(
            title = "分类变量说明", status = "warning", solidHeader = TRUE,
            width = 6,
            h5("二分类变量（5个）"),
            tags$ul(
              tags$li("性别 (GENDER): 0=女性, 1=男性"),
              tags$li("糖尿病周围神经病变 (dia_per): 0=无, 1=有"),
              tags$li("糖尿病视网膜病变 (dia_ret): 0=无, 1=有"),
              tags$li("肾病综合征 (nep_syndrome): 0=无, 1=有"),
              tags$li("尿红细胞异常 (urbc): 0=正常, 1=异常")
            )
          )
        ),
        fluidRow(
          box(
            title = "结果解读", status = "success", solidHeader = TRUE,
            width = 12,
            p(strong("预测概率："), "模型输出的 DKD 可能性概率（0-100%）"),
            p(strong("分类阈值："), paste0(round(THRESHOLD * 100, 2), "%")),
            tags$ul(
              tags$li(span(style = "color: green;", strong("低风险")), 
                      paste0("：概率 < ", round(THRESHOLD * 100, 2), "%，非DKD可能性较大")),
              tags$li(span(style = "color: red;", strong("高风险")), 
                      paste0("：概率 ≥ ", round(THRESHOLD * 100, 2), "%，DKD可能性较大"))
            ),
            hr(),
            p(style = "color: #999;", 
              "注意：本系统预测结果仅供临床参考，最终诊断需结合肾活检及其他临床资料综合判断。")
          )
        )
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
  
  # =========== 批量预测功能 ===========
  
  # 处理文件上传
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
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
          paste("警告：缺少以下变量：", paste(missing_vars, collapse = ", ")),
          type = "warning",
          duration = 10
        )
      }
      
      uploaded_data(df)
      
      showNotification("文件上传成功！", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste("文件读取错误：", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # 显示上传数据预览
  output$preview_table <- renderDT({
    req(uploaded_data())
    datatable(
      uploaded_data(),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Chinese.json")
      ),
      caption = paste("共", nrow(uploaded_data()), "条记录")
    )
  })
  
  # 上传状态
  output$upload_status <- renderText({
    if (is.null(uploaded_data())) {
      "请上传CSV文件"
    } else {
      paste("已加载", nrow(uploaded_data()), "条记录，", 
            ncol(uploaded_data()), "个变量")
    }
  })
  
  # 批量预测按钮事件
  observeEvent(input$btn_batch_predict, {
    req(uploaded_data())
    
    tryCatch({
      withProgress(message = "正在预测...", value = 0, {
        # 获取上传的数据
        df <- uploaded_data()
        
        incProgress(0.2, detail = "数据预处理中...")
        
        # 预处理数据
        df_processed <- preprocess_data(df)
        
        incProgress(0.5, detail = "模型预测中...")
        
        # 执行预测
        probs <- predict_dkd(lrn_xgboost, df_processed)
        
        incProgress(0.8, detail = "生成结果...")
        
        # 创建结果数据框
        results <- data.frame(
          序号 = 1:nrow(df),
          DKD概率 = round(probs * 100, 2),
          风险等级 = get_risk_level(probs),
          stringsAsFactors = FALSE
        )
        
        # 合并原始数据
        results <- cbind(results, df[, selected_vars])
        
        prediction_results(results)
        
        incProgress(1, detail = "完成！")
      })
      
      showNotification(
        paste("预测完成！共处理", nrow(uploaded_data()), "条记录"),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste("预测错误：", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # 显示预测结果表格
  output$results_table <- renderDT({
    req(prediction_results())
    
    datatable(
      prediction_results(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Chinese.json")
      ),
      caption = "预测结果"
    ) %>%
      formatStyle(
        "风险等级",
        backgroundColor = styleEqual(
          c("高风险 - DKD可能性大", "低风险 - 非DKD可能性大"),
          c("#ffcccc", "#ccffcc")
        )
      ) %>%
      formatStyle(
        "DKD概率",
        background = styleColorBar(c(0, 100), "lightblue"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
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
      urbc = as.integer(input$inp_urbc),
      bun = as.numeric(input$inp_bun),
      stringsAsFactors = FALSE
    )
  })
  
  # 单条预测结果存储
  single_result <- reactiveVal(NULL)
  
  # 单条预测按钮事件
  observeEvent(input$btn_single_predict, {
    tryCatch({
      # 获取输入数据
      df <- get_single_input()
      
      # 预处理
      df_processed <- preprocess_data(df)
      
      # 预测
      prob <- predict_dkd(lrn_xgboost, df_processed)
      
      single_result(prob)
      
      showNotification("预测完成！", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste("预测错误：", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  # 显示单条预测结果 - 概率
  output$single_result_display <- renderUI({
    if (is.null(single_result())) {
      div(
        style = "color: #999; font-size: 24px;",
        icon("question-circle"),
        p("点击上方按钮开始预测")
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
    prob <- single_result()
    risk_level <- get_risk_level(prob)
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
    prob <- single_result()
    
    if (prob >= THRESHOLD) {
      div(
        style = "margin-top: 20px; padding: 15px; background: #fff3cd; border-radius: 5px;",
        h5(icon("lightbulb"), " 临床建议"),
        p("该患者 DKD 可能性较高，建议："),
        tags$ul(
          tags$li("进一步完善相关检查"),
          tags$li("考虑肾活检以明确诊断"),
          tags$li("积极控制血糖、血压"),
          tags$li("定期监测肾功能变化")
        )
      )
    } else {
      div(
        style = "margin-top: 20px; padding: 15px; background: #d4edda; border-radius: 5px;",
        h5(icon("info-circle"), " 临床建议"),
        p("该患者 DKD 可能性较低，考虑非糖尿病肾病可能，建议："),
        tags$ul(
          tags$li("排查其他肾脏疾病病因"),
          tags$li("必要时行肾活检明确诊断"),
          tags$li("继续监测肾功能和尿蛋白")
        )
      )
    }
  })
  
  # 显示输入数据汇总
  output$input_summary <- renderTable({
    df <- get_single_input()
    
    summary_df <- data.frame(
      变量 = var_labels[names(df)],
      数值 = as.character(unlist(df[1, ])),
      stringsAsFactors = FALSE
    )
    
    summary_df
  })
  
} # server 函数结束

# =============================================================================
# 5. 运行应用
# =============================================================================

shinyApp(ui = ui, server = server)
