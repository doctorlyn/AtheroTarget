library(openxlsx)
library(shiny)
library(shinyFeedback)
library(readxl)
library(writexl)
library(DT)
library(shinyjs) 
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(shinyjs)

#--------UI布局--------
#--------标题--------
ui <- dashboardPage(
  skin = "blue",   # 整体主题色：可选 green, black, purple, yellow, red, blue
  dashboardHeader(
    title = HTML("AtheroTarget")
  ),
#--------分界面--------
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("输入界面",       tabName = "shuru",          icon = icon("edit")),
      menuItem("血症管理目标及治疗建议",       tabName = "shuchu",          icon = icon("table")),
      menuItem("常用药物查询表",     tabName = "interpretation",  icon = icon("search")),
      menuItem("版本信息",     tabName = "banbenxinxi",  icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    useShinyFeedback(), 
    useShinyjs(),
    #----全局样式微调----
    tags$head(tags$style(HTML("
    .box { box-shadow: 0 1px 3px rgba(0,0,0,0.2); }
    .action-button { width: 100%; }

    /* ↓↓↓ 新增 ↓↓↓ */
    .main-sidebar .sidebar-menu li a {
      color: #FFFFFF !important;
      font-weight: bold !important;
    }
    .main-sidebar .sidebar-menu li.active a,
    .main-sidebar .sidebar-menu li:hover a {
      color: #FFFFFF !important;
    }
    .box .box-header .box-title {
      color: #000000 !important;
      font-weight: bold !important;
    }
    /* 全部 box 的 title 居中 */
    .box .box-header .box-title {
      display: block !important;
      width: 100% !important;
      text-align: center !important;
    }
     /* primary 状态（默认深蓝）→ 深紫 */
    .box.box-primary > .box-header {
      background-color: #4F94CD !important;
      border-color: #4F94CD !important;
    }
    /* success 状态（默认绿）→ 深绿色 */
    .box.box-success > .box-header {
      background-color: #43CD80 !important;
      border-color: #43CD80 !important;
    }
    /* info 状态（默认青蓝）→ 青绿色 */
    .box.box-info > .box-header {
      background-color: #BA88BF !important;
      border-color: #BA88BF !important;
    }
    /* warning 状态（默认橙）→ 金色 */
    .box.box-warning > .box-header {
      background-color: #FFC30B !important;
      border-color: #FFC30B !important;
    }
    /* danger 状态（默认红）→ 粉红 */
    .box.box-danger > .box-header {
      background-color: #EE2C2C !important;
      border-color: #EE2C2C !important;
    }
  
  "))),
#-----所有界面布局------
    tabItems(
      # ---- 输入界面 ----
      tabItem(
        tabName = "shuru",
        
        # 基本信息
        fluidRow(
          column(2,
                 numericInput("age", "年龄（岁）",
                              value = 50, min = 1, max = 120)
          ),
          column(2,
                 selectInput("gender", "性别",
                             choices = c("男", "女"), selected = "男")
          ),
          column(2,
                 selectInput("hypertension", "高血压",
                             choices = c("是", "否", "不详"), selected = "否")
          ),
          column(2,
                 selectInput("diabetes", "糖尿病",
                             choices = c("是", "否", "不详"), selected = "否")
          ),
          column(2,
                 selectInput("type1diabetes", "1型糖尿病≥20年",
                             choices = c("是", "否", "不详"), selected = "否")
          ),
          column(2,
                 selectInput("smoking", "吸烟",
                             choices = c("是", "否", "不详"), selected = "否")
          )
        ),
        
        # 当高血压 = “是” 时，才显示以下两项
        conditionalPanel(
          condition = "input.hypertension == '是'",
          fluidRow(
            column(2,
                   numericInput("sbp", "收缩压（mmHg）",
                                value = 140, min = 100, max = 300)
            ),
            column(2,
                   numericInput("dbp", "舒张压（mmHg）",
                                value = 90, min = 30, max = 200)
            )
          )
        ),
  
        
        # 临床伴随病史
        fluidRow(
          box(
            title = "临床病史", status = "success", solidHeader = TRUE, width = 12,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "ckd",
                  "慢性肾衰竭3-5期（eGFR < 60mL/min/1.73m^2）",
                  choices = c("是", "否", "不详"),
                  selected = "否"
                )
              ),
              column(
                width = 2,
                # 用 div + margin-top 调整按钮垂直位置
                div(
                  style = "margin-top: 25px;",
                  actionButton(
                    "show_egfr_box",
                    label = tagList(icon("calculator"), "eGFR计算器"),
                    class = "btn-primary btn-block",
                    style = "font-size:16px; color:#FFFFFF;"
                  )
                )
              ),
              column(
                width = 4,
                selectInput(
                  "dialysis",
                  "透析状态",
                  choices = c("是", "否", "不详"),
                  selected = "否"
                )
              )
            ),
     
            conditionalPanel(
              condition = "input.show_egfr_box > 0",
              fluidRow(
                box(
                  title       = "eGFR 计算（简化 MDRD 公式）",
                  status      = "success",
                  solidHeader = TRUE,
                  width       = 12,
                  
                  # 输入肌酐 & 性别 & 立即计算按钮
                  fluidRow(
                    column(
                      width = 4,
                      numericInput(
                        "scr_umol",
                        "血清肌酐 (μmol/L)",
                        value = 80, min = 10, max = 2000, step = 1
                      )
                    ),
                    column(
                      width = 2,
                      div(
                        style = "margin-top: 25px;",
                        actionButton(
                          "calc_egfr",
                          label = tagList(icon("calculator"), "立即计算 eGFR"),
                          class = "btn-primary btn-block",
                          style = "font-size:16px; color:#FFFFFF;"
                        )
                      )
                    ),
                    column(
                      width = 4,
                      numericInput(
                        "egfr",
                        "eGFR (mL/min/1.73m²)",
                        value = NA,
                        min = 0, max = 200,
                        step = 0.1
                      )
                    )
                  )
      
                )
              )
            ),
            fluidRow(
              column(4,
                     selectInput("ascvd", "ASCVD",
                                 choices = c("是", "否", "不详"), selected = "不详")
              ),
              column(8,
                     div(style = "font-size:12px; color:#777; margin-top:25px;",
                         HTML("ASCVD包含临床诊断或影像学明确。<br>
                                临床诊断包括急性冠脉综合征、稳定性心绞痛、冠状动脉支架或搭桥术后、缺血性卒中、一过性脑缺血发作以及外周动脉疾病，其中外周动脉疾病主要指因外周动脉硬化引起的颈动脉、上肢或下肢动脉、肾动脉、肠系膜动脉以及多部位血管疾病。<br>
                                影像学确定指放射学或超声提示的血管明显斑块（<span style='color: red; font-weight: bold;'>狭窄＞50%</span>）")
                     )
              )
            ),
            
            
            # 如果有 ASCVD，展开详细项
            conditionalPanel(
              condition = "input.ascvd == '是'",
              
              # 严重事件
              box(
                title       = tagList(icon("exclamation-triangle"), "严重 ASCVD 事件"),
                status      = "danger", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(3, selectInput("recent_acs", "近1年发生急性冠脉综合征", choices = c("是","否","不详"), selected = "不详")),
                  column(3, selectInput("prior_mi",   "既往心肌梗死（>1年）",   choices = c("是","否","不详"), selected = "不详")),
                  column(3, selectInput("stroke_history","缺血性卒中史",    choices = c("是","否","不详"),selected = "不详")),
                  column(3, selectInput("pvd",        "有症状的周围血管病变/血运重建/截肢", choices = c("是","否","不详"),selected = "不详"))
                ),
                div(style="font-size:12px;color:#777;margin-top:10px;",
                    HTML("急性冠脉综合征指不稳定型心绞痛、急性心肌梗死；<br/>外周血管病变症状指间歇性跛行、腹痛、肾梗死等。"))
              ),
              
              # 高危因素
              box(
                title       = tagList(icon("exclamation-circle"), "高危因素"),
                status      = "warning", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(3,
                         selectInput("ldl_recurrent", "LDL ≤1.8 mmol/L 时再发严重ASCVD事件",
                                     choices = c("是","否","不详"),selected = "不详")
                  ),
                  column(3,
                         selectInput("early_cad", "早发冠心病（男<55, 女<65）",
                                     choices = c("是","否","不详"),selected = "不详")
                  ),
                  column(3,
                         selectInput("fh_high_ldl", "家族性高胆固醇血症或基线 LDL≥4.9",
                                     choices = c("是","否","不详"),selected = "不详")
                  ),
                  column(3,
                         selectInput("prior_revascularization", "既往行冠状动脉支架/搭桥",
                                     choices = c("是","否","不详"),selected = "不详")
                  )
                )
              )
            )
            
          )
        ),
        
        # 血脂 & 体格
        fluidRow(
          box(
            title = "血脂指标 & 体格指标", status = "info", solidHeader = TRUE, width = 12,
            
            # 若非 ASCVD 显示血脂和体格，否则隐藏
            conditionalPanel(
              condition = "input.ascvd != '是'",
              h4("血脂指标"),
              fluidRow(
                column(4, numericInput("tc",  "总胆固醇 (mmol/L)",  value = 5.0, min = 0.5, max = 30, step = 0.1)),
                column(4, numericInput("ldl", "低密度脂蛋白 (mmol/L)", value = 3.0, min = 0.01, max = 30, step = 0.1)),
                column(4, numericInput("hdl", "高密度脂蛋白 (mmol/L)", value = 1.2, min = 0.01, max = 30, step = 0.1))
              ),
              h4("体格指标"),
              fluidRow(
                column(6, numericInput("height", "身高 (cm)", value = 170, min = 100, max = 300)),
                column(6, numericInput("weight", "体重 (kg)", value = 65,  min = 20,  max = 200))
              )
            )
          )
        ),
        fluidRow(
          column(width = 4, offset = 4,
                 actionButton(
                   "calculate",
                   label = tagList(icon("calculator"), "计算风险"),
                   class = "btn-primary btn-lg",
                   style = paste(
                     "width: 100%;",
                     "font-size: 22px;",
                     "color: #FFFFFF;",
                     "display: flex;",
                     "justify-content: center;",
                     "align-items: center;"
                   )
                 )
          )
        ),
        # 新增一个重置按钮
        fluidRow(
          column(width = 4, offset = 4,
                 actionButton(
                   "reset",
                   label = tagList(icon("undo"), "恢复默认值"),
                   class = "btn-success btn-lg",
                   style = paste(
                     "width: 100%;",
                     "font-size: 22px;",
                     "color: #FFFFFF;",
                     "display: flex;",
                     "justify-content: center;",
                     "align-items: center; margin-top:10px;"
                   )
                 )
          )
        )
        
      ),  # end tabItem("shuru")
      #-----血症管理目标及治疗建议-----
      tabItem(
        tabName = "shuchu",
        
        # 第一行：风险分层 & LDL-c 目标
        fluidRow(
          box(
            title       = tagList(icon("chart-line"), "ASCVD 危险分层"),
            status      = "primary",
            solidHeader = TRUE,
            width       = 6,
            uiOutput("riskStratText")
          ),box(
            title      = tagList(icon("bullseye"), "LDL-c 控制目标"),
            status     = "primary", solidHeader = TRUE, width = 6,
            # 例如一段文字或 htmlOutput
            uiOutput("ldlTargetText")
          )),
  
        # 第二行：警告 & 治疗建议
        fluidRow(
          box(
            title      = tagList(icon("stethoscope"), "治疗建议"),
            status     = "success", solidHeader = TRUE, width = 12,
            # 放一组建议项
            uiOutput("treatmentAdvice")
          )),
        fluidRow(
          box(
            title      = tagList(icon("exclamation-triangle"), "警告"),
            status     = "danger", solidHeader = TRUE, width = 12,
            # 例如警告列表
            uiOutput("warningsUI")
          )),
        fluidRow(
          column(width = 4, offset = 4,
                 actionButton(
                   "backToInput",
                   label = tagList(icon("arrow-left"), "返回输入界面"),
                   class = "btn-success btn-lg",
                   style = paste(
                     "width: 100%;",
                     "font-size: 22px;",
                     "color: #FFFFFF;",
                     "display: flex;",
                     "justify-content: center;",
                     "align-items: center; margin-top:20px;"
                   )
                 )
          )
        )
        
      ),
      # ---常用药物查询表----
      tabItem(
        tabName = "interpretation",
        fluidRow(
          box(
            title       = tags$h4(
              "常用降胆固醇药物查询表",
              style = "font-weight:bold; font-size:24px; color:#FFFFFF; text-align:center;"
            ),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            # 滚动容器
            div(
              style = "overflow-x:auto; overflow-y:auto; max-height:600px; padding:5px;",
              tableOutput("excelTable")
            )
          )
        )
      ),
      # ---版本信息----
      tabItem(
        tabName = "banbenxinxi",
        fluidRow(
          box(
            width       = 12,
            solidHeader = TRUE,
            status      = "info",
            class       = "version-box",
            
            # 自定义 header 区块
            tags$div(" ", class = "version-header"),
            
            # 卡片主体
            tags$div(
              class = "box-body",
              
              # 版本号 + 作者
              tags$div(
                class = "version-meta",
                tags$p(
                  HTML("<strong>版本：</strong>AtheroTarget-低密度脂蛋白管理与心血管风险控制精准平台(V1.2)"),
                  style = "font-size:22px; font-weight:bold;"
                ),
                tags$p(
                  HTML("<strong>作者：</strong>刀客特lin"),
                  style = "font-size:22px; font-weight:bold;"
                )
              ),
              
              # 更新内容
              tags$div(
                class = "version-updates",
                tags$h4("V1.2更新内容：", style = "font-size:18px; font-weight:bold; color:#314A43;"),
                tags$ul(
                  tags$li("根据ESC 2025血脂指南，对ASCVD的定义做了一定的校正；"),
                  tags$li("重置了ASCVD输入界面的一些默认值，同于提高输入效率"),
                  tags$li("设定了胆固醇、低密度脂蛋白和高密度脂蛋白的反馈逻辑，防止出现不合理的数值"),
                  tags$li("输入界面增了一个按钮，用于返回初始默认值，以便于再次输入数值"),
                  tags$li("在结果页面添加了一个按钮返回输入界面，方便手机平台操作")
                ),
                tags$h4("V1.1更新内容：", style = "font-size:18px; font-weight:bold; color:#314A43;"),
                tags$ul(
                  tags$li("全面优化了 UI，使整体界面更加美观、现代，不同的危险分层以不同颜色显示；"),
                  tags$li("增加了Feedback功能及控制逻辑，当输入数值不符合判定逻辑时，无法进行计算；"),
                  tags$li("对年轻糖尿病患者的LDL-c管理按照指南特定章节进行了更新"),
                  tags$li("增加了常用药物的用法用量供专业人士参考"),
                  tags$li("添加了一个内置的eGFR计算器，并与CKD的输入信息进行了自动匹配"),
                  tags$li("修复了一些Bug")
                )
              ),
              
              # 版权 & 警示
              tags$div(
                class = "version-footer",
                "本软件使用人工智能方法协助专业人士进行动脉粥样硬化性心血管疾病（ASCVD）危险分层，并提供低密度脂蛋白的控制目标。现免费使用，禁止以盈利为目的转载!如遇 Bug，请联系 xwlok123@126.com"
              )
            )
          )
        )
      )
    )    # end tabItems
  )      # end dashboardBody
)        # end dashboardPage 
    
    

#----------server界面----------    
server <- function(input, output, session) {
  observe({
    # -------输入校验-------
    # 1. 年龄
    warn_age <- is.na(input$age) || input$age < 0 || input$age > 120
    feedbackWarning("age", warn_age, "年龄必须在0-120岁之间")
    
    # 2. 收缩压
    warn_sbp <- is.na(input$sbp) || input$sbp < 100 || input$sbp > 300
    feedbackWarning("sbp", warn_sbp, "收缩压必须在100-300mmHg之间")
    
    # 3. 舒张压范围
    warn_dbp_range <- is.na(input$dbp) || input$dbp < 30 || input$dbp > 200
    feedbackWarning("dbp", warn_dbp_range, "舒张压必须在30-200mmHg之间")
    
    # 4. 舒张压必须小于收缩压
    warn_dbp_order <- !is.na(input$sbp) && !is.na(input$dbp) && input$dbp >= input$sbp
    feedbackWarning("dbp", warn_dbp_order, "舒张压必须小于收缩压")
    
    # 5. 1型糖尿病校验
    warn_t1d <- !is.null(input$type1diabetes) &&
      input$type1diabetes == "是" &&
      input$diabetes       != "是"
    feedbackWarning(
      "type1diabetes",
      warn_t1d,
      "若选择“是”，则“糖尿病”必须为“是”"
    )
    
    # 6. 非ASCVD 时的其他校验
    warn_other <- FALSE
    if (!is.null(input$ascvd) && input$ascvd != "是") {
      tc_val  <- input$tc
      ldl_val <- input$ldl
      hdl_val <- input$hdl
      
      warn_tc  <- is.na(tc_val)  || tc_val  < 0.5  || tc_val  > 30
      warn_ldl <- is.na(ldl_val) || ldl_val < 0.01 || ldl_val > 30 ||
        (!is.na(tc_val) && !is.na(ldl_val) && ldl_val >= tc_val)
      warn_hdl <- is.na(hdl_val) || hdl_val < 0.01 || hdl_val > 30 ||
        (!is.na(tc_val) && !is.na(hdl_val) && hdl_val >= tc_val)
      
      warn_ht  <- is.na(input$height)   || input$height < 100 || input$height > 300
      warn_wt  <- is.na(input$weight)   || input$weight < 20  || input$weight > 200
      warn_cr  <- is.na(input$scr_umol) || input$scr_umol < 10 || input$scr_umol > 2000
      
      feedbackWarning("tc",   warn_tc,  "总胆固醇必须在 0.5–30 mmol/L 之间")
      feedbackWarning("ldl",  warn_ldl, "低密度脂蛋白必须在 0.01–30 mmol/L 之间，且小于总胆固醇（TC）")
      feedbackWarning("hdl",  warn_hdl, "高密度脂蛋白必须在 0.01–30 mmol/L 之间，且小于总胆固醇（TC）")
      feedbackWarning("height", warn_ht, "身高必须在 100–300 cm 之间")
      feedbackWarning("weight", warn_wt, "体重必须在 20–200 kg 之间")
      feedbackWarning("scr_umol", warn_cr, "血清肌酐必须在 10–2000 μmol/L 之间")
      
      # 汇总一次，供按钮禁用逻辑使用
      warn_other <- warn_tc || warn_ldl || warn_hdl || warn_ht || warn_wt || warn_cr
    }
    
    # 7. 任一警告时禁用按钮，否则启用
    any_warn <- warn_age || warn_sbp || warn_dbp_range ||
      warn_dbp_order || warn_t1d || warn_other
    
    if (any_warn) {
      shinyjs::disable("calculate")
    } else {
      shinyjs::enable("calculate")
    }
  })
  #------返回按钮----------
  observeEvent(input$reset, {
    # 恢复默认值
    updateNumericInput(session, "age",  value = 50)
    updateSelectInput(session, "gender", selected = "男")
    updateSelectInput(session, "hypertension", selected = "否")
    updateNumericInput(session, "sbp",  value = 140)
    updateNumericInput(session, "dbp",  value = 90)
    updateSelectInput(session, "diabetes", selected = "否")
    updateSelectInput(session, "smoking",  selected = "否")
    updateSelectInput(session, "type1diabetes", selected = "否")
    updateSelectInput(session, "ascvd", selected = "不详")
    updateSelectInput(session, "ckd",   selected = "否")
    updateSelectInput(session, "dialysis", selected = "否")
    
    # 血脂 & 体格
    updateNumericInput(session, "tc",  value = 5.0)
    updateNumericInput(session, "ldl", value = 3.0)
    updateNumericInput(session, "hdl", value = 1.2)
    updateNumericInput(session, "height", value = 170)
    updateNumericInput(session, "weight", value = 65)
    updateNumericInput(session, "scr_umol", value = 80)
    updateNumericInput(session, "egfr",    value = NA)
    
    # ASCVD详细项
    updateSelectInput(session, "recent_acs", selected = "不详")
    updateSelectInput(session, "prior_mi",   selected = "不详")
    updateSelectInput(session, "stroke_history", selected = "不详")
    updateSelectInput(session, "pvd", selected = "不详")
    updateSelectInput(session, "ldl_recurrent", selected = "不详")
    updateSelectInput(session, "early_cad", selected = "不详")
    updateSelectInput(session, "fh_high_ldl", selected = "不详")
    updateSelectInput(session, "prior_revascularization", selected = "不详")
  })
  #----------输出界面返回输入界面-----------
  #返回输入界面
  observeEvent(input$backToInput, {
    updateTabItems(session, "tabs", selected = "shuru")
  })
  #-----CKD的自动计算--------
  observeEvent(input$calc_egfr, {
    # 先把 μmol/L → mg/dL：除以 88.4
    scr_mgdl <- input$scr_umol / 88.4
    # 简化 MDRD 公式
    egfr_val <- 186 *
      (scr_mgdl ^ -1.154) *
      (input$age    ^ -0.203) *
      ifelse(input$gender == "女", 0.742, 1.000)
    # 更新到 egfr 输入框
    updateNumericInput(session,
                       "egfr",
                       value = round(egfr_val, 1))
    # 根据 eGFR 自动更新 CKD 选择
    updateSelectInput(
      session, "ckd",
      selected = if (egfr_val < 60) "是" else "否"
    )
  })
  #-------逻辑判断-------
  riskCategory <- reactiveVal(NULL)
  ##--------有ASCVD-------
  observeEvent(input$calculate, {
    # 先声明这两个变量，避免在某些分支下未定义
    initial_cat <- NULL
    note        <- ""
    
    # —— 处理 ASCVD = “是” 情况 —— 
    if (!is.null(input$ascvd) && input$ascvd == "是") {
      # 严重事件 + 高危因素 分层逻辑
      events_yes <- sum(
        input$recent_acs       == "是",
        input$prior_mi         == "是",
        input$stroke_history   == "是",
        input$pvd              == "是",
        na.rm = TRUE
      )
      events_no <- sum(
        input$recent_acs       == "否",
        input$prior_mi         == "否",
        input$stroke_history   == "否",
        input$pvd              == "否",
        na.rm = TRUE
      )
      
      factors_yes <- sum(
        input$ldl_recurrent            == "是",
        input$early_cad               == "是",
        input$fh_high_ldl             == "是",
        input$prior_revascularization == "是",
        input$diabetes                == "是",
        input$smoking                 == "是",
        input$hypertension            == "是",
        input$ckd                      == "是",
        na.rm = TRUE
      )
      factors_no <- sum(
        input$ldl_recurrent            == "否",
        input$early_cad               == "否",
        input$fh_high_ldl             == "否",
        input$prior_revascularization == "否",
        input$diabetes                == "否",
        input$smoking                 == "否",
        input$hypertension            == "否",
        input$ckd                      == "否",
        na.rm = TRUE
      )
      
      cond1 <- (events_yes >= 2) ||
        (events_yes >= 1 && factors_yes >= 2)
      cond2 <- (events_no == 4) ||
        (events_yes == 1 && events_no == 3 && factors_no >= 7)
      
      if (cond1) {
        initial_cat <- "超高危"
      } else if (cond2) {
        initial_cat <- "确定的极高危"
      } else {
        initial_cat <- "可能的极高危"
      }
      
    } 
    ##--------有ASCVD--------
    ##--------无ASCVD--------
    
    else {
      # —— ASCVD ≠ “是” 时，先直通高危判定 —— 
      if ((input$tc  >= 7.2) ||
          (input$ldl >= 4.9) ||
          (input$diabetes == "是" && input$age >= 40) ||(input$type1diabetes == "是")||
          (input$ckd      == "是")||
          (input$diabetes == "是" &&
              input$age < 40 &&
              input$hypertension == "是" &&
              input$smoking == "是" &&
              !is.na(input$hdl) &&
              input$hdl < 1.0)) {
        initial_cat <- "高危"
        
      } else {
        # 高血压+危险因素+胆固醇分档
        hasHighBP <- (input$hypertension == "是")
        
        riskFactors <- 0
        if (input$smoking == "是")                    riskFactors <- riskFactors + 1
        if (!is.na(input$hdl) && input$hdl < 1.0)     riskFactors <- riskFactors + 1
        if ((input$gender == "男" && input$age >= 45) ||
            (input$gender == "女" && input$age >= 55)) {
          riskFactors <- riskFactors + 1
        }
        
        TC  <- input$tc
        LDL <- input$ldl
        tc_band <- if      (TC >= 3.1 && TC < 4.1) 1
        else if (TC >= 4.1 && TC < 5.2) 2
        else if (TC >= 5.2 && TC < 7.2) 3
        else NA
        ldl_band <- if      (LDL >= 1.8 && LDL < 2.6) 1
        else if (LDL >= 2.6 && LDL < 3.4) 2
        else if (LDL >= 3.4 && LDL < 4.9) 3
        else NA
        cholesterolBand <- if (all(is.na(tc_band), is.na(ldl_band))) NA
        else max(tc_band, ldl_band, na.rm = TRUE)
        
        if (is.na(hasHighBP) ||
            is.na(riskFactors) ||
            is.na(cholesterolBand)) {
          initial_cat <- "信息不完整"
        } else if (!hasHighBP) {
          if (riskFactors <= 1) {
            initial_cat <- "低危"
          } else if (riskFactors == 2) {
            initial_cat <- if (cholesterolBand == 3) "中危" else "低危"
          } else {
            initial_cat <- if (cholesterolBand %in% c(2,3)) "中危" else "低危"
          }
        } else {
          if (riskFactors == 0) {
            initial_cat <- "低危"
          } else if (riskFactors == 1) {
            initial_cat <- if (cholesterolBand %in% c(2,3)) "中危" else "低危"
          } else if (riskFactors == 2) {
            initial_cat <- if (cholesterolBand == 1) "中危" else "高危"
          } else {
            initial_cat <- "高危"
          }
        }
      }
      
      # —— 中危且年龄 < 55 的“余生危险”备注 —— 
      if (initial_cat == "中危" && input$age < 55) {
        extra_factors <- 0
        if ((!is.null(input$sbp) && input$sbp >= 160) ||
            (!is.null(input$dbp) && input$dbp >= 100)) extra_factors <- extra_factors + 1
        if (!is.na(input$tc) && !is.na(input$hdl) &&
            (input$tc - input$hdl) >= 5.2)                         extra_factors <- extra_factors + 1
        if (!is.na(input$hdl) && input$hdl < 1.0)                  extra_factors <- extra_factors + 1
        if (!is.na(input$height) && !is.na(input$weight)) {
          bmi <- input$weight / ((input$height / 100)^2)
          if (bmi >= 28) extra_factors <- extra_factors + 1
        }
        if (input$smoking == "是")                                 extra_factors <- extra_factors + 1
        
        if (extra_factors >= 2) {
          note <- "；\n余生危险风险为高危"
        }
      }
    }
    
    #category <- paste0(initial_cat, note)
    #----------输出界面-------------
    #----------输出界面危险分层-----------
    # 跳转并渲染彩色结果 c("#CD2626", "#FFFFFF", "#009ACD" c("#C71585", "#FFFFFF", "#FFFFFF"))
    updateTabItems(session, "tabs", selected = "shuchu")
    output$riskStratText <- renderUI({
      req(riskCategory())
      cat      <- initial_cat
      noteLine <- nzchar(note)
      
      # 同步分层配色和图标
      style_map <- list(
        "低危"         = list(color = "#69b3a2", icon = "smile"             ),
        "中危"         = list(color = "#009ACD", icon = "meh"               ),
        "高危"         = list(color = "goldenrod", icon = "frown"           ),
        "确定的极高危" = list(color = "#CD2626", icon = "exclamation-circle"),
        "可能的极高危" = list(color = "#CD2626", icon = "question-circle"   ),
        "超高危"       = list(color = "#C71585", icon = "skull-crossbones"  )
      )
      cfg <- style_map[[cat]]
      if (is.null(cfg)) cfg <- list(color = "light-blue", icon = "question")
      
      tags$div(
        style = paste0(
          "background:",     cfg$color, ";",
          "color: white;      ",
          "padding: 20px;     ",
          "border-radius: 6px;",
          "text-align: center;"
        ),
        # 主标题
        tags$h3(
          icon(cfg$icon), " ", cat,
          style = "margin:0; font-weight:bold; font-size:2em;"
        ),
        # 如果有“余生危险”备注，则加一行红色警示
        if (noteLine) {
          tags$p(
            icon("exclamation-triangle"), "余生危险风险为高危",
            style = "color: red; font-weight:bold; margin-top:10px;"
          )
        }
      )
    })
    
    riskCategory(initial_cat)
    #---------输出界面：血脂目标--------
    output$ldlTargetText <- renderUI({
      req(riskCategory())
      cat <- riskCategory()
      
      # 同步分层配色和图标
      style_map <- list(
        "低危"         = list(color = "#69b3a2"),
        "中危"         = list(color = "#009ACD"),
        "高危"         = list(color = "goldenrod"),
        "确定的极高危" = list(color = "#CD2626"),
        "可能的极高危" = list(color = "#CD2626"),
        "超高危"       = list(color = "#C71585")
      )
      cfg <- style_map[[cat]]
      if (is.null(cfg)) cfg <- list(color = "light-blue", icon = "question")
      
      # 根据分类和糖尿病状态选出 LDL-c 控制目标文本
      target_text <- if (input$diabetes == "是") {
        if (cat %in% c("低危", "中危")) {
          "因合并糖尿病，因此控制目标：< 2.6 mmol/L"
        } else if (cat == "高危") {
          "因合并糖尿病，因此控制目标：< 1.8 mmol/L"
        } else {
          "因合并糖尿病，因此控制目标：< 1.4 mmol/L 且下降 > 50%"
        }
      } else {
        if (cat == "低危") {
          HTML("理想目标：< 2.6 mmol/L<br/>  合适目标：< 3.4 mmol/L")
        } else if (cat %in% c("中危", "高危")) {
          "< 2.6 mmol/L"
        } else if (cat %in% c("确定的极高危", "可能的极高危")) {
          "< 1.8 mmol/L 且下降 > 50%"
        } else {
          "< 1.4 mmol/L 且下降 > 50%"
        }
      }
      
      # 渲染一个彩色条块，白色加粗居中文字，并配一个图标
      tags$div(
        style = paste(
          "background:",    cfg$color,    ";",
          "color: white;      ",
          "padding: 20px;     ",
          "border-radius: 6px;",
          "text-align: center;"
        ),
        tags$h3(
          icon(cfg$icon), " ",
          target_text,
          style = "margin:0; font-weight:bold;"
        )
      )
    })
    
    #---------警告界面---------
    output$warningsUI <- renderUI({
      # 1. 生成消息列表
      msgs <- list(
        "患者ASCVD危险分层及LDL-c控制目标仅供参考，具体情况需结合患者临床表现（如存在典型心绞痛临床症状）及其他辅助检查（如冠脉钙化积分>300），在专业医师指导下制定治疗方案。"
      )
      
      # 2. “不详” 警告：仅在 (ASCVD=是 且 “可能的极高危”) 或 ASCVD≠是 时
      cond_unclear <- (
        (!is.null(input$ascvd) && input$ascvd == "是" && initial_cat == "可能的极高危") ||
          (is.null(input$ascvd) || input$ascvd != "是")
      )
      hist_fields <- c(
        input$diabetes,
        input$smoking,
        input$ckd,
        input$hypertension,
        input$ascvd,
        input$type1diabetes,
        input$recent_acs,
        input$prior_mi,
        input$stroke_history,
        input$pvd,
        input$ldl_recurrent,
        input$early_cad,
        input$fh_high_ldl,
        input$prior_revascularization
        )
      if (cond_unclear && any(hist_fields == "不详")) {
        msgs <- c(msgs,
                  "检测到患者的部分病史为“不详”，目前评估仅为初步评估，若需更加精确的判断，需明确“不详”的病史，或根据临床表现进一步完善相关检查。"
        )
      }
      
      # 3. 透析警告：ASCVD ≠ 是 且 透析 = 是
      if (!is.null(input$ascvd) && input$ascvd != "是" && input$dialysis == "是") {
        msgs <- c(msgs,
                  "检测到患者为透析状态的非ASCVD患者，不建议使用他汀类药物。"
        )
      }
      
      # 4. 年轻糖尿病警告：糖尿病=是 且 年龄<40
      if (!is.null(input$ascvd) && input$ascvd != "是" && input$diabetes == "是" && input$age < 40) {
        msgs <- c(msgs,
                  "检测到患者为40岁以下的糖尿病患者，只要出现明确靶器官损害（如蛋白尿、肾功能损害、神经病变或视网膜病变等），即为高危，LDL-c需控制在1.8 mmol/L以下。"
        )
      }
      
      # 5. 渲染：深红背景、白色加粗大字
      tags$div(
        lapply(msgs, function(m) {
          tags$div(
            style = paste(
              "background-color: #FFFFFF;",   # 深红背景
              "color: #800020;",                # 白色字体
              "font-weight: bold;",
              "font-size: 18px;",
              "border-radius: 6px;",
              "margin-bottom: 10px;",
              "display: flex; align-items: center;",
              "padding: .75rem 1.25rem;"
            ),
            icon("exclamation-triangle", class = "fa-lg"),
            tags$span(style = "margin-left: 10px;", m)
          )
        })
      )
    })
    #---------治疗界面---------
    output$treatmentAdvice <- renderUI({
      req(riskCategory())
      cat      <- initial_cat
      noteLine <- nzchar(note)
      
      # 构建建议列表
      advice <- list()
      
      # 低危或中危（且无“余生危险”备注）—— 仅生活方式干预
      if (cat %in% c("低危", "中危") && !noteLine) {
        advice <- list(
          "1.建议首先采用生活方式干预，包括合理膳食、适度增加运动、控制体重、戒烟和限制饮酒；",
          "2.若通过生活方式干预后仍未达标，中等强度他汀类药物作为起始治疗。"
        )
        
      } else {
        # 中危但余生危险、高危、确定/可能的极高危—— 五步治疗
        advice <- list(
          "1. 首先采用生活方式干预，包括合理膳食、适度增加运动、控制体重、戒烟和限制饮酒；",
          "2. 中等强度他汀类药物作为起始治疗；",
          "3. 若中等强度他汀治疗未达标，联合胆固醇吸收抑制剂；",
          "4. 若仍未达标，进一步联合 PCSK9 抑制剂；",
          "5. 无法耐受他汀者应考虑使用胆固醇吸收抑制剂或 PCSK9 抑制剂。"
        )
        # 超高危人群再加一步
        if (cat == "超高危") {
          advice <- c(advice,
                      "6. 若基线 LDL-C ≥4.9 mmol/L，或服用他汀后 LDL-C ≥2.6 mmol/L 且预计中等强度他汀+吸收抑制剂不能达标，可直接启动他汀+PCSK9抑制剂。"
          )
        }
      }
      
      # 渲染：绿色勾选 + 加粗大字
      tags$div(
        style = "padding:10px;",
        lapply(advice, function(txt) {
          tags$p(
            icon("check-circle", class = "fa-lg", style = "color:#28a745;"),
            span(txt,
                 style = "font-size:18px; font-weight:bold; margin-left:8px;")
          )
        })
      )
    })
   
#-------药品查询表---------
    output$excelTable <- renderTable({
      # 1. 读取项目目录下的“常用药物.xlsx”
      df <- read_excel("常用药物.xlsx")
      # 2. 返回数据框即可，renderTable 会自动生成 <table>
      df
    },
    # 以下参数启用 Bootstrap 样式：条纹、边框、悬浮高亮
    striped = TRUE,
    hover   = TRUE,
    bordered= TRUE,
    spacing = "s",    # 单元格紧凑间距
    width   = "100%"  # 表格宽度自适应容器
    )
#--  
  })#这个是大括号
  
}

shinyApp(ui, server)