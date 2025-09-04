# AtheroTarget
一个用于自动化评估ASCVD以及LDL-C的人工智能网页
## 运行方式
```r
# 进入项目目录后
install.packages("renv")
renv::restore()          # 复现依赖
shiny::runApp()          # 或 RStudio 点击 "Run App"
