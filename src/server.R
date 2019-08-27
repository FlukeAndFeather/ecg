library(shiny)
library(zeallot)
source("helpers.R")

shinyServer(function(input, output) {
  values <- reactiveValues(ecg_data = read_ubc("../data/max_ecg_190826b.ubc"))
  
  output$data_plot <- renderPlot({ 
    c(deploy_data, data_plot) %<-% plot_profile(values$ecg_data, input$data_brush)
    values$ecg_deploy <- deploy_data
    data_plot }, 
    height = 100)
  output$deploy_plot <- renderPlot({ 
    c(detail_data, deploy_plot) %<-% plot_profile(values$ecg_deploy, input$deploy_brush)
    values$ecg_detail <- detail_data
    deploy_plot },
    height = 100)
  output$detail_plot <- renderPlot({ plot_detail(values$ecg_detail) })
})