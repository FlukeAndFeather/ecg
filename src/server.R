library(shiny)
library(zeallot)
source("helpers.R")

shinyServer(function(input, output) {
  values <- reactiveValues(ecg_data = read_ubc("../data/max_ecg_190826b.ubc"),
                           heart_beats = NULL,
                           ecg_gaps = NULL)
  
  output$data_plot <- renderPlot({ 
    c(deploy_data, data_plot) %<-% plot_profile(values$ecg_data, 
                                                input$data_brush)
    values$ecg_deploy <- deploy_data
    data_plot 
  })
  output$deploy_plot <- renderPlot({ 
    c(detail_data, deploy_plot) %<-% plot_profile(values$ecg_deploy, 
                                                  input$deploy_brush)
    values$ecg_detail <- detail_data
    deploy_plot 
  })
  output$detail_plot <- renderPlot({
    c(p, beats, gaps) %<-% plot_detail(values$ecg_detail,
                                       isolate(values$heart_beats),
                                       isolate(values$ecg_gaps),
                                       input$detail_click,
                                       input$detail_brush,
                                       isolate(input$mode))
    values$heart_beats <- beats
    values$ecg_gaps <- gaps
    p
  })
})