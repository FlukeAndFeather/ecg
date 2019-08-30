library(shiny)
library(zeallot)
source("helpers.R")

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  
  observeEvent(input$ube_file, {
    values$ecg_data <- read_ube(input$ube_file$datapath)
    values$ecg_limits <- c(min(values$ecg_data$timestamp),
                           max(values$ecg_data$timestamp))
  })
  
  observeEvent(values$ecg_limits, {
    updateTextInput(session, 
                    "deploy_start", 
                    value = min(values$ecg_limits[1]))
    updateTextInput(session, 
                    "deploy_end", 
                    value = max(values$ecg_limits[2]))
    })
  
  output$data_plot <- renderPlot({ 
    c(deploy_data, data_plot) %<-% plot_profile(values$ecg_data, 
                                                values$ecg_limits,
                                                input$data_brush)
    values$ecg_deploy <- deploy_data
    data_plot 
  })
  output$deploy_plot <- renderPlot({ 
    c(detail_data, deploy_plot) %<-% plot_profile(values$ecg_deploy, 
                                                  values$ecg_limits,
                                                  input$deploy_brush,
                                                  values$heart_beats)
    values$ecg_detail <- detail_data
    deploy_plot 
  })
  output$detail_plot <- renderPlot({
    c(p, beats, gaps, limits) %<-% plot_detail(values$ecg_detail,
                                               isolate(values$heart_beats),
                                               isolate(values$ecg_gaps),
                                               isolate(values$ecg_limits),
                                               input$detail_click,
                                               input$detail_brush,
                                               isolate(input$mode))
    values$heart_beats <- beats
    values$ecg_gaps <- gaps
    values$ecg_limits <- limits
    p
  })
  output$output_all <- downloadHandler(
    filename = function() {
      format(now(), "ecg_%y%m%d%H%M%S.RData")
    },
    content = function(file) {
      save(list = prepare_rdata(values$ecg_deploy,
                                values$heart_beats,
                                values$ecg_gaps),
           file = file)
    })
})