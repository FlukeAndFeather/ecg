#
# ECG post-processor. For visualization and classification of ECG data.
#

library(shiny)
library(plotly)

fillPage(
  tags$script('
    $(document).on("keypress", function (e) {
              Shiny.onInputChange("keypress", e.which);
              });
              '),
  sidebarLayout(
    sidebarPanel(textInput("deploy_start",
                           "Deployment start:"),
                 textInput("deploy_end",
                           "Deployment end:"),
                 radioButtons("mode",
                              "Interaction mode",
                              list(`Add heart beat` = 1,
                                   `Clear heart beat` = 2,
                                   `Clear gap` = 3))),
    
    mainPanel(
      plotOutput("data_plot",
                 brush = "data_brush",
                 height = "100px"),
      plotOutput("deploy_plot",
                 brush = "deploy_brush",
                 height = "100px"),
      plotOutput("detail_plot",
                 click = "detail_click",
                 brush = "detail_brush",
                 height = "400px"))
  )
)
