#
# ECG post-processor. For visualization and classification of ECG data.
#

library(shiny)
library(plotly)

# 100MB limit
options(shiny.maxRequestSize = 100*1024^2)

fillPage(
  tags$script('
    $(document).on("keypress", function (e) {
              Shiny.onInputChange("keypress", e.which);
              });
              '),
  sidebarLayout(
    sidebarPanel(fileInput("ube_file",
                           "UBE File",
                           accept = "*.ube"),
                 textInput("deploy_start",
                           "Deployment start:"),
                 textInput("deploy_end",
                           "Deployment end:"),
                 radioButtons("mode",
                              "Interaction mode",
                              list(`Deploy start` = 1,
                                   `Deploy end` = 2,
                                   `Add heart beat` = 3,
                                   `Clear heart beat` = 4,
                                   `Add gap` = 5,
                                   `Clear gap` = 6)),
                 downloadButton("save_prog", "Save progress (.rds)"),
                 downloadButton("output_csv", "Download heart beats (.csv)")),
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
