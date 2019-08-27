#
# ECG post-processor. For visualization and classification of ECG data.
#

library(shiny)
library(plotly)

fillPage(
  fillRow(plotOutput("data_plot",
                     brush = "data_brush"),
          height = "15%"),
  fillRow(plotOutput("deploy_plot",
                     brush = "deploy_brush"),
          height = "15%"),
  fillRow(plotOutput("detail_plot",
                     height = "100%"),
          height = "70%")
)
