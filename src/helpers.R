library(lubridate)
library(tidyverse)

# Load data
read_ubc <- function(ubc_path) {
  ecg_ubc <- read_lines(ubc_path)
  ecg_head <- head(ecg_ubc, 2)
  ecg_list <- tail(ecg_ubc, -2) %>% 
    str_split(" ") %>% 
    map(~ .x[.x != ""])
  ecg_tbl <- tibble(timestamp = mdy_hms(paste(map_chr(ecg_list, 1), 
                                              map_chr(ecg_list, 2))),
                    ecg = parse_number(map_chr(ecg_list, 3)))
  attr(ecg_tbl, "created") <- mdy_hms(ecg_head[1])
  attr(ecg_tbl, "samplerate") <- parse_number(ecg_head[2])
  ecg_tbl
}

# plot_profile
# Input: plot data and brush box
# Output: brushed data and plot
#c(deploy, plot) %<-% plot_profile(values$ecg_data, input$data_brush)
plot_profile <- function(data, box) {
  # If no data yet, return a blank plot
  if (is.null(data))
    return(list(NULL, ggplot(NULL)))
  
  # Maximum 10,000 points
  data_sparse <- data %>%
    slice(seq(1, nrow(data), length.out = 1e4))
  
  # Date breaks
  date_rng <- max(data_sparse$timestamp) - min(data_sparse$timestamp)
  offset <- 0.05
  date_breaks <- seq(min(data_sparse$timestamp) + offset * date_rng,
                     max(data_sparse$timestamp) - offset * date_rng,
                     length.out = 5)
  
  # Plot
  p <- ggplot(data_sparse, aes(timestamp, ecg)) +
    geom_line(size = 0.1) +
    scale_x_datetime(breaks = date_breaks) +
    theme_classic() +
    theme(axis.title = element_blank())
  
  # Brushed data
  if (is.null(box))
    brushed_data <- NULL
  else
    brushed_data <- filter(data, between(timestamp, box$xmin, box$xmax))
  
  list(brushed_data, p)
}

# plot_detail
# Input: plot data
# Output: plot
#output$detail_plot <- renderPlot({ plot_detail(values$ecg_detail) })
plot_detail <- function(data) {
  # If no data yet, return a blank plot
  if (is.null(data))
    return(list(NULL, ggplot(NULL)))
  
  # Maximum 10,000 points
  data_sparse <- data %>%
    slice(seq(1, nrow(data), length.out = 1e4))
  
  ggplot(data_sparse, aes(timestamp, ecg)) +
    geom_line(size = 0.5) +
    theme_classic() +
    theme(axis.title = element_blank())
}
