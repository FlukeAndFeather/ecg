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
plot_detail <- function(data, beats, gaps, click, brush, mode) {
  # If no data yet, return a blank plot
  if (is.null(data))
    return(list(NULL, beats, gaps))
  
  # Maximum 10,000 points
  data_sparse <- data %>%
    slice(seq(1, nrow(data), length.out = 1e4))
  
  # Which heart beats and ECG gaps are visible?
  if (is.null(beats)) {
    beats <- filter(data, FALSE)
  }
  if (is.null(gaps)) {
    gaps <- tibble(timestamp_begin = now("UTC"),
                   timestamp_end = now("UTC")) %>% 
      filter(FALSE)
  }
  beats_visible <- filter(beats, 
                          between(timestamp, 
                                  min(data$timestamp), 
                                  max(data$timestamp)))
  gaps_visible <- filter(gaps,
                         timestamp_end > min(data$timestamp),
                         timestamp_begin < max(data$timestamp)) %>% 
    mutate(gap_xmin = pmax(min(data$timestamp), timestamp_begin),
           gap_xmax = pmin(max(data$timestamp), timestamp_end),
           gap_ymin = min(data$ecg),
           gap_ymax = max(data$ecg))
  # Add/remove beats/gaps
  if (!is.null(click)) {
    if (mode == 1) {
      nearest_ecg <- which.min(abs(click$x - as.numeric(data$timestamp)))
      search_range <- filter(data, 
                             timestamp > data$timestamp[nearest_ecg] - 0.5,
                             timestamp < data$timestamp[nearest_ecg] + 0.5)
      nearest_peak <- filter(search_range, ecg == max(ecg)) %>% slice(1)
      if (!nearest_peak$timestamp %in% beats$timestamp)
        beats <- rbind(beats, nearest_peak)
    } else {
      if (nrow(beats_visible) > 0) {
        nearest_beat <- which.min(abs(click$x - as.numeric(beats_visible$timestamp)))
        if (abs(click$x - beats_visible$timestamp[nearest_beat]) < 50) {
          beats <- slice(beats, -nearest_beat)
        }
      }
    }
  } else if (!is.null(brush)) {
    gaps <- rbind(gaps,
                  tibble(timestamp_begin = brush$xmin,
                         timestamp_end = brush$xmax))
  }
  
  # Create plot
  p <- ggplot(data_sparse, aes(timestamp, ecg)) +
    geom_line(size = 0.1) +
    geom_point(data = beats_visible,
               shape = 3,
               color = "red") +
    geom_rect(aes(xmin = gap_xmin,
                  xmax = gap_xmax,
                  ymin = gap_ymin,
                  ymax = gap_ymax),
              gaps_visible,
              inherit.aes = FALSE,
              color = "blue",
              alpha = 0.2) +
    labs(title = nrow(beats)) +
    theme_classic() +
    theme(axis.title = element_blank())
  list(p, beats, gaps)
}
