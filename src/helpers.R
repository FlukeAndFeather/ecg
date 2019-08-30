library(lubridate)
library(tidyverse)

# Constants
deploy_start <- 1
deploy_end <- 2
add_beat <- 3
clear_beat <- 4
add_gap <- 5
clear_gap <- 6

# Load data
read_ube <- function(ube_path) {
  ube_raw <- read_file_raw(ube_path)
  # First 32 bytes are download timestamp
  dl_time <- rawToChar(ube_raw[1:32], multiple = TRUE) %>% 
    paste(collapse = "") %>% 
    mdy_hms
  # Next 5 bytes are the time of recording beginning
  # What are bytes 38-40?
  record_start <- as.numeric(ube_raw[33:37])
  record_start <- do.call(ISOdatetime, flatten(list(year(now()), record_start, "UTC")))
  # Remaining bytes are data
  data_raw <- ube_raw[-(1:40)]
  ecg_channel <- 0x2F
  mode(ecg_channel) <- "raw"
  ecg_data <- integer(length(data_raw) / 2)
  i <- 1
  for (raw_i in seq(1, length(data_raw), by = 2)) {
    if ((ecg_channel | data_raw[raw_i]) != ecg_channel)
      next
    num <- as.numeric(data_raw[raw_i:(raw_i+1)])
    ecg_data[i] <- (num[1] - 0x20) * 0x100 + num[2]
    if (ecg_data[i] > 0xFFF)
      browser()
    i <- i + 1
  }
  ecg_data <- ecg_data[1:(i - 1)]
  ecg_time <- record_start + (seq_along(ecg_data) - 1) / 100
  result <- tibble(timestamp = ecg_time,
                   ecg = ecg_data)
  attr(result, "created") <- dl_time
  result
}

# plot_profile
# Input: plot data and brush box
# Output: brushed data and plot
#c(deploy, plot) %<-% plot_profile(values$ecg_data, input$data_brush)
plot_profile <- function(data, limits, box, beats = NULL) {
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
  start_visible <- if (between(limits[1], 
                               first(data_sparse$timestamp), 
                               last(data_sparse$timestamp))) {
    limits[1]
  } else {
    NULL
  }
  end_visible <- if (between(limits[2], 
                             first(data_sparse$timestamp), 
                             last(data_sparse$timestamp))) {
    limits[2]
  } else {
    NULL
  }
  p <- ggplot(data_sparse, aes(timestamp, ecg)) +
    geom_line(size = 0.1) +
    geom_vline(xintercept = start_visible,
               color = "blue",
               linetype = "dashed") +
    geom_vline(xintercept = end_visible,
               color = "red",
               linetype = "dashed") +
    scale_x_datetime(breaks = date_breaks) +
    theme_classic() +
    theme(axis.title = element_blank())
  
  if (!is.null(beats)) 
    p <- p + geom_point(data = beats, shape = 3, color = "red")
  
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
plot_detail <- function(data, beats, gaps, limits, click, brush, mode) {
  # If no data yet, return a blank plot
  if (is.null(data))
    return(list(NULL, beats, gaps, limits))
  
  # Maximum 10,000 points
  data_sparse <- data %>%
    slice(seq(1, nrow(data), length.out = 1e4))
  
  # Which heart beats and ECG gaps are visible?
  start_visible <- if (between(limits[1], 
                               first(data_sparse$timestamp), 
                               last(data_sparse$timestamp))) {
    limits[1]
  } else {
    NULL
  }
  end_visible <- if (between(limits[2], 
                             first(data_sparse$timestamp), 
                             last(data_sparse$timestamp))) {
    limits[2]
  } else {
    NULL
  }
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
    nearest_ecg <- which.min(abs(click$x - as.numeric(data$timestamp)))
    if (mode == deploy_start) {
      limits[1] <- data$timestamp[nearest_ecg]
    } else if (mode == deploy_end) {
      limits[2] <- data$timestamp[nearest_ecg]
    } else if (mode == add_beat) {
      search_range <- filter(data, 
                             timestamp > data$timestamp[nearest_ecg] - 0.5,
                             timestamp < data$timestamp[nearest_ecg] + 0.5)
      nearest_peak <- filter(search_range, ecg == max(ecg)) %>% slice(1)
      if (!nearest_peak$timestamp %in% beats$timestamp)
        beats <- rbind(beats, nearest_peak)
    } else if (mode == clear_beat) {
      if (nrow(beats_visible) > 0) {
        nearest_beat <- which.min(abs(click$x - as.numeric(beats_visible$timestamp)))
        drop_timestamp <- beats_visible$timestamp[nearest_beat]
        if (abs(click$x - as.numeric(drop_timestamp)) < 0.5) {
          beats <- filter(beats, timestamp != drop_timestamp)
        }
      }
    } else if (mode == clear_gap) {
      gap <- which(click$x > gaps$timestamp_begin & click$x < gaps$timestamp_end)
      if (length(gap) == 1)
        gaps <- slice(gaps, -gap)
    }
  } else if (!is.null(brush)) {
    if (mode == add_gap) {
      nearest_ecg <- integer(2)
      nearest_ecg[1] <- which.min(abs(brush$xmin - as.numeric(data$timestamp)))
      nearest_ecg[2] <- which.min(abs(brush$xmax - as.numeric(data$timestamp)))
      gaps <- rbind(gaps,
                    tibble(timestamp_begin = data$timestamp[nearest_ecg[1]],
                           timestamp_end = data$timestamp[nearest_ecg[2]]))
    }
  }
  
  # Collapse overlapping gaps
  merge_gaps <- function(.gaps, i) {
    # Merge a gap with the one after it
    .gaps$timestamp_end[i] <- max(.gaps$timestamp_end[c(i, i + 1)])
    slice(.gaps, -(i + 1))
  }
  gaps <- arrange(gaps, timestamp_begin)
  overlaps <- gaps$timestamp_end >= lead(gaps$timestamp_begin, 
                                         default = last(data$timestamp + 1))
  while (any(overlaps)) {
    overlap <- first(which(overlaps))
    gaps <- merge_gaps(gaps, overlap)
    overlaps <- gaps$timestamp_end >= lead(gaps$timestamp_begin, 
                                           default = last(data$timestamp + 1))
  }
  
  # Create plot
  p <- ggplot(data_sparse, aes(timestamp, ecg)) +
    geom_line(size = 0.1) +
    geom_vline(xintercept = start_visible,
               color = "blue",
               linetype = "dashed") +
    geom_vline(xintercept = end_visible,
               color = "red",
               linetype = "dashed") +
    geom_point(data = beats_visible,
               shape = 3,
               color = "red") +
    geom_rect(aes(xmin = gap_xmin,
                  xmax = gap_xmax,
                  ymin = gap_ymin,
                  ymax = gap_ymax),
              gaps_visible,
              inherit.aes = FALSE,
              fill = "blue",
              alpha = 0.1) +
    labs(title = sprintf("beats:%d;gaps:%d", nrow(beats), nrow(gaps))) +
    theme_classic() +
    theme(axis.title = element_blank())
  list(p, beats, gaps, limits)
}

# prepare_beats
# Input: beats, gaps
# Output: data frame
prepare_beats <- function(beats, gaps) {
  
}
# prepare_rdata
# Input: ecg_deploy, beats, gaps
# Output: 
#content = prepare_rdata(values$ecg_deploy, values$heart_beats, values$ecg_gaps)
prepare_rdata <- function(ecg_deploy, beats, gaps) {
  
}
prepare_csv <- function(beats, gaps) {
  
}