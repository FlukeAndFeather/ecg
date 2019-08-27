library(lubridate)
library(tidyverse)

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

ggplot(ubc_data, aes(timestamp, ecg)) +
  geom_line(size = 0.1) +
  labs(y = "ECG (via UBC file)") +
  theme_classic() +
  theme(axis.title.x = element_blank())
ggsave("figs/read_data/ubc.pdf")
shouka_start <- ymd_hm("2019-05-16 15:15")
shouka_end <- ymd_hm("2019-05-16 15:30")
shouka_data <- read_ube("data/Shouka 05-16-18 chest leads.ube") %>% 
  filter(between(timestamp, shouka_start, shouka_end))
ggplot(shouka_data, aes(timestamp, ecg)) +
  geom_line(size = 0.1) +
  labs(y = "ECG (via UBE file)") +
  theme_classic() +
  theme(axis.title.x = element_blank())
plotly::ggplotly()
ggsave("figs/read_data/ube.pdf")
