library(tidyverse)
library(stringr)

df = lapply(list.files("benchmarks"), function(folder) {
  matches = str_match(folder, "(.*)_(\\w+)_([a-z]+)")
  branch = matches[2]
  commit = matches[3]
  varmat = matches[4]
  
  lapply(list.files(file.path("benchmarks", folder)), function(csv) {
    tryCatch({
      read.csv(file.path("benchmarks", folder, csv)) %>%
        as_tibble() %>%
        rename(time = real_time) %>%
        select(name, time) %>%
        filter(!str_detect(name, "_mean$") &
                 !str_detect(name, "_median$") &
                 !str_detect(name, "_stddev$") &
                 !str_detect(name, "toss_me")) %>%
        separate(name, c("name", "n", "tmp"), "/") %>%
        mutate(n = as.integer(n)) %>%
        select(-tmp) %>%
        mutate(benchmark = folder)
    }, error = function(e) {
      print(paste(folder, csv, e))
      return(NULL)
    })
  }) %>%
    bind_rows()
}) %>%
  bind_rows()

saveRDS(df, file = "df.rds")
