library(tidyverse)
library(spelling)

ignore_words <- read_csv("spelling-whitelist.csv",
                         col_types = cols(word = col_character())) %>% 
  pull(word)

spell_check_files("index.html", ignore = ignore_words)

pagedown::chrome_print("index.html", output = "wjt-cv.pdf")
