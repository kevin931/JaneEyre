library(tidyverse)
library(tidytext)
library(textstem)

source("process.R")
source("analysis.R")

text_df <- load_file("janeeyre.csv")
text_df <- chapter_parse(text_df, 38)
text_df <- stop_word(text_df)
text_df$word_lem <- lemmatize_words(text_df$word)

freq <- term_freq(text_df)
tf_idf_df <- tf_idf(text_df)

pos(text_df)

pos_character(text_df, "i")
pos_character(text_df, c("he", "rochester", "john"))

pos_by_part(text_df)
