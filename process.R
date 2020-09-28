## This script loads, cleans, and processes text

load_file <- function(file_path) {
  
  df <- read_csv("janeeyre.csv")
  df <- df %>% 
    mutate(word = tolower(word))
  
  return(df)
  
}


chapter_parse <- function(df, chapter_num) {
  
  romans <- as.roman(1:chapter_num)
  romans <- tolower(romans)
  
  index <- which(df$word == "chapter")
  
  cleaned_index <- vector()
  
  for (i in 1:length(index)) {
    row <- index[i]
    next_word <- df$word[row + 1]
    
    if(next_word %in% romans) {
      cleaned_index <- append(cleaned_index, index[i])
    }
  }
  
  # Fill vector with chapter number
  chapter_num <- vector()
  for (i in 1:length(cleaned_index)) {
    word_num <- cleaned_index[i]
    
    if (i == length(cleaned_index)) {
      chapter_num[word_num:nrow(df)] <- i
      
    } else {
      word_num_next <- cleaned_index[i+1]-1
      chapter_num[word_num:word_num_next] <- i
    }
    
  }
  # Preface etc.
  chapter_num[which(is.na(chapter_num))] <- 0
  df$chapter <- chapter_num
  
  chapter_length <- df %>%
    group_by(chapter) %>%
    summarise(chapter_length = n()) %>%
    ungroup()
  
  df <- df %>%
    left_join(chapter_length, by = "chapter")
  
  return(df)
}

stop_word <- function(df) {
  
  data(stop_words)
  word <- df$word
  stop_word_list <- stop_words$word
  stop_word_list <- c(stop_word_list, "mr.", "'s", "mrs.")
  
  stop <- word %in% stop_word_list
  df$stop_word <- stop
  
  punctuations <- grepl("[[:alpha:]]", text_df$pos)
  df$punctuations <- !punctuations
  
  return(df)
}