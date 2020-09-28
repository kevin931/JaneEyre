dir_check <- function() {
  
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  if (!dir.exists("plots/term_frequency")) {
    dir.create("plots/term_frequency")
  }
  
  if (!dir.exists("plots/tf_idf")) {
    dir.create("plots/tf_idf")
  }
  
  if (!dir.exists("plots/pos")) {
    dir.create("plots/pos")
  }
}

term_freq <- function(df) {
  
  dir_check()
  
  data("stop_words")
  
  df <- df %>% 
    filter(chapter > 0) %>%
    group_by(chapter) %>%
    filter(!stop_word,
           !punctuations) %>%
    ungroup()
  
  # Global Word Frequency
  all_word_freq <- df %>% 
    count(word_lem, sort = TRUE)
  
  
  all_freq_plot <- all_word_freq %>%
    top_n(15) %>%
    mutate(word_lem = reorder(word_lem, n)) %>%    
    ggplot(aes(word_lem, n))+
    geom_col(fill="pink")+
    geom_text(aes(label=n))+
    coord_flip() +
    ggtitle("Word Frequency of Jane Eyre") +
    ylab("Frequency") +
    xlab("Word")
    
  ggsave("plots/term_frequency/all.jpeg", all_freq_plot)
  
  # Chapter by chapter
  for (i in 1:max(df$chapter)) {
    title_name <- paste("Chapter", i, "Weighted Word Frequency")
    
    chap_length <- unique(df$chapter_length)[i]
    print(chap_length)
    
    chapter_plot <- df %>% 
      filter(chapter == i) %>%
      count(word_lem, sort = TRUE) %>%
      top_n(15) %>%
      mutate(n_weight=n/chap_length) %>%
      mutate(word_lem = reorder(word_lem, n_weight)) %>%    
      ggplot(aes(word_lem, n_weight))+
      geom_col(fill="pink")+
      geom_text(aes(label=n))+
      coord_flip() +
      ggtitle(title_name) +
      ylab("Frequency") +
      ylim(c(0, 0.01))  +
      xlab("Word")
      
      
    save_path = paste0("plots/term_frequency/chapter_",i, ".jpeg")
    ggsave(save_path, chapter_plot)
    
  }
  
  return(all_word_freq)
}


tf_idf <- function(df) {
  
  dir_check()
  
  df <- df %>% 
    filter(chapter > 0,
           !stop_word,
           !punctuations) %>%
    group_by(chapter) %>%
    count(word_lem, sort=F) %>%
    ungroup() %>%
    bind_tf_idf(word_lem, chapter, n) %>%
    group_by(chapter, word_lem, tf_idf) %>%
    summarise(tf = sum(n)) %>%
    ungroup()
  
  y_max <- max(df$tf_idf)
  
  for (i in 1:max(df$chapter)) {
    title_name <- paste("Chapter", i, "TF-IDF")
    
    chapter_plot <- df %>% 
      filter(chapter == i) %>%
      top_n(15, tf_idf) %>%
      mutate(word_lem = reorder(word_lem, tf_idf)) %>%    
      ggplot(aes(word_lem, tf_idf))+
      geom_col(fill="cyan")+
      geom_text(aes(label=tf))+
      coord_flip() +
      ggtitle(title_name) +
      ylab("TF-IDF") +
      ylim(c(0, y_max))  +
      xlab("Word")
    
    
    save_path = paste0("plots/tf_idf/chapter_",i, ".jpeg")
    ggsave(save_path, chapter_plot)
    
  }
  
  return(df)
}


pos <- function(df) {
  
  dir_check()
  
  pos_distribution_plot <- df %>%
    filter(!punctuations) %>%
    group_by(pos) %>%
    summarise(n = n()) %>%
    top_n(15, n) %>%
    ggplot(aes(pos, n)) +
    geom_col()+
    labs(title ="Selected Distribution of POS",
         caption = "Note: Top 15 selected")+
    xlab("POS")+
    ylab("Count")
  
  ggsave("plots/pos/pos_distribution.jpeg", 
         pos_distribution_plot)
  
  verb_df <- df %>% 
    filter(pos %in% c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
           chapter > 0) %>%
    group_by(chapter, chapter_length) %>%
    summarise(n = n()) %>%
    ungroup()
  
  verb_density_plot <- verb_df %>%
    mutate(n = n/chapter_length) %>%
    ggplot(aes(chapter, n)) +
    geom_line()+
    geom_smooth()+
    labs(title = "Verb Density throughout Jane Eyre",
         subtitle = "Smooth Curve using 'LOESS'")+
    scale_x_continuous(breaks = seq(0, 38, 2))
  
  verb_count_plot <- verb_df  %>%
    ggplot(aes(chapter, n)) +
    geom_line()+
    geom_smooth()+
    labs(title = "Verb Count throughout Jane Eyre",
         subtitle = "Smooth Curve using 'LOESS'")+
    scale_x_continuous(breaks = seq(0, 38, 2))
  
  ggsave("plots/pos/verb_count_all.jpeg", verb_count_plot)
  ggsave("plots/pos/verb_density_all.jpeg", verb_density_plot)

}


pos_character <- function(df, character) {
  
  # Find the word and the next
  character_index <- vector()
  for (i in character) {
    
    temp <- which(df$word == i)
    character_index <- append(character_index, temp)
    
  }
  
  
  character_index <- character_index + 1
  df <- df[character_index,]
  
  df <- df %>%
    filter(chapter > 0,
           !stop_word,
           pos %in% c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"))
  
  df_count <- df %>%
    count(word_lem)
  
  count_plot <- df_count %>%
    top_n(10, n) %>%
    mutate(word_lem = reorder(word_lem, n)) %>%
    ggplot(aes(word_lem, n)) +
    geom_col()+
    labs(title = paste0("Verbs Associated with ", paste(character, collapse = ", ")),
         subtitle = "Raw Count") +
    xlab("Verb")+
    ylab("Count")
  
  ggsave(paste0("plots/pos/verbs_", character[1], "_count.jpeg"),
         count_plot)
  
  trend_plot <- df %>%
    group_by(chapter, chapter_length) %>%
    summarise(n = n()) %>%
    mutate(n = n/chapter_length) %>%
    ungroup() %>%
    ggplot(aes(chapter, n)) +
    geom_line()+
    geom_smooth()+
    labs(title = paste0("Verbs Associated with ", paste(character, collapse = ", ")),
         subtitle = "Smooth Curve using 'LOESS'")+
    scale_x_continuous(breaks = seq(0, 38, 2))
  
  ggsave(paste0("plots/pos/verbs_", character[1], "_trend.jpeg"),
         trend_plot)
}

# 1, 5, 11, 28, 36
pos_by_part <- function(df) {
  
  chapter_part <- df$chapter
  chapter_part[which(chapter_part < 5)] <- 1
  chapter_part[which(chapter_part >= 5 & chapter_part < 11)] <- 2
  chapter_part[which(chapter_part >= 11 & chapter_part < 28)] <- 3
  chapter_part[which(chapter_part >= 28 & chapter_part < 36)] <- 4
  chapter_part[which(chapter_part >= 36)] <-  5
  
  df$part <- chapter_part
  
  character_index <- which(df$word == "i")
  character_index <- character_index + 1
  
  df <- df[character_index,]
  
  df <- df %>%
    filter(chapter > 0,
           !stop_word,
           pos %in% c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")) %>%
    group_by(part) %>%
    count(word_lem) %>%
    ungroup()
  
  titles <- c("Gateshead", "Lowood", "Thornfield", 
              "Moor", "Ferndean")
  
  for (i in 1:5) {
    
    part_plot <- df %>%
      filter(part == i) %>%
      top_n(10, n) %>%
      mutate(word_lem = reorder(word_lem, n)) %>%
      ggplot(aes(word_lem, n)) +
      geom_col(fill = "purple")+
      labs(title = titles[i],
           subtitle = "Verbs associated with 'I'",
           caption = "Note: Top 10 selected")+
      xlab("Verbs")+
      ylab("Count")
    
    ggsave(paste0("plots/pos/part_", i, ".jpeg"), 
           part_plot)
    
  }
 
  
  
  
}
