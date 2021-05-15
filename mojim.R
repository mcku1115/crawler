library(rvest)
library(tidyverse)
library(future.apply)

mojim <- function(url) {
  title_left <- read_html(url) %>% 
    html_nodes('.hc3') %>%
    html_nodes('a') %>% 
    html_text()
  title_right <- read_html(url) %>% 
    html_nodes('.hc4') %>%
    html_nodes('a') %>% 
    html_text()
  title <- c(title_left, title_right)
  
  links_left <- read_html(url) %>% 
    html_nodes('.hc3') %>%
    html_nodes('a') %>% 
    html_attr('href')
  links_right <- read_html(url) %>% 
    html_nodes('.hc4') %>%
    html_nodes('a') %>% 
    html_attr('href')
  links <- c(links_left, links_right)

# remove duplication
  links <- links[-which(duplicated(title))] 

# extract info      
  urls <- paste('https://mojim.com', links, sep = '')
  df <- tibble()
  for (l in urls) {
    html <- read_html(l) 
    title <- html %>% 
      html_nodes('#fsZx2') %>% 
      html_text
    info <- html %>% 
      html_nodes('#fsZx3') %>% 
      toString()
    lyricist <- info %>% 
      str_extract('(?<=作詞：).+?(?=<br>)')
    composer <- info %>% 
      str_extract('(?<=作曲：).+?(?=<br>)')
    if (str_detect(info, '\\[(([a-z]|\\d|\\:|\\.|)+|([a-z]+:\\S{1,4}))\\]')) {
      lyrics <- info %>%
        str_extract('(?<=<br><br>).+') %>%
        str_remove('\\[(([a-z]|\\d|\\:|\\.|)+|([a-z]+:\\S{1,4}))\\].+') %>%
        str_replace_all('<.+?>', ' ') %>%
        str_remove('更多更詳盡歌詞 在  ※ Mojim.com　魔鏡歌詞網') %>%
        str_remove('感謝.{5,20}(修正|提供)歌詞')
    } 
    else {
      lyrics <- info %>%
        str_extract('(?<=<br><br>).+') %>%
        str_replace_all('<.+?>', ' ') %>%
        str_remove('更多更詳盡歌詞 在  ※ Mojim.com　魔鏡歌詞網') %>%
        str_remove('感謝.{5,20}(修正|提供)歌詞')
    }
    df <- rbind(df, tibble(title = title,
                           lyricist = lyricist,
                           composer = composer,
                           lyrics = lyrics))
  }
  return(df)
}

plan(multiprocess(workers = 8, gc = T))

