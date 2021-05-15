library(tidyverse)
library(rvest)
library(future.apply)

scrapePTT <- function(ptt_url, num_index_page) {
  
  # create session
  session <- submit_form(
    session = html_session(ptt_url),
    form = html_session(ptt_url) %>%
      html_node('form') %>%
      html_form,
    submit = 'yes'
  )
  
  # find latest page
  page.latest <- session %>%
    html_nodes('a') %>% # extract all <a> elements
    html_attr('href') %>%  # extract the attributes `href`
    str_subset('index[0-9]{2,}\\.html') %>% # find the `href` with the index number
    str_extract('[0-9]+') %>% # extract the number
    as.numeric()
  
  # define function to extract article links
  extract_art_links <- function(index_page) {
    links.article <- session %>%
      jump_to(index_page) %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      str_subset('[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html') %>%
      str_c('https://www.ptt.cc', .)
    
    return(links.article)
  }
  
  # define function to extract article table
  extract_art_df <- function(link) {
    temp.html <- session %>%
      jump_to(link) # link to the www
    # article header
    article.header <- temp.html %>%
      html_nodes('span.article-meta-value') %>% # meta info regarding the article
      html_text()
    
    # article meta
    article.author <- article.header[1] %>% str_extract('^[A-z0-9_]+') # author
    board.name <- article.header[2] # board name
    article.title <- article.header[3] # title
    
    # article content
    article.content <- temp.html %>%
      html_nodes( # article body
        xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
      ) %>%
      html_text(trim = TRUE) %>%
      str_c(collapse = '')
    
    # create article table 
    article.df <- tibble(
      board = board.name,
      author = article.author,
      title = article.title,
      content = article.content,
    )
    return(article.df)
  }
  
  # merge altogether
  index_pages <- vector()
  links <- vector()
  for (i in (page.latest-(num_index_page-1)):page.latest) {
    index_pages <- c(index_pages, paste0(ptt_url, '/index', i, '.html'))
  }
  
  for (j in seq_along(index_pages)) {
    links <- c(links, extract_art_links(index_pages[j]))
  }
  
  df <- links %>%
    map(extract_art_df) %>%
    bind_rows() %>%
    mutate(id = row_number()) %>%
    select(board, id, everything())
  
  return(df)
}

plan(multiprocess(workers = 8, gc = T))

