# Scrape chunk title and figure caption to be used to create list of
# Alternative text for SOEs and tech doc

text_scrape<-function(rmd, output){

  rmd<-readLines(here::here(rmd)) # read rmd

  ## Grab code chunk names
  r.title<-stringr::str_extract(string = rmd, pattern =("\\{\\s*r (.*?),")) %>% # scrape code chunk name
    stringr::str_remove(., pattern = "\\{\\s*r") %>% # remove {r
    stringr::str_remove(., pattern = ",") # rmeove comma

  ## Grab fig captions
  fig.cap<-stringr::str_extract(string = rmd, pattern =("fig.cap\\s*=(.*?)\\s*\\}")) %>%  # scrape everything after the fig.cap=
    stringr::str_remove(., pattern =("fig.cap\\s*=")) %>% # remove fig.cap=
    stringr::str_remove(., pattern =("paste0\\(\\s*")) %>%  # remove paste0(
    stringr::str_split_fixed(., pattern =("\\,\\s*fig.*"), n = 2) %>% # remove everything after the next fig.xx=
    stringr::str_split_fixed(., pattern =("\\s*message*"), n = 2) %>%  # remove everything after message =
    stringr::str_split_fixed(., pattern =("\\s*eval*"), n = 2) %>% # remove everything after eval
    stringr::str_remove(., pattern =("\\s*\\}")) # remove last }

  ## Combine grabs inro df
  df1<-cbind(r.title, fig.cap) # combine columns into data frame
  text.scrape<- as.data.frame(na.omit(df1)) %>% # remove rows with NA
    dplyr::filter(!fig.cap == "") # remove rows with no caption

  ## Build csv
  write.csv(text.scrape, file = output)

}
