### Convert Rmd code chunks into R scripts

rmd.list<-list("human_dimensions_MAB.Rmd", "LTL_MAB.Rmd", "macrofauna_MAB.Rmd",
               "human_dimensions_NE.Rmd", "LTL_NE.Rmd", "macrofauna_NE.Rmd") # Rmds to convert

# create r scripts from rmd
for (i in rmd.list) {
  options(knitr.duplicate.label = 'allow') #allows named chunks
    purl <- knitr::purl(here::here("workshop", i))
    knitr::read_chunk(purl)
    chunks <- knitr:::knit_code$get()
    invisible(mapply(function(chunk, name) {
      writeLines(c(paste0(""), chunk), here::here("chunk-scripts", paste0(i, "-" ,name,".R")))
    }, chunks, names(chunks)))
    unlink(purl) # delete the original purl script
    knitr:::knit_code$restore() # remove chunks from current knitr session
}

