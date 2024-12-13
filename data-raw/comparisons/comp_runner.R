files <- list.files(pattern = "[.]Rmd$")
files<-files[-10]
for (f in files) rmarkdown::render(f)
