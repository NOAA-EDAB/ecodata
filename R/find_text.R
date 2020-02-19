#' Find snippets of text in the ecodata chunk-scripts
#'
#' Scans all chapter R chunck scripts for \code{textSnippet}
#'
#'@param textSnippet Character string. A snippet of text
#'
#' @return Character vector. Names of the files that contain the text
#'

find_text <- function(textSnippet){
  # read yml file
  filenames <- list.files(here::here("chunk-scripts"))

  options(warn=-1)
  # loop over each rmd file
  for (afile in filenames) {
    # read in rmd
    chapterContent <- readLines(here::here("chunk-scripts",paste0(afile)))

    lineContrib <- chapterContent[grepl(textSnippet,chapterContent)]
    if (length(lineContrib) == 0) next

    print(afile)

  }

}
