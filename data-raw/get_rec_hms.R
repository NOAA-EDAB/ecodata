## HMS sharks occurence on the shelf
raw.dir <- here::here("data-raw")
rec_hms_input <- "rec_hms.rds"

get_rec_hms <- function(save_clean = F) {
  rec_hms <- readRDS(file.path(raw.dir, rec_hms_input))

  if (save_clean) {
    usethis::use_data(rec_hms, overwrite = T)
  } else {
    return(rec_hms)
  }
}
get_rec_hms(save_clean = T)
