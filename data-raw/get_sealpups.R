# Seal Pup COlonies from pupCountsColonies.Rmd
# from Kristin Precoda and Stephanie Wood
# Figure 5 in center reference doc
# https://repository.library.noaa.gov/view/noaa/46455


## Step 1
  reportResults <- function(startyear, dat, model, loglikdiff, aicdiff, addlstring=NULL) {
    printString <- paste("\nYears fit:", startyear, "to 2021. The model is fitted with",
                         sum(!is.na(dat)), "non-missing counts. ", addlstring)
    if (model$convergence==0) {
      printString <- paste(printString, "Converged successfully.")
    } else {
      printString <- paste(printString, "Did not converge.", paste0(model$errors, collapse=", "))
    }
    if (loglikdiff < 0.01 & aicdiff < 0.01) {
      printString <- paste0(printString, "\nU = ", signif(model$par$U, 3), ", Q = ",
                            signif(model$par$Q, 3))
      if (length(model$par$R) > 0) { # R only exists in the model if it was estimated and not fixed
        printString <- paste0(printString, ", R = ", signif(model$par$R, 3), ".\n")
      } else {
        printString <- paste0(printString, ", R set to ", model$call$model$R, ".\n")
      }
    } else {
      printString <- paste(printString, "Log likelihood and/or AIC vary more than 1% in runs",
                           "with different starting points.\n")
    }
    return(printString)
  }

## Step 2
  library("MARSS")
  library("tseries", warn.conflicts=F, quietly=T)
  library("flextable")
  library("varhandle")   # for check.numeric()

  minNYears <- 8  # only keep colonies that have counts in at least this many years

  dat <- read.csv(file.path(here::here("data-raw/pupcountsalldatraw_21-final2021.csv")))
  if ("minCount" %in% colnames(dat))
    dat$minCount[which(is.na(dat$minCount))] <- dat$count[which(is.na(dat$minCount))]
  if ("maxCount" %in% colnames(dat))
    dat$maxCount[which(is.na(dat$maxCount))] <- dat$count[which(is.na(dat$maxCount))]

  # keep colonies that have counts in at least minNYears years
  datw <- reshape(dat[which(dat$colony %in% names(which(table(dat$colony) >= minNYears))), ],
                  idvar="colony", timevar="Year", direction="wide")
  colnames(datw) <- gsub("count[.]", "", colnames(datw))
  rownames(datw) <- datw$colony
  datw <- datw[, which(colnames(datw) != "colony")]
  missingyrs <- setdiff(c(min(dat$Year):max(dat$Year)), unique(dat$Year))
  if (length(missingyrs) > 0) {  # fill in NA for years missed at all sites
    datw[, as.character(missingyrs)] <- NA
    if ("minCount" %in% colnames(dat))
      datw[, paste0("minCount.", as.character(missingyrs))] <- NA
    if ("maxCount" %in% colnames(dat))
      datw[, paste0("maxCount.", as.character(missingyrs))] <- NA
  }
  datw <- datw[, sort(colnames(datw))]  # reorder the columns to be in order by year
  datwmat <- as.matrix(datw, nrow=nrow(datw))
  datwmatlog <- log(datwmat)
  datwmatlog[datwmatlog== -Inf] <- 0

  if ("minCount" %in% colnames(dat)) {
    datwmatlogMin <- datwmatlog[, grep("minCount[.]", colnames(datwmat))]
    colnames(datwmatlogMin) <- gsub("minCount[.]", "", colnames(datwmatlogMin))
  }
  if ("maxCount" %in% colnames(dat)) {
    datwmatlogMax <- datwmatlog[, grep("maxCount[.]", colnames(datwmat))]
    colnames(datwmatlogMax) <- gsub("maxCount[.]", "", colnames(datwmatlogMax))
  }
  if (any(c("minCount", "maxCount") %in% colnames(dat)))
    datwmatlog <- datwmatlog[, grep("maxCount|minCount", colnames(datwmat), invert=T)]

  # dates after which each colony assumed to be or have been established
  dateFrom <- data.frame(colony=c("Muskeget", "Monomoy", "Green", "Seal", "Nomans Land"),
                         startyear=c(1988, 2009, 1994, 2000, 2011))

  # aggregate count: includes years where all known colonies were surveyed and
  # have what are believed to be valid counts
  agg <- data.frame(year=1988:2021,
                    count=c(5, 0, NA, 9, 12, NA, 97, 148, NA, 255, NA, NA,
                            NA, NA, 1064, NA, NA, 1391, 1158, 2134, 2622,
                            NA, NA, NA, NA, NA, NA, NA, 6308, NA, NA,
                            5664, NA, 6436))
  agg$logcount <- log(agg$count)
  agg$logcount[which(agg$logcount == -Inf)] <- 0


datlog <- data.frame(t(datwmatlogMax)) %>%
  dplyr::mutate(Time = 1988:2021,
                Var2 = c("log")) %>%
  tidyr::pivot_longer(cols = c("Seal","Monomoy",
                               "Muskeget","Green",
                               "Nomans.Land"),
                      names_to = "Var",
                      values_to = "Value")
datw<- datw %>%
  dplyr::select("1988":"2021")
datcount<- data.frame(t(datw)) %>%
  dplyr::mutate(Time = 1988:2021,
                Var2 = c("count")) %>%
  tidyr::pivot_longer(cols = c("Seal","Monomoy",
                               "Muskeget","Green",
                               "Nomans.Land"),
                      names_to = "Var",
                      values_to = "Value")

seal_pups<- rbind(datlog, datcount) %>%
  dplyr::mutate(Var = paste0(Var2, "-", Var),
                EPU = c("NE")) %>%
  dplyr::select(!Var2)

usethis::use_data(seal_pups, overwrite = T)

