
#### Adjust plot properties -------------------------------
adjustAxes <- 
  ggplot2::theme(axis.title   = element_text(size = 18),
                 axis.text    = element_text(size = 15),
                 plot.title   = element_text(size = 20))


#### Plot stacked bar with cpts for single var ------------
plot_stackbarcpts_single <- function(YEAR, var2bar,
                                     x, xlab, ylab,
                                     titl,
                                     file_suffix,
                                     leg_font_size = 10,
                                     remove_leg = FALSE,
                                     leg_ncol = 1,
                                     wcpts = TRUE,
                                     wdashed = TRUE,
                                     height = 5.5,
                                     width = 8,
                                     filt = TRUE,
                                     label = label,
                                     y.text = y.text,
                                     aggregate = FALSE) {
  
  dat2bar <- data.frame(YEAR, var2bar,
                        x)
  if (filt == TRUE){mab_species <-  list("SUMMER FLOUNDER","SCUP","BLACK SEA BASS","BLUEFISH",
                                         "NORTHERN SHORTFIN SQUID", "LONGFIN SQUID", "ATLANTIC MACKEREL",
                                         "BUTTERFISH","ATLANTIC SURFCLAM", "OCEAN QUAHOG", "TILEFISH",
                                         "BLUELINE TILEFISH","SPINY DOGFISH", "GOOSEFISH")
  dat2plot <-
    dat2bar %>%
    tidyr::gather(variable, value, -YEAR, -var2bar) %>%
    dplyr::mutate(var2bar = gsub(pattern      = "_", 
                                 replacement  = " ", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "Atl.", 
                                 replacement  = "ATLANTIC", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "Atl", 
                                 replacement  = "ATLANTIC", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "NS and combined", 
                                 replacement  = "", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "YT", 
                                 replacement  = "Yellowtail", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = " GoM", 
                                 replacement  = " GOM", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = " by EPU", 
                                 replacement  = "", 
                                 x            = var2bar)) %>%
    dplyr::filter(var2bar %in% mab_species)
} else if (filt == FALSE){
    dat2plot <-
    dat2bar %>%
    tidyr::gather(variable, value, -YEAR, -var2bar) %>%
    dplyr::mutate(var2bar = gsub(pattern      = "_", 
                                 replacement  = " ", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "Atl.", 
                                 replacement  = "ATLANTIC", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "Atl", 
                                 replacement  = "ATLANTIC", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "NS and combined", 
                                 replacement  = "", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = "YT", 
                                 replacement  = "Yellowtail", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = " GoM", 
                                 replacement  = " GOM", 
                                 x            = var2bar),
                  var2bar = gsub(pattern      = " by EPU", 
                                 replacement  = "", 
                                 x            = var2bar))
}
  if (aggregate){
   agg <- dat2plot %>%
     dplyr::group_by(YEAR) %>%
     dplyr::summarise(Total = sum(value, na.rm = T)) %>% 
     dplyr::mutate(Total = ifelse(Total == 0, NA, Total))
  }
  
  p <-   
    ggplot2::ggplot(dat2plot,
           aes(x = YEAR)) +
    ggplot2::geom_bar(data = dat2plot %>% filter(value > 0),
             aes(y = value, fill = var2bar),
             stat = "identity") +
    ggplot2::geom_bar(data = dat2plot %>% filter(value < 0),
             aes(y = value, fill = var2bar),
             stat = "identity") +
    {if(aggregate) geom_line(data = agg,aes(x = YEAR, y = Total),
                             size = 1)} +
    ggplot2::geom_hline(size = 0.3, aes(yintercept = 0)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(titl) +
    ggplot2::guides(fill = guide_legend(ncol = leg_ncol)) +
    ecodata::theme_ts()+
    ggplot2::theme(axis.title   = element_text(size = 16),
          axis.text    = element_text(size = 15),
          plot.title   = element_text(size = 20),
          legend.text  = element_text(size = leg_font_size),
          legend.title = element_blank()) +
    ggplot2::annotate("text", label = label, x = 1980, y = y.text,size = 8, colour = "black")
  

  
  if(remove_leg) p <- p + theme(legend.position = "none")
  
  return(p)
}

bar_dat <- ecodata::productivity_anomaly %>% 
  dplyr::filter(EPU == "MAB")

# mafmc <-plot_stackbarcpts_single(YEAR = bar_dat$Time,
#                          var2bar = bar_dat$Var,
#                          x = bar_dat$Value,
#                          titl = "",
#                          xlab = "",
#                          ylab = "Small fish per large fish biomass (anomaly)",
#                          height = 5.5,
#                          width = 9,
#                          filt = TRUE,
#                          label = "A",
#                          y.text = 4.5)

# 
mid <- plot_stackbarcpts_single(YEAR = bar_dat$Time,
                         var2bar = bar_dat$Var,
                         x = bar_dat$Value,
                         titl = "",
                         xlab = "",
                         ylab = "Small fish per large fish biomass (anomaly)",
                         height = 5.5,
                         width = 9,
                         filt = FALSE,
                         label = "",
                         y.text = 10,
                         aggregate = TRUE)

mid
