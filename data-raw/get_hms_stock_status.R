## get HMS Stock Status


library(dplyr)
library(tidyr)
library(ggplot2)

data.dir <- here::here('data-raw')
hms_stock_status_xlsx <- "2024_SAFE_B_F - Jennifer Cudney - NOAA Federal.xlsx"

get_hms_stockstatus <- function(save_clean = F){
  xlsx <- readxl::read_xlsx(file.path(data.dir, hms_stock_status_xlsx))


  hms_stock_status <-
    xlsx %>% dplyr::rename("B.Bmsy" = "current_rel_B_level",
                           "F.Fmsy" = "current_rel_F_level",
                           #"Include" = "Included in Kobe Plot? If No, Why?",
                           "Time" = "rel_F_year") %>%
    #dplyr::filter(Include == "Yes") %>%
    dplyr::select( "species_abr", "species", "Time", "F.Fmsy", "B.Bmsy" ) %>%
    tidyr::pivot_longer(cols = -c("species_abr", "species", "Time"),
                        names_to = "Var", values_to = "Value") %>%
    dplyr::mutate(Var = paste0(species_abr, ":", species, ":", Var),
                  EPU = "ALL") %>%
    dplyr::select(-species_abr, -species) %>%
    dplyr::filter(!Time == "NA")




  if (save_clean){
    usethis::use_data(hms_stock_status, overwrite = T)
  } else {
    return(hms_stock_status)
  }
  # metadata ----
  attr(hms_stock_status, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/stockstatus.html"
  attr(hms_stock_status, "data_files")   <- list(
    hms_stock_status_xlsx = hms_stock_status_xlsx)
  attr(stock_status, "data_steward") <- c(
    "Jennifer Cudney <jennifer.cudney@noaa.gov>")
}
get_hms_stockstatus(save_clean = T)




# ## Draft Plor
# unknown <- data.frame(text = c("Unknown Status", "ATL SBH", "SPF", "WA BFT"),
#                       x = rep(0.9*x.max,4), y = seq(0.88*y.max,3.8,-0.2))
#
#
#
# stock_status<-hms_stock_status %>%
#   tidyr::spread(.,Var,Value) %>%
#   tidyr::separate(stock, c("species_abr", "spp"), ":") %>%
#   dplyr::group_by(spp) %>%
#   dplyr::mutate(score = case_when(
#     (B.Bmsy <0.5) ~"a",
#     (B.Bmsy == 0.5) ~"a",
#     (F.Fmsy == 1) ~ "a",
#     (F.Fmsy >1) ~ "a",
#     (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
#     (F.Fmsy < 1 & B.Bmsy > 1) ~ "c"))
# #Plot constants
# y.max <- 5
# x.max <- 2
#
# #Plotting code
# ggplot2::ggplot(data = stock_status) +
#   ggplot2::geom_vline(xintercept = 1, linetype = "dotted", color = "grey60")+
#   ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60")+
#   ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
#   ggplot2::geom_point(aes(x = B.Bmsy,
#                           y = F.Fmsy,
#                           color = stock_status$score)) +
#   ggrepel::geom_text_repel(aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
#                                y = F.Fmsy,
#                                label = species_abr,
#                                color = stock_status$score), show.legend = FALSE,nudge_y = -0.01, nudge_x = 0.05) +
#   ggplot2::ylim(0,y.max) +
#   ggplot2::xlim(0,x.max*1.1) +
#   ggplot2::geom_text(data = unknown, aes(x = x, y = y, label = text), #Custom legend for unknown stock status
#                      size = c(4.75,rep(4,3))) +
#   ggplot2::annotate(geom="text", x=0.43, y=5, label="ATL SBN (F/Fmsy = 22.5)",
#                     color="#1B9E77")+
#   ggplot2::annotate("rect", xmin = 0.8*x.max,
#                     xmax = x.max,
#                     ymin = 0.65*y.max,
#                     ymax = 0.90*y.max,
#                     alpha = 0.1) +
#   ggplot2::scale_color_brewer(palette = "Dark2", #Change legend labels for clarity
#                               breaks = stock_status$score) +
#   ggplot2::xlab(expression(~B/B[msy])) +
#   ggplot2::ylab(expression(~F/F[msy])) +
#   ggplot2::guides(color = FALSE) +
#   ecodata::theme_ts()
#
#








