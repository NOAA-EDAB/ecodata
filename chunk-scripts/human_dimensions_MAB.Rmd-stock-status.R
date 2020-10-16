
#Get data, spread for plotting, and filter
stock_status <- ecodata::stock_status %>%
  dplyr::mutate(Code = recode(Code, "Dogfish" = "Sp. Dogfish" )) %>% 
  tidyr::spread(.,Var,Value) %>% 
  dplyr::filter(Council %in% c("MAFMC","Both")) %>% 
  dplyr::group_by(Stock) %>% 
  dplyr::mutate(score = case_when(
    (B.Bmsy <0.5) ~"a",
    (F.Fmsy >1) ~ "a", 
    (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
    (F.Fmsy < 1 & B.Bmsy > 1) ~ "c"))
#Plot constants
y.max <- 2.0 #1.75 mackerel cut off F/Fmsy is 1.8
x.max <- 2.6
#A dataframe that defines custom legend for stocks with unknown status
unknown <- data.frame(text = c("Unknown Status", "Longfin Squid",
                              "Shortfin Squid", "N. Goosefish", "S. Goosefish"),
                    x = rep(0.9*x.max,5), y = seq(0.93*y.max,1.4,-.1))

# Custom Color
custom_color<- c("#56B4E9", "#009E73", "#0072B2")
#Plotting code
ggplot2::ggplot(data = stock_status) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted")+
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed")+
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
  ggplot2::geom_point(aes(x = B.Bmsy,
                 y = F.Fmsy,
                 shape = Council,
                 color = score)) +
  ggrepel::geom_text_repel(aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
                      y = F.Fmsy,
                      label = Code, 
                      color = score), 
                  show.legend = FALSE, nudge_y = -0.01, nudge_x = 0.05) +
  ggplot2::scale_color_brewer(palette = "Dark2",
                     breaks = stock_status$score) +
  ggplot2::ylim(0,y.max) +
  ggplot2::xlim(0,x.max) +
  ggplot2::geom_text(data = unknown, aes(x = x, y = y, label = text), #Custom legend for unknown stock status
            size = c(4.75,rep(4,4))) +
  ggplot2::annotate("rect", xmin = 0.8*x.max,
           xmax = x.max,
           ymin = 0.65*y.max,
           ymax = 0.90*y.max,
           alpha = 0.1) +
  ggplot2::xlab(expression(~B/B[msy])) +
  ggplot2::ylab(expression(~F/F[msy])) +
  ggplot2::guides(color = FALSE) +
  ecodata::theme_ts()
