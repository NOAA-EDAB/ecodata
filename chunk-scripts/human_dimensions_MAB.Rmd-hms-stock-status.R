
y.max <- 5
x.max <- 2
#Get data, spread for plotting, and filter
unknown <- data.frame(text = c("Unknown Status", "ATL SBH", "SPF", "WA BFT"),
                      x = rep(0.9*x.max,4), y = seq(0.88*y.max,3.5,-0.3))



stock_status<-ecodata::hms_stock_status %>% 
  tidyr::spread(.,Var,Value) %>% 
  tidyr::separate(stock, c("species_abr", "spp"), ":") %>% 
  dplyr::group_by(spp) %>% 
  dplyr::mutate(score = case_when(
    (B.Bmsy <0.5) ~"a",
    (B.Bmsy == 0.5) ~"a", 
    (F.Fmsy == 1) ~ "a",
    (F.Fmsy >1) ~ "a", 
    (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
    (F.Fmsy < 1 & B.Bmsy > 1) ~ "c"))
#Plot constants


#Plotting code
ggplot2::ggplot(data = stock_status) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted", color = "grey60")+
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60")+
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  ggplot2::geom_point(aes(x = B.Bmsy,
                          y = F.Fmsy,
                          color = stock_status$score)) +
  ggrepel::geom_text_repel(aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
                               y = F.Fmsy,
                               label = species_abr,
                               color = stock_status$score), show.legend = FALSE,
                           nudge_y = -0.01, nudge_x = 0.05) +
  ggplot2::ylim(0,y.max) +
  ggplot2::xlim(0,x.max*1.1) +
  ggplot2::geom_text(data = unknown, aes(x = x, y = y, label = text), #Custom legend for unknown stock status
                     size = c(4.75,rep(4,3))) +
  ggplot2::annotate(geom="text", x=0.43, y=5, label="ATL SBN (F/Fmsy = 22.5)",
                    color="#1B9E77")+
  ggplot2::annotate("rect", xmin = 0.8*x.max,
                    xmax = x.max,
                    ymin = 0.65*y.max,
                    ymax = 0.90*y.max,
                    alpha = 0.1) +
  ggplot2::scale_color_brewer(palette = "Dark2", #Change legend labels for clarity
                              breaks = stock_status$score) +
  ggplot2::xlab(expression(~B/B[msy])) +
  ggplot2::ylab(expression(~F/F[msy])) +
  ggplot2::guides(color = FALSE) +
  ecodata::theme_ts()
