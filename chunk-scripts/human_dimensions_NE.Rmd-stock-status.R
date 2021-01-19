
#Get data, spread for plotting, and filter
stock_status <- ecodata::stock_status %>%
  dplyr::mutate(Code = dplyr::recode(Code, "Dogfish" = "Sp. Dogfish" )) %>% 
  tidyr::spread(.,Var,Value) %>% 
  dplyr::filter(Council %in% c("NEFMC","Both")) %>% 
  dplyr::group_by(Stock) %>% 
  dplyr::mutate(score = case_when(
    (B.Bmsy <0.5) ~"a",
    (F.Fmsy >1) ~ "a", 
    (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
    (F.Fmsy < 1 & B.Bmsy > 1) ~ "c"))
#Plot constants
y.max <- 1.5
x.max <- 10
all_missing <- stock_status %>%
  dplyr::filter(is.na(B.Bmsy),is.na(F.Fmsy)) %>% 
  dplyr::select(Code, Council)
b_missing <- stock_status %>%
  dplyr::filter(is.na(B.Bmsy), !is.na(F.Fmsy)) %>% 
  dplyr::select(Code, Council)
f_missing <- stock_status %>%
  dplyr::filter(is.na(F.Fmsy), !is.na(B.Bmsy)) %>% 
  dplyr::select(Code, Council)
#A dataframe that defines custom legend for stocks with unknown status
all.df <- data.frame(text = all_missing$Code,
                    x = rep(x.max*0.9,length(all_missing$Code)),
                    y = seq(1.45,0.7, length.out = 11))
b.df <- data.frame(text = b_missing$Code,
                    x = rep(x.max*0.7,length(b_missing$Code)),
                    y = seq(1.45,1.30, length.out = 3))
f.df <- data.frame(text = f_missing$Code,
                    x = rep(x.max*0.5,length(f_missing$Code)),
                    y = seq(1.45,1, length.out = 7))

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
                      label = Code,
                      color = stock_status$score), show.legend = FALSE,nudge_y = -0.01, nudge_x = 0.05) +
  ggplot2::ylim(0,y.max) +
  ggplot2::xlim(0,x.max*1.1) +
  ggplot2::geom_text(data = all.df, aes(x = x, y = y, label = text),show.legend = FALSE, size = 3)+
  ggplot2::geom_text(data = b.df, aes(x = x, y = y, label = text),show.legend = FALSE, size = 3)+
  ggplot2::geom_text(data = f.df, aes(x = x, y = y, label = text),show.legend = FALSE, size = 3)+
  ggplot2::scale_color_brewer(palette = "Dark2", #Change legend labels for clarity
                     breaks = stock_status$score) +
  ggplot2::annotate("rect", xmin = 0.924*x.max,
           xmax = 1.08*x.max,
           ymin = 0.645*y.max,
           ymax = 0.98*y.max,
           alpha = 0.01) +
  ggplot2::annotate("text", x = 9, y = 1.5, label = "F and B missing", fontface =2, size = 3)+
  ggplot2::annotate("rect",  
             xmin = 0.70*x.max,
             xmax = 0.85*x.max,
             ymin = 0.30*y.max,
             ymax = 0.98*y.max,
             alpha = 0.01) +
  ggplot2::annotate("text", x = 7, y = 1.5, label = "B missing", fontface =2, size = 3)+
  ggplot2::annotate("rect", xmin = 0.509*x.max,
           xmax = 0.681*x.max,
           ymin = 0.65*y.max,
           ymax = 0.98*y.max,
           alpha = 0.01) +
  ggplot2::annotate("text", x = 5, y = 1.5, label = "F missing", fontface =2, size = 3)+
  ggplot2::xlab(expression(~B/B[msy])) +
  ggplot2::ylab(expression(~F/F[msy])) +
  ggplot2::guides(color = FALSE) +
  ecodata::theme_ts()
