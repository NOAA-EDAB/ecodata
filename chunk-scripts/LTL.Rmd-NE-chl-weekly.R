
GB_chl_weekly <- interp_chl_pp(epu = "GB", year = 2019,Variable = "WEEKLY_CHLOR_A_MEDIAN")
GOM_chl_weekly <- interp_chl_pp(epu = "GOM", year = 2019,Variable = "WEEKLY_CHLOR_A_MEDIAN")
ne_chl_weekly<-rbind(GB_chl_weekly, GOM_chl_weekly)

ne_chl <- ggplot2::ggplot(data = ne_chl_weekly) +
  ggplot2::geom_line(aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  ggplot2::geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggplot2::ggtitle(expression("GB chlorophyll"~italic(a)~"")) +
  ggplot2::ylim(c(0, 3))+
  ggplot2::facet_wrap(EPU~., ncol = 2)+
  ggplot2::guides(color = F) +
  ggplot2::xlab("")+
  ggplot2::ylab("") +
  ggplot2::scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  ggplot2::scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  ecodata::theme_ts()


GB_ppd_weekly <- interp_chl_pp(epu = "GB",  year = 2019,Variable = "WEEKLY_PPD_MEDIAN")
GOM_ppd_weekly <- interp_chl_pp(epu = "GOM",  year = 2019,Variable = "WEEKLY_PPD_MEDIAN")
ne_ppd_weekly<-rbind(GB_ppd_weekly, GOM_ppd_weekly)

ne_ppd <- ggplot2::ggplot(data = ne_ppd_weekly) +
  ggplot2::geom_line(aes(x = Time, y = LTM)) +
  ggplot2::geom_ribbon(aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  ggplot2::geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggplot2::ggtitle(expression("GB primary production")) +
  guides(color = F) +
  ggplot2::facet_wrap(EPU~., ncol = 2)+
  ggplot2::xlab("")+
  ggplot2::ylab("") +
  ggplot2::scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  ggplot2::scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  ecodata::theme_ts()

ne_ppd
ne_chl
