
# ecodata::bottom_temp_glorys %>% 
#   dplyr::filter(!EPU == "MAB", 
#                 !EPU == "SS")%>%
#   dplyr::group_by(EPU) %>% 
#   dplyr::mutate(hline = mean(Value)) %>% 
# ggplot2::ggplot() +
#   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
#       xmin = x.shade.min , xmax = x.shade.max,
#       ymin = -Inf, ymax = Inf) +
#   ggplot2::geom_line(aes(x = Time, y = Value)) +
#   #ecodata::geom_gls(aes(x = Time, y = Value)) +
#   ggplot2::geom_point(aes(x = Time, y = Value), size = 1) +
#   ggplot2::ylab("Temperature (C)") +
#   ggplot2::xlab(element_blank())+
#   
#   ggplot2::ggtitle("Bottom temperature GLORYS anomaly") +
#   ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
#   ggplot2::theme(strip.text=element_text(hjust=0),
#         plot.title = element_text(size = 12))+
#   ggplot2::geom_hline(aes(yintercept = hline),
#            size = hline.size,
#            alpha = hline.alpha,
#            linetype = hline.lty) +
#   ggplot2::facet_wrap(~EPU)+
#   ecodata::theme_ts()


temp_anom <- ecodata::bottom_temp %>% 
  dplyr::filter(EPU %in% c("GB")) %>% 
  tidyr::complete(Time = tidyr::full_seq(min(bottom_temp$Time):max(bottom_temp$Time),1),
           tidyr::nesting(Var)) %>% 
  dplyr::mutate(hline = 0)%>%
 dplyr::filter(Var == "bottom temp anomaly in situ")

gl_bt<- ecodata::bottom_temp_glorys%>% 
  dplyr::filter(EPU %in% c("GB"))
#bot_temp<-temp_anom %>%
# dplyr::filter(Var == "bottom temp anomaly in situ") %>%
ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = temp_anom$Time, y = temp_anom$Value), color = "black") +
  ggplot2::geom_point(aes(x = temp_anom$Time, y = temp_anom$Value), size = 1, color = "black") +
  ggplot2::geom_point(aes(x = gl_bt$Time, y = gl_bt$Value), size = 1, color = "red") +
  ggplot2::geom_line(aes(x = gl_bt$Time, y = gl_bt$Value), color = "red") +
  ecodata::geom_gls(aes(x = temp_anom$Time, y = temp_anom$Value)) +
  ggplot2::ylab("Temperature (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Geroges Bank Bottom temperature anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  #ggplot2::facet_wrap(~ gl_bt$EPU)+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty) +
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()


temp_anom <- ecodata::bottom_temp %>% 
  dplyr::filter(EPU %in% c("GOM")) %>% 
  tidyr::complete(Time = tidyr::full_seq(min(bottom_temp$Time):max(bottom_temp$Time),1),
           tidyr::nesting(Var)) %>% 
  dplyr::mutate(hline = 0)%>%
 dplyr::filter(Var == "bottom temp anomaly in situ")

gl_bt<- ecodata::bottom_temp_glorys%>% 
  dplyr::filter(EPU %in% c("GOM"))
#bot_temp<-temp_anom %>%
# dplyr::filter(Var == "bottom temp anomaly in situ") %>%
ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line(aes(x = temp_anom$Time, y = temp_anom$Value), color = "black") +
  ggplot2::geom_point(aes(x = temp_anom$Time, y = temp_anom$Value), size = 1, color = "black") +
  ggplot2::geom_point(aes(x = gl_bt$Time, y = gl_bt$Value), size = 1, color = "red") +
  ggplot2::geom_line(aes(x = gl_bt$Time, y = gl_bt$Value), color = "red") +
  ecodata::geom_gls(aes(x = temp_anom$Time, y = temp_anom$Value)) +
  ggplot2::ylab("Temperature (C)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Gulf of Maine Bottom temperature anomaly") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::theme(strip.text=element_text(hjust=0),
        plot.title = element_text(size = 12))+
  #ggplot2::facet_wrap(~ gl_bt$EPU)+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty) +
  ecodata::theme_ts()+
  ecodata::theme_facet()+
  ecodata::theme_title()
