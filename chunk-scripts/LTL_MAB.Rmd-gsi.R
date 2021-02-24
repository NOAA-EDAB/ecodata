
# ecodata::gsi %>% 
#   dplyr::mutate(Year = floor(Time)) %>% 
#   dplyr::group_by(Year) %>% 
#   dplyr::summarise(Value = mean(Value)) %>% 
#   dplyr::mutate(hline = mean(Value)) %>% 
#   dplyr::rename(Time = Year) %>% 
#   ggplot2::ggplot(aes(x = Time, y = Value)) +
#   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
#       xmin = x.shade.min , xmax = x.shade.max,
#       ymin = -Inf, ymax = Inf) +
#   ecodata::geom_gls() +
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::ylab("Gulf Stream position anomaly") +
#   ggplot2::xlab(element_blank())+
#   ggplot2::ggtitle("Gulf Stream Index") +
#   ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
#   ggplot2::geom_hline(aes(yintercept = hline),
#            size = hline.size,
#            alpha = hline.alpha,
#            linetype = hline.lty)+
#   ecodata::theme_ts() +
#   ggplot2::theme(strip.text=element_text(hjust=0,
#                                 face = "italic"))

gsi<- ecodata::gsi %>% 
  dplyr::mutate(Year = floor(Time)) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  dplyr::mutate(hline = mean(Value)) %>%
  dplyr::rename(Time = Year)

gsi_old<- ecodata::gsi_old %>% 
  dplyr::mutate(Year = floor(Time)) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Value = mean(Value)) %>%
  dplyr::mutate(hline = mean(Value)) %>%
  dplyr::rename(Time = Year)

ggplot2::ggplot() +
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ecodata::geom_gls(aes(x = gsi$Time, y = gsi$Value)) +
  ggplot2::geom_line(aes(x = gsi$Time, y = gsi$Value)) +
  ggplot2::geom_point(aes(x = gsi$Time, y = gsi$Value)) +
  #ecodata::geom_gls(aes(x = gsi_old$Time, y = gsi_old$Value)) +
  #ggplot2::geom_line(aes(x = gsi_old$Time, y = gsi_old$Value), color ="red") +
  #ggplot2::geom_point(aes(x = gsi_old$Time, y = gsi_old$Value), color = "red") +
  ggplot2::ylab("Gulf Stream position anomaly") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Gulf Stream Index") +
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01))+
  #ggplot2::geom_hline(aes(yintercept = hline),
  #         size = hline.size,
  #         alpha = hline.alpha,
  #         linetype = hline.lty)+
  ecodata::theme_ts() +
  ggplot2::theme(strip.text=element_text(hjust=0,
                                face = "italic"), 
                 axis.title.y = element_text(size = 7))+
  ecodata::theme_title()
