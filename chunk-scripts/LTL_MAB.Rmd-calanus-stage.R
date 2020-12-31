
cal <- ecodata::CalanusStage %>% 
  dplyr::filter(EPU == epu_abbr)

cal1<- cal %>% filter(Var %in% c("CIII", "CIV", "CV", "Adt"), 
                      season == "Spring") %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ylab("Calanus Stage (N/100m^3)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Calanus Stage Spring") +
  ggplot2::theme(legend.position = "bottom")
cal2<-cal %>% filter(Var %in% c("CIIIpct", "CIVpct", "CVpct", "Adtpct"), 
                      season == "Spring") %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ylab("Percent Calanus Stage") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("percent Calanus Stage Spring") +
  ggplot2::theme(legend.position = "bottom")
cal1+cal2
  
cal3<- cal %>% filter(Var %in% c("CIII", "CIV", "CV", "Adt"), 
                      season == "Summer") %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ylab("Calanus Stage (N/100m^3)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Calanus Stage Summer") +
  ggplot2::theme(legend.position = "bottom")
cal4<-cal %>% filter(Var %in% c("CIIIpct", "CIVpct", "CVpct", "Adtpct"), 
                      season == "Summer") %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ylab("Percent Calanus Stage") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("percent Calanus Stage Spring") +
  ggplot2::theme(legend.position = "bottom")
cal3+cal4

cal5<- cal %>% filter(Var %in% c("CIII", "CIV", "CV", "Adt"), 
                      season == "Fall") %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ylab("Calanus Stage (N/100m^3)") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Calanus Stage Fall") +
  ggplot2::theme(legend.position = "bottom")
cal6<-cal %>% filter(Var %in% c("CIIIpct", "CIVpct", "CVpct", "Adtpct"), 
                      season == "Fall") %>% 
  ggplot2::ggplot(aes(x = Year, y = Value, color = Var, fill = Var)) +
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  ggplot2::ylab("Percent Calanus Stage") +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("percent Calanus Stage Fall") +
  ggplot2::theme(legend.position = "bottom")
cal5+cal6
