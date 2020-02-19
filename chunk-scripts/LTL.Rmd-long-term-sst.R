
lt_sst <- ecodata::long_term_sst %>%
  mutate(hline = mean(Value, na.rm = TRUE))

hline <- mean(lt_sst$Value)

lt_sst %>%
  ggplot(aes(x = Time, y = Value, group = Var)) +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = hline),
             size = hline.size,
             alpha = hline.alpha,
           linetype = hline.lty)+
  ylab("Temperature (C)") +
  xlab(element_blank())+
  ggtitle("Long-term SST") +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(1840,2010,10))+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
