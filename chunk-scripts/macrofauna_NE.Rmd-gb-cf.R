
a<-knitr::include_graphics(file.path(image.dir, "GBcondition_2020_viridis_final.jpg"))
b<-knitr::include_graphics(file.path(image.dir, "GOMcondition_2020_viridis_final.jpg"))

cowplot::plot_grid(a, b)
