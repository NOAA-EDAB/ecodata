
p1<- ecodata::engagement %>%
  dplyr::filter(Region == "NE",
                Fishery == "Recreational") %>%
  dplyr::select(Community, PDI ,
                PCI, PI)  %>%
  dplyr::mutate(Community = stringr::str_remove(Community, pattern = (" ")),
                Community = stringr::str_remove(Community, pattern = (" ")),
                Community = stringr::str_remove(Community, pattern = (" ")), 
                Community = stringr::str_remove(Community, pattern = (",")), 
                Community = stringr::str_remove(Community, pattern = ("-")),
                Community = stringr::str_remove(Community, pattern = ("/"))) %>% 
  tibble::column_to_rownames(var="Community")
  
p2<- t(p1)
p2<- rbind(rep(1.5, 14) , rep(0, 14), std1 = rep(1, 14), std0.5 = rep(0.5, 14), p2) 
p3<- data.frame(p2) 

colors_border <- c("black", "grey49",  "#1B9E77", "#D95F02" ,"#7570B3")

fmsb::radarchart(p3, 
                 pcol=colors_border , plwd=2 , plty=1,
                 cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8,
                 vlcex=0.8, title = "Environmental Justice Vulnerabilty in Top Recreational Fishing Communities (New England)" )
legend(x=1.2, y=1, legend = rownames(p3[-c(1,2),]), bty = "n", pch=20 ,
      col = colors_border, text.col = "black", cex=1.2, pt.cex=1)
