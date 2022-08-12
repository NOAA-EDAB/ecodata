StatREGIME <- ggplot2::ggproto("StatREGIME",
                               ggplot2::Stat,
                            #required_aes = c("x", "y"),
                            compute_group = function(data, scales, warn) {

                              `%>%` <- magrittr::`%>%`
                              data <- data %>%
                                dplyr::arrange(x) %>%
                                #Fill in time steps if there are missing values
                                tidyr::complete(x = tidyr::full_seq(min(data$x):max(data$x),1))

                              #Regime fitting -------------------------------------------------------
                              Regime <- rpart::rpart(y~x, data=data)
                              optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
                              optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
                              Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
                              Regime <- Regime_pruned

                              split_calc<- as.data.frame(Regime[["splits"]]) %>%
                                dplyr::mutate(xintercept = index)%>%
                                dplyr::select(xintercept)

                              split_ceiling<- split_calc %>%
                                dplyr::mutate(Time = ceiling(xintercept))

                              dat<- data %>% left_join(split_ceiling)

                              #dat<- as.vector(data1$xintercept)
                              browser()

                              print(dat)
                              return(tat)

                            }

)
