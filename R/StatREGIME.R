StatREGIME <- ggplot2::ggproto("StatREGIME",
                            ggplot2::Stat,
                            required_aes = c("x", "y"),
                            compute_group = function(data, scales, warn) {

                              `%>%` <- magrittr::`%>%`
                              data <- data %>%
                                dplyr::arrange(x) %>%
                                #Fill in time steps if there are missing values
                                tidyr::complete(x = tidyr::full_seq(min(data$x):max(data$x),1))

                              #Model fitting -------------------------------------------------------
                              Regime <- rpart::rpart(y~x, data=data)
                              optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
                              optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
                              Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
                              Regime <- Regime_pruned

                              data<- as.data.frame(Regime[["splits"]]) %>%
                                dplyr::mutate(intercept = index)%>%
                                dplyr::select(intercept)

                              print(data)


                              return(data)
                            }
)
