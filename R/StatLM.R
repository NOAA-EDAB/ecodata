#' StatLM Stat protto based on ecodata StatGLS
#' Need to pass arguments
#'
#' nBootSamples, pVal significance, number datapoints
#'

StatLM <- ggplot2::ggproto("StatLM",
                            ggplot2::Stat,
                            required_aes = c("x", "y"),

                            extra_params = c("n", "pValThreshold", "nBootSamples", "na.rm"),
                            compute_group = function(data, scales, warn, n, pValThreshold,  nBootSamples) {

                              `%>%` <- magrittr::`%>%`

                              ## Geom removes NAs from data. arfit needs full timeseries with NAs
                              # arfit pads the time series and returns this padded data set.
                              # the returned data set is used in plotting the fitted model


                              if(n>4) {
                                dataUse <- data %>%
                                  dplyr::arrange(x) %>%
                                  # Select last n years
                                  dplyr::filter(x %in% (max(x)-(n-1)):max(x)) %>%
                                  dplyr::mutate(x = x-min(x)+1)

                                # print("########RAW##########")
                                # print(data)
                                # print((max(data$x)-(n-1)))
                                # print(max(data$x))
                                # print(data$x-min(data$x)+1)
                                #
                                # print("#######USE##########")
                                # print(dataUse)
                                # print("#######################")

                                xmax <- max(data$x)
                                xmin <- xmax-n +1

                                # Linear model with AR1 error
                                linear_ar1 <-
                                  try(arfit::fit_real_data(dataUse,nBootSims=nBootSamples))
                                #print(linear_ar1$pValue)
                                if (is.na(linear_ar1$pValue)){
                                  return(best_lm <- data.frame(model = NA,
                                                               coefs..Intercept = NA,
                                                               coefs.time = NA,
                                                               coefs.time2 = NA,
                                                               pval = NA))

                                }

                                # pick out model. Either null (no trend) or alternative (trend)
                                if (linear_ar1$pValue <= pValThreshold) { # Trend detected
                                  dataNAs <- linear_ar1$data # arfit package returns data after padding for NAs
                                  coefs <- linear_ar1$alt$betaEst
                                  xMat <- as.matrix(cbind(rep(1,n),dataNAs$x)) # design matrix

                                # } else { # no trend
                                #   coefs <- linear_ar1$null$betaEst
                                #   xMat <- as.matrix(cbind(rep(1,n))) # design matrix
                                # }

                                  # predict

                                  predy <- xMat %*% coefs

                                  newtime <- seq(xmin, xmax, length.out=n)
                                  fittedData <- data.frame(x = newtime,
                                                       y = predy)

                                  return(fittedData)
                                }
                              }

                            }
)
