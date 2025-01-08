#'A geom to fit a LM model to a short time series
#'
#'@param mapping Set of aesthetic mappings created by \code{aes()}. By default \code{inherit.aes = TRUE}, which
#'assigns the top-level plotting \code{aes()} to the GLS geom.
#'@param data Input series to be analyzed. If NULL, data is inherited from previous layer or \code{ggplot} call.
#'@param stat stat
#'@param position position
#'@param na.rm remove NAs
#'@param show.legend show legend
#'@param inherit.aes inherit aesthetics
#'@param warn Conditional. If \code{TRUE}, a warning message will be returned when N < 30.
#'@param n Numeric. Number of points to use for trend. Default = 10.
#'@param nBootSamples Numeric. Number of bootstrap samples used to test Null hypothesis. Default = 499
#'@param pValThreshold Numeric. Significance level of the test. Default = 0.05
#'@param ... Other arguments may be passed to the stat, including fixed aesthetics.
#'
#'
#'@export
#'
#'@return If slope is significantly different from 0 (p < 0.05), then a line of best fit is returned. Otherwise output
#'is \code{NULL}.
#'
#'
#'@examples
#'library(ggplot2)
#'
#'#Generate series
#'
#'m <- 0.1
#'x <- 1:10
#'y <-  m*x + rnorm(10, sd = 0.35)
#'
#'data <- data.frame(x = x,
#'                   y = y)
#'
#'#Plot series with trend
#'ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   geom_lm(aes(x = x, y = y), n = 10)

geom_lm <- function(mapping = NULL, data = NULL, stat = "LM",
                     position = "identity",show.legend = NA, na.rm = FALSE,
                     inherit.aes = TRUE, warn = TRUE, n= 10, nBootSamples = 499,
                     pValThreshold = 0.05, ...) {

  ggplot2::layer(
    geom = GeomLM, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n,
                  na.rm = FALSE,
                  nBootSamples = nBootSamples,
                  pValThreshold = pValThreshold,
                  ...)

  )
}

## GeomLM based on GeomGLS - add dashed linetype
GeomLM <- ggplot2::ggproto("GeomLM",
                           ggplot2::Geom,
                           requird_aes = c("x", "y"),
                           extra_params = c("n", "na.rm", "pValThreshold", "nBootSamples"),

                           default_aes = ggplot2::aes(size = 2, color = NA,fill = NA,
                                                      linetype = 1, alpha = 0.7),

                           draw_key = ggplot2::draw_key_path,

                           draw_group = function(data, panel_params, coord) {

                             coords <- coord$transform(data, panel_params)
                             first_row <- coords[1, , drop = FALSE]

                             #Select default color based on positive/negative trend
                             if (coords$y[1] < coords$y[which.max(coords$x)]){
                               first_row$color <- "orange4"
                             } else {
                               first_row$color <- "purple4"
                             }

                             grid::linesGrob(
                               coords$x, coords$y,
                               gp = grid::gpar(
                                 col = first_row$color,
                                 alpha = first_row$alpha,
                                 lwd = first_row$size * ggplot2::.pt,
                                 lty = first_row$linetype
                               )
                             )
                           }
)

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

                             ## Geom removes NAs from data. arfit needs full timeseries with NAs
                             # arfit pads the time series and returns this padded data set.
                             # the returned data set is used in plotting the fitted model

                             if(n>4) {
                               dataUse <- data |>
                                 dplyr::arrange(x) |>
                                 # Select last n years
                                 dplyr::filter(x %in% (max(x)-(n-1)):max(x)) |>
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
                                                          y = predy,
                                                          pval = linear_ar1$pValue)

                                 print(fittedData)

                                 return(fittedData)
                               }
                             }

                           }
)


