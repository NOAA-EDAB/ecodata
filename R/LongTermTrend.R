#'A geom to fit a GLS model to a time series
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
#'
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
#'x <- 1:30
#'y <-  m*x + rnorm(30, sd = 0.35)
#'
#'data <- data.frame(x = x,
#'                   y = y)
#'
#'#Plot series with trend
#'ggplot(data = data) +
#'   geom_line(aes(x = x, y = y)) +
#'   geom_gls(aes(x = x, y = y))

geom_gls <- function(mapping = NULL, data = NULL, stat = "GLS",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, warn = TRUE, ...) {
  ggplot2::layer(
    geom = GeomGLS, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, warn = warn, ...)
  )
}


# defines line appearance
GeomGLS <- ggplot2::ggproto("GeomGLS",
                            ggplot2::Geom,
                            required_aes = c("x", "y"),

                            default_aes = ggplot2::aes(size = 2, color = NA,fill = NA,
                                                       linetype = 1, alpha = 0.5),

                            draw_key = ggplot2::draw_key_path,

                            draw_group = function(data, panel_params, coord) {

                              coords <- coord$transform(data, panel_params)
                              first_row <- coords[1, , drop = FALSE]

                              #Select default color based on positive/negative trend
                              if (coords$y[1] < coords$y[which.max(coords$x)]){
                                first_row$color <- "orange"
                              } else {
                                first_row$color <- "purple"
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

# where all the computation takes place
StatGLS <- ggplot2::ggproto("StatGLS",
                            ggplot2::Stat,
                            required_aes = c("x", "y"),
                            compute_group = function(data, scales, warn) {

                              `%>%` <- magrittr::`%>%`
                              data <- data %>%
                                dplyr::arrange(x) %>%
                                #Fill in time steps if there are missing values
                                tidyr::complete(x = tidyr::full_seq(min(data$x):max(data$x),1))

                              if (warn & nrow(data) < 30){
                                message("N < 30")
                              }

                              #Model fitting -------------------------------------------------------
                              constant_norm <-
                                nlme::gls(y ~ 1,
                                          data = data, na.action = na.omit)
                              constant_ar1 <-
                                try(nlme::gls(y ~ 1,
                                              data = data,
                                              correlation = nlme::corAR1(form = ~x),
                                              na.action = na.omit))
                              if (class(constant_ar1) == "try-error"){
                                return(best_lm <- data.frame(model = NA,
                                                             aicc  = NA,
                                                             coefs..Intercept = NA,
                                                             coefs.time = NA,
                                                             coefs.time2 = NA,
                                                             pval = NA))
                              }



                              # Linear model with normal error
                              linear_norm <- nlme::gls(y ~ x, data = data, na.action = na.omit)

                              # Linear model with AR1 error
                              linear_ar1 <-
                                try(nlme::gls(y ~ x,
                                              data = data,
                                              correlation = nlme::corAR1(form = ~x),
                                              na.action = na.omit))
                              if (class(linear_ar1) == "try-error"){
                                return(best_lm <- data.frame(model = NA,
                                                             aicc  = NA,
                                                             coefs..Intercept = NA,
                                                             coefs.time = NA,
                                                             coefs.time2 = NA,
                                                             pval = NA))

                              }

                              # Polynomial model with normal error
                              data$x2 <- data$x^2
                              poly_norm <- nlme::gls(y ~ x + x2, data = data, na.action = na.omit)

                              # Polynomial model with AR1 error
                              poly_ar1 <-
                                try(nlme::gls(y ~ x + x2,
                                              data = data,
                                              correlation = nlme::corAR1(form = ~x),
                                              na.action = na.omit))
                              if (class(poly_ar1) == "try-error"){
                                return(best_lm <- data.frame(model = NA,
                                                             aicc  = NA,
                                                             coefs..Intercept = NA,
                                                             coefs.time = NA,
                                                             coefs.time2 = NA,
                                                             pval = NA))
                              }



                              # Calculate AICs for all models
                              df_aicc <-
                                data.frame(model = c("poly_norm",
                                                     "poly_ar1",
                                                     "linear_norm",
                                                     "linear_ar1"),
                                           aicc  = c(AICcmodavg::AICc(poly_norm),
                                                     AICcmodavg::AICc(poly_ar1),
                                                     AICcmodavg::AICc(linear_norm),
                                                     AICcmodavg::AICc(linear_ar1)),
                                           coefs = rbind(coef(poly_norm),
                                                         coef(poly_ar1),
                                                         c(coef(linear_norm), NA),
                                                         c(coef(linear_ar1),  NA)),
                                           # Calculate overall signifiance (need to use
                                           # ML not REML for this)
                                           pval = c(anova(update(constant_norm, method = "ML"),
                                                         update(poly_norm, method = "ML"))$`p-value`[2],
                                                   anova(update(constant_ar1, method = "ML"),
                                                         update(poly_ar1, method = "ML"))$`p-value`[2],
                                                   anova(update(constant_norm, method = "ML"),
                                                         update(linear_norm, method = "ML"))$`p-value`[2],
                                                   anova(update(constant_ar1, method = "ML"),
                                                         update(linear_ar1, method = "ML"))$`p-value`[2]))

                              best_lm <- df_aicc %>%
                                dplyr::filter(grepl("linear",model)) %>% #Select only linear models (removal of poly fit)
                                dplyr::filter(aicc == min(aicc)) #Select model with lowest AICc

                              if (best_lm$model == "poly_norm") {
                                model <- poly_norm
                              } else if (best_lm$model == "poly_ar1") {
                                model <- poly_ar1
                              } else if (best_lm$model == "linear_norm") {
                                model <- linear_norm
                              } else if (best_lm$model == "linear_ar1") {
                                model <- linear_ar1
                              }

                              if (best_lm$pval < 0.05){

                                newtime <- seq(min(data$x), max(data$x), length.out=length(data$x))
                                newdata <- data.frame(x = newtime,
                                                      x2 = newtime^2)
                                lm_pred <- AICcmodavg::predictSE(model,
                                                                 newdata = newdata,
                                                                 se.fit = TRUE) #Get BLUE
                                data <- data.frame(x = data$x,
                                                   y = lm_pred$fit)

                                return(data)
                              }
                            }
)
