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
