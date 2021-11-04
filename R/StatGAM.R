StatGAM <- ggplot2::ggproto("StatGAM",
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

                                ind<- ind()
                                #print(ind)

                                sp.len <- 200 # Spline length
                                nb <- 1000  # Number of bootstrap replicates
                                gam.mat <- matrix(nrow=sp.len, ncol=nb) ## create a matrix to be filled with bootstrapped splines (200 x 1000)
                                dif1 <- 1 # First derivative
                                dif2 <- 2 # Second derivative
                                ks <- 4   #If ks=3, no relationships have edf values > 2.0
                                #rand <- rep(1,length(ind$Value))

                                rand <- data.frame(rep(1,length(data$x))) %>%
                                  dplyr::rename("rand" = "rep.1..length.ind.Value..")
                                ind<- ind %>% cbind(rand)
                                #ts.length <- ind$Time
                                #print(rand)
                                #print(ts.length)
                                #### Step 1: Fit GAM to answer whether temporal autocorrelation is important? Use the residuals
                                #### from the gam and a log likelihood ratio test to calculate the "P.ac" value. A significant p.ac
                                #### value suggests a model with autocorrelated error structure explains more of the variation in the
                                #### residuals of the gam model than a model without autocorrelated error structure. Thus, using a
                                #### GAMM is necessary to account for autocorrelation in the time series...use GAMM if p.ac < 0.05.
                                #### If p.ac > 0.05, then GAM will be used to look for non-linearities, so this code will also fit
                                #### the model and provide selection criteria (i.e. edf, GCV and AIC scores from GAM and Linear model (linear) to compare)

                                gam1  <- mgcv::gam(Value ~ s(Time, bs= "tp",k = ks), optimMmethod="GCV.Cp",se = T, data = ind)
                                #print(gam1)
                                linear <- mgcv::gam(Value ~ Time, method = "GCV.Cp", se = T, data = ind)
                                dev.resid <- data.frame(stats::residuals(gam1,type='deviance')) %>%
                                  dplyr::rename("dev.resid" = "stats..residuals.gam1..type....deviance..")
                                ind<- ind %>% cbind(dev.resid)

                                lme1 <- nlme::lme(dev.resid~1,random=~1|rep(1,length(Value)),
                                                  correlation=nlme::corAR1(form=~Time),method='ML', data = ind)
                                #lme1 <- nlme::lme(dev.resid~1,random=~1|rep(1,length(Value)),correlation=nlme::corAR1(form=~Time),method='ML', data = ind)
                                lm1 <- lm(ind$dev.resid~1)
                                p.ac <- 1-pchisq(2*(logLik(lme1)[1]-logLik(lm1)[1]),2)
                                delta.GCV.gam.lm <- summary(gam1)$sp.criterion - summary(linear)$sp.criterion   #A negative value means the GAM with a smoother is a better model than the linear model
                                delta.AIC.gam.lm <- AICcmodavg::AICc(gam1) - AICcmodavg::AICc(linear)                                   #A negative value means the GAM with a smoother is a better model than the linear model
                                dev.diff.gam.lm <- summary(gam1)$dev.expl-summary(linear)$dev.expl
                                #print(ts.length)
                                #### Step 2: Fit GAMM to get selection criteria for relationships where p.ac < 0.05 (i.e. edf and AIC) and
                                #### calculate deviance explained by GAMM ("gamm.dev.expl" below)
                                # try(gamm <- mgcv::gamm(Value ~ s(Time, bs= "tp",k = ks), data = ind, optimMmethod="GCV.Cp",
                                #                  se = T,correlation=nlme::corAR1(form=~ind$Time)))
                                #if (length(gamm)==0)
                                #{
                                #print(imod)
                                gamm<- mgcv::gamm(Value ~ s(Time, bs= "tp",k = ks), data = ind, optimMmethod="GCV.Cp",
                                                  se = T,correlation=nlme::corAR1(form=~Time))

                                #gamm <- mgcv::gamm(Value ~ s(Time),se = T, data = ind, correlation=nlme::corAR1(form=~ts.length))
                                #}
                                #Fit null model to compute deviance explained by gamm
                                null  <- MASS::glmmPQL(Value ~ 1,random=list(rand=~1),family='gaussian', data = ind)
                                dr <- sum(residuals(gamm$gam)^2)
                                dn0 <- sum(residuals(null)^2)
                                gamm.dev.expl <- (dn0-dr)/dn0



                                #Step 3. Fit linear model with autocorrelation (LMAC) to get selection criteria (i.e. AIC) and calculate deviance explained by LMAC ("lmac.dev.expl" below).
                                # try(lmac <- mgcv::gamm(Value ~ Time, optimMmethod="GCV.Cp", data = ind,
                                #                  se = T,random=list(rand=~1),correlation=nlme::corAR1(form=~Time)))
                                # if (length(lmac)==0)
                                # {
                                #print(imod)
                                lmac <- mgcv::gamm(Value ~ Time,random=list(rand=~1), se = T,
                                                   correlation=nlme::corAR1(form=~Time), data = ind)
                                # lmac <- mgcv::gamm(Value ~ Time,random=list(rand=~1),se = T,
                                #                    correlation=nlme::corAR1(form=~Time), data = ind)
                                # }
                                dr2 <- sum(residuals(lmac$gam)^2)
                                lmac.dev.expl <- (dn0-dr2)/dn0

                                # Calculate difference in deviance and AIC between GAMM and LMAC ("dev.diff.gamm.lmac" and "delta.AIC.gamm.lmac" respectively below).
                                dev.diff.gamm.lmac <- gamm.dev.expl-lmac.dev.expl
                                delta.AIC.gamm.lmac <- summary(gamm$lme)$AIC-summary(lmac$lme)$AIC   #A negative value means the GAMM is a better model than the linear model with temporal autocorrelation


                                # Pull out relevant model outputs:

                                #FOR GAMM:
                                summary.gamm <- as.data.frame(cbind("GAMM",                         # Model name
                                                                    #resp.name,                      # Response variable
                                                                    #dri.name,                       # Pressure variable
                                                                    summary(gamm$lme)$AIC,          # AICc
                                                                    summary(gamm$lme)$logLik,       # log likelihood
                                                                    gamm.dev.expl,                  # Deviance explained by gamm
                                                                    summary(gamm$gam)$edf,          # estimated degrees of freedom
                                                                    summary(gamm$gam)$s.pv,         # p-value of the smoother
                                                                    summary(gamm$gam)$r.sq,         # R-squared value
                                                                    NA,                             # placeholder for GCV value used in gam
                                                                    NA,                             # placeholder for delta.GCV data used as selection criteria for gams
                                                                    as.numeric(p.ac),                           # p-value of log likelihood ratio test that tests whether the residuals from the gam are better explained by a model with or without temporal autocorrelation; p.ac values < 0.05 suggest autocorrelation is important and a gamm (instead of a gam) should be used.
                                                                    delta.AIC.gamm.lmac,            # delta AIC between gamm and lmac models; negative value means gamm is a better model than LMAC
                                                                    dev.diff.gamm.lmac              # difference in deviance explained between gamm and lmac
                                ))

                                colnames(summary.gamm)<- c("MODEL", "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")

                                #FOR Linear Model with Auto Correlation
                                summary.lmac <- as.data.frame(cbind("LMAC",                         # Model name
                                                                    #resp.name,                      # Response variable
                                                                    #dri.name,                       # Pressure variable
                                                                    summary(lmac$lme)$AIC,          # AICc
                                                                    summary(lmac$lme)$logLik,       # log likelihood
                                                                    lmac.dev.expl,                  # Deviance explained by LMAC
                                                                    summary(lmac$gam)$residual.df,  # residual degrees of freedom
                                                                    summary(lmac$gam)$p.pv[[2]],    # p-value of the null hypothesis
                                                                    summary(lmac$gam)$r.sq,         # R-squared value
                                                                    NA,                             # placeholder for GCV value used in gam
                                                                    NA,                             # placeholder for delta.GCV data used as selection criteria for gams
                                                                    NA,                             # placeholder for p.ac for gamm
                                                                    NA,                             # placeholder for delta AIC
                                                                    NA                              # placeholder for diff in deviance explained
                                ))

                                colnames(summary.lmac)<- c("MODEL", "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")

                                #FOR GAM
                                summary.gam1 <- as.data.frame(cbind("GAM",                          # Model name
                                                                    #resp.name,                       # Response variable
                                                                    #dri.name,                        # Pressure variable
                                                                    AICcmodavg::AICc(gam1),                      # AICc
                                                                    logLik(gam1),                    # Log likelihood
                                                                    summary(gam1)$dev.expl,          # deviance explained by gam
                                                                    summary(gam1)$edf,               # estimated degrees of freedom
                                                                    summary(gam1)$s.pv,              # p-value of the smoother
                                                                    summary(gam1)$r.sq,              # R-squared value
                                                                    summary(gam1)$sp.criterion,      # GCV value for gam
                                                                    delta.GCV.gam.lm,                # GAM GCV score minus LM GCV score; #A negative value means the GAM with a smoother is a better model than the linear model
                                                                    NA,                              # placeholder for p.ac value for gamm
                                                                    delta.AIC.gam.lm,                # delta AICc between gam and linear model; a negaive value means the GAM with a smoother is a better model than the linear model
                                                                    dev.diff.gam.lm                  # Difference in deviance explained between the gam and the linear model
                                ))

                                colnames(summary.gam1)<- c("MODEL", "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")

                                #FOR LINEAR MODEL
                                summary.linear <- as.data.frame(cbind("Linear",                     # Model name
                                                                      #resp.name,                          # Response variable
                                                                      #dri.name,                           # Pressure variable
                                                                      AICcmodavg::AICc(linear),                       # AICc
                                                                      logLik(linear),                     # Log likelihood
                                                                      summary(linear)$dev.expl,           # deviance explained by linear model
                                                                      summary(linear)$residual.df,        # residual degrees of freedom
                                                                      summary(linear)$p.pv[[2]],          # p-value of the null hypothesis
                                                                      summary(linear)$r.sq,               # R-squared value
                                                                      summary(linear)$sp.criterion,       # GCV value for linear model
                                                                      NA,                                 # placeholder for delta.GCV data used as selection criteria for gams
                                                                      NA,                                 # placeholder for p.ac for gamm
                                                                      NA,                                 # placeholder for delta AIC
                                                                      NA                                  # placeholder for diff in deviance explained
                                ))

                                colnames(summary.linear)<- c("MODEL",  "AICc", "logLik","dev.expl", "edf", "Pvalue","R-squared","GCV","delta.GCV","p.ac","delta.AIC","diff.dev.expl")




                                ### Identify "best model"#####
                                ### 1a) Is p.ac <= 0.05? If yes, keep and evaluate selection criteria between GAMM and LMAC as best model.If no, move to step 2.
                                ### 1b) Is GAMM edf > 2.0 for GAMM? If yes, keep. If no, LMAC is best.
                                ### 1c) Is delta.AIC > 2.0 between GAMM and LMAC. If yes, GAMM is best. If no, LMAC is most parsimonious.
                                ### 2a) If p.ac > 0.05, then revert to GAM model and ask if edf of GAM > 2.0? If yes, keep GAM. If no, linear model is best model.
                                ### 2b) Is GCV minimized in GAM compared to Linear model? If delta.GCV.gam.lm is negative then keep GAM. If delta.GCV.gam.lm is positive then linear model is best.
                                ### 2c) Is deltaAIC > 2.0 for GAM? If yes, then GAM is best model. If no, then linear model is best model.

                                summary.gamm$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))<=0.05,
                                                                 ifelse(as.numeric(as.character(summary.gamm$edf))>=1.99,
                                                                        ifelse(as.numeric(as.character(summary.gamm$delta.AIC))>=2.0,"yes","no"),"no"),"no")

                                summary.lmac$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))<=0.05,
                                                                 ifelse(as.numeric(as.character(summary.gamm$edf))>=1.99,
                                                                        ifelse(as.numeric(as.character(summary.gamm$delta.AIC))>=2.0,"no","yes"),"yes"),"no")

                                summary.gam1$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))>0.05,
                                                                 ifelse(as.numeric(as.character(summary.gam1$edf))>1.99,
                                                                        ifelse(as.numeric(as.character(summary.gam1$delta.GCV))<0,
                                                                               ifelse(as.numeric(as.character(summary.gam1$delta.AIC))<=-2.0,"yes","no"),"no"),"no"),"no")

                                summary.linear$best.model = ifelse(as.numeric(as.character(summary.gamm$p.ac))>0.05,
                                                                   ifelse(as.numeric(as.character(summary.gam1$edf))<1.99,"yes",
                                                                          ifelse(as.numeric(as.character(summary.gam1$delta.GCV))>0,"yes",
                                                                                 ifelse(as.numeric(as.character(summary.gam1$delta.AIC))<=-2.0,"no","yes"))),"no")



                                allSummary<- data.frame()
                                allSummary <- rbind(allSummary, summary.gamm, summary.lmac, summary.gam1,
                                                    summary.linear)
                                choseMod<- allSummary %>% dplyr::filter(allSummary$best.model == "yes")
                                #print(choseMod)
                                new.dat<-data.frame(Time = ind$Time, # newdata
                                                    Value = ind$Value)

                                dat<- if(choseMod$MODEL == "GAM"){
                                  dat<- data.frame(pred = mgcv::predict.gam(gam1, new.dat, se.fit = TRUE)) %>% # calc predicted values
                                    dplyr::mutate(Time = ind$Time) %>%
                                    left_join(ind) %>% # join with orig data set
                                    dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci
                                                  lower = pred.fit - pred.se.fit,
                                                  choseMod = c("GAM"))
                                  fm1 <- gratia::derivatives(gam1)
                                  trend <- fm1 %>%
                                    mutate(upper_bound = ifelse(upper < 0,
                                                                "upper", "NA"),
                                           lower_bound = ifelse(lower > 0,
                                                                "lower", "NA")) %>%
                                    mutate(Time = round(data)) %>%
                                    group_by(Time) %>%
                                    slice(1) %>%
                                    ungroup() %>%
                                    select(Time, upper_bound, lower_bound)
                                  dat<- dat %>% left_join(trend) %>%
                                    mutate(cat2 = case_when(upper_bound == "upper" & lower_bound == "NA" ~ 1,
                                                            upper_bound == "NA" & lower_bound == "lower" ~ 0,
                                                            upper_bound == "NA" & lower_bound == "NA" ~ -1))
                                } else if(choseMod$MODEL == "Linear"){
                                  dat<- data.frame(pred = predict(linear, new.dat, se.fit = TRUE)) %>% # calc predicted values
                                    dplyr::mutate(Time = ind$Time) %>%
                                    left_join(ind) %>% # join with orig data set
                                    dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci
                                                  lower = pred.fit - pred.se.fit,
                                                  choseMod = c("Linear"),
                                                  upper_bound = ifelse(linear$coefficients[2] > 0, "upper", "NA"),
                                                  lower_bound = ifelse(linear$coefficients[2] < 0, "lower", "NA")) %>%
                                    dplyr::mutate(cat2 = case_when(upper_bound == "upper" ~ 0,
                                                                   lower_bound == "lower" ~ 1,
                                                                   upper_bound == "NA" & lower_bound == "NA" ~ -1))
                                } else if(choseMod$MODEL == "GAMM"){
                                  dat <- data.frame(pred = mgcv::predict.gam(gamm$gam, new.dat, se.fit = TRUE)) %>% # calc predicted values
                                    dplyr::mutate(Time = ind$Time) %>%
                                    left_join(ind) %>% # join with orig data set
                                    dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci
                                                  lower = pred.fit - pred.se.fit,
                                                  choseMod = c("GAMM"))
                                  fm1 <- gratia::derivatives(gamm$gam)
                                  trend <- fm1 %>%
                                    mutate(upper_bound = ifelse(upper < 0,"upper", "NA"),
                                           lower_bound = ifelse(lower > 0,"lower", "NA")) %>%
                                    mutate(Time = round(data)) %>%
                                    group_by(Time) %>%
                                    slice(1) %>%
                                    ungroup() %>%
                                    select(Time, upper_bound, lower_bound)
                                  dat<- dat %>% left_join(trend) %>%
                                    mutate(cat2 = case_when(upper_bound == "upper" & lower_bound == "NA" ~ 1,
                                                            upper_bound == "NA" & lower_bound == "lower" ~ 0,
                                                            upper_bound == "NA" & lower_bound == "NA" ~ -1))
                                } else if(choseMod$MODEL == "LMAC"){
                                  dat<- data.frame(pred = mgcv::predict.gam(lmac$gam, new.dat, se.fit = TRUE)) %>% # calc predicted values
                                    dplyr::mutate(Time = ind$Time) %>%
                                    left_join(ind) %>% # join with orig data set
                                    dplyr::mutate(upper = pred.fit + pred.se.fit, # calc upper and lower ci
                                                  lower = pred.fit - pred.se.fit,
                                                  choseMod = c("LMAC"),
                                                  upper_bound = ifelse(lmac$lme$coefficients$fixed[2] > 0, "upper", "NA"),
                                                  lower_bound = ifelse(lmac$lme$coefficients$fixed[2] < 0, "lower", "NA")) %>%
                                    dplyr::mutate(cat2 = case_when(upper_bound == "upper" ~ 0,
                                                                   lower_bound == "lower" ~ 1,
                                                                   upper_bound == "NA" & lower_bound == "NA" ~ -1))
                                } else(print("No Model"))

                                ###### Andy's loop-
                                catlabel <- 1
                                df<- dat %>% select(Time, cat2) %>%
                                  mutate(change = cat2,
                                         cat = NA)
                                for (irow in 1:nrow(df)) {
                                  #print(irow)
                                  if (irow == 1) {
                                    df$cat[1] <- catlabel
                                    next
                                  }

                                  if ((df$change[irow]-df$change[irow-1]) == 0) {
                                  } else {
                                    catlabel=catlabel + 1
                                  }
                                  df$cat[irow] <- catlabel
                                  #
                                }

                                dat<- dat%>% left_join(df) %>%
                                  mutate(cat = as.character(cat),
                                         cat2 = as.character(cat2))
                                #write.csv(dat, "lsst.csv")
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
