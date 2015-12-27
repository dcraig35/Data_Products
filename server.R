#setwd("~/Data Products")

library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(cowplot)

raw_data_lb <- read.csv("data/lb.csv", header = TRUE)
raw_data_lv <- read.csv("data/lv.csv", header = TRUE)
raw_data_ll <- read.csv("data/ll.csv", header = TRUE)
raw_data_sd <- read.csv("data/sd.csv", header = TRUE)
raw_data_gla <- read.csv("data/gla.csv", header = TRUE)
raw_data_visn <- read.csv("data/visn.csv", header = TRUE)
raw_data_wla <- read.csv("data/wla.csv", header = TRUE)

#create the linear models for consult volume as a function of providers
#and overall unique patients.  This assumes that both provider volume and unique 
#patients act as independent variables.  All but LL have an R^2 value of at least
#75% (LL was ~65%) and the residuals seem consistent among each of the sites.

lb_con_fit <- lm(Consults ~ Providers + Unique_Outpat_Month, raw_data_lb)
lv_con_fit <- lm(Consults ~ Providers + Unique_Outpat_Month, raw_data_lv)
ll_con_fit <- lm(Consults ~ Providers + Unique_Outpat_Month, raw_data_ll)
sd_con_fit <- lm(Consults ~ Providers + Unique_Outpat_Month, raw_data_sd)
gla_con_fit <- lm(Consults ~ Providers + Unique_Outpat_Month, raw_data_gla)
visn_con_fit <- lm(Consults ~ Providers + Unique_Outpat_Month, raw_data_visn)

#For each of the models the AIC value was used as the judging criteria.  Whichever
#model had the lowest AIC value was selected regardless of differences in staff grouping.
#However, since staff volume is the variable trying to ultimately be estimated it 
#can't be included as part of the model.  Therefore only consults was used as a 
#regressor.  When doing this the Poisson glm was compared to the lm and even though
#they were very similar the Poisson model appears to be the better of the two
#since both the model and regressor are considered significant, the R^2 for the lm
#was consistantly < 50% and the max % difference between the model value and the 
#actual value was always lower with the glm.

sd_wl_fit1 <- glm(Total_Unique_POandSI ~ Consults, family = poisson(link = log), data = raw_data_sd)
lb_wl_fit1 <- glm(Total_Unique_POandSI ~ Consults, family = poisson(link = log), data = raw_data_lb)
lv_wl_fit1 <- glm(Total_Unique_POandSI ~ Consults, family = poisson(link = log), data = raw_data_lv)
ll_wl_fit1 <- glm(Total_Unique_POandSI ~ Consults, family = poisson(link = log), data = raw_data_ll)
gla_wl_fit1 <- glm(Total_Unique_POandSI ~ Consults, family = poisson(link = log), data = raw_data_gla)
visn_wl_fit1 <- glm(Total_Unique_POandSI ~ Consults, family = poisson(link = log), data = raw_data_visn)

#calculate the month to month percentage change in the number of providers and the
#number of unique patients.

sd_month_prov_chng <-  as.numeric((raw_data_sd$Providers[2] - raw_data_sd$Providers[1])/raw_data_sd$Providers[2] * 100)
sd_month_uni_chng <-  as.numeric((raw_data_sd$Unique_Outpat_Month[2] - raw_data_sd$Unique_Outpat_Month[1])/raw_data_sd$Unique_Outpat_Month[2] * 100)

lb_month_prov_chng <-  as.numeric((raw_data_lb$Providers[2] - raw_data_lb$Providers[1])/raw_data_lb$Providers[2] * 100)
lb_month_uni_chng <-  as.numeric((raw_data_lb$Unique_Outpat_Month[2] - raw_data_lb$Unique_Outpat_Month[1])/raw_data_lb$Unique_Outpat_Month[2] * 100)

lv_month_prov_chng <-  as.numeric((raw_data_lv$Providers[2] - raw_data_lv$Providers[1])/raw_data_lv$Providers[2] * 100)
lv_month_uni_chng <-  as.numeric((raw_data_lv$Unique_Outpat_Month[2] - raw_data_lv$Unique_Outpat_Month[1])/raw_data_lv$Unique_Outpat_Month[2] * 100)

ll_month_prov_chng <-  as.numeric((raw_data_ll$Providers[2] - raw_data_ll$Providers[1])/raw_data_ll$Providers[2] * 100)
ll_month_uni_chng <-  as.numeric((raw_data_ll$Unique_Outpat_Month[2] - raw_data_ll$Unique_Outpat_Month[1])/raw_data_ll$Unique_Outpat_Month[2] * 100)

gla_month_prov_chng <-  as.numeric((raw_data_gla$Providers[2] - raw_data_gla$Providers[1])/raw_data_gla$Providers[2] * 100)
gla_month_uni_chng <-  as.numeric((raw_data_gla$Unique_Outpat_Month[2] - raw_data_gla$Unique_Outpat_Month[1])/raw_data_gla$Unique_Outpat_Month[2] * 100)

visn_month_prov_chng <-  as.numeric((raw_data_visn$Providers[2] - raw_data_visn$Providers[1])/raw_data_visn$Providers[2] * 100)
visn_month_uni_chng <-  as.numeric((raw_data_visn$Unique_Outpat_Month[2] - raw_data_visn$Unique_Outpat_Month[1])/raw_data_visn$Unique_Outpat_Month[2] * 100)

for(i in 3 : 36){
     
     sd_month_prov_chng[i-1] <-  as.numeric((raw_data_sd$Providers[i] - raw_data_sd$Providers[i-1])/raw_data_sd$Providers[i] * 100)    
     sd_month_uni_chng[i-1] <-  as.numeric((raw_data_sd$Unique_Outpat_Month[i] - raw_data_sd$Unique_Outpat_Month[i-1])/raw_data_sd$Unique_Outpat_Month[i] * 100)
     
     lb_month_prov_chng[i-1] <-  as.numeric((raw_data_lb$Providers[i] - raw_data_lb$Providers[i-1])/raw_data_lb$Providers[i] * 100)    
     lb_month_uni_chng[i-1] <-  as.numeric((raw_data_lb$Unique_Outpat_Month[i] - raw_data_lb$Unique_Outpat_Month[i-1])/raw_data_lb$Unique_Outpat_Month[i] * 100)
     
     lv_month_prov_chng[i-1] <-  as.numeric((raw_data_lv$Providers[i] - raw_data_lv$Providers[i-1])/raw_data_lv$Providers[i] * 100)    
     lv_month_uni_chng[i-1] <-  as.numeric((raw_data_lv$Unique_Outpat_Month[i] - raw_data_lv$Unique_Outpat_Month[i-1])/raw_data_lv$Unique_Outpat_Month[i] * 100)
     
     ll_month_prov_chng[i-1] <-  as.numeric((raw_data_ll$Providers[i] - raw_data_ll$Providers[i-1])/raw_data_ll$Providers[i] * 100)    
     ll_month_uni_chng[i-1] <-  as.numeric((raw_data_ll$Unique_Outpat_Month[i] - raw_data_ll$Unique_Outpat_Month[i-1])/raw_data_ll$Unique_Outpat_Month[i] * 100)
     
     gla_month_prov_chng[i-1] <-  as.numeric((raw_data_gla$Providers[i] - raw_data_gla$Providers[i-1])/raw_data_gla$Providers[i] * 100)    
     gla_month_uni_chng[i-1] <-  as.numeric((raw_data_gla$Unique_Outpat_Month[i] - raw_data_gla$Unique_Outpat_Month[i-1])/raw_data_gla$Unique_Outpat_Month[i] * 100)
     
     visn_month_prov_chng[i-1] <-  as.numeric((raw_data_visn$Providers[i] - raw_data_visn$Providers[i-1])/raw_data_visn$Providers[i] * 100)    
     visn_month_uni_chng[i-1] <-  as.numeric((raw_data_visn$Unique_Outpat_Month[i] - raw_data_visn$Unique_Outpat_Month[i-1])/raw_data_visn$Unique_Outpat_Month[i] * 100)
     
}

#Subset the LB, LV, and WLA data for the time periods when they were compliant
#with the timeliness monitor.

lv_comp <- raw_data_lv[which(raw_data_lv$X5_Days_Month > 74.99),]

lb_comp <- raw_data_lb[which(raw_data_lb$X5_Days_Month > 74.99),]

wla_comp <- raw_data_wla[which(raw_data_wla$X5_Days_Month > 74.99),]

overall_comp <- rbind(lv_comp, wla_comp)

overall_comp <- rbind(overall_comp, wla_comp)

#Calculate the average production per staff group for each facility and
#overall.

overall_prod_rate <- with(overall_comp, data.frame(G1_POSI = Total_Unique_POandSI/Staff_Group_1,
                                                   G1_SI = Unique_SI/Staff_Group_1,
                                                   G1_PO = Unique_PO/Staff_Group_1
))

#Calculate the overall mean and SD.

overall_prod_rate_mean_sd <- colMeans(overall_prod_rate)

temp <- colwise(sd) (overall_prod_rate)

overall_prod_rate_mean_sd <- rbind(overall_prod_rate_mean_sd, temp)

#Calculate the mean and SD for the consult volumes during the same time
#periods.

overall_mean_consults <- mean(overall_comp$Consults)
overall_sd_consults <- sd(overall_comp$Consults)

#con_thresh <- overall_mean_consults - overall_sd_consults

prod_thresh <<- overall_prod_rate_mean_sd[1,] - overall_prod_rate_mean_sd[2,]

#Use the last actual provider and unique patient count as the starting point for
#estimating the future number of providers and unique patients over the next 
#five years based on the mean month to month percentage change 

sd_prov_est <- raw_data_sd$Providers[36] + ((raw_data_sd$Providers[36]*mean(sd_month_prov_chng)/100))
sd_uni_est <- raw_data_sd$Unique_Outpat_Month[36] + ((raw_data_sd$Unique_Outpat_Month[36]*mean(sd_month_uni_chng)/100))

lb_prov_est <- raw_data_lb$Providers[36] + ((raw_data_lb$Providers[36]*mean(lb_month_prov_chng)/100))
lb_uni_est <- raw_data_lb$Unique_Outpat_Month[36] + ((raw_data_lb$Unique_Outpat_Month[36]*mean(lb_month_uni_chng)/100))

lv_prov_est <- raw_data_lv$Providers[36] + ((raw_data_lv$Providers[36]*mean(lv_month_prov_chng)/100))
lv_uni_est <- raw_data_lv$Unique_Outpat_Month[36] + ((raw_data_lv$Unique_Outpat_Month[36]*mean(lv_month_uni_chng)/100))

ll_prov_est <- raw_data_ll$Providers[36] + ((raw_data_ll$Providers[36]*mean(ll_month_prov_chng)/100))
ll_uni_est <- raw_data_ll$Unique_Outpat_Month[36] + ((raw_data_ll$Unique_Outpat_Month[36]*mean(ll_month_uni_chng)/100))

gla_prov_est <- raw_data_gla$Providers[36] + ((raw_data_gla$Providers[36]*mean(gla_month_prov_chng)/100))
gla_uni_est <- raw_data_gla$Unique_Outpat_Month[36] + ((raw_data_gla$Unique_Outpat_Month[36]*mean(gla_month_uni_chng)/100))

visn_prov_est <- raw_data_visn$Providers[36] + ((raw_data_visn$Providers[36]*mean(visn_month_prov_chng)/100))
visn_uni_est <- raw_data_visn$Unique_Outpat_Month[36] + ((raw_data_visn$Unique_Outpat_Month[36]*mean(visn_month_uni_chng)/100))

lv_prov_est <- raw_data_lv$Providers[36] + ((raw_data_lv$Providers[36]*mean(lv_month_prov_chng)/100))
lv_uni_est <- raw_data_lv$Unique_Outpat_Month[36] + ((raw_data_lv$Unique_Outpat_Month[36]*mean(lv_month_uni_chng)/100))

for(i in 2 : 60) {
     
    lv_prov_est[i] <- lv_prov_est[i-1] + ((lv_prov_est[i-1] * mean(lv_month_prov_chng)/100))
    lv_uni_est[i] <- lv_uni_est[i-1] + ((lv_uni_est[i-1] * mean(lv_month_uni_chng)/100)) 

    sd_prov_est[i] <- sd_prov_est[i-1] + ((sd_prov_est[i-1] * mean(sd_month_prov_chng)/100))
    sd_uni_est[i] <- sd_uni_est[i-1] + ((sd_uni_est[i-1] * mean(sd_month_uni_chng)/100))
                    
    lb_prov_est[i] <- lb_prov_est[i-1] + ((lb_prov_est[i-1] * mean(lb_month_prov_chng)/100))
    lb_uni_est[i] <- lb_uni_est[i-1] + ((lb_uni_est[i-1] * mean(lb_month_uni_chng)/100))
                   
    ll_prov_est[i] <- ll_prov_est[i-1] + ((ll_prov_est[i-1] * mean(ll_month_prov_chng)/100))
    ll_uni_est[i] <- ll_uni_est[i-1] + ((ll_uni_est[i-1] * mean(ll_month_uni_chng)/100))
                 
    gla_prov_est[i] <- gla_prov_est[i-1] + ((gla_prov_est[i-1] * mean(gla_month_prov_chng)/100))
    gla_uni_est[i] <- gla_uni_est[i-1] + ((gla_uni_est[i-1] * mean(gla_month_uni_chng)/100))
                    
    visn_prov_est[i] <- visn_prov_est[i-1] + ((visn_prov_est[i-1] * mean(visn_month_prov_chng)/100))
    visn_uni_est[i] <- visn_uni_est[i-1] + ((visn_uni_est[i-1] * mean(visn_month_uni_chng)/100))
    
    }

#calculate the projected consult volume based on the estimated number of providers
#and unique patients.

lv_est_con_vol <- coef(lv_con_fit)[1] + coef(lv_con_fit)[2] * lv_prov_est + coef(lv_con_fit)[3] * lv_uni_est     
sd_est_con_vol <- coef(sd_con_fit)[1] + coef(sd_con_fit)[2] * sd_prov_est + coef(sd_con_fit)[3] * sd_uni_est
lb_est_con_vol <- coef(lb_con_fit)[1] + coef(lb_con_fit)[2] * lb_prov_est + coef(lb_con_fit)[3] * lb_uni_est
ll_est_con_vol <- coef(ll_con_fit)[1] + coef(ll_con_fit)[2] * ll_prov_est + coef(ll_con_fit)[3] * ll_uni_est
gla_est_con_vol <- coef(gla_con_fit)[1] + coef(gla_con_fit)[2] * gla_prov_est + coef(gla_con_fit)[3] * gla_uni_est
visn_est_con_vol <- coef(visn_con_fit)[1] + coef(visn_con_fit)[2] * visn_prov_est + coef(visn_con_fit)[3] * visn_uni_est
          
#Calculate the exact time inteval value.  Even though it rounds to 1 in each case
#the slight difference due to rounding causes different values when plugged into
#the regression equation.

lv_t_interval <- lv_wl_fit1$fitted.values/(exp(lv_wl_fit1$coefficients[1] + (lv_wl_fit1$coefficients[2] * raw_data_lv$Consults[1:36])))
sd_t_interval <- sd_wl_fit1$fitted.values/(exp(sd_wl_fit1$coefficients[1] + (sd_wl_fit1$coefficients[2] * raw_data_sd$Consults[1:36])))
lb_t_interval <- lb_wl_fit1$fitted.values/(exp(lb_wl_fit1$coefficients[1] + (lb_wl_fit1$coefficients[2] * raw_data_lb$Consults[1:36])))
ll_t_interval <- ll_wl_fit1$fitted.values/(exp(ll_wl_fit1$coefficients[1] + (ll_wl_fit1$coefficients[2] * raw_data_ll$Consults[1:36])))
gla_t_interval <- gla_wl_fit1$fitted.values/(exp(gla_wl_fit1$coefficients[1] + (gla_wl_fit1$coefficients[2] * raw_data_gla$Consults[1:36])))
visn_t_interval <- visn_wl_fit1$fitted.values/(exp(visn_wl_fit1$coefficients[1] + (visn_wl_fit1$coefficients[2] * raw_data_visn$Consults[1:36])))

#Calculate the projected total unique SI and PO volume based on the projected consult
#volume.

lv_est_SIPO_vol <- mean(lv_t_interval) * exp(lv_wl_fit1$coefficients[1] + (lv_wl_fit1$coefficients[2] * lv_est_con_vol))          
sd_est_SIPO_vol <- mean(sd_t_interval) * exp(sd_wl_fit1$coefficients[1] + (sd_wl_fit1$coefficients[2] * sd_est_con_vol))
lb_est_SIPO_vol <- mean(lb_t_interval) * exp(lb_wl_fit1$coefficients[1] + (lb_wl_fit1$coefficients[2] * lb_est_con_vol))
ll_est_SIPO_vol <- mean(ll_t_interval) * exp(ll_wl_fit1$coefficients[1] + (ll_wl_fit1$coefficients[2] * ll_est_con_vol))
gla_est_SIPO_vol <- mean(gla_t_interval) * exp(gla_wl_fit1$coefficients[1] + (gla_wl_fit1$coefficients[2] * gla_est_con_vol))
visn_est_SIPO_vol <- mean(visn_t_interval) * exp(visn_wl_fit1$coefficients[1] + (visn_wl_fit1$coefficients[2] * visn_est_con_vol))
          
#Calculate the SE for the models.

lv_con_se <- sqrt(sum(resid(lv_con_fit)^2)/lv_con_fit$df.residual)
lv_wl1_se <- sqrt(sum(resid(lv_wl_fit1)^2)/lv_wl_fit1$df.residual)

sd_con_se <- sqrt(sum(resid(sd_con_fit)^2)/sd_con_fit$df.residual)
sd_wl1_se <- sqrt(sum(resid(sd_wl_fit1)^2)/sd_wl_fit1$df.residual)

lb_con_se <- sqrt(sum(resid(lb_con_fit)^2)/lb_con_fit$df.residual)
lb_wl1_se <- sqrt(sum(resid(lb_wl_fit1)^2)/lb_wl_fit1$df.residual)
          
ll_con_se <- sqrt(sum(resid(ll_con_fit)^2)/ll_con_fit$df.residual)
ll_wl1_se <- sqrt(sum(resid(ll_wl_fit1)^2)/ll_wl_fit1$df.residual)
          
gla_con_se <- sqrt(sum(resid(gla_con_fit)^2)/gla_con_fit$df.residual)
gla_wl1_se <- sqrt(sum(resid(gla_wl_fit1)^2)/gla_wl_fit1$df.residual)
          
visn_con_se <- sqrt(sum(resid(visn_con_fit)^2)/visn_con_fit$df.residual)
visn_wl1_se <- sqrt(sum(resid(visn_wl_fit1)^2)/visn_wl_fit1$df.residual)

#Calculate the upper and lower CI for each model at 95%.
#Note: The Z value is only used for the workload model as it is a
#slightly different type of regression model.
          
t = 2.179

z = 1.96

lv_con_ci_upper <- lv_est_con_vol + t * lv_con_se
lv_con_ci_lower <- lv_est_con_vol - t * lv_con_se
lv_wl1_ci_upper <- lv_est_SIPO_vol + z * lv_wl1_se
lv_wl1_ci_lower <- lv_est_SIPO_vol - z * lv_wl1_se

sd_con_ci_upper <- sd_est_con_vol + t * sd_con_se
sd_con_ci_lower <- sd_est_con_vol - t * sd_con_se
sd_wl1_ci_upper <- sd_est_SIPO_vol + z * sd_wl1_se
sd_wl1_ci_lower <- sd_est_SIPO_vol - z * sd_wl1_se

lb_con_ci_upper <- lb_est_con_vol + t * lb_con_se
lb_con_ci_lower <- lb_est_con_vol - t * lb_con_se
lb_wl1_ci_upper <- lb_est_SIPO_vol + z * lb_wl1_se
lb_wl1_ci_lower <- lb_est_SIPO_vol - z * lb_wl1_se
          
ll_con_ci_upper <- ll_est_con_vol + t * ll_con_se
ll_con_ci_lower <- ll_est_con_vol - t * ll_con_se
ll_wl1_ci_upper <- ll_est_SIPO_vol + z * ll_wl1_se
ll_wl1_ci_lower <- ll_est_SIPO_vol - z * ll_wl1_se
          
gla_con_ci_upper <- gla_est_con_vol + t * gla_con_se
gla_con_ci_lower <- gla_est_con_vol - t * gla_con_se
gla_wl1_ci_upper <- gla_est_SIPO_vol + z * gla_wl1_se
gla_wl1_ci_lower <- gla_est_SIPO_vol - z * gla_wl1_se
          
visn_con_ci_upper <- visn_est_con_vol + t * visn_con_se
visn_con_ci_lower <- visn_est_con_vol - t * visn_con_se
visn_wl1_ci_upper <- visn_est_SIPO_vol + z * visn_wl1_se
visn_wl1_ci_lower <- visn_est_SIPO_vol - z * visn_wl1_se

#Calculate the over/under staffing for the projected data
#Staffing levels are based on the 9/25/15 org chart, except for LV
#which is based on the 3/17/15 org chart.

lvstaff <<- 6

lv_final <<- data.frame(Period = 1:60,
                        Uniques = lv_uni_est,
                        Consults = lv_est_con_vol,
                        Consults_Up = lv_con_ci_upper,
                        Consults_Low = lv_con_ci_lower,
                        POandSI = lv_est_SIPO_vol,
                        POandSI_Up = lv_wl1_ci_upper,
                        POandSI_Low = lv_wl1_ci_lower)

lv_final$SG1 <<- lvstaff - (lv_final$POandSI/as.numeric(prod_thresh[1]))

sdstaff <<- 10

sd_final <<- data.frame(Period = 1:60,
                       Uniques = sd_uni_est,
                       Consults = sd_est_con_vol,
                       Consults_Up = sd_con_ci_upper,
                       Consults_Low = sd_con_ci_lower,
                       POandSI = sd_est_SIPO_vol,
                       POandSI_Up = sd_wl1_ci_upper,
                       POandSI_Low = sd_wl1_ci_lower)
          
sd_final$SG1 <<- sdstaff - (sd_final$POandSI/as.numeric(prod_thresh[1]))

lbstaff <<- 7

lb_final <<- data.frame(Period = 1:60,
                       Uniques = lb_uni_est,
                       Consults = lb_est_con_vol,
                       Consults_Up = lb_con_ci_upper,
                       Consults_Low = lb_con_ci_lower,
                       POandSI = lb_est_SIPO_vol,
                       POandSI_Up = lb_wl1_ci_upper,
                       POandSI_Low = lb_wl1_ci_lower)
     
lb_final$SG1 <<- lbstaff - (lb_final$POandSI/as.numeric(prod_thresh[1]))
          
llstaff <<- 6

ll_final <<- data.frame(Period = 1:60, 
                       Uniques = ll_uni_est,
                       Consults = ll_est_con_vol,
                       Consults_Up = ll_con_ci_upper,
                       Consults_Low = ll_con_ci_lower,
                       POandSI = ll_est_SIPO_vol,
                       POandSI_Up = ll_wl1_ci_upper,
                       POandSI_Low = ll_wl1_ci_lower)
          
ll_final$SG1 <<- llstaff - (ll_final$POandSI/as.numeric(prod_thresh[1]))

glastaff <<- 6 + 6

gla_final <<- data.frame(Period = 1:60,
                        Uniques = gla_uni_est,
                        Consults = gla_est_con_vol, 
                        Consults_Up = gla_con_ci_upper,
                        Consults_Low = gla_con_ci_lower,
                        POandSI = gla_est_SIPO_vol,
                        POandSI_Up = gla_wl1_ci_upper,
                        POandSI_Low = gla_wl1_ci_lower)
          
gla_final$SG1 <<- glastaff - (gla_final$POandSI/as.numeric(prod_thresh[1]))

visnstaff <<- 10 + 7 + 6 + 6 + 6 + 6

visn_final <<- data.frame(Period = 1:60,
                         Uniques = visn_uni_est,
                         Consults = visn_est_con_vol, 
                         Consults_Up = visn_con_ci_upper,
                         Consults_Low = visn_con_ci_lower,
                         POandSI = visn_est_SIPO_vol,
                         POandSI_Up = visn_wl1_ci_upper,
                         POandSI_Low = visn_wl1_ci_lower)
          
visn_final$SG1 <<- visnstaff - (visn_final$POandSI/as.numeric(prod_thresh[1]))
          

shinyServer(function(input, output) { 

output$textDisplay <- renderTable({ 
     
     if(input$comboBox == "Las Vegas") {
          getMat = matrix(c("Starting Monthly Consult Volume", lv_final$Consults[1],
                            "Ending Monthly Consult Volume", lv_final$Consults[input$periods],
                            "Starting Monthly Workload", lv_final$POandSI[1],
                            "Ending Monthly Workload", lv_final$POandSI[input$periods],
                            "Starting Number of Staff", lvstaff,
                            "Ending Staff Need/Excess", lv_final$SG1[input$periods],
                            "Production Standard (Unique PO's and Stock Issues per Staff Member per Month)", prod_thresh[1]),
                             ncol=2, byrow = TRUE)
                     
          colnames(getMat) = c("Variables", "Values")
                     
          getMat
     }
     
     else if(input$comboBox == "Long Beach") {
          getMat1 = matrix(c("Starting Monthly Consult Volume", lb_final$Consults[1],
                            "Ending Monthly Consult Volume", lb_final$Consults[input$periods],
                            "Starting Monthly Workload", lb_final$POandSI[1],
                            "Ending Monthly Workload", lb_final$POandSI[input$periods],
                            "Starting Number of Staff", lbstaff,
                            "Ending Staff Need/Excess", lb_final$SG1[input$periods],
                            "Production Standard (Unique PO's and Stock Issues per Staff Member per Month)", prod_thresh[1]),
                          ncol=2, byrow = TRUE)
          
          colnames(getMat1) = c("Variables", "Values")
          
          getMat1
     }
     
     else if(input$comboBox == "Loma Linda") {
          getMat = matrix(c("Starting Monthly Consult Volume", ll_final$Consults[1],
                            "Ending Monthly Consult Volume", ll_final$Consults[input$periods],
                            "Starting Monthly Workload", ll_final$POandSI[1],
                            "Ending Monthly Workload", ll_final$POandSI[input$periods],
                            "Starting Number of Staff", llstaff,
                            "Ending Staff Need/Excess", ll_final$SG1[input$periods],
                            "Production Standard (Unique PO's and Stock Issues per Staff Member per Month)", prod_thresh[1]),
                          ncol=2, byrow = TRUE)
          
          colnames(getMat) = c("Variables", "Values")
          
          getMat
     }
     
     else if(input$comboBox == "San Diego") {
          getMat = matrix(c("Starting Monthly Consult Volume", sd_final$Consults[1],
                            "Ending Monthly Consult Volume", sd_final$Consults[input$periods],
                            "Starting Monthly Workload", sd_final$POandSI[1],
                            "Ending Monthly Workload", sd_final$POandSI[input$periods],
                            "Starting Number of Staff", sdstaff,
                            "Ending Staff Need/Excess", sd_final$SG1[input$periods],
                            "Production Standard (Unique PO's and Stock Issues per Staff Member per Month)", prod_thresh[1]),
                          ncol=2, byrow = TRUE)
          
          colnames(getMat) = c("Variables", "Values")
          
          getMat
     }
     
     else if(input$comboBox == "Greater LA") {
          getMat = matrix(c("Starting Monthly Consult Volume", gla_final$Consults[1],
                            "Ending Monthly Consult Volume", gla_final$Consults[input$periods],
                            "Starting Monthly Workload", gla_final$POandSI[1],
                            "Ending Monthly Workload", gla_final$POandSI[input$periods],
                            "Starting Number of Staff", glastaff,
                            "Ending Staff Need/Excess", gla_final$SG1[input$periods],
                            "Production Standard (Unique PO's and Stock Issues per Staff Member per Month)", prod_thresh[1]),
                          ncol=2, byrow = TRUE)
          
          colnames(getMat) = c("Variables", "Values")
          
          getMat
     }
     
     else if(input$comboBox == "VISN") {
          getMat = matrix(c("Starting Monthly Consult Volume", visn_final$Consults[1],
                            "Ending Monthly Consult Volume", visn_final$Consults[input$periods],
                            "Starting Monthly Workload", visn_final$POandSI[1],
                            "Ending Monthly Workload", visn_final$POandSI[input$periods],
                            "Starting Number of Staff", visnstaff,
                            "Ending Staff Need/Excess", visn_final$SG1[input$periods],
                            "Production Standard (Unique PO's and Stock Issues per Staff Member per Month)", prod_thresh[1]),
                          ncol=2, byrow = TRUE)
          
          colnames(getMat) = c("Variables", "Values")
          
          getMat
     }
})
     
output$uniGraph <- renderPlot({
 
     if(input$comboBox == "Las Vegas") {
          
          tmplv_final <- subset(lv_final, Period <= input$periods, 
                                select = c("Period", "Uniques"))
          
          uni_plot <- ggplot(tmplv_final, aes(x = Period, y = Uniques)) +
                             geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
                             labs(x = "Forecast Period", y = "Unique Outpatient Volume") +
                             ggtitle("Las Vegas Projected Facility Unique Outpatient Volume") +
                             background_grid(major = "xy", minor = "none")
               
          print(uni_plot)
     }
     
     if(input$comboBox == "Long Beach") {
          
          tmplb_final <- subset(lb_final, Period <= input$periods, 
                                select = c("Period", "Uniques"))
          
          uni_plot <- ggplot(tmplb_final, aes(x = Period, y = Uniques)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique Outpatient Volume") +
               ggtitle("Long Beach Projected Facility Unique Outpatient Volume") +
               background_grid(major = "xy", minor = "none")
          
          print(uni_plot)
     }
     
     if(input$comboBox == "Loma Linda") {
          
          tmpll_final <- subset(ll_final, Period <= input$periods, 
                                select = c("Period", "Uniques"))
          
          uni_plot <- ggplot(tmpll_final, aes(x = Period, y = Uniques)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique Outpatient Volume") +
               ggtitle("Loma Linda Projected Facility Unique Outpatient Volume") +
               background_grid(major = "xy", minor = "none")
          
          print(uni_plot)
     }
     
     if(input$comboBox == "San Diego") {
          
          tmpsd_final <- subset(sd_final, Period <= input$periods, 
                                select = c("Period", "Uniques"))
          
          uni_plot <- ggplot(tmpsd_final, aes(x = Period, y = Uniques)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique Outpatient Volume") +
               ggtitle("San Diego Projected Facility Unique Outpatient Volume") +
               background_grid(major = "xy", minor = "none")
          
          print(uni_plot)
     }
     
     if(input$comboBox == "Greater LA") {
          
          tmpgla_final <- subset(gla_final, Period <= input$periods, 
                                select = c("Period", "Uniques"))
          
          uni_plot <- ggplot(tmpgla_final, aes(x = Period, y = Uniques)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique Outpatient Volume") +
               ggtitle("Greater LA Projected Facility Unique Outpatient Volume") +
               background_grid(major = "xy", minor = "none")
          
          print(uni_plot)
     }
     
     if(input$comboBox == "VISN") {
          
          tmpvisn_final <- subset(visn_final, Period <= input$periods, 
                                select = c("Period", "Uniques"))
          
          uni_plot <- ggplot(tmpvisn_final, aes(x = Period, y = Uniques)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique Outpatient Volume") +
               ggtitle("VISN Projected Facility Unique Outpatient Volume") +
               background_grid(major = "xy", minor = "none")
          
          print(uni_plot)
     }
})

output$conGraph <- renderPlot({

     if(input$comboBox == "Las Vegas") {
     
          tmp1lv_final <- subset(lv_final, Period <= input$periods,
                                select = c("Period", "Consults", "Consults_Up", "Consults_Low"))
          
          con_plot <- ggplot(tmp1lv_final, aes(x = Period, y = Consults)) +
                             geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
                             labs(x = "Forecast Period", y = "Consult Volume") +
                             ggtitle("Las Vegas Projected Consult Volume") +  
#                              geom_line(aes(y = tmplv_final$Consults_Up), colour = "red") +
#                              geom_line(aes(y = tmplv_final$Consults_Low), colour = "red") +
                             background_grid(major = "xy", minor = "none")
                                    
          print(con_plot)
     }
     
     if(input$comboBox == "Long Beach") {
          
          tmp1lb_final <- subset(lb_final, Period <= input$periods, 
                                 select = c("Period", "Consults", "Consults_Up", "Consults_Low"))
          
          con_plot <- ggplot(tmp1lb_final, aes(x = Period, y = Consults)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Consult Volume") +
               ggtitle("Long Beach Projected Consult Volume") +
#                geom_line(aes(y = tmp1lb_final$Consults_Up), colour = "red") +
#                geom_line(aes(y = tmp1lb_final$COnsults_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(con_plot)
     }
     
     if(input$comboBox == "Loma Linda") {
          
          tmp1ll_final <- subset(ll_final, Period <= input$periods, 
                                 select = c("Period", "Consults", "Consults_Up", "Consults_Low"))
          
          con_plot <- ggplot(tmp1ll_final, aes(x = Period, y = Consults)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Consult Volume") +
               ggtitle("Loma Linda Projected Consult Volume") +
#                geom_line(aes(y = tmp1ll_final$Consults_Up), colour = "red") +
#                geom_line(aes(y = tmp1ll_final$COnsults_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(con_plot)
     }
     
     if(input$comboBox == "San Diego") {
          
          tmp1sd_final <- subset(sd_final, Period <= input$periods, 
                                 select = c("Period", "Consults", "Consults_Up", "Consults_Low"))
          
          con_plot <- ggplot(tmp1sd_final, aes(x = Period, y = Consults)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Consult Volume") +
               ggtitle("San Diego Projected Consult Volume") +
#                geom_line(aes(y = tmp1sd_final$Consults_Up), colour = "red") +
#                geom_line(aes(y = tmp1sd_final$COnsults_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(con_plot)
     }
     
     if(input$comboBox == "Greater LA") {
          
          tmp1gla_final <- subset(gla_final, Period <= input$periods, 
                                 select = c("Period", "Consults", "Consults_Up", "Consults_Low"))
          
          con_plot <- ggplot(tmp1gla_final, aes(x = Period, y = Consults)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Consult Volume") +
               ggtitle("Greater LA Projected Consult Volume") +
#                geom_line(aes(y = tmp1gla_final$Consults_Up), colour = "red") +
#                geom_line(aes(y = tmp1gla_final$COnsults_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(con_plot)
     }
     
     if(input$comboBox == "VISN") {
          
          tmp1visn_final <- subset(visn_final, Period <= input$periods, 
                                 select = c("Period", "Consults", "Consults_Up", "Consults_Low"))
          
          con_plot <- ggplot(tmp1visn_final, aes(x = Period, y = Consults)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Consult Volume") +
               ggtitle("VISN Projected Consult Volume") +
#                geom_line(aes(y = tmp1visn_final$Consults_Up), colour = "red") +
#                geom_line(aes(y = tmp1visn_final$COnsults_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(con_plot)
     }
})

output$wlGraph <- renderPlot({
     
     if(input$comboBox == "Las Vegas") {
         
         tmp2lv_final <- subset(lv_final, Period <= input$periods, 
                                 select = c("Period", "POandSI", "POandSI_Up", "POandSI_Low"))
          
         wl_plot <- ggplot(tmp2lv_final, aes(x = Period, y = POandSI)) +
                            geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
                            labs(x = "Forecast Period", y = "Unique PO and SI Volume") +
                            ggtitle("Las Vegas Projected Unique PO and SI Volume") +
#                             geom_line(aes(y = tmp2lv_final$POandSI_Up), colour = "red") +
#                             geom_line(aes(y = tmp2lv_final$POandSI_Low), colour = "red") +
                            background_grid(major = "xy", minor = "none")
                                    
          print(wl_plot)
     }
     
     if(input$comboBox == "Long Beach") {
          
          tmp2lb_final <- subset(lb_final, Period <= input$periods, 
                                 select = c("Period", "POandSI", "POandSI_Up", "POandSI_Low"))
          
          wl_plot <- ggplot(tmp2lb_final, aes(x = Period, y = POandSI)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique PO and SI Volume") +
               ggtitle("Long Beach Projected Unique PO and SI Volume") +
#                geom_line(aes(y = tmp2lb_final$POandSI_Up), colour = "red") +
#                geom_line(aes(y = tmp2lb_final$POandSI_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(wl_plot)
     }
     
     if(input$comboBox == "Loma Linda") {
          
          tmp2ll_final <- subset(ll_final, Period <= input$periods, 
                                 select = c("Period", "POandSI", "POandSI_Up", "POandSI_Low"))
          
          wl_plot <- ggplot(tmp2ll_final, aes(x = Period, y = POandSI)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique PO and SI Volume") +
               ggtitle("Loma Linda Projected Unique PO and SI Volume") +
#                geom_line(aes(y = tmp2ll_final$POandSI_Up), colour = "red") +
#                geom_line(aes(y = tmp2ll_final$POandSI_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(wl_plot)
     }
     
     if(input$comboBox == "San Diego") {
          
          tmp2sd_final <- subset(sd_final, Period <= input$periods, 
                                 select = c("Period", "POandSI", "POandSI_Up", "POandSI_Low"))
          
          wl_plot <- ggplot(tmp2sd_final, aes(x = Period, y = POandSI)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique PO and SI Volume") +
               ggtitle("San Diego Projected Unique PO and SI Volume") +
#                geom_line(aes(y = tmp2sd_final$POandSI_Up), colour = "red") +
#                geom_line(aes(y = tmp2sd_final$POandSI_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(wl_plot)
     }
     
     if(input$comboBox == "Greater LA") {
          
          tmp2gla_final <- subset(gla_final, Period <= input$periods, 
                                 select = c("Period", "POandSI", "POandSI_Up", "POandSI_Low"))
          
          wl_plot <- ggplot(tmp2gla_final, aes(x = Period, y = POandSI)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique PO and SI Volume") +
               ggtitle("Greater LA Projected Unique PO and SI Volume") +
#                geom_line(aes(y = tmp2gla_final$POandSI_Up), colour = "red") +
#                geom_line(aes(y = tmp2gla_final$POandSI_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(wl_plot)
     }
     
     if(input$comboBox == "VISN") {
          
          tmp2visn_final <- subset(visn_final, Period <= input$periods, 
                                 select = c("Period", "POandSI", "POandSI_Up", "POandSI_Low"))
          
          wl_plot <- ggplot(tmp2visn_final, aes(x = Period, y = POandSI)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Unique PO and SI Volume") +
               ggtitle("VISN Projected Unique PO and SI Volume") +
#                geom_line(aes(y = tmp2visn_final$POandSI_Up), colour = "red") +
#                geom_line(aes(y = tmp2visn_final$POandSI_Low), colour = "red") +
               background_grid(major = "xy", minor = "none")
          
          print(wl_plot)
     }
})

output$staffGraph <- renderPlot({
     
     if(input$comboBox == "Las Vegas") {
          
          tmp3lv_final <- subset(lv_final, Period <= input$periods, 
                                 select = c("Period", "SG1"))
          
          staff_plot <- ggplot(tmp3lv_final, aes(x = Period, y = SG1)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Staff Level") +
               ggtitle("Las Vegas Projected Staffing Needs or Excess") +
               background_grid(major = "xy", minor = "none")
          
          print(staff_plot)
     }
     
     if(input$comboBox == "Long Beach") {
          
          tmp3lb_final <- subset(lb_final, Period <= input$periods, 
                                 select = c("Period", "SG1"))
          
          staff_plot <- ggplot(tmp3lb_final, aes(x = Period, y = SG1)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Staff Level") +
               ggtitle("Long Beach Projected Staffing Needs or Excess") +
               background_grid(major = "xy", minor = "none")
          
          print(staff_plot)
     }
     
     if(input$comboBox == "Loma Linda") {
          
          tmp3ll_final <- subset(ll_final, Period <= input$periods, 
                                 select = c("Period", "SG1"))
          
          staff_plot <- ggplot(tmp3ll_final, aes(x = Period, y = SG1)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Staff Level") +
               ggtitle("Loma Linda Projected Staffing Needs or Excess") +
               background_grid(major = "xy", minor = "none")
          
          print(staff_plot)
     }
     
     if(input$comboBox == "San Diego") {
          
          tmp3sd_final <- subset(sd_final, Period <= input$periods, 
                                 select = c("Period", "SG1"))
          
          staff_plot <- ggplot(tmp3sd_final, aes(x = Period, y = SG1)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Staff Level") +
               ggtitle("San Diego Projected Staffing Needs or Excess") +
               background_grid(major = "xy", minor = "none")
          
          print(staff_plot)
     }
     
     if(input$comboBox == "Greater LA") {
          
          tmp3gla_final <- subset(gla_final, Period <= input$periods, 
                                 select = c("Period", "SG1"))
          
          staff_plot <- ggplot(tmp3gla_final, aes(x = Period, y = SG1)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Staff Level") +
               ggtitle("Greater LA Projected Staffing Needs or Excess") +
               background_grid(major = "xy", minor = "none")
          
          print(staff_plot)
     }
     
     if(input$comboBox == "VISN") {
          
          tmp3visn_final <- subset(visn_final, Period <= input$periods, 
                                 select = c("Period", "SG1"))
          
          staff_plot <- ggplot(tmp3visn_final, aes(x = Period, y = SG1)) +
               geom_bar(stat = "identity", fill = "#0072B2", colour = "black") +
               labs(x = "Forecast Period", y = "Staff Level") +
               ggtitle("VISN Projected Staffing Needs or Excess") +
               background_grid(major = "xy", minor = "none")
          
          print(staff_plot)
     }
})

})