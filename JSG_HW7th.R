
#Lab 7th

#Team: Jennifer Lavayen, Lauren Done, Hugo Pinto  & Joaquin Sanchez Gomez




Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)
summary(Household_Pulse_data$vaxx)
vaxx_factor <- as.factor(Household_Pulse_data$vaxx)
levels(vaxx_factor) <- c("no","yes")

d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
summary(d_educ)
levels(dat_use1$EEDUC)


d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))
d_Anxious <- data.frame(model.matrix(~dat_use1$ANXIOUS))
d_sexual_orientation <- data.frame(model.matrix(~dat_use1$SEXUAL_ORIENTATION))
d_public_health <- data.frame(model.matrix(~dat_use1$PUBHLTH))
d_vaxx <- data.frame(model.matrix(~ dat_use1$vaxx)) # check number of obs to see that this snips off NA values

# note that, depending on your subgroup, this might snip off some columns so make sure to check summary() of each -- don't want Min = Max = 0!

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],
  dat_use1$TBIRTH_YEAR[!is.na(dat_use1$vaxx)],
  d_educ[!is.na(dat_use1$vaxx),2:7],
  d_marstat[!is.na(dat_use1$vaxx),2:6],
  d_race[!is.na(dat_use1$vaxx),2:4],
  d_hispanic[!is.na(dat_use1$vaxx),2],
  d_gender[!is.na(dat_use1$vaxx),2:5],
  d_region[!is.na(dat_use1$vaxx),2:4], # need [] since model.matrix includes intercept term
#added variable 
  d_Anxious[!is.na(dat_use1$vaxx),2:5],
  d_sexual_orientation[!is.na(dat_use1$vaxx),2:6],
  d_public_health[!is.na(dat_use1$vaxx),2:3])
  
# this is just about me being anal-retentive, see difference in names(dat_for_analysis_sub) before and after running this bit
names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[2] <- "TBIRTH_YEAR"
names(dat_for_analysis_sub)[17] <- "Hispanic" #17 in brackets because its the 17th variable on the list 
names(dat_for_analysis_sub)[22] <- "South"
names(dat_for_analysis_sub)[23] <- "Midwest"
names(dat_for_analysis_sub)[24] <- "West"
names(dat_for_analysis_sub)[25] <- "no.anxiety.over.past.2.weeks"
names(dat_for_analysis_sub)[26] <- "anxiety.over.several.days.in.past.2.weeks"
names(dat_for_analysis_sub)[27] <- "anxiety.more.than.half.the.days.in.past.2.weeks"
names(dat_for_analysis_sub)[28] <- "anxiety.nearly.every.day"
names(dat_for_analysis_sub)[29] <- "gay.or.lesbian"
names(dat_for_analysis_sub)[30] <- "straight"
names(dat_for_analysis_sub)[31] <- "bisexual"
names(dat_for_analysis_sub)[32] <- "something.else"
names(dat_for_analysis_sub)[33] <- "dont.know"
names(dat_for_analysis_sub)[34] <- "public.health.insurance"
names(dat_for_analysis_sub)[35] <- "no.public.health.insurance"


require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.1) # use 10% as training data
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
library(standardize)
sobj <- standardize(vaxx ~ TBIRTH_YEAR + EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      Hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      no.anxiety.over.past.2.weeks + anxiety.over.several.days.in.past.2.weeks + anxiety.more.than.half.the.days.in.past.2.weeks +
                      anxiety.nearly.every.day + gay.or.lesbian + straight + bisexual + something.else + dont.know + public.health.insurance + no.public.health.insurance
                    
                    , dat_train, family = binomial)
summary(sobj)
s_dat_test <- predict(sobj, dat_test)
#Then start with some models. I'll give code for the Linear Probability Model 
#(ie good ol' OLS)#and logit, to show how to call those with the 
#standardized object.
summary(sobj$data)

#have to remove the Regions because are NA's and Nan, though we standarized them regions inirtally it still came up as nan and NAs

model_1pm1 <- lm(sobj$formula, sobj$data)
summary(model_1pm1)

pred_vals_lpm <- predict(model_1pm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$vaxx)
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)

