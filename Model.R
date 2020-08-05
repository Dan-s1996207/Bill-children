##Calling in and installing packages

install.packages("foreign", dependencies=TRUE)
install.packages("lavaan", dependencies=TRUE)
library(foreign) 
library(lavaan)
library(readr)

#loading the dataset from python 
Bill <- read_csv("Project 2/Bill.csv")

#scalling this dataset to be used for CFA
scaled.Bill <- scale(Bill)


#DEfining the model to be run in CFA
model <- '
# latent variable definitions
SC=~et_falsebelief_testtrial_preference_score + 
  figuretask_per_looking_interacting + 
  tomi_compmean +
  tom_tb_totalscore
EF=~brief_raw_self_monitor + 
  brief_raw_initiate + 
  brief_raw_working_memory +
  brief_raw_plan_organise + 
  brief_raw_task_monitor + 
  brief_raw_organisation_of_materials + 
  pvt_mean_rt + 
  pvt_count_falsestarts + 
  flanker_mean_rt_congruent 
LAN=~bpvs_raw +
  vocabprocess_processing_speed_target
bpvs_raw ~~ 0*bpvs_raw
'

#Run the CFA
fit <- cfa(model, data=scaled.Bill, missing = "fiml")

#Obtain a summary of the results
summary(fit, fit.measures = TRUE)




#Get the models predictions of the latent variables and attache them to the original dataset.
factors = lavPredict(fit, type = "lv")

bill_an = cbind(Bill, factors)

#Fit a regression 
model_SC <- lm(SC ~ gender + age_m + diagnosis*bilec_total_input + wasi_sum_rawscores, data = bill_an)
model_EF <- lm(EF ~ gender + age_m + diagnosis*bilec_total_input + wasi_sum_rawscores, data = bill_an)
model_LAN <- lm(LAN ~ gender + age_m + diagnosis*bilec_total_input + wasi_sum_rawscores, data = bill_an)



#Summarry of the regressions
summary(model_SC)
summary(model_EF)
summary(model_LAN)


#CReate path analysis diagram of the CFA
install.packages("semPlot", dependencies=TRUE)
library(semPlot)

lab = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15","SC", "EF", 'LAN')

semPaths(fit, layout = "tree", sizeMan = 5, intercepts = FALSE, nodeLabels = lab)








