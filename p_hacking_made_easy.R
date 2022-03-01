###########################################
### Reliability in regressions Work #######
###########################################

remove(list=ls())

########################
### baseball data ######
########################

#source: https://www4.stat.ncsu.edu/~boos/var.select/baseball.html
#data explanation: https://www4.stat.ncsu.edu/~boos/var.select/baseball.doc

baseball_data <- read.csv("https://www4.stat.ncsu.edu/~boos/var.select/baseball.txt")

colnames(baseball_data) <- c("Salary", 
                             "Bat_avg", "OBP", "Runs", "Hits", "Doubles", 
                             "triples", "home_runs", "RBI", "Walks",
                             "strike_outs", "stolen_bases", "errors",
                             "free_agency_eligibility", "free_agent_in_1991_2",
                             "arbitration_eligibility", "arbitration_in_1991_2")

###################################
## making independent variables ###
## easier to interpret  ###########
###################################

baseball_data$Bat_avg <- baseball_data$Bat_avg*1000
baseball_data$OBP <- baseball_data$OBP*100

#######################################
### Historgam of Dependent Variables  #
#######################################

hist(baseball_data$Salary,
     main="Salaries of 337 MLB Field Players in 1992",
     xlab="(in thousands)")

#########################################
## plot of univariate OLS ###############
#########################################


plot(baseball_data$Bat_avg,baseball_data$Salary,
     main="Does Batting Average Matter?",
     xlab="Batting Avg. 1991",ylab="Salary 1992 (in thousands)" )
abline(lm(baseball_data$Salary~baseball_data$Bat_avg),col="red")
legend(locator(n=1), legend=c("OLS Regression"),
       col=c("red"), lty=1,lwd=c(1),cex=0.8)


#How many possible right-hand sides 2^16,is 65,536,
#but we're always including batting average, so we're running
#2^15 = 32,768 total regressions

salary <- baseball_data[,1]
dep_var <- baseball_data[,2:17]

######################################
#small example for illustration: #####
######################################

demonstration <- tidyr::crossing(var1 = 1:1, var2 = 0:1, var3 = 0:1, var4=0:1)
#https://stackoverflow.com/questions/18705153/generate-list-of-all-possible-combinations-of-elements-of-vector

demonstration

####################################
### full sample for baseball data ##
####################################

#note: var1 is always 1, becuase we always include battting average

combo_test <- tidyr::crossing(var1 = 1:1, var2 = 0:1, var3 = 0:1, var4=0:1,
                              var5 = 0:1, var6 = 0:1, var7 = 0:1, var8=0:1,
                              var9 = 0:1, var10 = 0:1, var11 = 0:1, var12=0:1,
                              var13 = 0:1, var14 = 0:1, var15 = 0:1, var16=0:1) #https://stackoverflow.com/questions/18705153/generate-list-of-all-possible-combinations-of-elements-of-vector

combo_test <- t(combo_test)
combo_test <- as.data.frame(combo_test)
rownames(combo_test) <- colnames(dep_var)

#############################################################
#loop to run regressions over all possible combinations #####
#############################################################

t_stat_storage <- as.data.frame(matrix(0,nrow = ncol(combo_test),ncol=2)) #number of columns is the number of lags you want to try:1,2,,,,

for(i in 1:ncol(combo_test)){
  
  #select the set of dependent variables (i.e. column from combo_test)
  loop_data <- dep_var
  loop_factor <- as.list(combo_test[,i])
  
  #zero out the variables you need to exclude
  for(j in 1:ncol(loop_data)){
    loop_data[,j] <- loop_data[,j]*loop_factor[[j]][1]
  }
  
  #turn the zero into NA
  for(j in 1:ncol(loop_data)){ 
    #j  <- 1
    if(sum(loop_data[,j])==0)
    {loop_data[,j] <- NA}
  }
  
  #remove the NAs
  loop_data <- t(na.omit(t(loop_data)))
  
  #run the regression and store the coefficent and t-stat
  reg_1 <- lm(salary~loop_data)
  summary_test <- summary(reg_1)
  t_stat_storage[i,1] <- summary_test$coefficients[2,3]
  t_stat_storage[i,2] <- summary_test$coefficients[2,1]
}

#################################################
## scatter plot: coeffcient and t statistic  ####
## for all possible sets of dependent variables##
#################################################

t_stat_storage$col  <- "lightgrey"
t_stat_storage$col[1] <- "red" #battin average only
t_stat_storage$col[32768] <- "blue" #all possible controls


plot(t_stat_storage[,1], t_stat_storage[,2],col=t_stat_storage$col,
     xlab = "Batting Averge t-statistic",
     ylab = "Batting Average OLS Coefficent",
     main= "Regression of Batting Average on Salary for 32,768 Sets of Control Variables",
     sub = "t-statistics outside of (-2,2) are 'signficant'")
abline(v=c(-2,2))
abline(h=0)


legend(locator(n=1), legend=c("Batting Averge Only",
                              "All Possible Controls"),
       col=c("red", "blue"),pch=1,cex=0.8)

