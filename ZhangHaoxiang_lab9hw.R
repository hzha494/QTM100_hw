#This function performs a one-sample t-test on repeated samples from a single quantitative variable
#variable - variable of interest
#sample.size - sample size
#alpha - level of significance
#num.reps - number of samples to draw
inference.means<-function(variable,sample.size,alpha,num.reps){
  
  samp.est<-rep(NA,num.reps)
  stdev<-rep(NA,num.reps)
  se.xbar<-rep(NA,num.reps)
  test.stat<-rep(NA,num.reps)
  p.val<-rep(NA,num.reps)
  decision<-rep(NA,num.reps)
  lcl<-rep(NA,num.reps)
  ucl<-rep(NA,num.reps)
  capture<-rep(NA,num.reps)
  true.mean<-mean(variable)
  
  for(i in 1:num.reps){
    samp<-sample(variable,sample.size)
    samp.est[i]<-mean(samp)
    stdev[i]<-sd(samp)
    se.xbar[i]<-stdev[i]/sqrt(sample.size)
    test.stat[i]<-(samp.est[i]-true.mean)/se.xbar[i]
    df<-sample.size-1
    p.val[i]<-2*pt(abs(test.stat[i]),df,lower.tail=FALSE)
    t.score<-qt(1-alpha/2,df)
    lcl[i]<-samp.est[i]-t.score*se.xbar[i]
    ucl[i]<-samp.est[i]+t.score*se.xbar[i]
    
    decision[i]<-ifelse(p.val[i]<=alpha,"reject Ho","fail to reject Ho")
    capture[i]<-ifelse(lcl[i]<=true.mean & ucl[i]>=true.mean,"yes","no")
  }
  
  results<-data.frame(samp.est=round(samp.est,4),
                      test.stat=round(test.stat,4),
                      p.val=round(p.val,4),
                      decision=decision,
                      lcl=round(lcl,4),
                      ucl=round(ucl,4),
                      capture=capture)
  return(results)
}






#This function plots confidence interval results
#results - an object created by either inference.means or inference.proportions
#true.val - the true population parameter value
plot.ci<-function(results,true.val){
  par(mar=c(4, 1, 2, 1), mgp=c(2.7, 0.7, 0),xpd=T)
  k <- length(results$lcl)
  xR <- c(min(results$lcl),max(results$ucl))
  yR <- c(0, 41*k/40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  cols<-ifelse(results$capture=="yes","white","firebrick2")
  segments(results$lcl,1:k,results$ucl,1:k,col=cols,lwd=4)
  points(results$samp.est,1:k,pch=20,col="black")
  segments(results$lcl,1:k,results$ucl,1:k,col="black")
  segments(true.val,0-42/40,true.val,42*k/40, lty=2, col="royalblue3")
  axis(1)
  text(true.val,yR[2],paste("true =",round(true.val,4)),col="royalblue3",pos=3)
}

#The assignment starts here, by setting the working directory
setwd("C:/Users/Tom Zhang/Documents/R QTM100")

#Firstly, load the data set into r
yrbss <- read.csv("yrbss2013.csv")

#Then, we would want to examine the distribution of height_m
plot(density(yrbss$height_m))
#We can observe that the distribution is approximately a belt 
#shape curve with some variation around the mean. The mean is
#approximately 1.70.

#Then, we would also want to know the population mean and
#population standard deviation of height_m
yrbss_mean <- mean(yrbss$height_m)
yrbss_sd <- sd(yrbss$height_m)
yrbss_mean
yrbss_sd
#The population mean is 1.69 and the population standard
#deviation is 0.10

#There is more than 30 samples if we take 300 sampels, which
#according to the CLT the sample mean distribution is going
#to be approximately normal. In this case the assumption is
#met to take 300 samples. Moreover, all samples are independent
#to each other, so it is fine to just randomly sample the
#population.

#We are testing H0 :$\bar{x}$ = 1.69 versus Ha :$\bar{x}$ != 1.69. 
#In the hypothesis test, we run the risk of committing a Type I 
#error because in reality the null hypothesis is actually true. 
#The targeted Type I error rate is 0.05 and the targeted 
#confidence interval coverage is 0.95. Because sampling 
#distribution assumptions are satisfied, we expect the observed 
#Type I error rate and confidence interval coverage from simulation 
#results to equal the targeted levels.

#Next, we would want to utilize the provided functions in order
#to investigate what would happen with 100 sample means, each
#generated with 20 samples.
results <- inference.means(yrbss$height_m, 20, 0.05, 100)

#Then, we would want to explore different distributions of
#sample statistics

#Firstly we would start with the sample mean
plot(density(results$samp.est))
#The distribution is an approximately bell shaped curve that has
#a mean at approximately 1.70

#With similar code, we would also want to explore the
#distributions of t statistic and p-values
plot(density(results$test.stat))
#The distribution of the t statistic is slightly skewed to the
#left with a peak at around 0.
plot(density(results$p.val))
#This distribution is bimodal with two peaks around 0.25 and
#0.75 with the median that is slightly more than 0.5

#We would want to see s the percent of samples that commit an 
#error in hypothesis test results
mean(results$decision == "reject Ho" & results$capture == "no") * 100
#There is 4%.

#Then we would want to plot all of the confidence intervals
#and investigate their association with the true value
plot.ci(results, yrbss_mean)
#Only 4 of the results does not contain the population mean, so
#96% of the confidence intervals contain the true value. The
#confidence intervals does come in different lengths, but they
#are all around 0.10, which is similar to the population
#standard deviation. Moreover, the samples does show to be
#randomly scattered around the population mean, and they show
#fine demonstration for reasonable bounds of the mean.