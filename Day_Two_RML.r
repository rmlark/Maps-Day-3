###########################################################################
###########################################################################
#
#  Introduction to R
#
#  Day 3.  Some basic statistics in R
#
#
###########################################################################
###########################################################################
#
  source("custom_functions/SBStat.R")
#
###########################################################################
###########################################################################
#
# 1.   R warm-up, sampling from a remote sensor image and the normal 
#	distribution
#
# First, read the image into a data frame.
#
#     
    dataKenya.df<-read.table("MSS.dat",header=T)

# The variable is called "dn", the values correspond to reflection
# of visible red light from pixels in a Landsat MSS image of north-west
# Kenya

# Compute a histogram, boxplot and other summary statistics of the data

names(dataKenya.df)    

hist(dataKenya.df$dn)
summa(dataKenya.df$dn)
summaplot(dataKenya.df$dn)
#
# Now convert the data to a matrix (128 x 128 pixels)

    kenya<-matrix(data=dataKenya.df$dn,nrow=128,ncol=128)
    
#  The following command allows you to plot the image
    
    image(kenya)
# 
#  Can you interpret the image and relate it to the histogram of the data?    
#
# 
#  We now treat the image as a field to be sampled. Select a sample size n  
#
    
    n<-100
#
#  The command SRSimage(n,mat), from SBStat, will draw a simple random sample
#  of size n from a matrix mat, and draw a map showing the sample locations

	sample<-SRSimage(n,kenya)

	hist(sample)
	summa(sample)
#
#  Compute the mean of the data in your sample, compare it with the mean of
#  the whole image.
#
#  the next command (from SBStat) will draw 1000 samples of size
#  n.  It shows the histogram and QQ plot for the 1000 sample means.
#

   multisample(n,kenya)

# Experiment using different sample sizes.  What links the variance
# of the data, the variance of the sample mean and the sample size?
#
# How does the histogram of the sample means differ from the histogram 
# of the data?    
#
# Some bits of R code to help with understanding of the normal
# distribution
#
#   pnorm(x,mean,sd)
    
    pnorm(0.0,0.0,1)
    
    pnorm(2,0.0,1)
    
    
    1-pnorm(1.96,0.0,1)

# A cider-maker uses a griddle to sort apples, which will remove any 
# with a diameter of less than 4 cm.  If apple diameter is normally
# distributed in a batch, the mean diameter is 7 cm and the standard
# deviation is 2 cm, what proportion of the batch do you expect to 
# be removed? 
#
# Can you find out (perhaps by trial and error) what griddle size would
# remove 15% of the apples?
#
#
    mu<-7
    sd<-2
    gr<-4
    
#    prob that an apple has a diameter <= gr is
    
    pnorm(gr,mu,sd)
    
###########################################################################
###########################################################################
#
# 2.  Testing hypotheses: tasting tea and testing t  
#
#
#   Bong beans come in pods of five.  Bong wasps lay their single egg in
#   a bong bean.  If the probability of a single bean's being parasitized is 
#   0.1, what is the probability that a randomly selected bong pod will
#   contain two parasitised beans if parasitisation of beans occurs 
#   independently and at random?
    
    #  The long-hand way
    
    p_2_3<-0.1^2*0.9^3 #Probability for some combination of 
    #2 out of 5 beans parasitized
    
    C_2_5<-ncol(combn(5,2))  #number of combinations of 2 beans drawn from 5
    
    p_lh<-p_2_3*C_2_5
    
    print(p_lh)
    
    #  The quick and easy way:
    
    dbinom(2,5,0.1)
    
#
#
#  What is the probability that 2 or more beans are parasitised?
    
    dbinom(2,5,0.1)+dbinom(3,5,0.1)+dbinom(4,5,0.1)+dbinom(5,5,0.1)
    
#  The quick and easy way
    
    1-pbinom(1,5,0.1) # i.e. pbinom(1,5,0.1) is the probability that 1 or fewer
                      # beans is parasitised
    
    
#                           0	  1	  2	  3	  4	  5
    
#       pbinom(2,5,0.1)		  *	  *	  *
#       1-pbinom(2,5,0.1)					      *	  *	  *
      


# Tasting tea
#

  N<-20 # number of trials
  s<-15 # number of successes
  p0<-0.5 # probability of a success under the null hypothesis

  Tea_taste(s,N,p0)

# Exercise 2.1  If you ran an experiment with 40 trials, and had
# 30 successes, do you think that the evidence against the null
# hypothesis will be the same (given that the proportion of 
# successes is the same?)  Use Tea_taste to find out whether you
# are right
  

  
  1-pbinom(14,20,0.5)
  
#
# Exercise 2.2  A device has been invented to detect oestrous in 
# dairy cattle.  In a trial with 50 cows, all of which were known 
# to be in oestrus, 35 were identified as such by the device.  
# State a null hypothesis, and test it using the Tea_taste function.
# Does this show that the device might be useful for dairy farmers?
  
#  H0 OVUTECH is equivalent to random guesswork.
#  ~ its output is like tossing a coin  
  
  
  
#  
##############################################################################
#
# t-tests to compare independent samples
#
##############################################################################
#
#  

data.df<-read.table("chicks.dat",header=T,stringsAsFactors = T)

names(data.df)

t.test(Weight.gain~Feed,var.equal = TRUE,data=data.df)

boxplot(Weight.gain~Feed,data=data.df)
tapply(data.df$Weight.gain,data.df$Feed,summa)

#
#  Exercise 2.3
#
#  The rate of carbon dioxide emission was measured by incubation of soil 
#  cores collected from randomly located selections in a region
#  of Bedfordshire from either arable land or grassland 
#  Test for evidence of a difference between these land uses.
#
#   Data: CO2.dat, land use (arable or grass) and CO2 emission 
#   (micro g C per kg soil per day)
#

data.df<-read.table("CO2.dat",header=T,stringsAsFactors = T)




###########################################################################
###########################################################################
#
###########################################################################
###########################################################################
#
# 3.  Analysis of variance with lm()  
#
#  Example 1.  Data in cabbage.dat
#
#  Yields of spring cabbage from plots to which one of four treatments
#  was applied.  The allocation of treatments to plots was done at
#  random.  Treatments are:
#
#  C: Control, no nitrogen applied.
# AS: Nitrogen applied as ammonium sulphate
# AN: Nitrogen applied as ammonium nitrate
# NC: Nitrogen applied as nitrochalk (ammonium nitrate + chalk)
#  
#
data_ca.df<-read.table("cabbage.dat",header=T,stringsAsFactors = T)
#
names(data_ca.df)
#
# (i)  Initial model fit
#
mod<-lm(Yield~Treatment,data=data_ca.df)
#
#  (ii) Inspect the residuals 
#
summa(mod$residuals)
summaplot(mod$residuals)
plot(mod$fitted.values,mod$residuals,pch=16)
#
#  (iii)  Test the general or omnibus null hypothesis
#
anova(mod)
#
#
boxplot(Yield~Treatment,data=data_ca.df)
#
#
# Example  Data in cashmore.dat
#
# These are data on soil properties from sites in different soil 
# series or classes (the variable "Soil_Series", which is read in
# as a factor).
#
# Use the LM and ANOVA to examine evidence that differences between
# the soil series account for variation in water content (GWC_T, GWC_S)
# and OM content.
#
#  The soil series are quite distinct, note the following information
#  about them
#       
#     Series Name   Symbol  Parent            
#                           material
#
#     Lowlands      Lw     Lower Greensand    Colluvial*
#     Hallsworth    Ha      "     "           Pelostagnogley**
#     Nercwys       Ne      "     "           Stagnogley**
#     Evesham       Ev     Gault Clay 
#     Bardsey       Ba      "     "
#     Enborne       En     Alluvial Clay
#
#   The colluvial soil is relatively coarse material over the LGS
#   The stagnogleys are finer material, drainage is impeded. 
#   In the pelostagnogley the finer minerals are prone to swell when
#    wet and shrink and crack when dry.
#   The Evesham series is formed in heavy clay, the Bardsey series and
#   Enborne series are both sandy clay loams over clay loams.

data_csh.df<-read.table("cashmore.dat",header=T,stringsAsFactors=T)

names(data_csh.df)

mod<-lm(GWC_T~Soil_Series,data=data_csh.df)
summa(mod$residuals)
summaplot(mod$residuals)
plot(mod$fitted.values,mod$residuals,pch=16)

anova(mod)

boxplot(GWC_T~Soil_Series,data=data_csh.df)
#


mod<-lm(OM_T~Soil_Series,data=data_csh.df)
summa(mod$residuals)
summaplot(mod$residuals)
plot(mod$fitted.values,mod$residuals,pch=16)

anova(mod)

boxplot(OM_T~Soil_Series,data=data_csh.df)

###########################################################################
###########################################################################
#
###########################################################################
###########################################################################
#
# 4.  Regression analysis with lm()  
#
#
#  Example 1.  
#
#
   data.df<-read.table("soil.dat",header=T)
   names(data.df)
#
#   Plot pH in CaCl2 against pH in water
#

plot(data.df$pHCaCl,pch=16,data.df$pHw,xlab="pH in CaCl2",ylab="pH in water",
xlim=c(2,8.5),ylim=c(2,8.5))

# Draw the bisector, or 1:1 line

lines(c(2,8.5),c(2,8.5))
#
#
#  Compute the correlation coefficient between the two variables
#

cor(data.df$pHCaCl,data.df$pHw)

#
#  What do you conclude about the relationship between the methods for
#  measuring pH, just from the plot?
#


#  Regression model using lm

mod<-lm(pHw~pHCaCl,data=data.df)

summary(mod)

#
#  Assumptions
#

summa(mod$residuals)
summaplot(mod$residuals)

plot(mod$fitted,mod$residuals,xlab="Fitted values",ylab="Residuals",pch=16)

#
#  What is the evidence provided from the model about the relationship?
#  
#  Draw the regression line
#
#  First, replot the data

plot(data.df$pHCaCl,data.df$pHw,pch=16,xlab="pH in CaCl2",ylab="pH in water",
xlim=c(2,8.5),ylim=c(2,8.5))

# Now, chose a minimum and maximum x value for the line

minx<-2
maxx<-8.5

regline(minx,maxx,mod)

#
# Using the regression model for prediction
#

# Now predict the pH in water for some values in CaCl (these would be new
# samples for which we only know pHCaCl.

# values for prediction: we present a set of 5 values

pHCaCl<-c(4.2,5.6,6.4,7.3,8.1)

# make a dataframe from these values

pHCaCl.df<-data.frame(pHCaCl)

# give the column in the data frame the same name as the same variable
# in the data frame we used for the regression.

colnames(pHCaCl.df)<-"pHCaCl"


names(data.df)
#  Now make the predictions, the first term is the model object, the second
# is the data frame with the pH in CaCl2 values for which we want predictions.
# 
#

pHwaterpred<-predict(mod,pHCaCl.df,
interval="prediction",level=0.95)

print(pHwaterpred)

# The first column is the prediction, the second two are the upper and lower
# bounds of each 95% confidence interval for a point prediction.  Note that this
# is the confidence interval for a point prediction (i.e. a new single measurement).

##############################################################################################
#
#  Exercises
#
#  The file cashmore.dat, contains data on a number of soil properties measured at 100 sample sites
#  in an experimental field.  These include Gravimetric Water Content in the topsoil (GWC_T) and in 
#  the subsoil (GWC_S).  
#  
#  (i)  It is easier to sample the topsoil than the subsoil. Use regression 
#  analysis to test whether subsoil water content can be predicted from 
#  topsoil water content.  Find the predicted subsoil water content for
#  soils where the topsoil water content is 20, 25 and 30%.
#
#  (ii)  It is easier to sample the topsoil than the subsoil. Use regression 
#  analysis to test whether subsoil organic matter content can be predicted 
#  from topsoil water content.  Find the predicted subsoil OM content for
#  soils where the topsoil OM content is 1.5%, 2.5% and 3.5%.

data.df<-read.table("cashmore.dat",header=T,stringsAsFactors=T)

names(data.df)


##########################################################################



