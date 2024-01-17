#read in crime data
#data was modified to remove excess header data
getwd()
setwd('C:/Users/conno/Documents/BANA 6610 - Stats for BA/Final Project')
require(gdata)
crimes = read.xls('data_for_r.xlsx', perl = "C:\\Perl64\\bin\\perl.exe"
                    ,sheet = 1, header = TRUE)


#code for importing from work computer
#gdata will not work on my work computer as i cannot install perl
#getwd()
#setwd('//us/home/c/connor.buchanan/CU Denver/BANA 6610 - Statistics for BA')
#crimes = read.csv('data_for_r.csv',header=TRUE)
#end code code for importing from work computer



head(crimes)
names(crimes)

#remove invalid and non-numeric rows
crimes$X = NULL
crimes$X.1 = NULL
crimes$County = NULL
crimes = crimes[complete.cases(crimes),]

#determine correlation
cor(crimes)

install.packages('corrplot')
library(corrplot)
corrplot(cor(crimes),method='number')


#attempt the stepwise model building
#build out model to predict total offenses
ARsq = 0
ARsqName = ''

for (i in colnames(crimes)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society'){
    TotalMod = lm(Total.Offenses ~ get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#attempt 2 for total offenses
for (i in colnames(crimes)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'){
    TotalMod = lm(Total.Offenses ~ Unemployed + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#attempt 3 for total offenses
for (i in colnames(crimes)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Medical.Marijuana.Patients'){
    TotalMod = lm(Total.Offenses ~ Unemployed + Medical.Marijuana.Patients + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#attempt 4 for total offenses
for (i in colnames(crimes)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Medical.Marijuana.Patients' && i != 'Median.Income'){
    TotalMod = lm(Total.Offenses ~ Unemployed + Medical.Marijuana.Patients + 
                    Median.Income + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#attempt 5 for total offenses
for (i in colnames(crimes)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Medical.Marijuana.Patients' && i != 'Median.Income' && i != 'Foreclosure.Sales'){
    TotalMod = lm(Total.Offenses ~ Unemployed + Medical.Marijuana.Patients + 
                    Median.Income + Foreclosure.Sales + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)


####Attempt to scale the data with log of target variable####

#add new column calculating the log
crimes$TotalOffLog = log(crimes$Total.Offenses)
crimes$Total.Offenses = NULL
head(crimes)

#determine correlation
cor(crimes)

corrplot(cor(crimes),method='number')

#stepwise method to predic scaled log crimes

ARsq = 0
ARsqName = ''

for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society'){
    TotalMod = lm(TotalOffLog ~ get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#second variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'){
    TotalMod = lm(TotalOffLog ~ Unemployed + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#Third variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Primary.Election.Turnout'){
    TotalMod = lm(TotalOffLog ~ Unemployed + Primary.Election.Turnout + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#fourth variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Primary.Election.Turnout' && i != 'Metro.County'){
    TotalMod = lm(TotalOffLog ~ Unemployed + Primary.Election.Turnout + Metro.County + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#fifth variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Primary.Election.Turnout' && i != 'Metro.County' && i != 'Medical.Marijuana.Patients'){
    TotalMod = lm(TotalOffLog ~ Unemployed + Primary.Election.Turnout + Metro.County + 
                    Medical.Marijuana.Patients + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#sixth variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Primary.Election.Turnout' && i != 'Metro.County' && i != 'Medical.Marijuana.Patients'
      && i != 'Unemployment.rate'){
    TotalMod = lm(TotalOffLog ~ Unemployed + Primary.Election.Turnout + Metro.County + 
                    Medical.Marijuana.Patients + Unemployment.rate +get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#seventh variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Primary.Election.Turnout' && i != 'Metro.County' && i != 'Medical.Marijuana.Patients'
      && i != 'Unemployment.rate' && i != 'Median.Income'){
    TotalMod = lm(TotalOffLog ~ Unemployed + Primary.Election.Turnout + Metro.County + 
                    Medical.Marijuana.Patients + Unemployment.rate + Median.Income + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#eighth variable
for (i in colnames(crimes)){
  if (i != 'County' && i != 'TotalOffLog' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Unemployed'
      && i != 'Primary.Election.Turnout' && i != 'Metro.County' && i != 'Medical.Marijuana.Patients'
      && i != 'Unemployment.rate' && i != 'Median.Income' && i != 'Sales.Tax.Rate'){
    TotalMod = lm(TotalOffLog ~ Unemployed + Primary.Election.Turnout + Metro.County + 
                    Medical.Marijuana.Patients + Unemployment.rate + Median.Income +
                    Sales.Tax.Rate + get(i), data=crimes)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)


#### second effort at regression using all scaled data ####
getwd()
setwd('C:/Users/conno/Documents/BANA 6610 - Stats for BA/Final Project')
require(gdata)
scaled = read.xls('data_for_r_scaled.xlsx', perl = "C:\\Perl64\\bin\\perl.exe"
                  ,sheet = 1, header = TRUE)

head(scaled)
names(scaled)

scaled$X = NULL
scaled$X.1 = NULL

ARsq = 0
ARsqName = ''

for (i in colnames(scaled)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Population'){
    TotalMod = lm(Total.Offenses ~ get(i), data=scaled)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#second attempt
for (i in colnames(scaled)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Population'
      && i != 'Metro.County'){
    TotalMod = lm(Total.Offenses ~ Metro.County + get(i), data=scaled)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

#third attempt
for (i in colnames(scaled)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Crimes.Against.Persons' && 
      i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society' && i != 'Population'
      && i != 'Metro.County' && 'Marijuana.Patients.Ratio'){
    TotalMod = lm(Total.Offenses ~ Metro.County + Marijuana.Patients.Ratio + get(i), data=scaled)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)

scaled$Total.Offenses.Scaled = scaled$Total.Offenses/scaled$Population
head(scaled)


#attempt to build model with scaled data
ARsq = 0
ARsqName = ''

for (i in colnames(scaled)){
  if (i != 'County' && i != 'Total.Offenses' && i != 'Total.Offenses.Scaled' && 
      i != 'Crimes.Against.Persons' && i != 'Crimes.Against.Property' && i != 'Crimes.Against.Society'){
    TotalMod = lm(Total.Offenses.Scaled ~ get(i), data=scaled)
    print(i)
    print('Adj R squared is ')
    print(summary(TotalMod)$adj.r.squared)
    if(summary(TotalMod)$adj.r.squared > ARsq){
      ARsq = summary(TotalMod)$adj.r.squared
      ARsqName = i
    }
  }
}
#print results of highest scores
print(cat('The highest adj r value is',ARsqName,ARsq))
plot(TotalMod$fitted.values,TotalMod$residuals)