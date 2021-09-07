# Problem statement - To  perform a linear regression analysis to arrive at a model that can be used to predict the average life expectancy of people 
#                     from different countries around the world. To get what are the factors that are affecting the average life span of people and to 
#                     derive business insights from it.


#........................................................................................IMPORTING THE DATASET..................................................................................................................
data <- read.csv(choose.files()) # Importing the data-set in csv format
dim(data)  # Dimension of data-set which is 2938 rows and 23 columns (attributes)
head(data) # First few rows of the data-set
           # Here the target variable (dependent variable) is "Life_Expectancy" (It is continuous)
           # We will predict "Life_Expectancy" by using linear regression model as per the problem statement given



#.........................................................................IDENTIFYING VARIABLE TYPE, ELIMINATION OF GARBAGE VARIABLES, DATA CONERSION AND DATA SUMMARY..........................................................
############################################### VARIABLE TYPE ##################
str(data) # Type of each variable
          # character categorical - "Country" and "Status"
          # Rest all are numerical attributes 

############################################### GARBAGE VARIABLE ###############
# The attribute "Country" has too many levels, and doesn't give additional information to predict "Life_Expectancy"
# Similarly the attribute "Year" is a time series and doesn't give additional information to predict "Life_Expectancy"
# So, we will eliminate them
data <- subset(data, select = -c(Country,Year))  # Deleting garbage attribute which are "Country" and "Year"

############################################### DATA CONVERSION ################
# As it is known, all countries need to reach more than 90% national coverage for all vaccines in the country's routine immunization
# So, on the basis of this we are going to mutate the "Hepatitis_B", "Polio", and "Diphtheria" into character categorical variables with 2 value: "Under 90%" and "above 90%"
mut_cols <- c("Hepatitis_B", "Polio", "Diphtheria") # columns which are needed to be changed into character categorical variables
for(item in mut_cols) # For loop to change "Hepatitis_B", "Polio", and "Diphtheria" into character categorical variables with 2 value: "Under 90%" and "above 90%"
{
  data[,c(item)] = ifelse(data[,c(item)] < 90, "under 90%", "above 90%")
}
char_cols <- c( "Status", "Hepatitis_B", "Polio", "Diphtheria") # columns which are needed to be changed into factors
for(item in char_cols)  # For loop to change each character variable into factor
{ 
  data[,c(item)] <- as.factor(data[,c(item)])
}  
str(data) # Type of each variable
          # factor - "Status","Hepatitis_B", "Polio" and "Diphtheria" 
          # Rest all are numerical attributes 

############################################### SUMMARY ########################
summary(data) # General summary of the data
              # Half of the life span lies between 63 yrs to 75 yrs with an average life span of 69 yrs
              # Almost half of the countries have less than 90% people vaccinated for Hepatitis_B, Polio and Diphtheria
              # The infant_Deaths is as high as 1800 in some countries & is as small as 0 in some other counties ( mostly European )
              # The data consist of countries with GDP ranging from 3.751e+07 to 1.822e+13



#........................................................................................DATA PRE-PROCESSING.....................................................................................................................
################### MISSING VALUES #############################################
colSums((is.na(data)))/nrow(data)*100  # Checking the presence of % missing values
# There are missing values in dataset for "Life_Expectancy", "Adult_Mortality", "Alcohol", "Hepatitis_B", "BMI", "Polio", "Total_Expenditure", "Diphtheria", "GDP", 
#                                          "Per_Capita_GDP", "Population", "Thinness_1.19_Years", "Thinness_5.9_Years", "Income_Composition_of_Resources" and "Schooling" 
# Since all the attributes have less than 20% missing values so instead of dropping the attribute we will replace the missing values with mean, median or mode in each one of them
boxplot(data$Life_Expectancy) # negative outliers detected
                              # Since there are outliers present so we will replace the missing values with median
data$Life_Expectancy[which(is.na(data$Life_Expectancy))] <- median(data$Life_Expectancy, na.rm = T) #Replacing the missing values with median
boxplot(data$Adult_Mortality) # positive outliers detected
                              # Since there are outliers present so we will replace the missing values with median
data$Adult_Mortality[which(is.na(data$Adult_Mortality))] <- median(data$Adult_Mortality, na.rm = T) #Replacing the missing values with median
boxplot(data$Alcohol) # no outliers detected
                      # Since there are no outliers present so we will replace the missing values with mean
data$Alcohol[which(is.na(data$Alcohol))] <- mean(data$Alcohol, na.rm = T) #Replacing the missing values with median
boxplot(data$BMI) # no outliers detected
                  # Since there are no outliers present so we will replace the missing values with mean
data$BMI[which(is.na(data$BMI))] <- mean(data$BMI, na.rm = T) #Replacing the missing values with median
boxplot(data$Total_Expenditure) # positive outliers detected
                                # Since there are outliers present so we will replace the missing values with median
data$Total_Expenditure[which(is.na(data$Total_Expenditure))] <- median(data$Total_Expenditure, na.rm = T) #Replacing the missing values with median
boxplot(data$GDP) # positive outliers detected
                  # Since there are outliers present so we will replace the missing values with median
data$GDP[which(is.na(data$GDP))] <- median(data$GDP, na.rm = T) #Replacing the missing values with median
boxplot(data$Per_Capita_GDP) # positive outliers detected
                             # Since there are outliers present so we will replace the missing values with median
data$Per_Capita_GDP[which(is.na(data$Per_Capita_GDP))] <- median(data$Per_Capita_GDP, na.rm = T) #Replacing the missing values with median# Now to check the presence of outlier we will use boxplot
boxplot(data$Population) # positive outliers detected
                         # Since there are outliers present so we will replace the missing values with median
data$Population[which(is.na(data$Population))] <- median(data$Population, na.rm = T) #Replacing the missing values with median# Boxplot will be created for only continuous variables as there is no point of having outliers in categorical variables or the target variable which is binary
boxplot(data$Thinness_1.19_Years) # positive outliers detected
                                  # Since there are outliers present so we will replace the missing values with median
data$Thinness_1.19_Years[which(is.na(data$Thinness_1.19_Years))] <- median(data$Thinness_1.19_Years, na.rm = T) #Replacing the missing values with median
boxplot(data$Thinness_5.9_Years) # positive outliers detected
                                 # Since there are outliers present so we will replace the missing values with median
data$Thinness_5.9_Years[which(is.na(data$Thinness_5.9_Years))] <- median(data$Thinness_5.9_Years, na.rm = T) #Replacing the missing values with medianboxplot(data$VistID) # No outliers detected
boxplot(data$Income_Composition_of_Resources) # negative outliers detected
                                              # Since there are outliers present so we will replace the missing values with median
data$Income_Composition_of_Resources[which(is.na(data$Income_Composition_of_Resources))] <- median(data$Income_Composition_of_Resources, na.rm = T) #Replacing the missing values with median
boxplot(data$Schooling) # outliers detected
                        # Since there are outliers present so we will replace the missing values with median
data$Schooling[which(is.na(data$Schooling))] <- median(data$Schooling, na.rm = T) #Replacing the missing values with median
# Since all other variables are categorical so we will replace the missing values with mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
data$Hepatitis_B[which(is.na(data$Hepatitis_B))] <- getmode(data$Hepatitis_B) #Replacing the missing values with mode
data$Polio[which(is.na(data$Polio))] <- getmode(data$Polio) #Replacing the missing values with mode
data$Diphtheria[which(is.na(data$Diphtheria))] <- getmode(data$Diphtheria) #Replacing the missing values with mode
colSums((is.na(data)))/nrow(data)*100  # no missing values

################### OUTLIERS ###################################################
boxplot(data$Life_Expectancy) # Negative outliers to be treated
quantile(data$Life_Expectancy,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                             # Since there are negative outliers so we will change every data (1% of overall data) below 45.600 to 45.600
data$Life_Expectancy <- ifelse(data$Life_Expectancy < 45.600,45.600,data$Life_Expectancy) # Changing all the data below 45.600 to 45.600
boxplot(data$Life_Expectancy) # No outliers to be treated
boxplot(data$Adult_Mortality) # Positive outliers to be treated
quantile(data$Adult_Mortality,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                             # Since there are positive outliers so we will change every data (2% of overall data) above 492.04 to 492.04
data$Adult_Mortality <- ifelse(data$Adult_Mortality > 492.04,492.04,data$Adult_Mortality) # Changing all the data above 492.04 to 492.04
boxplot(data$Adult_Mortality) # No outliers to be treated
boxplot(data$Infant_Deaths) # Positive to be treated
quantile(data$Infant_Deaths,seq(0,1,0.01))  # Quantile to get an idea of what range of data needs to be treated
                                            # Since there are positive outliers so we will change every data (2% of overall data) above 246.52 to 246.52
data$Infant_Deaths <- ifelse(data$Infant_Deaths > 246.52,246.52,data$Infant_Deaths) # Changing all the data above 246.52 to 246.52
boxplot(data$Infant_Deaths) # No outliers to be treated
boxplot(data$Alcohol) # Positive outliers to be treated
quantile(data$Alcohol,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                     # Since there are positive outliers so we will change every data (1% of overall data) above 13.450000 to 13.450000
data$Alcohol <- ifelse(data$Alcohol > 13.450000,13.450000,data$Alcohol) # Changing all the data above 13.450000 to 13.450000
boxplot(data$Alcohol) # No outliers to be treated
boxplot(data$Percentage_Expenditure) # Positive outliers to be treated
quantile(data$Percentage_Expenditure,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                                    # Since there are positive outliers so we will change every data (1% of overall data) above 1.019941e+04 to 1.019941e+04
data$Percentage_Expenditure <- ifelse(data$Percentage_Expenditure > 1.019941e+04,1.019941e+04,data$Percentage_Expenditure) # Changing all the data above 1.019941e+04 to 1.019941e+04
boxplot(data$Percentage_Expenditure) # No outliers to be treated
boxplot(data$Measles) # Positive outliers to be treated
quantile(data$Measles,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                     # Since there are positive outliers so we will change every data (1% of overall data) above 54163.36 to 54163.36
data$Measles <- ifelse(data$Measles > 54163.36,54163.36,data$Measles) # Changing all the data above 54163.36 to 54163.36
boxplot(data$Measles) # No outliers to be treated
boxplot(data$BMI) # No outliers to be treated
boxplot(data$Under.five_Deaths) # Positive outliers to be treated
quantile(data$Under.five_Deaths,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                               # Since there are positive outliers so we will change every data (2% of overall data) above 346.68 to 346.68
data$Under.five_Deaths <- ifelse(data$Under.five_Deaths > 346.68,346.68,data$Under.five_Deaths) # Changing all the data above 346.68 to 346.68
boxplot(data$Under.five_Deaths) # No outliers to be treated
boxplot(data$Total_Expenditure) # Positive outliers to be treated
quantile(data$Total_Expenditure,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                               # Since there are positive outliers so we will change every data (1% of overall data) above 12.7071 to 12.7071
data$Total_Expenditure <- ifelse(data$Total_Expenditure > 12.7071,12.7071,data$Total_Expenditure) # Changing all the data above 12.7071 to 12.7071
boxplot(data$Total_Expenditure) # No outliers to be treated
boxplot(data$HIV.AIDS) # Positive outliers to be treated
quantile(data$HIV.AIDS,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                      # Since there are positive outliers so we will change every data (2% of overall data) above 19.052 to 19.052
data$HIV.AIDS <- ifelse(data$HIV.AIDS > 19.052,19.052,data$HIV.AIDS) # Changing all the data above 19.052 to 19.052
boxplot(data$HIV.AIDS) # No outliers to be treated
boxplot(data$GDP) # Positive outliers to be treated
quantile(data$GDP,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                 # Since there are positive outliers so we will change every data (1% of overall data) above 4.873789e+12 to 4.873789e+12
data$GDP <- ifelse(data$GDP > 4.873789e+12,4.873789e+12,data$GDP) # Changing all the data above 4.873789e+12 to 4.873789e+12
boxplot(data$GDP) # No outliers to be treated
boxplot(data$Per_Capita_GDP) # Positive outliers to be treated
quantile(data$Per_Capita_GDP,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                            # Since there are positive outliers so we will change every data (1% of overall data) above 82288.2274 to 82288.2274
data$Per_Capita_GDP <- ifelse(data$Per_Capita_GDP > 82288.2274,82288.2274,data$Per_Capita_GDP) # Changing all the data above 82288.2274 to 82288.2274
boxplot(data$Per_Capita_GDP) # No outliers to be treated
boxplot(data$Population) # Positive outliers to be treated
quantile(data$Population,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                        # Since there are positive outliers so we will change every data (2% of overall data) above 2.240667e+08 to 2.240667e+08
data$Population <- ifelse(data$Population > 2.240667e+08,2.240667e+08,data$Population) # Changing all the data above 2.240667e+08 to 2.240667e+08
boxplot(data$Population) # No outliers to be treated
boxplot(data$Thinness_1.19_Years) # Positive outliers to be treated
quantile(data$Thinness_1.19_Years,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                                 # Since there are positive outliers so we will change every data (1% of overall data) above 19.500 to 19.500
data$Thinness_1.19_Years <- ifelse(data$Thinness_1.19_Years > 19.500,19.500,data$Thinness_1.19_Years) # Changing all the data above 19.500 to 19.500
boxplot(data$Thinness_1.19_Years) # No outliers to be treated
boxplot(data$Thinness_5.9_Years) # Positive outliers to be treated
quantile(data$Thinness_5.9_Years,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                                # Since there are positive outliers so we will change every data (1% of overall data) above 19.900 to 19.900 
data$Thinness_5.9_Years <- ifelse(data$Thinness_5.9_Years > 19.900 ,19.900 ,data$Thinness_5.9_Years) # Changing all the data above 19.900  to 19.900 
boxplot(data$Thinness_5.9_Years) # No outliers to be treated
boxplot(data$Income_Composition_of_Resources) # Negative outliers to be treated
quantile(data$Income_Composition_of_Resources,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                                             # Since there are negative outliers so we will change every data (5% of overall data) below 0.29100 to 0.29100
data$Income_Composition_of_Resources <- ifelse(data$Income_Composition_of_Resources < 0.29100 ,0.29100 ,data$Income_Composition_of_Resources) # Changing all the data below 0.29100 to 0.29100
boxplot(data$Income_Composition_of_Resources) # No outliers to be treated
boxplot(data$Schooling) # Negative outliers to be treated
quantile(data$Schooling,seq(0,1,0.01)) # Quantile to get an idea of what range of data needs to be treated
                                       # Since there are negative outliers so we will change every data (1% of overall data) below 2.900 to 2.900
data$Schooling <- ifelse(data$Schooling < 2.900 ,2.900 ,data$Schooling) # Changing all the data below 2.900 to 2.900
boxplot(data$Schooling) # No outliers to be treated
# All the outliers successfully treated



#...................................................................................EDA - UNIVARIATE AND BIVARIATE ANALYSIS............................................................................................................
#univariate analysis
################### HISTOGRAMS #################################################
colsforhist1=c("Life_Expectancy", "Adult_Mortality", "Infant_Deaths", "Alcohol", "Percentage_Expenditure", "Measles", "BMI", "Under.five_Deaths", "Total_Expenditure" ) # Columns to plot histograms for
library(RColorBrewer) # Importing library to plot histograms and barplots
par(mfrow=c(3,3)) # Setting parameters
for(hist_cols in colsforhist1)  # For loop to plot histograms by using hist function
{ 
  hist(data[,c(hist_cols)],main=paste('Histogram of:',hist_cols),col=brewer.pal(8,"Paired")) 
} 
colsforhist2=c("HIV.AIDS", "GDP", "Per_Capita_GDP", "Population", "Thinness_1.19_Years", "Thinness_5.9_Years", "Income_Composition_of_Resources", "Schooling") # Columns to plot histograms for
library(RColorBrewer) # Importing library to plot histograms and barplots
par(mfrow=c(3,3)) # Setting parameters
for(hist_cols in colsforhist2)  # For loop to plot histograms by using hist function
{ 
  hist(data[,c(hist_cols)],main=paste('Histogram of:',hist_cols),col=brewer.pal(8,"Paired")) 
}
# No outliers - Prefect for building linear regression model, However most of the attributes are little bit skewed except "Life_Expectancy", "Total_Expenditure" and "Schooling"
# These skewed attributes may affect the results but log transformation can be a solution for this
# Results can be good as our dependent variable "Life_Expectancy" is normally distributed

################### BARPLOTS ###################################################
ColsForBar =c("Status", "Hepatitis_B", "Polio", "Diphtheria") # Columns to plot barplot for
par(mfrow=c(2,2)) # Setting parameters
for(bar_cols in ColsForBar)  # For loop to plot barplots by using barplot function
{
  barplot(table(data[,c(bar_cols)]),main=paste('Barplot of :',bar_cols),col=brewer.pal(8,"Paired"))
}    
# No attribute is perfectly balanced however the difference in no of data for both the classes ( 1 and 0 ) is not very large except for "Status" attribute
# We have more data for developing counties as compared to developed countries in "Status" attribute
# It will not affect the results ( only "Status" can make the predictions biased towards developing countries )
# Solution is the elimination of "Status" which we will confirm with bivariate analysis

#Bivariate analysis
################### SCATTER PLOT ###############################################
# Continuous Vs Continuous --- Scatter plot and correlation matrix
ContinuousCols=c("Life_Expectancy", "Adult_Mortality", "Infant_Deaths", "Alcohol", "Percentage_Expenditure", "Measles", "BMI", "Under.five_Deaths", "Total_Expenditure" , "HIV.AIDS", "GDP", "Per_Capita_GDP", "Population", "Thinness_1.19_Years", "Thinness_5.9_Years", "Income_Composition_of_Resources", "Schooling") # Columns to plot scatter
plot(data[,ContinuousCols],col='blue')  # Scatter plot
# As per the scatter plot all are good predictors except "Infant_Deaths", "Alcohol", "Percentage_Expenditure", "Measles", "Under.five_Deaths", "Total_Expenditure", "GDP","Population", "Thinness_1.19_Years" and  "Thinness_5.9_Years"


################### CORRLEATION MATRIX #########################################
CorrData=cor(data[,ContinuousCols ], use = "complete.obs") # Correlation matrix
names(CorrData['Life_Expectancy',][abs(CorrData['Life_Expectancy',]) < 0.5]) # Finding non-correalted variables by fixing a barrier of  0.5
# All are good predictors except "Infant_Deaths", "Alcohol", "Percentage_Expenditure", "Measles", "Under.five_Deaths", "Total_Expenditure", "GDP", "Population", "Thinness_1.19_Years" and  "Thinness_5.9_Years" as we saw in scatter plot
# Life expectancy and adult mortality rates have high negative correlation
# BMI has a positive correlation with life expectancy
# GDP however is showing no correlation as can be seen in scatter plot also
# Not suprisingly schooling years have high positive correlation with life expectancy because schooling leads to adoption of healthy habits

################### ANOVA TEST #################################################
# Continuous Vs Categorical --- ANOVA test
# Null hypothesis: the target variable and continuous variables are not correlated
# if p>0.05, then Null accepted and we conclude that variables are uncorrelated
# if p<0.05 : there is variation in Clicked with respect to different
colsforanova <- c("Status", "Hepatitis_B", "Polio", "Diphtheria") # Columns for ANOVA testing
for(anovacols in colsforanova) # For loops for ANOVA testing on each set of categorical and continuous (independent) variable
{
  anovaresult <- summary(aov(Life_Expectancy ~ data[,c(anovacols)],data=data)) # ANOVA test
  print(anovacols) # Variable
  print(anovaresult) # ANOVA test result
} 
# This test shows that all categorical variables are correlated and no one needs to be eliminated



#............................................................................FEATURE SELECTION AND DATA CONVERSION...............................................................................................................
################### FEATURE SELECTION ##########################################
# AS per the analysis, further features to be rejected are "Infant_Deaths", "Alcohol", "Percentage_Expenditure", "Measles", "Under.five_Deaths", "Total_Expenditure", "GDP","Population", "Thinness_1.19_Years" and  "Thinness_5.9_Years"
# SO we will eliminate them
data <- subset(data, select = -c(Infant_Deaths, Alcohol, Percentage_Expenditure, Measles, Under.five_Deaths, Total_Expenditure, GDP, Population, Thinness_1.19_Years, Thinness_5.9_Years)) # Deleting unwanted columns (features)

################### DATA CONVERSION #########################
# Now we also have factors ( all with 2 levels only ) so we will convert those factors into 0 and 1
data$Status = ifelse(data$Status == "Developing", 0, 1)
data$Hepatitis_B = ifelse(data$Hepatitis_B == "under 90%", 0, 1)
data$Polio = ifelse(data$Polio== "under 90%", 0, 1)
data$Diphtheria = ifelse(data$Diphtheria == "under 90%", 0, 1)
str(data) # Type of each independent variable -  numeric


#.........................................DATA SPLITTING ........................................................................................................................................................................
library(caTools) # Importing library to split the data-set
library(caret) # Importing library to create dummy variables
set.seed(101) # Setting initialization parameter
split <- sample.split(data$Life_Expectancy, SplitRatio = 0.70) # SPlitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 2076 rows
test <- subset(data, split==F) # Test data - 862 rows

#.............................................................................MODEL BUILDING.....................................................................................................................................
Model_Reg1=lm(Life_Expectancy~.,data=training) # Building linear regression model
summary(Model_Reg1) # According to the summary of model - r2 value is 0.8382 ( good performance but still scope of improvement)
#                                   - There are some non-significant variables like "Status" and "Polio"
#                                   - SO we will delete these variables and again build our model
data <- subset(data, select = -c(Status, Polio)) # Deleting non-significant columns
split <- sample.split(data$Life_Expectancy, SplitRatio = 0.70) # Again splitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 2076 rows
test <- subset(data, split==F) # Test data - 862 rows
Model_Reg2=lm(Life_Expectancy~.,data=training) # Building linear regression model
summary(Model_Reg2) # According to the summary of model - r2 value is 0.8312 ( good performance but still scope of improvement) with no non significant variables

#..............................................................................MULTI-COLLINEARITY CHECK...........................................................................................................................
install.packages('car') # Installing package to import car library
library(car) # Importing car library to do multi-collinearity check
vif(Model_Reg2) # Check
                # According to this no attributes are showing any sign of multicollinearity ( no value greater than 5)

#..............................................................................MODEL ACCURACY......................................................................................................................................
lin_pred1 <- predict(Model_Reg2, newdata = test) # testing 
combind1 = cbind(test$Life_Expectancy, lin_pred1) # Combined data having both predicted and actual values
# Accuracy
APE = 100 *(abs(test$Life_Expectancy-lin_pred1)/test$Life_Expectancy) # Calculating the Absolute Percentage Error for each prediction
MeanAPE=mean(APE) # Calculating MAPE
MedianAPE=median(APE) # Calculating Median APE
Mean_A = 100 - MeanAPE # Calculating mean accuracy
Median_A = 100 - MedianAPE # Calculating median accuracy

#                                        SO ---------------  MAPE = 4.204787
#                                                            Median-APE = 3.045557
#                                                            Mean Accuracy - 95.79521
#                                                            Median Accuracy - 96.95444

# Accuracy scores suggest that model is doing good
mean(lin_pred1) # Average life expectancy of people over the years in different countries is 69 yrs which is good but can be better by adopting healthy habits
 
#..............................................................................TRY FOR IMPROVING THE MODEL..................................................................................................................................
str(data) # By seeing at the histograms and structure of data we can see some columns like "Income_Composition_of_Resources", "Per_Capita_GDP", "HIV.AIDS",  "BMI" and  "Adult_Mortality" are little bit skewed or not normally distributed
          # And "Adult_Mortality" and "Per_Capita_GDP" are little bit large in magnitude than others 
          # so we can try different transformations and then can check the model again ( it may improve )
          # For skewed data we will do log transformation which will automatically scale the data also

################### TRANSFORMATION #############################################
data$Per_Capita_GDP <- ifelse(data$Per_Capita_GDP == 0,1,data$Per_Capita_GDP) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$Per_Capita_GDP = log(data$Per_Capita_GDP) # Log transformation
data$HIV.AIDS <- ifelse(data$HIV.AIDS == 0,1,data$HIV.AIDS) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$HIV.AIDS = abs(log(data$HIV.AIDS)) # Log transformation
data$BMI <- ifelse(data$BMI == 0,1,data$BMI) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$BMI = log(data$BMI) # Log transformation
data$Adult_Mortality <- ifelse(data$Adult_Mortality == 0,1,data$Adult_Mortality) # Converting 0 to 1 before applying transformation as presence of zero can give undefined data
data$Adult_Mortality = log(data$Adult_Mortality) # Log transformation
str(data) # Now it looks good
colsforhist=c("Life_Expectancy", "Adult_Mortality", "BMI", "HIV.AIDS", "Per_Capita_GDP", "Income_Composition_of_Resources", "Schooling" ) # Columns to plot histograms for
library(RColorBrewer) # Importing library to plot histograms and barplots
par(mfrow=c(2,4)) # Setting parameters
for(hist_cols in colsforhist)  # For loop to plot histograms by using hist function
{ 
  hist(data[,c(hist_cols)],main=paste('Histogram of:',hist_cols),col=brewer.pal(8,"Paired")) 
} # Now most of the data are normally distributed

############################# TRAINING AND TESTING THE MODEL AGAIN #############
split <- sample.split(data$Life_Expectancy, SplitRatio = 0.70) # Again splitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 2076 rows
test <- subset(data, split==F)  # Test data - 862 rows
Model_Reg3=lm(Life_Expectancy~.,data=training) # Building linear regression model
summary(Model_Reg3) # According to the summary of model - r2 value is 0.7052 ( bad performance and scope of improvement)
#                                   - There is a non-significant variable "HIV.AIDS"
#                                   - SO we will delete this variable and again build our model
data <- subset(data, select = -c(HIV.AIDS)) # Deleting non-significant column
split <- sample.split(data$Life_Expectancy, SplitRatio = 0.70) # Again splitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 2076 rows
test <- subset(data, split==F) # Test data - 862 rows
Model_Reg4=lm(Life_Expectancy~.,data=training) # Building linear regression model
summary(Model_Reg4) # According to the summary of model - r2 value is 0.7135 ( Again bad performance and scope of improvement) with no non significant variables
vif(Model_Reg4) # multicollinearity Check
                # According to this  attributes "Income_Composition_of_Resources" is showing sign of multicollinearity (  value greater than 5)
                # So we will eliminate it and again run our model
data <- subset(data, select = -c(Income_Composition_of_Resources)) # Elimination of "Income_Composition_of_Resources"
split <- sample.split(data$Life_Expectancy, SplitRatio = 0.70) # Again splitting the data into 70% (training) and 30% (test) ratio
training <- subset(data, split==T) # Training data - 2076 rows
test <- subset(data, split==F) # Test data - 862 rows
Model_Reg5=lm(Life_Expectancy~.,data=training) # Building linear regression model
summary(Model_Reg5) # According to the summary of model - r2 value is 0.6928 ( bad performance and scope of improvement) with no non significant variables
vif(Model_Reg5) # multicollinearity Check
                # No multocollinearity
lin_pred2 <- predict(Model_Reg5, newdata = test) # testing 
combind2 = cbind(test$Life_Expectancy, lin_pred2) # Combined data having both predicted and actual values
# Accuracy
APE2 = 100 *(abs(test$Life_Expectancy-lin_pred2)/test$Life_Expectancy) # Calculating the Absolute Percentage Error for each prediction
MeanAPE2 = mean(APE2) # Calculating MAPE
MedianAPE2 = median(APE2) # Calculating Median APE
Mean_A2 = 100 - MeanAPE2 # Calculating mean accuracy
Median_A2 = 100 - MedianAPE2 # Calculating median accuracy
#                                        SO ---------------  MAPE = 6.0739
#                                                            Median-APE = 4.2401
#                                                            Mean Accuracy - 93.92600
#                                                            Median Accuracy - 95.7958
# Accuracy scores suggest that model is performing bad after transformations
# So we will go with the old model

#.............................................................................. INSIGHTS .................................................................................................................................................

# 1. WHO ( World health organization ) can keep track of the life expectancy of people by observing factors like adult mortality rate, vaccines doses, issues of HIV & AIDS,
#    Income composition of resource (Human Development Index in terms of income composition of resources (index ranging from 0 to 1)) and schooling rather than observing 
#    factors like country, year, expenditure etc.

# 2. Life expectancy and adult mortality rates have high negative correlation which means reaching 15 yrs of age does not ensure a good life ahead if one does not take care of his/her healthy habits

# 3. Not suprisingly schooling years have high positive correlation with life expectancy because schooling leads to adoption of healthy habits and discipline

# 4. A country's GDP and income composition  affect life expectancy more broadly. So including these factors government should also focus on vaccination, Per capita GDP and  education to improve the country in terms of life expectancy

# 5. One can extend their life span by adopting a healthy lifestyle, having proper body is to mass ratio, proper education, and getting vaccinated on time for each disease

# 6. Average life expectancy of people over the years in different countries is 69 yrs which is good but can be better by adopting healthy habits

# 7. Some parameters like pollution and environmental index are missing in the data but can be included to get more insight into the factors affecting life expectancy
 
