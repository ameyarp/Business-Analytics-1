setwd("E:\\My Future and Career\\TAMU\\Crowd Analytics\\Business Analytics1")
startup<- read.csv(file="CAX_Startup_Data.csv", header=TRUE,as.is=T,stringsAsFactors=FALSE)

#Checking Dimensions and Understanding Data
dim(startup) # 472 X 116
head(startup)
names(startup)
fix(startup)

# replacing 'No Info' and 'blanks' with NA
startup[startup=="No Info"]<- NA
startup[startup==""]<- NA

# converting column as date
startup$Est..Founding.Date <- as.Date(startup$Est..Founding.Date, "%m/%d/%Y")
startup$Last.Funding.Date <- as.Date(startup$Last.Funding.Date, "%m/%d/%Y")

# R code for converting character vector to numeric
# display column header of data
colnames(startup) 

#####Create Additional Features Here
## Creating a feature- Inverstors.count
startup$Investor.count<-length(strsplit(startup$Investors, "|",fixed=T))
for (i in (1:length(startup$Investors)))
{
  if(is.na(startup$Investors[i])==T){
    startup$Investor.count[i]<- NA}
  else{
    lst<-strsplit(startup$Investors[i], "|", fixed=T)
    startup$Investor.count[i]<-length(lst[[1]])
  } }
dim(startup) # 472 X 117

## Creating a feature- Industry.of.company.count 
class(startup$Industry.of.company)
length(startup$Industry.of.company)
startup$Industry.of.company.count<-length(strsplit(startup$Industry.of.company, "|",fixed=T))
for (i in (1:length(startup$Industry.of.company)))
{
  if(is.na(startup$Industry.of.company[i])==T){
    startup$Industry.of.company.count[i]<- NA}
  else{
    lst<-strsplit(startup$Industry.of.company[i], "|", fixed=T)
    startup$Industry.of.company.count[i]<-length(lst[[1]])
  } }
dim(startup) # 472 X 118

# noting columns that needs to be converted to numeric
col<- c(3:5,10,11,18:23,25,61,66,68:70,72,74,88,92,94:96,98,99,102:118)

#allcol=c(1:116)
colMeans(is.na(startup))*100
colSums(is.na(startup))


apply(startup, 2, anyNA) ##### indicates whether a column has any NAs or not.

# using for loop to convert column as numeric
for(i in col)
  {
   startup[,i]<-as.numeric(startup[,i])
  }

str(startup)

########Creating a new dataframe named --df-- containing columns with less than 40% missing values.########
df <- startup[,colSums(is.na(startup))<=(nrow(startup)*0.4)] ### we see that 3 columns have been dropped.
dim(df)
#nums <- sapply(df, is.numeric)
#df_numeric_columns=df[,nums]
#dim(df_numeric_columns)
#chars=sapply(df, is.character)
#df_character_columns=df[,chars]
#dim(df_character_columns)
df_numeric_columns<-df[,c(1,2,3:5,10,12:14,17:22,24,60,65,67:69,71,73,85,89,91:93,95,96,99:115)]
df_character_columns=df[,-c(3:5,10,12:14,17:22,24,60,65,67:69,71,73,85,89,91:93,95,96,99:115)]
dim(df_numeric_columns)  # 472 X 47
dim(df_character_columns)# 472 X 70

##############Treating missing values in df #############
minus_one=-1
dim(df_character_columns) #472 * 70
df_character_columns = as.data.frame(sapply(df_character_columns, toupper))
df_character_columns <- data.frame(lapply(df_character_columns, trimws))
#df_character_columns$Short.Description.of.company.profile[is.na(df_character_columns$Short.Description.of.company.profile)]=minus_one
#fix(df_character_columns_success)

## Detecting outliers in numeric columns
col1<- c(3:6,9:47)
# using for loop for converting column as numeric
for(i in col1)
{
  df_numeric_columns[,i]<-as.numeric(df_numeric_columns[,i])
}
# computing quantiles
for(i in c(3:6,9:47)) #7:45
{
  #print(range(df_numeric_columns$i, na.rm = FALSE, finite=TRUE))
  print(quantile(df_numeric_columns[,i], probs = seq(0, 1, by= 0.025),na.rm=TRUE))
  print(colnames(df_numeric_columns)[i])
}
# checking histograms for outlier detection.
hist(df_numeric_columns$Skills.score, nclass = 100)
print(quantile(df_numeric_columns$Skills.score, probs = seq(0.9, 1, by= 0.01),na.rm=TRUE))
hist(df_numeric_columns$Number.of.Direct.competitors, nclass = 100)
print(quantile(df_numeric_columns$Number.of.Direct.competitors, probs = seq(0.9, 1, by= 0.01),na.rm=TRUE))
hist(df_numeric_columns$Employees.per.year.of.company.existence, nclass = 100)
print(quantile(df_numeric_columns$Employees.per.year.of.company.existence, probs = seq(0.9, 1, by= 0.01),na.rm=TRUE))
hist(df_numeric_columns$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment, nclass = 100)
print(quantile(df_numeric_columns$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment, probs = seq(0.9, 1, by= 0.01),na.rm=TRUE))
hist(df_numeric_columns$Percent_skill_Entrepreneurship, nclass = 100)
print(quantile(df_numeric_columns$Percent_skill_Entrepreneurship, probs = seq(0.975, 1, by= 0.0025),na.rm=TRUE))
hist(df_numeric_columns$Percent_skill_Data.Science, nclass = 100)
print(quantile(df_numeric_columns$Percent_skill_Data.Science, probs = seq(0.975, 1, by= 0.0025),na.rm=TRUE))

#Capping Outliers
df_numeric_columns$Skills.score[df_numeric_columns$Skills.score>53.45]<-53.45
df_numeric_columns$Employees.per.year.of.company.existence[df_numeric_columns$Employees.per.year.of.company.existence>156.57]<-156.57
df_numeric_columns$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment[df_numeric_columns$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment>70.54]<-70.54
df_numeric_columns$Percent_skill_Entrepreneurship[df_numeric_columns$Percent_skill_Entrepreneurship>33.22917]<-33.22917

#Replace NAs in df_numeric_columns by respective medians
for(i in c(3:6,9:47)) #7:45
{
  df_numeric_columns[,i][is.na(df_numeric_columns[,i])]=median(df_numeric_columns[,i], na.rm = TRUE)
}



# Seperating Character Dataframe based on Response Variable
df_character_columns_success=subset(df_character_columns,df_character_columns$Dependent.Company.Status=='SUCCESS',select = c(1:70),drop=TRUE)
df_character_columns_failed=subset(df_character_columns,df_character_columns$Dependent.Company.Status=='FAILED',select = c(1:70),drop=TRUE)
dim(df_character_columns_failed) # 167 * 70
dim(df_character_columns_success)# 305 * 70

Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}
#df_character_columns_success$Local.or.global.player[is.na(df_character_columns_success$Local.or.global.player)]<-Mode(df_character_columns_success$Local.or.global.player)

# Replacing Missing Values in columns of character df by mode based on response type.
z=c(5,7,8:45,47:67)

### Response: Success
for(i in c(5,7,8:45,47:67))
{
  df_character_columns_success[,i][is.na(df_character_columns_success[,i])]=Mode(df_character_columns_success[,i])
  
}
for(i in c(5,7,8:45,47:67))
{
  print(unique(is.na(df_character_columns_success[,i])))
  
}
### Response: failed; unique values show FALSE in columns.
for(i in c(5,7,8:45,47:67))
{
  df_character_columns_failed[,i][is.na(df_character_columns_failed[,i])]=Mode(subset(df_character_columns_failed[,i], df_character_columns_failed[,i] != "NA"))
  
}
for(i in c(5,7,8:45,47:67))
{
  print(unique(is.na(df_character_columns_failed[,i])))
  
}

#Mode(df_character_columns_success[,i])
#print(i)
#is.na(df_character_columns_success[,i])


#####df_character_columns_success$df_character_columns_success[,i][is.na(df_character_columns_success$df_character_columns_success[,i])]<-Mode(df_character_columns_success$df_character_columns_success[,i])

## Row-Binding df_character_columns_success and df_character_columns_filed for Chi-Sq test
df_character_columns_rbind=rbind(df_character_columns_success,df_character_columns_failed)

# perform t test to see which features are significant in df_numeric_columns





## Code Given by CrowdAnalytics is as follows:
# Percent missing value for each variable
mis_val<-sapply(startup, function(x) sum(is.na(x)))
percent_mis<-as.data.frame(round((mis_val/nrow(startup))*100,1))

name<-row.names(percent_mis)
pcnt_mis_var<-cbind(name,percent_mis)
row.names(pcnt_mis_var)<-NULL
colnames(pcnt_mis_var)<-c("variable","Percent.Missing")

# keeping only variables with less than 40% missing
new_var<-as.character(pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing<=40)])
new_startup<-startup[new_var]

# separate data frame for more than 40% missing
other_var<-as.character(pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing>40)])
other_data<-startup[other_var]


# writing new data as csv file
write.csv(new_startup,"filtered_data.csv",row.names=F)

# Separate data frame for numeric variables
cnt_df<-new_startup[,c(3:5,10,12:14,17:22,24,60,65,67:69,71,73,85,89,91:93,
                       95,96,99:113)]

# separate data frame for character variables
cnt_var<-colnames(cnt_df)
var <- colnames(new_startup) %in% cnt_var 
char_df <- new_startup[!var]

# checking distribution of continuous variable for outlier detection and missing values
summary(cnt_df$Team.size.all.employees)
quantile(cnt_df$Team.size.all.employees, probs = seq(0, 1, by= 0.05),na.rm=T)

# further exploration to determine cutoff for capping
quantile(cnt_df$Team.size.all.employees, probs = seq(.9, 1, by= 0.01),na.rm=T)

# capping values
cnt_df$Team.size.all.employees[cnt_df$Team.size.all.employees>103.8]<-103.8
                                 
# checking distribution of categorical variable for missing values
table(char_df$Local.or.global.player,useNA="always")

# convert a variable to uppercase
char_df$Local.or.global.player<-toupper(char_df$Local.or.global.player)

# trimming whitespaces
char_df$Local.or.global.player<-trimws(char_df$Local.or.global.player)

# Recoding variable levels
#char_df$Local.or.global.player[char_df$Local.or.global.player=='LOCAL']<-0
#char_df$Local.or.global.player[char_df$Local.or.global.player=='GLOBAL']<-1

char_df$Local.or.global.player<- as.factor(char_df$Local.or.global.player)

# Create additional features like counting number of investors for company
char_df$Investor.count<-length(strsplit(char_df$Investors, "|",fixed=T))
for (i in (1:length(char_df$Investors)))
{
  if(is.na(char_df$Investors[i])==T){
    char_df$Investor.count[i]<- NA}
  else{
    lst<-strsplit(char_df$Investors[i], "|", fixed=T)
    char_df$Investor.count[i]<-length(lst[[1]])
  } }

# adding dependent variable to numeric data frame
cnt_df$Dependent.Company.Status<-char_df$Dependent.Company.Status

# boxplot of employee count
boxplot(cnt_df$Employee.Count, main="box plot of employee count", 
        ylab="Employee count")

# histogram with black outline, white fill and median line
library(ggplot2)
ggplot(cnt_df, aes(x=Employee.Count))+
  geom_histogram(binwidth=5, colour="black", fill="white")+
  geom_vline(aes(xintercept=median(Employee.Count, na.rm=T)),   
             color="red", linetype="dashed", size=1)+
  ggtitle("Histogram of Employee count")+
  xlab("Employee Count") + 
  ylab("Frequency")+
  theme_light()

# box plot to see differnce in mean of team size w.r.t two categories of dependent
ggplot(cnt_df, aes(x=Dependent.Company.Status,y=Team.size.all.employees, 
                   fill=Dependent.Company.Status)) + 
  geom_boxplot()

# data preparation for bar chart
avg_emp<-aggregate(as.numeric(cnt_df$Team.size.all.employees), 
                   by=list(as.factor(cnt_df$Dependent.Company.Status)), 
                           FUN=mean, na.rm=TRUE)
colnames(avg_emp)<-c("company.status","Avg.Employee.size")

# bar chart to check for difference in mean
ggplot(avg_emp, aes(x = company.status, y = Avg.Employee.size)) +
  geom_bar(stat = "identity")

# Missing value treatemnt using median
cnt_df$Team.size.all.employees[is.na(cnt_df$Team.size.all.employees)]<-median(cnt_df$Team.size.all.employees,na.rm=T)

# function to calculate mode
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
  }
# Filling missing values with mode
char_df$Local.or.global.player[is.na(char_df$Local.or.global.player)]<-Mode(char_df$Local.or.global.player)

# t-test for checking difference in mean
t.test(Team.size.all.employees~Dependent.Company.Status, data=cnt_df)

# tabulating data for chi-sq test
tab<- table(char_df$Dependent.Company.Status,char_df$Local.or.global.player)

# chi-sq test
chisq.test(tab)

