#NG LUM THYN
#TP061914

#Each code block will be sectioned according to 
#the assignment headings along with corresponding
#page number in the word document.

#====================================================================
#2.1 Data Import (page 4)

#Get the current working directory
getwd()

#Set new working directory
setwd("D:/ONEDRIVE/OneDrive - Asia Pacific University/APU/CT127-3-2-PFDA/Assignment")

#Importing the data
emp_data <- read.csv("D:/ONEDRIVE/OneDrive - Asia Pacific University/APU/CT127-3-2-PFDA/Assignment/employee_attrition.csv")

#Install all necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("crayon")
install.packages("janitor")
install.packages("ggpubr")
install.packages("ggrepel")
install.packages("ggridges")
library(ggplot2)
library(dplyr)
library(crayon)
library(janitor)
library(ggpubr)
library(ggrepel)
library(ggridges)

#View the dataset
View(emp_data)


#====================================================================
#2.2 Data Exploration (page 5)


#Get number of rows/columns
ncol(emp_data)
nrow(emp_data)

#Get column names
names(emp_data)

#Get class information
str(emp_data, 5)

#Get summary of dataset
summary(emp_data)


#====================================================================
#2.3 Data Cleaning (page 6-7)


#standardizing column names
names(emp_data)
cleaned_emp_data <- clean_names(emp_data)
colnames(cleaned_emp_data)
rm(emp_data)

#removing unnecessary columns
#recorddate_key removed
cleaned_emp_data$recorddate_key <- NULL
#gender_full removed
cleaned_emp_data$gender_full <- NULL

#renaming column name
cleaned_emp_data = rename(cleaned_emp_data, gender = gender_short)
cleaned_emp_data = rename(cleaned_emp_data, store_code = store_name)


#====================================================================
#2.4 Data Pre-Processing (page 8-10)
#------------------------------------------------- (page 8)

#checking for missing values
sum(is.na(cleaned_emp_data))

#viewing the structure of the data frame
str(cleaned_emp_data)

#removing empty rows/columns (if any)
cleaned_emp_data %>% remove_empty(whic=c("rows"))
cleaned_emp_data %>% remove_empty(whic=c("cols"))

#converting character to date
cleaned_emp_data <- cleaned_emp_data %>% 
  mutate_at(vars(birthdate_key, orighiredate_key, terminationdate_key), as.Date, format="%m/%d/%Y")

#------------------------------------------------- (page 9)

#access the rows of terminationdate_key
#first 100 rows
head(cleaned_emp_data$terminationdate_key, 100)

#arbitrarily decided range of rows
cleaned_emp_data$terminationdate_key[3000:3500]

#last 100 rows
tail(cleaned_emp_data$terminationdate_key, 100)

#check total occurrences in terminationdate_key
nrow(cleaned_emp_data[cleaned_emp_data$terminationdate_key == "1900-01-01",]) 

#------------------------------------------------- (page 10)

#creating a pie chart
result = c(42450, 49653-42250)
label <- round((result/sum(result)) * 100, 1)
label <- paste(label, "%", sep="")
pie(result, main="Percentage of data in terminatedate_key column", col= c("red", "green"), labels=label, cex=0.8)
legend(-1.2, 1.0, c("Faulty data", "Good data"), fill= c("red", "green"),)

#set value to NA
#"1900-01-01" to NA
cleaned_emp_data = cleaned_emp_data %>% 
  mutate(across(where(is.double), ~na_if(., "1900-01-01")))

#"Not Applicable" to NA
cleaned_emp_data = cleaned_emp_data %>% 
  mutate(across(where(is.character), ~na_if(., "Not Applicable")))

#checking if values have been converted
head(cleaned_emp_data, 100)


#============================================================Q1 1.1========
#3 Analysis 
#3.1 Question 1
#3.1.1 Analysis 1-1 (page 11-14)
#------------------------------------------------- (page 11)

#create a data frame grouped by status_year
emp_total <- as.data.frame.matrix(cleaned_emp_data %>%
                                          group_by(status_year) %>%
                                          select(gender) %>% 
                                          table())

#adding the new column TOTAL_EMPLOYEES
emp_total <- cbind(emp_total, 
                   TOTAL_EMPLOYEES = emp_demographic$M + emp_demographic$F)

View(emp_total)


#------------------------------------------------- (page 12)

#line graph of employee numbers
#retrieve status years
STATUS_YEAR <- as.vector(as.numeric(levels(factor(cleaned_emp_data$status_year))))

#plotting graph
plot(STATUS_YEAR, emp_total$TOTAL_EMPLOYEES, ylim = c(2000, 5400), type = "o", 
     xlab = "STATUS_YEAR", ylab = "COUNT", 
     main = "Line graph of employee numbers", col = "black")
lines(STATUS_YEAR, emp_total$M, type = "o", col = "blue")
lines(STATUS_YEAR, emp_total$F, type = "o", col = "red")
legend("center", 
       legend = c("Total number of employees", "Total number of male employees", "Total number of female employees"), 
       col = c("black", "blue", "red"), lty = 1)

#remove variable
rm(emp_total) 

#------------------------------------------------- (page 14)

#plotting bar chart
ggplot(cleaned_emp_data, aes(x = as.factor(status_year))) + 
  geom_bar(aes(fill = gender), position = position_dodge())+
  geom_text(stat = "count", aes(label = ..count.., colour = gender ),
            position=position_dodge(width = 0.9), vjust = -1) +
  ggtitle("Number of male and female employees by gender") +
  labs(x = "STATUS_YEAR", y = "EMPLOYEE_COUNT") 


#============================================================Q1 1.2========
#3 Analysis 
#3.1 Question 1
#3.1.2 Analysis 1-2 (page 15-17)
#------------------------------------------------- (page 15)

#determine number and factor of departments
nlevels(factor(cleaned_emp_data$department_name))
levels(factor(cleaned_emp_data$department_name))

#bar graph
ggplot(cleaned_emp_data, aes(y = as.factor(department_name))) + 
  geom_bar(aes(fill = gender), stat = "count", position = "dodge") +
  labs(title = "Distribution of male and female employees based on department from 2006 to 2015",
       x = "Frequency", y = "Department", fill = "Gender") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0, hjust = .5) +
  facet_wrap(~gender)

#------------------------------------------------- (page 17)

#generate data frame to store employee data based on status year and department
emp_total_dept <- as.data.frame.matrix(cleaned_emp_data %>%
                                         group_by(department_name) %>%
                                         select(status_year) %>% 
                                         table())


#============================================================Q1 1.3========
#3 Analysis 
#3.1 Question 1
#3.1.3 Analysis 1-3 (page 18-20)
#------------------------------------------------- (page 18)

#overall median age
#empty vector
all_median_age <- vector()

#for loop to store retrieved median age into vector
for (i in STATUS_YEAR){
  all_median_age <- append(all_median_age, 
                           median(subset(cleaned_emp_data, status_year == i)$age))
}

#------------------------------------------------- (page 19)

#'*TO WHOM IT MAY CONCERN:*
#'*The following chunk of code will require some time to run,*
#'*So please give it a minute to finish compiling before proceeding*
#'*To run any of the other code*

#median age based on gender
#creating empty vectors
m_age <- vector()
f_age <- vector()
m_median_age <- vector()
f_median_age <- vector()

#set initial value
j = 2006
#while loop to store age based on gender into vectors
while( j <= 2015){
  for(k in 1:nrow(subset(cleaned_emp_data, status_year == j))){
    if (subset(cleaned_emp_data, status_year == j)$gender[k] == "M"){
      m_age <- append(m_age, subset(cleaned_emp_data, status_year == j)$age[k])
    } else {
      f_age <- append(f_age, subset(cleaned_emp_data, status_year == j)$age[k])
    }
  }
  #appending median age to a new vector
  m_median_age <- append(m_median_age, median(m_age))
  f_median_age <- append(f_median_age, median(f_age))
  
  #reset the vectors
  m_age <- vector()
  f_age <- vector()
  
  #increment
  j = j + 1
}

#remove variable
rm(m_age, f_age, i, j, k)

#------------------------------------------------- (page 19)

#creating data frame for median age
emp_medianage <- array(c(m_median_age, f_median_age, all_median_age), c(10,3,1))
emp_medianage <- as.data.frame(emp_medianage)
rownames(emp_medianage) <- STATUS_YEAR
colnames(emp_medianage) <- c("MALE_MEDIAN_AGE", "FEMALE_MEDIAN_AGE", "OVERALL_MEDIAN_AGE")

View(emp_medianage)

#------------------------------------------------- (page 19)

#adding temporary column to generate line graph
emp_medianage <- cbind(emp_medianage, YEAR = as.character(STATUS_YEAR))

#plotting line graph
colors <- c("Overall median age" = "black", "Male median age" = "blue", "Female median age" = "red")
ggplot(emp_medianage, aes(x = YEAR, group = 1)) + 
  geom_line(aes(y = OVERALL_MEDIAN_AGE, color = "Overall median age")) + 
  geom_line(aes(y = MALE_MEDIAN_AGE, color = "Male median age")) + 
  geom_line(aes(y = FEMALE_MEDIAN_AGE, color="Female median age")) + 
  labs(x = "YEAR", y = "MEDIAN_AGE", color = "Legend") +
  scale_color_manual(values = colors)


#removing temporary column
emp_medianage$YEAR <- NULL

#remove variables
rm(m_age, f_age, i, j, k, colors, emp_medianage)


#============================================================Q1 1.4========
#3 Analysis 
#3.1 Question 1
#3.1.4 Analysis 1-4 (page 21-27)
#------------------------------------------------- (page 21)

#creating a data frame to store frequency data
age_dist_all <- transform(table(cut(cleaned_emp_data$age, seq(15.5,65.5, by = 5))))
age_dist_all <- transform(age_dist_all, rf = round(prop.table(Freq) * 100, digits = 2))
colnames(age_dist_all) <- c("AGE_GROUP", "FREQUENCY", "REL_FREQ")

#pie chart of employee distribution based on age
ggpie(
  age_dist_all, x = "REL_FREQ", label = "REL_FREQ",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "AGE_GROUP", color = "white",
  palette = "jco"
) +
  ggtitle("Pie chart for distribution of employees based on age group") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Age Groups")


#remove variables
rm(age_dist_all)

#------------------------------------------------- (page 23)

#creating empty array to store frequency data
col_names <- c("AGE_GROUP", "FREQ", "REL_FREQ(%)", "CUM_FREQ" )
row_names <- c(1:10)
emp_age_dist = array(dim = c(10, 4, 10), dimnames = list(row_names, col_names, STATUS_YEAR)) 

#(a, b] means from a to b, not including a, but include b
age_grp = c("(15.5,20.5]", "(20.5,25.5]", "(25.5,30.5]", "(30.5,35.5]", "(35.5,40.5]", 
            "(40.5,45.5]", "(45.5,50.5]", "(50.5,55.5]", "(55.5,60.5]", "(60.5,65.5]")

#populate first column to every dimension
emp_age_dist[, 1,] <- age_grp


#repeat loop to store frequency data
i = 2006
j = 1
repeat
{
  data <- filter(cleaned_emp_data, status_year == i)
  age_dist <- transform(table(cut(data$age, seq(15.5,65.5, by = 5))))
  age_dist <- transform(age_dist, rf = prop.table(Freq), cf = cumsum(Freq))
  emp_age_dist[, 2, j] <- age_dist$Freq
  emp_age_dist[, 3, j] <- round(age_dist$rf * 100, digits = 1)
  emp_age_dist[, 4, j] <- age_dist$cf
  
  #resetting the age_dist variable
  rm(age_dist)
  i = i + 1
  j = j + 1
  if (i > 2015){
    break
  }
}

#view entire array
print(emp_age_dist, quote = FALSE, right = TRUE)

#------------------------------------------------- (page 24)

#function to display selected array
display<-function()
{
  input = readline("Select year to view data (2006~2015): ")
  output = switch(input, 
                  "2006" = 1, 
                  "2007" = 2,
                  "2008" = 3,
                  "2009" = 4,
                  "2010" = 5,
                  "2011" = 6,
                  "2012" = 7,
                  "2013" = 8,
                  "2014" = 9,
                  "2015" = 10)
  message(green("Showing "), 
          magenta$italic$underline("Employee Frequency Data for YEAR "), 
          red$bgYellow$bold(as.character(input)))
  print(emp_age_dist[,,output], quote = FALSE, right = TRUE)
}
display()

#------------------------------------------------- (page 25)

#histogram for distribution of employees grouped by age & status year
ggplot(cleaned_emp_data, aes(x = age)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue", breaks = seq(15, 70, by = 5)) + 
  labs(title = "Distribution of employees based on age group from 2006 to 2015",
       x = "Age", y = "Frequency") + ylim(c(0, 800)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), breaks = seq(15, 70, by = 5), vjust = -1) +
  facet_wrap(~status_year)

#------------------------------------------------- (page 27)

#same histogram, with added categorization
ggplot(cleaned_emp_data, aes(x = age, fill = as.factor(gender))) + 
  geom_histogram(binwidth = 1, color = "white", breaks = seq(15, 70, by = 5)) + 
  labs(title = "Distribution of male and female employees based on age group from 2006 to 2015",
       x = "Age", y = "Frequency", fill = "Gender") + ylim(c(0, 700)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), breaks = seq(15, 70, by = 5), vjust = -1) + 
  facet_wrap(~status_year)


#============================================================Q2 2.1========
#3 Analysis 
#3.2 Question 2
#3.2.1 Analysis 2-1 (page 28-30)
#------------------------------------------------- (page 28)

#Create a data frame that stores unemployment rate
unemprate_count <- as.data.frame.matrix(cleaned_emp_data %>%
                                         group_by(status_year) %>%
                                         select(status) %>%
                                         table())

#calculate total employees, add data as new column
unemprate_count <- cbind(unemprate_count, 
                         TOTAL_EMPLOYEES = unemprate_count$ACTIVE + unemprate_count$TERMINATED)
#calculate unemployment rate, add data as new column
unemprate_count <- cbind(unemprate_count, UNEMPLOYMENT_RATE = 
                           round((unemprate_count$TERMINATED/unemprate_count$TOTAL_EMPLOYEES)*100, 
                                 digits = 2))

#------------------------------------------------- (page 29)

#checking mean unemployment rate
mean(unemprate_count$UNEMPLOYMENT_RATE)


#dot and line graph for unemployment rate
ggplot(unemprate_count, aes(x = as.factor(STATUS_YEAR), y = UNEMPLOYMENT_RATE))+
  geom_point(color = dplyr::case_when(unemprate_count$UNEMPLOYMENT_RATE > 3 ~ "red",
                                      TRUE ~ "black"), size = 5) + 
  geom_line(group = 1) +
  geom_hline(yintercept = mean(unemprate_count$UNEMPLOYMENT_RATE), color="blue") +
  geom_label_repel(aes(label = UNEMPLOYMENT_RATE), 
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Unemployment rate from 2006 to 2015", x = "Year", y = "Unemployment Rate")


#============================================================Q2 2.2========
#3 Analysis 
#3.2 Question 2
#3.2.2 Analysis 2-2 (page 31-32)
#------------------------------------------------- (page 31)

#filter only terminated employees
term_emp <- subset(cleaned_emp_data, status == "TERMINATED")

#Creating temporary data frame to store employee number grouped by status year and gender
gender_dist_all <- as.data.frame.matrix(table(cleaned_emp_data$status_year, cleaned_emp_data$gender))
gender_dist_term <- as.data.frame.matrix(table(term_emp$status_year, term_emp$gender))

#create empty data frame
gender_unemp_rate = data.frame(status_year = c(STATUS_YEAR))

#adding columns of male and female unemployment rate
gender_unemp_rate$male_unemp_rate <- round((gender_dist_term$M / gender_dist_all$M) * 100, digits = 2) 
gender_unemp_rate$female_unemp_rate <- round((gender_dist_term$F / gender_dist_all$F) * 100, digits = 2) 


#generating Cleveland dot plot
ggplot(gender_unemp_rate, aes(y = as.factor(status_year))) +
  geom_segment(aes(x = male_unemp_rate, xend = female_unemp_rate, yend = as.factor(status_year))) +
  geom_point(aes(x = male_unemp_rate, color = "Male"), size = 3) +
  geom_point(aes(x = female_unemp_rate, color = "Female"), size = 3) + 
  geom_text(aes(x = male_unemp_rate, label = male_unemp_rate, color = "Male"), vjust = -1) +
  geom_text(aes(x = female_unemp_rate, label = female_unemp_rate, color = "Female"), vjust = -1) +
  labs(title = "Unemployment rate based on gender", 
       y = "Status Year", x = "Unemployment Rate", color = "Gender") 


#============================================================Q2 2.3========
#3 Analysis 
#3.2 Question 2
#3.2.3 Analysis 2-3 (page 33-36)
#------------------------------------------------- (page 33)

#creating data frame to store frequency data
#by age group
age_dist_all <- transform(table(cut(cleaned_emp_data$age, seq(15.5,65.5, by = 5))))
age_dist_term <- transform(table(cut(term_emp$age, seq(15.5,65.5, by = 5))))
#by length of service
LOS_dist_all <- transform(table(cut(cleaned_emp_data$length_of_service, seq(-0.5, 30.5, by = 5))))
LOS_dist_term <- transform(table(cut(term_emp$length_of_service, seq(-0.5, 30.5, by = 5))))
#by department
dept_dist_all <- as.data.frame(table(cleaned_emp_data$department_name)) 
dept_dist_term <- as.data.frame(table(term_emp$department_name)) 
#by business unit
bu_dist_all <- as.data.frame(table(cleaned_emp_data$business_unit)) 
bu_dist_term <- as.data.frame(table(term_emp$business_unit)) 


#assigning new columns to data frame
#by age group
age_dist_term$total_freq <- age_dist_all$Freq
age_dist_term$unemp_rate <- round((age_dist_term$Freq / age_dist_term$total_freq) * 100, digits = 2)
#by length of service
LOS_dist_term$total_freq <- LOS_dist_all$Freq
LOS_dist_term$unemp_rate <- round((LOS_dist_term$Freq / LOS_dist_term$total_freq) * 100, digits = 2)
#by department
dept_dist_all <- dept_dist_all[ !(dept_dist_all$Var1 %in% c("Executive")), ] #remove the row "Executive"
dept_dist_term$total_freq <- dept_dist_all$Freq
dept_dist_term$unemp_rate <- round((dept_dist_term$Freq / dept_dist_term$total_freq) * 100, digits = 2)
#by business unit
bu_dist_term$total_freq <- bu_dist_all$Freq
bu_dist_term$unemp_rate <- round((bu_dist_term$Freq / bu_dist_term$total_freq) * 100, digits = 2)


#calculate mean unemployment rate
mean(age_dist_term$unemp_rate)
mean(LOS_dist_term$unemp_rate)
mean(dept_dist_term$unemp_rate)
mean(bu_dist_term$unemp_rate)

#------------------------------------------------- (page 34)

#point and line graph of unemployment rate based on age group
a <- ggplot(age_dist_term, aes(x = as.factor(Var1), y = unemp_rate))+
  geom_point(color = dplyr::case_when(age_dist_term$unemp_rate > 3.6 ~ "red",
                                      TRUE ~ "black"), size = 5) + 
  geom_line(group = 1) +
  geom_hline(yintercept = mean(age_dist_term$unemp_rate), color = "blue") +
  geom_label_repel(aes(label = unemp_rate), 
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Unemployment rate based on age group from 2006 to 2015", x = "Age Group", y = "Unemployment Rate")

#point and line graph of unemployment rate based on length of service
b <- ggplot(LOS_dist_term, aes(x = as.factor(Var1), y = unemp_rate))+
  geom_point(color = dplyr::case_when(LOS_dist_term$unemp_rate > 9.8 ~ "red",
                                      TRUE ~ "black"), size = 5) + 
  geom_line(group = 1) +
  geom_hline(yintercept = mean(LOS_dist_term$unemp_rate), color = "blue") +
  geom_label_repel(aes(label = unemp_rate), 
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Unemployment rate based on Length of service from 2006 to 2015", x = "Length of Service", y = "Unemployment Rate")

#point and line graph of unemployment rate based on department
c <- ggplot(dept_dist_term, aes(x = as.factor(Var1), y = unemp_rate))+
  geom_point(color = dplyr::case_when(dept_dist_term$unemp_rate > 11.4 ~ "red",
                                      TRUE ~ "black"), size = 5) + 
  geom_line(group = 1) +
  geom_hline(yintercept = mean(dept_dist_term$unemp_rate), color = "blue") +
  geom_label_repel(aes(label = unemp_rate), 
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Unemployment rate based on department from 2006 to 2015", x = "Department", y = "Unemployment Rate")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#point and line graph of unemployment rate based on business unit
d <- ggplot(bu_dist_term, aes(x = as.factor(Var1), y = unemp_rate))+
  geom_point(color = dplyr::case_when(bu_dist_term$unemp_rate > 7.3 ~ "red",
                                      TRUE ~ "black"), size = 5) + 
  geom_line(group = 1) +
  geom_hline(yintercept = mean(bu_dist_term$unemp_rate), color = "blue") +
  geom_label_repel(aes(label = unemp_rate), 
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  labs(title = "Unemployment rate based on business unit from 2006 to 2015", x = "Business Unit", y = "Unemployment Rate")

#combine the graphs
ggarrange(a, b, c, d, ncol = 2, nrow = 2)


#removing variables
rm(age_dist_all, LOS_dist_all, dept_dist_all, bu_dist_all,
   age_dist_term, LOS_dist_term, dept_dist_term, bu_dist_term,
   a, b, c, d)

#============================================================Q3 3.1========
#3 Analysis 
#3.3 Question 3
#3.3.1 Analysis 3-1 (page 37-39)
#------------------------------------------------- (page 37)

#filter only terminated employees
term_emp <- subset(cleaned_emp_data, status == "TERMINATED")

#create data frame storing frequency based on gender
term_emp_by_dept <- as.data.frame.matrix(term_emp %>% 
                                           group_by(department_name) %>% 
                                           select(gender) %>% 
                                           table())

#------------------------------------------------- (page 38)

#bar chart to show distribution of terminated employees by department and gender
ggplot(term_emp, aes(y = as.factor(department_name), fill = as.factor(gender))) + 
  geom_bar(stat = "count", color = "black") +
  labs(title = "Distribution of terminated employees based on department from 2006 to 2015",
       x = "Frequency", y = "Department", fill = "Gender") + xlim(0,450) +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0, hjust = -1) + 
  facet_wrap(~gender)


#============================================================Q3 3.1.1========
#3 Analysis 
#3.3 Question 3
#3.3.1.1 Analysis 3-1-1 (page 40-41)
#------------------------------------------------- (page 40)

#filtering the data based on the 6 departments
term_emp_produce <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & department_name == "Produce")
term_emp_profoods <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & department_name == "Processed Foods")
term_emp_meats <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & department_name == "Meats")
term_emp_dairy <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & department_name == "Dairy")
term_emp_custserv <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & department_name == "Customer Service")
term_emp_bakery <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & department_name == "Bakery")

#creating individual density graphs with different mean age lines
produce <- ggplot(term_emp_produce, aes(x = age)) + geom_density(aes(fill = gender), alpha = 0.6) + 
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 2, color = "red")
profoods <- ggplot(term_emp_profoods, aes(x = age)) + geom_density(aes(fill = gender), alpha = 0.6) + 
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 2, color = "red")
meats <- ggplot(term_emp_meats, aes(x = age)) + geom_density(aes(fill = gender), alpha = 0.6) + 
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 2, color = "red")
dairy <- ggplot(term_emp_dairy, aes(x = age)) + geom_density(aes(fill = gender), alpha = 0.6) + 
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 2, color = "red")
custserv <- ggplot(term_emp_custserv, aes(x = age)) + geom_density(aes(fill = gender), alpha = 0.6) + 
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 2, color = "red")
bakery <- ggplot(term_emp_bakery, aes(x = age)) + geom_density(aes(fill = gender), alpha = 0.6) + 
  geom_vline(aes(xintercept = mean(age)), linetype = "dashed", size = 2, color = "red")


#combining 6 graphs
combined_graph <- ggarrange(produce, profoods, meats,dairy, custserv, bakery,
                            common.legend = TRUE, legend = "bottom",
                            labels = c("Produce Department", "Processed Foods Department", "Meats Department",
                                       "Dairy Department", "Customer Service Department", "Bakery Department"),
                            ncol = 3, nrow = 2)


#adding title
annotate_figure(combined_graph, top = text_grob("Density distribution of age across 6 departments", 
                                                color = "red", face = "bold", size = 14))

#remove variable
rm(term_emp_produce, term_emp_profoods, term_emp_meats, term_emp_dairy, term_emp_custserv, term_emp_bakery, 
   produce, profoods, meats, dairy, custserv, bakery, combined_graph)

#============================================================Q3 3.2========
#3 Analysis 
#3.3 Question 3
#3.3.2 Analysis 3-2 (page 42-43)
#------------------------------------------------- (page 42) 

#create data frame storing frequency based on business unit
term_emp_by_dept <- as.data.frame.matrix(term_emp %>% 
                                           group_by(department_name) %>% 
                                           select(business_unit) %>% 
                                           table())
#adding total occurrences
term_emp_by_dept <- rbind(term_emp_by_dept, 
                          TOTAL = c(sum(term_emp_by_dept$HEADOFFICE), sum(term_emp_by_dept$STORES)))

#remove variables
rm(term_emp_by_dept)

#------------------------------------------------- (page 43)

#added categorization by business unit
ggplot(term_emp, aes(y = as.factor(department_name), fill = as.factor(business_unit))) + 
  geom_bar(stat = "count", color = "black") +
  labs(title = "Distribution of terminated employees based on department from 2006 to 2015",
       x = "Frequency", y = "Department", fill = "Business Unit") + xlim(0,450) +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0, hjust = -1) + 
  facet_wrap(~status_year)


#============================================================Q3 3.3========
#3 Analysis 
#3.3 Question 3
#3.3.3 Analysis 3-3 (page 44-46)
#------------------------------------------------- (page 44)

#boxplot 
ggplot(term_emp, aes(x = as.factor(status_year), y = age)) +
  geom_boxplot()+
  geom_jitter(aes(color = gender, shape = gender), size = 3) +
  labs(title = "Boxplot for age of terminated employees from 2006 to 2015",
       y = "Age", x = "Status Year")

#------------------------------------------------- (page 45)

#bar graph to compare between years
a <- ggplot(term_emp, aes(x = age, fill = as.factor(gender))) + 
  geom_histogram(binwidth = 1, color = "white", breaks = seq(15, 65, by = 5), position = "dodge") + 
  labs(title = "Distribution of terminated employees based on age group from 2006 to 2015",
       x = "Age", y = "Frequency", fill = "Gender") + ylim(c(0,130)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), breaks = seq(15, 65, by = 5), position = "dodge", vjust = -1) +
  geom_vline(xintercept = seq(from=15, to=65, by = 5), linetype = "dashed") +
  facet_grid(rows = vars(status_year))

#bar graph 
b <- ggplot(term_emp, aes(x = age, fill = as.factor(gender))) + 
  geom_histogram(binwidth = 1, color = "white", breaks = seq(15, 65, by = 5), position = "dodge") + 
  labs(title = "Total distribution of terminated employees based on age group",
       x = "Age", y = "Frequency", fill = "Gender") +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), breaks = seq(15, 65, by = 5), position = "dodge", vjust = -1) +
  geom_vline(xintercept = seq(from=15, to=65, by = 5), linetype = "dashed")

#combine the two graphs
ggarrange(b, a, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#remove variables
rm(a, b)


#============================================================Q3 3.4========
#3 Analysis 
#3.3 Question 3
#3.3.4 Analysis 3-4 (page 47)


#density plot
ggplot(term_emp, aes(x = length_of_service)) + 
  geom_density(aes(fill = gender), alpha = 0.6) +
  labs(title = "Density plot for length of service of terminated employees from 2006 to 2015",
       y = "Density", x = "Length of Service (Years)", fill = "Gender")
 

#============================================================Q4 4.1========
#3 Analysis 
#3.4 Question 4
#3.4.1 Analysis 4-1 (page 48-50)
#------------------------------------------------- (page 48)

#checking number and factor of termination types
levels(factor(term_emp$termtype_desc))

#creating data frame for employee data grouped by gender and termination type
term_type <- as.data.frame.matrix(term_emp %>% 
                                    group_by(termtype_desc) %>%
                                    select(gender) %>% 
                                    table())
#adding new column
term_type$total <- term_type$F + term_type$M

#------------------------------------------------- (page 49)

#data frames to store percentage data
vol_df <- data.frame(gender = c("Male", "Female"))
invol_df <- data.frame(gender = c("Male", "Female"))
m_data <- matrix(round((term_type$M / term_type$total * 100), digits = 2))
f_data <- matrix(round((term_type$F / term_type$total * 100), digits = 2))
data <- cbind(m_data, f_data)

#appending data from matrix
vol_df$value <- data[1,]
invol_df$value <- data[2,]


#voluntarily terminated
labs <- paste0(vol_df$gender, " (", vol_df$value, "%)")
a <- ggpie(vol_df, "value", label = labs,
           lab.pos = "in", lab.font = "white",
           fill = "gender", color = "white")+
  labs(fill = "Gender", title = "Voluntarily Terminated") +
  theme(plot.title = element_text(hjust = 0.5)) 

#involuntarily terminated
labs <- paste0(invol_df$gender, " (", invol_df$value, "%)")
b <- ggpie(invol_df, "value", label = labs,
           lab.pos = "in", lab.font = "white",
           fill = "gender", color = "white") +
  labs(fill = "Gender", title = "Involuntarily Terminated") +
  theme(plot.title = element_text(hjust = 0.5)) 


#combine pie charts
combined_graph <- ggarrange(a, b, common.legend = TRUE, legend = "bottom", ncol = 2)

#Add title
annotate_figure(combined_graph, 
                top = text_grob("Gender distribution of terminated employees from 2006 to 2015", 
                                color = "red", face = "bold", size = 14))

#remove variable
rm(vol_df, invol_df, m_data, f_data, data, a, b, combined_graph)


#============================================================Q4 4.2========
#3 Analysis 
#3.4 Question 4
#3.4.2 Analysis 4-2 (page 51-52)
#------------------------------------------------- (page 51)

#bar graph to compare between genders
a <- ggplot(term_emp, aes(x = as.factor(status_year), fill = as.factor(termtype_desc))) + 
  geom_bar(stat = "count", color = "black", position = "stack") +
  labs(title = "Distribution of terminated employees grouped by termination type from 2006 to 2015",
       y = "Frequency", x = "Status Year", fill = "Termination Type") + ylim(c(0, 180)) +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(), vjust = -0.5) +
  facet_wrap(~gender, ncol = 1)

#bar graph for total distribution
b <- ggplot(term_emp, aes(x = as.factor(status_year), fill = as.factor(termtype_desc))) + 
  geom_bar(stat = "count", color = "black", position = "stack") +
  labs(title = "Total distribution of terminated employees grouped by termination type from 2006 to 2015",
       y = "Frequency", x = "Status Year", fill = "Termination Type") + ylim(c(0, 300)) +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(), vjust = -0.5)

#combine the graphs
ggarrange(b, a, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom")

#remove variable
rm(a, b)


#============================================================Q4 4.3========
#3 Analysis 
#3.4 Question 4
#3.4.3 Analysis 4-3 (page 53-54)
#------------------------------------------------- (page 53)

#violin and box plot 
ggplot(term_emp, aes(y = age, x = as.factor(termtype_desc), color = as.factor(termtype_desc))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  labs(title = "Age distribution of terminated employees grouped by termination type and gender from 2006 to 2015",
       x = "Termination Type", y = "Age") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~gender, nrow = 1)


#============================================================Q4 4.4========
#3 Analysis 
#3.4 Question 4
#3.4.4 Analysis 4-4 (page 55-56)
#------------------------------------------------- (page 55)

#density ridge line plot 
ggplot(term_emp, aes(x = length_of_service, y = as.factor(termtype_desc))) +
  geom_density_ridges(aes(fill = gender), alpha = 0.6) +
  labs(title = "Length of service of terminated employees grouped by termination type and gender from 2006 to 2015",
       y = "Termination Type", x = "Length of Service", fill = "Gender")


#============================================================Q4 4.5========
#3 Analysis 
#3.4 Question 4
#3.4.5 Analysis 4-5 (page 57-58)
#------------------------------------------------- (page 57)

#data frame to store data based on business unit and department names
term_bu <- as.data.frame.matrix(term_emp %>% 
                                  group_by(department_name) %>%
                                  select(business_unit) %>% 
                                  table())

#remove variables
rm(term_bu)

#------------------------------------------------- (page 58)

#bar graph for distribution by business unit and department
ggplot(term_emp, aes(y = as.factor(department_name), fill = as.factor(business_unit))) + 
  geom_bar(stat = "count", color = "white", position = "stack") +
  labs(title = "Distribution of terminated employees grouped by 
       termination type, department, and business unit from 2006 to 2015",
       y = "Department", x = "Frequency", fill = "Termination Type") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~termtype_desc, nrow = 1)


#============================================================Q4 4.6========
#3 Analysis 
#3.4 Question 4
#3.4.6 Analysis 4-6 (page 59-60)
#------------------------------------------------- (page 59)

#sampling the data set
sample_frac(term_emp, .5) %>% filter(termreason_desc == "Resignation")

#determining number and factor of termreason_desc
levels(factor(term_emp$termreason_desc))


#changing spelling error "Resignaton" to "Resignation"
term_emp <- term_emp %>% 
  mutate(termreason_desc = ifelse(as.character(termreason_desc) == "Resignaton", "Resignation", 
                                  as.character(termreason_desc)))

#bar graph for distribution by status year
ggplot(term_emp, aes(y = as.factor(status_year), fill = as.factor(termreason_desc))) + 
  geom_bar(stat = "count", color = "white", position = "stack") +
  labs(title = "Distribution of terminated employees grouped by 
       termination type and termination reason from 2006 to 2015",
       y = "Status Year", x = "Frequency", fill = "Termination Reason") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~termtype_desc, nrow = 1)


#============================================================Q4 4.7========
#3 Analysis 
#3.4 Question 4
#3.4.7 Analysis 4-7 (page 61-63)
#------------------------------------------------- (page 61)

#data frame to store data based on term reason and department names
term_reason <- as.data.frame.matrix(term_emp %>% 
                                    group_by(department_name) %>%
                                    select(termreason_desc) %>% 
                                    table())

#------------------------------------------------- (page 62)

#bar graph for distribution by department
ggplot(term_emp, aes(y = as.factor(department_name), fill = as.factor(termreason_desc))) + 
  geom_bar(stat = "count", color = "white", position = "stack") +
  labs(title = "Distribution of terminated employees grouped by 
       termination type and department from 2006 to 2015",
       y = "Department", x = "Frequency", fill = "Termination Type") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------- (page 63)

#arrange department by highest frequency of each termination type
#by layoffs
arrange(term_reason, Layoff) %>% select("Layoff")
#by resignation
arrange(term_reason, Resignation) %>% select("Resignation")
#by retirement
arrange(term_reason, Retirement) %>% select("Retirement")


#============================================================Q4 4.8========
#3 Analysis 
#3.4 Question 4
#3.4.8 Analysis 4-8 (page 64-63)
#------------------------------------------------- (page 64)

#box plot of age distribution of terminated employees grouped by termination reason
ggplot(term_emp, aes(y = age, x = as.factor(termreason_desc), color = as.factor(termreason_desc))) +
  geom_boxplot() +
  geom_jitter(width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "black") +
  labs(title = "Age distribution of terminated employees grouped by termination reason from 2006 to 2015",
       x = "Termination Reason", y = "Age") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

#------------------------------------------------- (page 65)

#create data frame to store mean and median age based on termination reason
term_emp_meaned <- term_emp %>% 
  group_by(termreason_desc) %>% 
  summarise(mean_age = mean(age), median_age = median(age))
term_emp_meaned$mean_age <- round(term_emp_meaned$mean_age, 2)

#remove variables
rm(term_emp_meaned)

#------------------------------------------------- (page 66)

#density ridgeline plot length of service of terminated employees grouped by termination reason
ggplot(term_emp, aes(x = length_of_service, y = as.factor(termreason_desc), 
                     fill = as.factor(termreason_desc))) +
  geom_density_ridges(quantile_lines = TRUE, quantile_fun = mean) +
  labs(title = "Length of service of terminated employees 
       grouped by termination reason from 2006 to 2015",
       y = "Termination Reason", x = "Length of Service", fill = "Gender") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#------------------------------------------------- (page 67)

#create data frame to store mean and median length of service based on termination reason
term_emp_meaned <- term_emp %>% 
  group_by(termreason_desc) %>% 
  summarise(mean_LOS = mean(length_of_service), median_LOS = median(length_of_service))
term_emp_meaned$mean_LOS <- round(term_emp_meaned$mean_LOS, 2)

#remove variables
rm(term_emp_meaned)

#====================================================================
