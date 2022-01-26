ggplot(cleaned_emp_data, aes(y = as.factor(department_name))) +
  geom_histogram(binwidth = 1) +
  stat_count(binwidth=1, geom = "text", aes(label = ..count..), vjust = 0, hjust = 0.5) +
  facet_wrap(~status_year)

ggplot(cleaned_emp_data, aes(y = as.factor(department_name))) +
  geom_histogram(binwidth = 1, color = "white", stat = "count") +
  labs(title = "Distribution of male and female employees based on age group from 2006 to 2015",
       x = "Age", y = "Frequency", fill = "Gender") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0, hjust = .5)+
  facet_wrap(~status_year)


subset(total_emp_dept, Freq <= 100)
total_emp_dept <- as.data.frame(table(cleaned_emp_data$department_name))

ggplot(x, aes(x = status_year, y = mean_LOS, color = mA)) +
  geom_point()

x = as.data.frame(cleaned_emp_data %>% group_by(status_year) %>% summarise(medianAge = median(age)))
y = cleaned_emp_data %>% group_by(status_year) %>% summarise(mean_LOS = round(mean(length_of_service), digits = 1))
x = cbind(x, mean_LOS = y$mean_LOS)


ggplot(term_emp, aes(x = factor(1), y = age))+
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = gender, shape = gender), 
              width = 0.1, size = 1) +
  labs(x = NULL)

ggplot(term_emp, aes(x = age)) +
  geom_area(aes(fill = gender), color = "white", 
            stat ="bin", bins = 30) +
  facet_grid(cols = vars(status_year))

ggplot(term_emp, aes(y = age, x = length_of_service)) +
  # geom_point() +
  geom_jitter(aes(color = gender, shape = gender))+ 
  geom_smooth()
facet_wrap(~status_year)

huh <- as.data.frame(term_emp %>% 
                       group_by(department_name) %>%
                       select(termreason_desc) %>% 
                       table())

plot(STATUS_YEAR, emp_medianage$OVERALL_MEDIAN_AGE, type = "l", ylim = c(35, 50),
     xlab = "STATUS_YEAR", ylab = "", 
     main = "Line graph of employee numbers", col = "black")
lines(STATUS_YEAR, emp_medianage$MALE_MEDIAN_AGE, type = "l", col = "blue")
lines(STATUS_YEAR, emp_medianage$FEMALE_MEDIAN_AGE, type = "l", col = "red")
legend("top", 
       legend = c("Overall median age", "Male median age", "Female median age"), 
       col = c("black", "blue", "red"), lty = 1) 

#filter only active employees
active_emp <- subset(cleaned_emp_data, status == "ACTIVE")

ggplot(term_emp, aes(x = length_of_service, y = as.factor(status_year))) + 
  geom_density_ridges(aes(fill = as.factor(status_year))) + 
  facet_wrap(~status_year)

ggplot(term_emp, aes(x = age)) + 
  geom_density(aes(fill = gender), alpha = 0.6) +
  labs(title = "Density plot for length of service of terminated employees from 2006 to 2015",
       y = "Density", x = "Age", fill = "Gender") +
  facet_wrap(~ status_year)

ggplot(term_emp, aes(x = as.factor(status_year), y = age)) +
  geom_point()+ 
  geom_jitter(aes(color = as.factor(status_year))) +
  facet_wrap(~status_year, ncol = 1)


term_emp_v1 <- subset(cleaned_emp_data, status == "TERMINATED" & business_unit == "STORES" & 
                        (department_name == "Produce" | 
                           department_name == "Processed Foods" |
                           department_name == "Meats" |
                           department_name == "Dairy" |
                           department_name == "Customer Service" |
                           department_name == "Bakery"))

ggplot(term_emp) + 
  geom_bar(aes(y = ..count..,x =as.factor(department_name),
               fill = as.factor(termreason_desc)), position = "stack") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5))

levels(factor(term_emp_v1$job_title))

ggplot(term_emp, aes(x = as.factor(status_year), fill = as.factor(termtype_desc))) + 
  geom_bar(stat = "count", color = "black") +
  labs(title = "Distribution of terminated employees based on department from 2006 to 2015",
       x = "Frequency", y = "Department", fill = "Business Unit") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0, hjust = -1) + 
  facet_wrap(~gender)

ggpie(huh, "Freq", label = "termreason_desc")

ggpie(vol_df, "value", label = labs,
      lab.pos = "in", lab.font = "white",
      fill = "gender", color = "white") +
  labs(fill = "Gender", title = "Voluntarily Terminated") +
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(term_emp, aes(x = age, fill = as.factor(gender))) + 
  geom_histogram(binwidth = 1, color = "white", breaks = seq(15, 65, by = 5), position = "dodge") + 
  labs(title = "Total distribution of terminated employees based on age group",
       x = "Age", y = "Frequency", fill = "Gender") +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), breaks = seq(15, 65, by = 5), position = "dodge", vjust = -1) +
  geom_vline(xintercept = seq(from=15, to=65, by = 5), linetype = "dashed")

ggplot(term_emp, aes(x = age, fill = as.factor(gender))) + 
  geom_histogram(binwidth = 1, color = "white", breaks = seq(15, 65, by = 5), position = "dodge") + 
  labs(title = "Distribution of terminated employees based on age group from 2006 to 2015",
       x = "Age", y = "Frequency", fill = "Gender") + ylim(c(0,130)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), breaks = seq(15, 65, by = 5), position = "dodge", vjust = -1) +
  geom_vline(xintercept = seq(from=15, to=65, by = 5), linetype = "dashed") +
  facet_grid(rows = vars(status_year))

