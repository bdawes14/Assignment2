#Downloading/formatting data
library(tidyverse)
library(gtsummary)
library(ggplot2)
cohort <- read.csv("raw-data/cohort.csv")
cohort$smoke <- factor(cohort$smoke, levels = c(0, 1), labels = c("No", "Yes"))
cohort$female <- factor(cohort$female, levels = c(0, 1), labels = c("Male", "Female"))
cohort$cardiac <- factor(cohort$cardiac, levels = c(0, 1), labels = c("No", "Yes"))

##Descriptive table of variables
cohort %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ± {sd}")) 

##Predictive models
#Predicting cardiac disease
cardiac_model <- glm(cardiac~female + smoke + age, data=cohort, family=binomial)
summary(cardiac_model)
cardiac_model %>%
  tbl_regression(exponentiate = TRUE) %>%
  add_glance_table(include = c(logLik, AIC)) %>%
  modify_caption("Prediction Model for Cardiac Disease")

#Predicting Cost
cost_model <- lm(cost ~ female + smoke + age + cardiac, data = cohort)
summary(cost_model)
cost_model %>%
  tbl_regression() %>%
  add_glance_table(include = c(r.squared, AIC)) %>%
  modify_caption("Predictive Model for Healthcare Costs")

##Figure: Average cost by smoking status and age
ggplot(cohort, aes(x=smoke, y=cost)) + geom_violin() + 
  labs(title = "Cost by smoking status", x="smoking status", y="cost") + theme_minimal()

##README TEXT
#In this data we analyzed the associations between sex, smoking status, cardiac disease, and cost. As there was not a data dictionary, I am assuming "cardiac" is cardiac disease
#and "cost" is something like healthcare cost per year. 
#Table 1 shows the relative distribution of our data. Approximately 13% (n=500) smoked, 57% were female, and the average age was 45. 5.5% had cardiac disease and the average cost was $9,398.
#Table 2 and 3 show results for regression analysis analyzing potential risk factors for cardiac disease and cost, respectively.
#For Table 2, we see that smoking was strongly associated with cardiac disease (OR-9.59), age was also associated with an OR-1.01 per year. Female sex was protective with an OR of 0.08.
#Table 3 shows potential contributions of these variables to cost. Importantly, somiking and cardiac disease independently were associated with approximately $500 more cost/year. Again, female sex reduced cost. 
#Lastly, figure 1 shows a violin plot of cost by smoking status and demonstrates the increased cost and in particular the differences in minimums between the 2 groups. 
