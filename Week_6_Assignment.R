# load packages

library(tidyverse)
library(scales)
library(ggthemes)

# import data

survey <- read_csv("survey.csv")

# creating new data frame to plot

avg_salary_by_industry_and_state <- survey |> 
  select(industry, state, annual_salary) |> 
  group_by(state, industry) |> 
  filter(industry %in% c("Retail", "Food Service", "Accounting, Banking & Finance", "Agriculture or Forestry", "Insurance", "Social Work", "Sales"), state %in% c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Floria", "Georgia", "Idaho", "Illinois")) |> 
  summarize(avg_salary = mean(annual_salary)) |> 
  arrange(avg_salary)

# create scatterplot 
ggplot(data = avg_salary_by_industry_and_state,
       mapping = aes(x = avg_salary,
                     y = industry, 
                     color = state)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 150000),
                     # add dollar sign formattin
                     labels = scales::dollar,
                     # replace c() with seq()
                     breaks = seq(0, 150000, 25000)) +
  labs(title = "Average Salary by Industry and State",
       x = "Average Salary",
       y = NULL,
       # Change legend label
       color = "State") +
  theme_minimal()
