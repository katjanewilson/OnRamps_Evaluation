

#Statistical Analysis
First regression is a regression of engagement related to overal students Unit 1,2,3, and End of Year Exam performance.
```{r}

regression1 <- lm(`Average Score Unit 1`  ~ NumberModules.clicked.,data = All_Data_Clicked)
regression2 <- lm(`Average Score Unit 2` ~ NumberModules.clicked., data = All_Data_Clicked)
regression3 <- lm(`Average Score Unit 3` ~ NumberModules.clicked., data = All_Data_Clicked)
regression4 <- lm(`Final Score` ~ NumberModules.clicked., data = All_Data_Clicked)

summary(regression1)
summary(regression2)
summary(regression3)
summary(regression4)

regression5 <- lm( `Final Score`~ NumberModules.clicked. + Grade + NumberModules.clicked.: Grade, data = All_Data_Clicked)

summary(regression5)
# The following code shows the interaction plot between the number of modules clicked and grade level
library(interactions)
interact_plot(regression5, pred = NumberModules.clicked., modx = Grade)