# OnRamps
Dual Enrollment programs offer high school students the opportunity to explore college curricula and earn college credit before graduation. With a large number of students participating in these programs each year in the state of Texas, researchers have the comfort of a large sample size and the opportunity to ethically randomize interventions. We evaluate how changes in growth mindset influence sense of belonging, course completion, and inclination to pursue STEM fields for a large cohort (10,000 students).

<p align="center">
<img src="https://github.com/katjanewilson/OnRamps_Evaluation/blob/master/images/logo.png"
  alt="Size Limit comment in pull request about bundle size changes"
  width="500" height="300">
</p>


[GitHub action]: https://github.com/andresz1/size-limit-action
[cult-img]:      http://cultofmartians.com/assets/badges/badge.svg
[cult]:          http://cultofmartians.com/tasks/size-limit-config.html

## Data

The data were collected from 3,484 high school students enrolled in the OnRamps Precalculus course for the 2017-2018 academic year. Students completed a survey before the course began that consisted of 14 items related to motivationa beliefs and leanring goals, and 14 items related to sense of belonging. The results of this survey were stored in the Beginning of Year Assessment data file (BOCA). Information regarding student’s open- ended responses to the sentiment analysis quesiton, as well as student’s open-ended responses to their intended major question, are stored in this file.


## Cleaning Notes
 * After that, we selected the variables of interest, which were Unit 1-3 Exam Data, Student ID, Grade, School, the answer to the question “I like to be challenged in class”, student’s self- reported intended major, student’s first and second exam intervention module choice, the click through rates for the modules, and the final UT grade given to students.

 * Next, we removed any missing data values from the data, and then created a new variable that identified whether or not the students recieved credit for the course. As noted in the original explanation, the purpose of the OnRamps course to give crexit for a UT course depends on the final UT given grade, that it is over a 70%.
 
 * To continue, we created unified variables of the mean score of Unit 1 through 3 grades for each Grade level (freshman through seniors) that students were in.
Another way that we parsed that data was through the number of modules completed, giving us an opportunity to look at the differences in outcome scores based on the number of modules completed.
To continue with sentiment analysis at a later time, we identified the text components of the quesiton “What does being an independent learner mean to you?”. This step required use of the tidytext package in R, which required that we munge the data into a specific structure in which the table has one-token-per-row, so that a meaningful unit of text, or a toke, can be analyzed and arranged. After organizing the data into the tidytext format, we gathered positive and negative sentiments from the “bing” lexicon, and joined them with the tidytext versions of our data, eventually using this newly joined data set to create visualizations of sentiments.

```
library(tidyverse) All_Data <- AllData %>%
select(Student_ID, School, Grade, `I like to be challenged in my classes`, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked., UT_Average) % >%
mutate(credit = ifelse(UT_Average > 70, "Credit", "No Credit"), Grade = as.factor(Grade)) %>%
rename("Credit Outcome" = credit) %>% filter(!is.na(Grade),
!is.na(Unit_1_Exam), !is.na(Unit_2_Exam),
!is.na(Unit_3_Exam_), !is.na(NumberModules.clicked.))

All_Data_grouped <- All_Data %>%
select(Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked
.) %>% group_by(Grade) %>%
summarise(N= n(),
"Average Score Unit 1" = mean(Unit_1_Exam), sd_Unit1 = sd(Unit_1_Exam),
"Average Score Unit 2" = mean(Unit_2_Exam), sd_Unit2 = sd(Unit_2_Exam),
"Average Score Unit 3"= mean(Unit_3_Exam_), sd_Unit3 = sd(Unit_3_Exam_))
is.factor(All_Data_grouped$Grade) ## [1] TRUE
levels(All_Data_grouped$Grade) <- c("Freshman (9th)", "Sophomore (10th)", "Ju nior (11th)", "Senior (12th)")

```



## Packages

* [tidyverse](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)
* [tidytext](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf)




## Associative Analysis

We wrote a function to run graphs of distribution of exam scores based on students' intended college major

```
#Create a function that takes in the major, and the data, and returns a graph of the distribution of Average UT Exam scores
graphlabels <- c("Freshman", "Sophomores", "Juniors", "Seniors") library(ggpubr)
function_graph <- function(major, data) {
box <- ggboxplot(data, x= "Grade", y = "UT_Average", color = "Grade", palet te = "lancet") +
theme(text = element_text(size = 8)) + labs( y = "UT Average") + scale_x_discrete(labels = graphlabels) +
theme(axis.text.x = element_text(color = "black", size = 8, angle = 30, v just = .8, hjust = 0.8)) +
theme(legend.position = "none")
dot <- ggdotplot(data, x= "Grade", y = "UT_Average", color = "Grade", palett e = "lancet", binwidth = 2, dotsize = 0.9) +
theme(text = element_text(size = 8)) + labs( y = "UT Average") +
scale_x_discrete(labels = graphlabels) +
theme(axis.text.x = element_text(color = "black", size = 8, angle = 30, v
just = .8, hjust = 0.8)) + theme(legend.position = "none")
figure_function <- ggarrange(box, dot) figure_function <- figure_function %>%
annotate_figure(figure_function,
top = text_grob(major, color = "blue", face = "bold"))
}
#Run the function in different major's data
graph_bio <- function_graph("Biology", biodat)
graph_bio
```

## Regression Analyses

The regression model above investigates whether the relationship between average final score and number of modules clicked is moderated by the school grade level. By looking at the summary of the regression model presented above, only for sophomores the relationship between the average final score and number of modules clicked is significantly moderated by the grade level (B = 5.661, t(6) = 2.492, p <.05).

## Sample

<p align="center">
<img src="https://github.com/katjanewilson/OnRamps_Evaluation/blob/master/images/graph2.png"
  alt="Size Limit comment in pull request about bundle size changes"
  width="500" height="300">
</p>


<p align="center">
<img src="https://github.com/katjanewilson/OnRamps_Evaluation/blob/master/images/graph3.png"
  alt="Size Limit comment in pull request about bundle size changes"
  width="500" height="300">
</p>
