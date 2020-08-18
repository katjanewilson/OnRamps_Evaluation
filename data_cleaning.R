
#Make grade a factor, so that it goes in order of freshman ,sohpomore, junir, senio
#The number of modules completed graph, treat it as a factor if it has colors
#Treat the number of modules as a factor so that the boxplots will show up next to eachother
#a new variable that has the same name and an udnerscore fa
#Put the narrative of each figure below each figure

library(tidyverse)
BOCA <- read_csv("../../../../Data/BOCA.csv")
Grades1 <- read.csv("../../../../Data/ExamGrades.csv")
Grades <- Grades1 %>%
  rename( "Student_ID" = "?..Student_ID")
FEI <- read.csv("../../../../Data/FirstExamIntervention.csv")
data <- merge(BOCA, FEI, by="Student_ID", all=T)
AllData <- merge(data, Grades, by="Student_ID", all=T)
dim(BOCA)
dim(Grades)
dim(FEI)
dim(AllData)


library(tidyverse)
All_Data <- AllData %>%
  select(Student_ID, School, Grade, `I like to be challenged in my classes`, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked., UT_Average) %>%
  mutate(credit = ifelse(UT_Average > 70, "Credit", "No Credit"),
         Grade = as.factor(Grade)) %>%
  rename("Credit Outcome" = credit) %>%
  filter(!is.na(Grade),
         !is.na(Unit_1_Exam),
         !is.na(Unit_2_Exam),
         !is.na(Unit_3_Exam_),
         !is.na(NumberModules.clicked.))


All_Data_grouped <- All_Data %>%
  select(Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked.) %>%
  group_by(Grade) %>%
  summarise(N= n(),
            "Average Score Unit 1" = mean(Unit_1_Exam),
            sd_Unit1 = sd(Unit_1_Exam),
            "Average Score Unit 2" = mean(Unit_2_Exam),
            sd_Unit2 = sd(Unit_2_Exam),
            "Average Score Unit 3"= mean(Unit_3_Exam_),
            sd_Unit3 = sd(Unit_3_Exam_))
is.factor(All_Data_grouped$Grade)

levels(All_Data_grouped$Grade) <- c("Freshman (9th)", "Sophomore (10th)", "Junior (11th)", "Senior (12th)")
All_Data_grouped




All_Data_final <- All_Data %>%
  select(Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, UT_Average, NumberModules.clicked.) %>%
  group_by(Grade, NumberModules.clicked.) %>%
  mutate(final_average = mean(UT_Average)) %>%
  group_by(final_average, Grade, NumberModules.clicked.) %>%
  summarise(N= n())
All_Data_final




All_Data_Exams <- All_Data %>%
  select(Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked.) %>%
  mutate(NumberModules.clicked. = as.factor(All_Data$NumberModules.clicked.)) %>%
  group_by( NumberModules.clicked.) %>%
  arrange(NumberModules.clicked.) %>%
  gather(key = "Unit", value = "Score", Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_)
All_Data_Exams



All_Data_Clicked <- All_Data %>%
  select(Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked., UT_Average) %>%
  group_by(NumberModules.clicked., Grade) %>%
  summarise(N= n(),
            "Average Score Unit 1" = mean(Unit_1_Exam),
            "Average Score Unit 2" = mean(Unit_2_Exam),
            "Average Score Unit 3" = mean(Unit_3_Exam_),
            "Final Score" = mean(UT_Average))
All_Data_Clicked


#Create one data frame with the variables of interest
library(stringr)
library(tidytext)
Tidy_Text_Data <- AllData %>%
  select(Student_ID, School, Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, UT_Average, `What does being an independent learner mean to you`) %>%
  filter(!is.na(Unit_1_Exam),
         !is.na(Unit_2_Exam),
         !is.na(Unit_3_Exam_))
#Convert the data frame into a vector, then a tibble, then tokenize the responses so that each word is in one spot
data <- Tidy_Text_Data %>%
  select(txt = `What does being an independent learner mean to you`, Grade, Student_ID) %>%
  mutate(txt = as.vector(txt))
data <- data %>%
  unnest_tokens(word, txt)
#Remove the stop words
library(tidytext)
data(stop_words)
stop_words
data <- data %>%
  anti_join(stop_words)
#Group the  data by grade and create a new dataset for each grade (freshman not included)
data_grade <- data %>%
  group_by(Grade, word) %>%
  count(word, sort = TRUE) %>%
  group_by(Grade) %>%
  filter(!is.na(word))
juniors <- data_grade %>%
  filter(Grade == "Junior (11th)") %>%
  slice(1:10)
seniors <- data_grade %>%
  filter(Grade == "Senior (12th)") %>%
  slice(1:10)
sophomores <- data_grade %>%
  filter(Grade == "Sophomore (10th)") %>%
  top_n(10)
newdata <- rbind(seniors, juniors,sophomores)
newdata
#Then, do a sentiment analysis, using the bing lexicon
library(tidytext)
get_sentiments("bing")
#Create a function that will join the negative and positive sentiments into a data frame for each grade level
function_words <- function(grade, data) {
  grade_level <- data %>%
    filter(Grade == grade)
  data_grade <- grade_level %>%
    select(word, n)
  word_counts <- data_grade %>%
    inner_join(get_sentiments("bing"))
  word_counts_positive <- 
    word_counts %>%
    filter(sentiment == "positive") %>%
    slice(1:10)
  word_counts_negative <-
    word_counts %>%
    filter(sentiment == "negative") %>%
    slice(1:10)
  words_grade <- rbind(word_counts_negative, word_counts_positive)
  return(words_grade)
}
#Run the function on the senior, junior, and sophomore dataset
sophomore_sentiments <- function_words("Sophomore (10th)", data_grade)
sophomore_sentiments
junior_sentiments <- function_words("Junior (11th)", data_grade)
junior_sentiments
seniors_sentiments <- function_words("Senior (12th)", data_grade)
seniors_sentiments
