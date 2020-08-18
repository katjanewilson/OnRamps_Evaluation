levels(All_Data$Grade) <- c("Freshman (9th)", "Sophomore (10th)", "Junior (11th)", "Senior (12th)")
credit_graph <- ggplot(data = All_Data) +
  aes(x = `Credit Outcome`, fill = `Credit Outcome`) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of Credit/No Credit for Each Grade",
       x = "Grade Type",
       y = "Number of Students") +
  theme_bw() +
  facet_wrap(vars(Grade)) +
  coord_flip()
credit_graph



All_Data_grouped_1 <- All_Data %>%
  select(Grade, Unit_1_Exam, Unit_2_Exam, Unit_3_Exam_, NumberModules.clicked.) %>%
  group_by(Grade) %>%
  summarise(N= n(),
            "Average Score Unit 1" = mean(Unit_1_Exam),
            sd_Unit1 = sd(Unit_1_Exam),
            "Average Score Unit 2" = mean(Unit_2_Exam),
            sd_Unit2 = sd(Unit_2_Exam),
            "Average Score Unit 3"= mean(Unit_3_Exam_),
            sd_Unit3 = sd(Unit_3_Exam_))
grouped_graph <- All_Data_grouped_1 %>%
  gather(key = "Unit", value = "Score", `Average Score Unit 1`, `Average Score Unit 2`, `Average Score Unit 3`)
Graph <- ggplot(data = grouped_graph) +
  aes(x = Unit, weight = Score, fill = Grade) +
  geom_bar(alpha = 0.7) +
  theme_bw() +
  facet_wrap(vars(Grade)) +
  labs(title = "Average Unit 1-3 Exam Scores Separated by Grade Level",
       x = "Exam",
       y = "Score")
Graph



grouped_graph2 <- All_Data_final %>%
  gather(key = "Grades", value = "Final Average Score", final_average)
levels(grouped_graph2$Grade) <- c("Freshman (9th)", "Sophomore (10th)", "Junior (11th)", "Senior (12th)")
graph2 <- ggplot(data = grouped_graph2) +
  aes(x = Grade, y = `Final Average Score`, fill = Grade) +
  geom_violin(alpha = .5) +
  geom_boxplot(alpha = .5, width = 0.3) +
  labs(title = "Final Average Score for Each Grade" ,x = "High School Grade", y = "Final Average Score") +
  theme_minimal() +
  guides(fill = FALSE)
graph2




Modules_plot_1 <- ggplot(All_Data_Exams, aes(x = NumberModules.clicked., y = Score)) + 
  geom_boxplot(fill = "#dadaeb") +
  labs(x = "Number of Modules Clicked", y = "Unit Score")+
  theme_minimal() +
  labs(title = "Unit Average Scores Distribution by Number of Clicked Modules")
Modules_plot_1

levels(All_Data_final$Grade) <- c("Freshman (9th)", "Sophomore (10th)", "Junior (11th)", "Senior (12th)")
Modules_plot_2 <- ggplot(data = All_Data_final) +
  aes(x = final_average, y = NumberModules.clicked.) +
  geom_line(color = "#6a3d9a") +
  labs(title = "Final Average Score Distribution by Number of Clicked Modules for Each Grade",
       x = "Final Score Average",
       y = "Number of Modules Clicked") +
  geom_jitter(alpha = .2) +
  #geom_smooth() +
  theme_bw() +
  facet_wrap(vars(Grade))
Modules_plot_2



data_count <- data %>%
  count(word, sort = TRUE) %>%
  filter(!is.na(word))
data_count_graph <- data_count %>%
  mutate(word = reorder(word, n)) %>%
  top_n(10)
graph <- ggplot(data = data_count_graph) +
  aes(x = word, weight = n) +
  geom_bar(fill = "#35b779") +
  theme_update() +
  coord_flip() +
  ggtitle("Frequent Words in Response to Independent Learner Question") +
  ylab("Word Count") +
  xlab("Word")
graph

#A visualization of the top positive and negative words for each grade level

sophomore_graph <- sophomore_sentiments %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  ggtitle("Sophomore's Most Positive and Negative Words") +
  ylab("Count") +
  xlab("Word")

junior_graph <- junior_sentiments %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  ggtitle("Junior's Most Positive and Negative Words") +
  ylab("Count") +
  xlab("Word")

senior_graph <- seniors_sentiments %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  ggtitle("Seniors's Most Positive and Negative Words") +
  ylab("Count") +
  xlab("Word")

sophomore_graph
junior_graph
senior_graph
