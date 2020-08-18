#Create a function that takes in the major, and the data, and returns a graph of the distribution of Average UT Exam scores
graphlabels <- c("Freshman", "Sophomores", "Juniors", "Seniors")
library(ggpubr)
function_graph <- function(major, data) {
  box <- ggboxplot(data, x= "Grade", y = "UT_Average", color = "Grade", palette = "lancet") +
    theme(text = element_text(size = 8)) +
    labs( y = "UT Average") +
    scale_x_discrete(labels = graphlabels) +
    theme(axis.text.x = element_text(color = "black", size = 8, angle = 30, vjust = .8, hjust = 0.8)) +
    theme(legend.position = "none")
  dot <- ggdotplot(data, x= "Grade", y = "UT_Average", color =  "Grade", palette = "lancet", binwidth = 2, dotsize = 0.9) +
    theme(text = element_text(size = 8)) +
    labs( y = "UT Average") +
    scale_x_discrete(labels = graphlabels) +
    theme(axis.text.x = element_text(color = "black", size = 8, angle = 30, vjust = .8, hjust = 0.8)) +
    theme(legend.position = "none")
  figure_function <- ggarrange(box, dot)
  figure_function <- figure_function %>%
    annotate_figure(figure_function,
                    top = text_grob(major, color = "blue", face = "bold"))
}
#Run the function in different major's data
graph_bio <- function_graph("Biology", biodat)
graph_bio
graph_chem <- function_graph("Chemistry", chemdat)
graph_chem
graph_education <- function_graph("Education", educationdat)
graph_education
graph_engineering <- function_graph("Engineering", engineeringdat)
graph_engineering
graph_statistics <- function_graph("Statistics", statisticsdat)
graph_statistics
graph_art <- function_graph("Art", artdat)
graph_art
graph_english <- function_graph("English", englishdat)
graph_english
