
pacman::p_load(ompr,
               ggplot2,
               purrr,
               dplyr,
               ompr.roi,
               ROI.plugin.glpk)

#courses have equal capacity, but the capacity can vary among courses.

CurrentStudents <- 40
events <- 4
eventStudentCapacity<-11
capacity <- rep.int(eventStudentCapacity, events) # all have equal capacities

# In addition, each student has three preferences. To model this we have a function that gives us three courses 
# for each student. The first component has perference 1, second 2, and third 3
# This does a random sample of 1:m without replacement.  In this example, each student gets three preferences.
# In a future verson, the number of preferences should be dictated by the number of options each student can have.
# For production, we will not be using: set.seed(1234)
set.seed(1234)
alternateEventOptionsData <- lapply(seq_len(CurrentStudents), function(x) sample(seq_len(events), 3))
alternateEventOptions <- function(student) alternateEventOptionsData[[student]]

# The last component we need is a weight functions to make the model formulation easier.
# This function gives us the preference weighting for a course and student pair.

# the weight of a student choosing a course
# if the course is not among the preferences, the weight is -100000
##Assigns the weight if the value is numeric.  If there is no selection, the value becomes so 
##negative that the problem optimization results will be horrible if it assigns that course.
weight <- function(student, course) {
  p <- which(as.numeric(course) == alternateEventOptions(as.numeric(student)))
  as.integer(if (length(p) == 0) {
    -100000
  } else {
    p
  })
}


#Let's take a look at our random preferences. We plot the number of votes for each course grouped by the preference (1, 2, 3).

plot_data <- expand.grid(
  course = seq_len(events),
  weight = 1:3
) %>% rowwise() %>% 
  mutate(count = sum(map_int(seq_len(CurrentStudents), ~weight(.x, course) == weight))) %>% 
  mutate(course = factor(course), weight = factor(weight))
ggplot(plot_data, aes(x = course, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = eventStudentCapacity)


## The model
# The idea is to introduce a binary variable $x_{i, j}$ that is $1$ if student $i$ is matched to course $j$. As an objective we will try to satisfy preferences according to their weight. So assigning a student to a course with preference 3 gives 3 points and so forth. The model assumes, that the total capacity of the courses is enough for all students.
# Here it is in mathematical notation:


model <- MIPModel() %>%
  
  # 1 iff student i is assigned to course m
  add_variable(x[i, j], i = 1:CurrentStudents, j = 1:events, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:CurrentStudents, j = 1:events)) %>%
  
  # we cannot exceed the capacity of a course
  add_constraint(sum_expr(x[i, j], i = 1:CurrentStudents) <= capacity[j], j = 1:events) %>% 
  
  # each student needs to be assigned to one course
  add_constraint(sum_expr(x[i, j], j = 1:events) == 1, i = 1:CurrentStudents)
model


## Solve the model
#We will use `glpk` to solve the above model.

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


#We solved the problem with an objective value of `r objective_value(result)`.


matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j) %>% 
  rowwise() %>% 
  mutate(weight = weight(as.numeric(i), as.numeric(j)), 
         alternateEventOptions = paste0(alternateEventOptions(as.numeric(i)), collapse = ",")) %>% ungroup

matching %>% 
  group_by(weight) %>% 
  summarise(count = n())


# `r nrow(filter(matching, weight == 3))` students got their top preference.
# `r nrow(filter(matching, weight == 2))` students were assigned to their second choice and `r nrow(filter(matching, weight == 1))` students got their least preferable course.
# 
# The course assignment now looks like this:


plot_data <- matching %>% 
  mutate(course = factor(j), weight = factor(weight, levels = c(1, 2, 3))) %>% 
  group_by(course, weight) %>% 
  summarise(count = n()) %>% 
  tidyr::complete(weight, fill = list(count = 0))
ggplot(plot_data, aes(x = course, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = eventStudentCapacity)


#rm(list = ls())
