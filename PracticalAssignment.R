pacman::p_load(ompr,
               ggplot2,
               purrr,
               dplyr,
               ompr.roi,
               ROI.plugin.glpk)

CurrentStudents <- 40
events <- 4
eventStudentCapacity<-9


#capacity <- rep.int(eventStudentCapacity, events) #all have equal capacities
capacity<-8:11 #all have different capacities

# Each student has three options. To model this we have a function that gives us three events 
# for each student. The first component has perference 1, second 2, and third 3
# This does a random sample of 1:m without replacement.  In this example, each student gets three preferences.
# In a future verson, the number of preferences should be dictated by the number of options each student can have.
# For production, we will not be using: set.seed(1234)
set.seed(1234)

#For my problem, I need there to be an initial assignment of students to events.  Next, the students
#that perform events need to be indexed to the next event.  Finally, the next day needs to run until n days.
alternateEventOptionsData <- lapply(seq_len(CurrentStudents), function(x) sample(seq_len(events), 3))

alternateEventOptionsData2 <- lapply(alternateEventOptionsData, function(x) x+1)



alternateEventOptions <- function(student) alternateEventOptionsData[[student]]

# The last component we need is a weight functions to make the model formulation easier.
# This function gives us the preference weighting for a event and student pair.

# the weight of a student choosing a event
# if the event is not among the preferences, the weight is -100000
# Assigns the weight if the value is numeric.  If there is no selection, the value becomes so 
# negative that the problem optimization results will be horrible if it assigns that event.
weight <- function(student, event) {
  p <- which(as.numeric(event) == alternateEventOptions(as.numeric(student)))
  as.integer(if (length(p) == 0) {
    -100000
  } else {
    p
  })
}


# plot the initial random assignments
plot_data <- expand.grid(
  event = seq_len(events),
  weight = 1:3
) %>% rowwise() %>% 
  mutate(count = sum(map_int(seq_len(CurrentStudents), ~weight(.x, event) == weight))) %>% 
  mutate(event = factor(event), weight = factor(weight))
ggplot(plot_data, aes(x = event, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = eventStudentCapacity)


## The model
# The idea is to introduce a binary variable $x_{i, j}$ that is $1$ if student $i$ is matched to event $j$.
# As an objective we will try to satisfy preferences according to their weight. So assigning a student to 
# an event with preference 3 gives 3 points and so forth. 

# `r nrow(filter(matching, weight == 3))` students got their top preference.
# `r nrow(filter(matching, weight == 2))` students were assigned to their second choice and 
# `r nrow(filter(matching, weight == 1))` students got their least preferable event.
# For production, these will not be assigned randomly, but will place preference on the events that
# are closest to the end of the course.


model <- MIPModel() %>%
  # 1 iff student i is assigned to event m
  add_variable(x[i, j], i = 1:CurrentStudents, j = 1:events, type = "binary") %>%
  # maximize the preferences
  set_objective(sum_expr(weight(i, j) * x[i, j], i = 1:CurrentStudents, j = 1:events)) %>%
  # we cannot exceed the capacity of a event
  add_constraint(sum_expr(x[i, j], i = 1:CurrentStudents) <= capacity[j], j = 1:events)

model

#We will use `glpk` to solve the above model because this is a mixed integer programming problem.
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))


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

# plotting the optimized model
plot_data <- matching %>% 
  mutate(event = factor(j), weight = factor(weight, levels = c(1, 2, 3))) %>% 
  group_by(event, weight) %>% 
  summarise(count = n()) %>% 
  tidyr::complete(weight, fill = list(count = 0))
ggplot(plot_data, aes(x = event, y = count, fill = weight)) + 
  geom_bar(stat = "identity") + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  geom_hline(yintercept = eventStudentCapacity)


#rm(list = ls())
