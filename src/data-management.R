# library initialization
library(magrittr)
library(tibble)
library(zeallot)
library(tidyverse)


# define input
input_vector <- c(0.4, 0.5, 0.6)

mean(round(input_vector))

input_vector %>%
  round() %>%
  mean()

weird_rts <- c("RT = 323", "RT = 345", "RT = 421", "RT = 50")

weird_rts %>%
  stringr::str_sub(start = 6) %>%
  as.numeric() %>%
  log() %>%
  mean() %>%
  signif(digits = 2)


# tree pipe %T>% print or plot not in the chain computation
# but as a intermediate result
input_vector %>%
  mean() %T>%
  print %>%
  sum(3)


# expo pipe makes names available
# tibble(x = 1:3) %>% sum(x) not working
tibble(x = 1:3) %$% sum(x)

# assignment operator
x <- c(0.4, 0.5, 0.6)
x %<>%
  sum(3) %>%
  mean()

print(x)


c(x, y) %<-% c(3, "huhu")
print(x)
print(y)


# data is too wide
exam_results_visual <- tribble(
  ~exam, ~"Rozz", ~"Andrew", ~"Siouxsie",
  "midterm", "1.3", "2.0", "1.7",
  "final", "2.3", "1.7", "1.0"
)
exam_results_visual

# making data longer
exam_results_tidy <- exam_results_visual %>%
  pivot_longer(
    cols = -1,
    names_to = "student",
    values_to = "grade"
  ) %>%
  select(student, exam, grade)
exam_results_tidy


# data is too long
mixed_results_too_long <-
  tibble(
    student = rep(c("Rozz", "Andrew", "Siouxsie"), times = 2),
    what = rep(c("grade", "participation"), each = 3),
    howmuch = c(2.7, 2.0, 1.0, 75, 93, 33)
  )
mixed_results_too_long

# making data wider
mixed_results_too_long %>%
  pivot_wider(
    names_from = what,
    values_from = howmuch
  )

# add and modify columns
exam_results_tidy %>%
  mutate(
    passed = grade <= 1.7,
    exam = factor(exam, ordered = T)
  )

# change column name
exam_results_tidy %>%
  rename(participant = student)


homework_results_untidy <-
  tribble(
    ~student,      ~results,
    "Rozz",        "1.0,2.3,3.0",
    "Andrew",      "2.3,2.7,1.3",
    "Siouxsie",    "1.7,4.0,1.0"
  )


homework_results_untidy %>%
  separate(
    col = results,
    into = str_c("HW_", 1:3),
    sep = ",",
    convert = T
  )

exam_results_tidy %>%
  arrange(desc(student), grade)

# bind two tables together
new_exam_results_tidy <- tribble(
  ~student, ~exam, ~grade,
  "Rozz", "bonus", 1.7,
  "Andrew", "bonus", 2.3,
  "Siouxsie", "bonus", 1.0
)

rbind(
  exam_results_tidy,
  new_exam_results_tidy
)


exam_results_tidy
student_numbers <- tribble(
  ~student, ~student_number,
  "Rozz", "666",
  "Andrew", "1969",
  "Siouxsie", "3.14"
)

full_join(exam_results_tidy, student_numbers, by = "student")

exam_results_tidy %<>% mutate(grade = as.numeric(grade))
# extracting mean grade for Rozz
mean_grade_Rozz <- exam_results_tidy %>%
  filter(student == "Rozz") %>%
  pull(grade) %>%
  mean()
mean_grade_Rozz

# not satisfactory, clumsy and error-prone
get_mean_for_student <- function(student_name) {
  exam_results_tidy %>%
    filter(student == student_name) %>%
    pull(grade) %>%
    mean()
}

map_dbl(
  exam_results_tidy %>% pull(student) %>% unique(),
  get_mean_for_student
)

# tidyverse is the universe!
exam_results_tidy %>%
  group_by(student) %>%
  summarise(
    student_mean = mean(grade)
  )

exam_results_tidy %>%
  group_by(student) %>%
  mutate(
    student_mean = mean(grade)
  )
  

# Case Study for KoF
library(aida)
data_kof_raw <- aida::data_KoF_raw
glimpse(data_kof_raw)

data_kof_raw %>% pull(comments) %>% unique
data_kof_raw %>% pull(languages) %>% unique

data_kof_raw %<>% select(
  -languages, -comments, -age,
  -RT, -education, -gender
)

glimpse(data_kof_raw)


data_kof_raw %>%
  filter(trial_type != "practice", submission_id == 192) %>%
  select(trial_type, item_version, question) %>%
  arrange(desc(trial_type), item_version) %>%
  print(n = Inf)


data_kof_processed <- data_kof_raw %>%
  select(-trial_name) %>%
  filter(trial_type != "practice") %>%
  mutate(
    # add a condition variable
    condition = case_when(
      trial_type == "special" ~ "background check",
      trial_type == "main" ~ str_c("Condition ", item_version),
      TRUE ~ "filler"
    ) %>%
      # make 'condition' variable a factor
      factor(
        ordered = T,
        levels = c(
          str_c("Condition ", c(0, 1, 6, 9, 10)),
          "background check", "filler"
        )
      )
  )

## Data cleaning for KoF

### Cleaning by participant
subject_error_rate <- data_kof_processed %>%
  filter(trial_type == "filler") %>%
  group_by(submission_id) %>%
  summarise(
    proportion_correct = mean(correct_answer == response),
    outlier_subject = proportion_correct < 0.5
  ) %>%
  arrange(proportion_correct)


d_cleaned <-
  full_join(data_kof_processed, subject_error_rate, by = "submission_id") %>%
  filter(outlier_subject == FALSE)


### Cleaning by trial
d_cleaned %<>% filter(trial_type == "special") %>%
  mutate(
    background_correct = correct_answer == response
  ) %>%
  select(submission_id, vignette, background_correct) %>%
  right_join(d_cleaned, by = c("submission_id", "vignette")) %>%
  filter(trial_type == "main" & background_correct == TRUE)