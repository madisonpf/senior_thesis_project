######################################### HOUSEKEEPING ##########################################

## LOAD PACKAGES
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(ggthemes)
library(purrr)
library(tidyverse)
library(nnet)
library(broom)
library(car)
library(lme4)
library(ggsankey)
library(multicool)
library(glmmTMB)
library(DHARMa)
library(mediation)
library(lavaan)

## SET WORKING DIRECTORY & LOAD DATA FILES
setwd("~/Documents")

degree             <- read.csv("./PhD Documents/Senior Thesis Project/data/Degree.csv",      header=TRUE, sep=",")
non_degree         <- read.csv("./PhD Documents/Senior Thesis Project/data/Non-Degree.csv", header=TRUE, sep=",")
quarter_performance<- read.csv("./PhD Documents/Senior Thesis Project/data/QuarterPerformance.csv", header=TRUE, sep=",")
quarter_info       <- read.csv("./PhD Documents/Senior Thesis Project/data/QuarterInfo_v2_feb5.csv", header=TRUE, sep=",")
class_performance  <- read.csv("./PhD Documents/Senior Thesis Project/data/ClassPerformance_v2_apr22.csv", header=TRUE, sep=",")
offerings          <- read.csv("./PhD Documents/Senior Thesis Project/data/Offerings.csv", header=TRUE, sep=",")

########################################## DATA FILTERING ##############################################
# Check occurrences of BIOE courses
courses <- c("BIOE 20F","BIOE 82","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 158L","BIOE 145L",
             "BIOE 157A","BIOE 161L","BIOE 159A","BIOE 150L","BIOE 151A","ENVS 107A","BIOE 153A",
             "BIOE 245L","BIOE 195","BIOE 85","ENVS 104A")
course_counts <- sapply(courses, function(crs) sum(class_performance$SE.Subject.Catalog.Nbr == crs, na.rm=TRUE))
print(course_counts)

# Create dataframe for visualization
bioe_courses <- tibble(
  Course   = courses,
  Students = c(453,1293,253,260,73,167,157,28,422,429,67,313,555,11,0,189,133,351)
)

# Plot BIOE course distribution
ggplot(bioe_courses, aes(x=reorder(Course,Students), y=Students, fill=Course)) +
  geom_col(show.legend=FALSE) + coord_flip() +
  labs(x="Course", y="Number of Students") +
  theme_minimal() + theme(axis.text=element_text(size=12), plot.title=element_text(size=14,face="bold"))


# Add completion flags to class_performance
a <- bioe_courses$Course
final_class_performance <- {
  class_data <- class_performance
  for(course in a) {
    col <- paste0("completed_", gsub(" ", "_", course))
    class_data <- class_data %>%
      mutate(!!col := if_else(SE.Subject.Catalog.Nbr == course, "Yes", "No"))
  }
  class_data
}

# Merge quarter info + perf
quarter_data <- left_join(quarter_info, quarter_performance, by=c("CodedID","TA.Term.Code"))
# Merge degree + quarter
degree_quarter <- left_join(degree, quarter_data, by=c("CodedID","PLNA.Plan.Descr")) %>%
  mutate(TA.Term.Code=as.character(TA.Term.Code))
# Merge with class flags
bioe_195_final <- left_join(degree_quarter,
                            final_class_performance %>% mutate(TA.Term.Code=as.character(TA.Term.Code)),
                            by=c("CodedID","TA.Term.Code"))
# Export
write.csv(bioe_195_final,"./PhD Documents/Senior Thesis Project/data/bioe_195_final.csv",row.names=FALSE)

# Filter majors of interest
bioe_students <- bioe_195_final %>%
  filter(PLNA.Plan.Descr %in% c("Ecology and Evolution BS","Marine Biology BS",
                                "Biology BS","Plant Sciences BS"),
         Term_Units_Earned != 0)

## Quality check
length(unique(bioe_students$CodedID))
message("Unique instructor per term: ", !any(bioe_students$multiple_instructors))
table(bioe_students$SE.Subject.Catalog.Nbr)

##### FIELD COURSE PARTICIPATION ASSOCIATIONS ###############################################
BIOE_students_na_removed <- bioe_students %>%
  filter(!is.na(completed_BIOE_195))

student_flags <- BIOE_students_na_removed %>%
  group_by(CodedID) %>%
  summarize(
    completed_BIOE_195   = any(completed_BIOE_195 == "Yes"),
    completed_BIOE_82    = any(completed_BIOE_82  == "Yes"),
    completed_BIOE_85    = any(completed_BIOE_85  == "Yes"),
    completed_BIOE_128L  = any(completed_BIOE_128L== "Yes"),
    completed_BIOE_141L  = any(completed_BIOE_141L== "Yes"),
    completed_BIOE_151A  = any(completed_BIOE_151A== "Yes"),
    completed_BIOE_153A  = any(completed_BIOE_153A== "Yes"),
    completed_BIOE_155L  = any(completed_BIOE_155L== "Yes"),
    completed_BIOE_157A  = any(completed_BIOE_157A== "Yes"),
    completed_BIOE_159A  = any(completed_BIOE_159A== "Yes"),
    completed_BIOE_161L  = any(completed_BIOE_161L== "Yes"),
    completed_BIOE_150L  = any(completed_BIOE_150L== "Yes"),
    completed_ENVS_107A  = any(completed_ENVS_107A== "Yes"),
    completed_ENVS_104A  = any(completed_ENVS_104A== "Yes")
  ) %>%
  mutate(across(starts_with("completed_"), as.integer))

course_cols <- names(student_flags)[grepl("^completed_", names(student_flags)) & names(student_flags) != "completed_BIOE_195"]
# Reshape for plotting
students_flags_long <- student_flags %>%
  dplyr::select(completed_BIOE_195, all_of(course_cols)) %>%
  pivot_longer(cols = -completed_BIOE_195, names_to = "Course", values_to = "Completed")

# Plot the proportions using a stacked bar chart
ggplot(students_flags_long, aes(x = factor(Completed), fill = factor(completed_BIOE_195))) +
  geom_bar(position = "fill") + 
  facet_wrap(~ Course, ncol = 4) +  # Facet by course for better comparison
  scale_fill_manual(values = c("0" = "#e0b1cb", "1" = "#231942")) +
  labs(x = "Course Completion (0=No, 1=Yes)", 
       y = "Proportion of BIOE 195 Completion", 
       fill = "BIOE 195 Completion") +
  theme_minimal() + 
  theme(strip.text = element_text(size = 10, face = "bold"))

# Contingency tables
contingency_tables <- lapply(course_cols, function(col) {
  tbl <- table(student_flags$completed_BIOE_195, student_flags[[col]])
  cat("
Contingency table for:", col, "
")
  print(tbl)
  tbl
})

##### MULTINOMIAL LOGISTIC REGRESSION & CHORD PLOT ###########################################
# Factorize outcome
student_flags <- student_flags %>% mutate(completed_BIOE_195 = factor(completed_BIOE_195, levels=c(0,1)))

# Fit multinom model
model_formula <- as.formula(paste("completed_BIOE_195 ~", paste(course_cols, collapse=" + ")))

# Fit the multinomial logistic regression model
multi_mod_all_courses <- nnet::multinom(model_formula, data = student_flags)

# Check the summary of the fitted model
summary(multi_mod_all_courses)
tidy_model <- tidy(multi_mod_all_courses)
# View the tidied results
print(tidy_model)

# Calculate Odds Ratios (exp(estimate)) and 95% Confidence Intervals
or_table <- broom::tidy(multi_mod_all_courses, conf.int=TRUE) %>%
  mutate(
    OR = exp(estimate),  # Odds Ratio
    OR_low = exp(conf.low),
    OR_high = exp(conf.high)
  ) %>%
  dplyr::select(term, OR, OR_low, OR_high, p.value)

# Print the Odds Ratios table
print(or_table)

significant_results <- or_table %>%
  filter(p.value < 0.05) %>%
  filter(term != "completed_BIOE_153A")  # Remove completed_BIOE_153A from the results

# Plot the significant odds ratios with confidence intervals
ggplot(significant_results, aes(x = term, y = OR, ymin = OR_low, ymax = OR_high)) +
  # Plot the point ranges
  geom_pointrange(
    color = "#34495E",       # Point and range color
    size = 1.5,              # Line thickness for the point range
    shape = 21,              # Circle shape for points
    fill = "#e0b1cb",        # Fill color for the points
    fatten = 2               # Make points a bit larger
  ) +
  # Add a reference line at OR = 1
  geom_hline(
    yintercept = 1, 
    linetype = "dashed", 
    color = "red", 
    size = 1.2               # Thicker line for the reference
  ) +
  coord_flip() +  # Flip the coordinates for better label readability
  labs(
    x = "Predictor", 
    y = "Odds Ratio", 
    title = "Significant Odds Ratios for Predicting BIOE 195 Completion"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme with larger base font size
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Title style
    axis.text = element_text(size = 12),  # Axis text style
    axis.title = element_text(size = 14) # Axis title style
  )



model_coefficients <- tidy(multi_mod_all_courses) %>%
  filter(term != "(Intercept)") %>%  # Remove the intercept
  filter(p.value < 0.05) %>%  # Keep only significant predictors (p-value < 0.05)
  filter(term != "completed_BIOE_153A")  # Exclude BIOE_153A

# Step 3: Build the chord matrix based on model coefficients (log-odds)
chord_matrix <- t(sapply(model_coefficients$term, function(col) {
  # Check if the term exists in student_flags columns (i.e., course columns)
  if (col %in% names(student_flags)) {
    # Get the indices where the student has completed this course
    idx <- which(student_flags[[col]] == 1)  
    
    # Extract the coefficient (log-odds) for this term
    log_odds_coef <- model_coefficients$estimate[model_coefficients$term == col]
    
    # Convert log-odds to probabilities using the logistic function
    prob_bioe <- exp(log_odds_coef) / (1 + exp(log_odds_coef))  # Probability of BIOE_195 completion
    prob_no <- 1 - prob_bioe  # Probability of not completing BIOE_195
    
    # Return the probabilities for BIOE_195 and No_BIOE_195
    return(c(BIOE_195 = prob_bioe, No_BIOE_195 = prob_no))
  } else {
    # If the course doesn't exist in student_flags columns, return NA
    return(c(BIOE_195 = NA, No_BIOE_195 = NA))
  }
}))

# Step 4: Clean up the chord matrix (remove rows with NA values)
chord_matrix <- na.omit(chord_matrix)

# Normalize the chord matrix to get percentages
chord_matrix <- chord_matrix / rowSums(chord_matrix) * 100

# Step 5: Clean up row names (remove the "completed_" prefix from course names)
rownames(chord_matrix) <- gsub("completed_", "", rownames(chord_matrix))

# Step 6: Define the column names for the chord matrix
colnames(chord_matrix) <- c("BIOE_195", "No_BIOE_195")

# Step 7: Define the color palette for the chord diagram
manual_colors <- c(
  "BIOE_195" = "#1D84B5", "No_BIOE_195" = "#999999",
  "BIOE_82" = "#17BEBB", "BIOE_128L" = "#747C92",
  "BIOE_141L" = "#CD5334", "BIOE_151A" = "#372772",
  "BIOE_155L" = "#FCBCB8"
)

# Step 8: Create a list of link colors based on the row names
link_cols <- sapply(rownames(chord_matrix), function(x) manual_colors[x])
names(link_cols) <- paste(rownames(chord_matrix), "BIOE_195", sep = "->")
link_cols_no <- link_cols
names(link_cols_no) <- paste(rownames(chord_matrix), "No_BIOE_195", sep = "->")
link_cols <- c(link_cols, link_cols_no)

# Step 9: Define grid colors for the chord diagram
grid_cols <- manual_colors[union(rownames(chord_matrix), colnames(chord_matrix))]

# Step 10: Plot the chord diagram using the circlize package
library(circlize)

# Clear any previous plots
circos.clear()

# Set the plot parameters
circos.par(start.degree = 90, gap.degree = 3, canvas.xlim = c(-1, 1), canvas.ylim = c(-1, 1))

# Create the chord diagram
chordDiagram(chord_matrix, 
             grid.col = grid_cols, 
             col = link_cols, 
             transparency = 0.25, 
             annotationTrack = "grid", 
             preAllocateTracks = list(track.height = 0.08))

# Add labels to the chord diagram
circos.trackPlotRegion(track.index = 1, 
                       panel.fun = function(x, y) {
                         sec <- get.cell.meta.data("sector.index")
                         xlim <- get.cell.meta.data("xlim")
                         circos.text(mean(xlim), 1.1, sec, facing = "clockwise", niceFacing = TRUE, cex = 0.75)
                       }, 
                       bg.border = NA)



####
##### DEGREE-TIMING & FIELD-COURSES ##############################################################

# Function to extract year and quarter
get_year <- function(tc) as.integer(substr(tc, 1, 3)) + 1800
get_quarter <- function(tc) as.integer(case_when(
  substr(tc, 4, 4) == "8" ~ 1,
  substr(tc, 4, 4) == "0" ~ 2,
  substr(tc, 4, 4) == "2" ~ 3,
  substr(tc, 4, 4) == "4" ~ 4,
  TRUE ~ NA_integer_
))

# Compute summary for degree timing and course completion
time_completed_summary <- bioe_195_final %>% mutate(
  admit_year = get_year(UAD.Admit.Term.Code),
  admit_q = get_quarter(UAD.Admit.Term.Code),
  course_year = get_year(TA.Term.Code),
  course_q = get_quarter(TA.Term.Code),
  grad_year = get_year(DG.Completion.Term.Code),
  grad_q = get_quarter(DG.Completion.Term.Code),
  quarters_since_admit = (course_year - admit_year) * 4 + (course_q - admit_q),
  total_quarters = (grad_year - admit_year) * 4 + (grad_q - admit_q),
  course_percentage_degree = quarters_since_admit / total_quarters
)

# Plot timing of courses
avg_course_pct <- time_completed_summary %>%
  group_by(SE.Subject.Catalog.Nbr) %>%
  reframe(
    mean_pct = mean(course_percentage_degree, na.rm = TRUE),
    lq = quantile(course_percentage_degree, 0.25, na.rm = TRUE),
    uq = quantile(course_percentage_degree, 0.75, na.rm = TRUE),
    n = n(),
    Course = SE.Subject.Catalog.Nbr,
    .groups = "drop"
  ) %>%
  mutate(Course = reorder(Course, mean_pct))

ggplot(avg_course_pct, aes(x = Course, y = mean_pct)) +
  geom_segment(aes(xend = Course, y = 0, yend = mean_pct), color = "gray") +
  geom_point(aes(size = n), color = "#e0b1cb") +
  geom_errorbar(aes(ymin = lq, ymax = uq), width = 0.2, color = "#555555") +
  coord_flip() +
  labs(
    title = "When Students Take High-Impact Courses",
    x = "Course",
    y = "% of Degree Completed",
    size = "# Students"
  ) +
  theme_minimal(base_size = 13)

# Field course distribution
field_only <- bioe_195_final %>% filter(SE.Subject.Catalog.Nbr != "BIOE 195")
field_counts <- field_only %>%
  group_by(CodedID) %>%
  summarise(n_field_courses = n_distinct(SE.Subject.Catalog.Nbr), .groups = "drop")

field_dist <- field_counts %>%
  group_by(n_field_courses) %>%
  summarise(n_students = n()) %>%
  mutate(
    prop = n_students / sum(n_students),
    pct = scales::percent(prop)
  )

ggplot(field_dist, aes(x = factor(n_field_courses), y = n_students)) +
  geom_col(fill = "#e0b1cb") +
  geom_text(aes(label = pct), vjust = -0.5, size = 4) +
  labs(x = "# Field Courses", y = "# Students") +
  theme_minimal(base_size = 14)

##### PRIOR-COURSE INDICATOR & GLMM ###############################################################
# Build prior-course indicator for every student
# ————————————————————————————————
# 1) One row per student: did they ever enroll in BIOE 195?
student_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(
    completed_BIOE_195 = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
    .groups = "drop"
  )

# ————————————————————————————————
bioe195_inst <- time_completed_summary %>%
  filter(
    SE.Student.Enrollment.Status.Code == "E",
    SE.Subject.Catalog.Nbr == "BIOE 195"
  ) %>%
  mutate(across(where(~ inherits(.x, "Rle")), as.vector)) %>%
  mutate(
    CodedID              = as.character(CodedID),
    bioe195_instructor   = as.character(CI.Instructor.Name),
    TA.Term.Code_bioe195 = as.integer(TA.Term.Code)
  ) %>%
  dplyr::select(CodedID, bioe195_instructor, TA.Term.Code_bioe195)

# 2) All *other* enrollments for those students
history_all <- time_completed_summary %>%
  filter(
    SE.Student.Enrollment.Status.Code == "E",
    SE.Subject.Catalog.Nbr != "BIOE 195",
    CodedID %in% bioe195_inst$CodedID
  ) %>%
  mutate(across(where(~ inherits(.x, "Rle")), as.vector)) %>%
  mutate(
    CodedID            = as.character(CodedID),
    prior_instructor   = as.character(CI.Instructor.Name),
    TA.Term.Code_prior = as.integer(TA.Term.Code),
    prior_course       = as.character(SE.Subject.Catalog.Nbr)
  ) %>%
  dplyr::select(CodedID, prior_instructor, TA.Term.Code_prior, prior_course) %>%
  distinct()

# 3) Match per BIOE 195 attempt to the earliest *prior* same-instructor course
# (dplyr >= 1.1: non-equi join; else use fallback below)
prior_match_details <- history_all %>%
  inner_join(
    bioe195_inst,
    by = dplyr::join_by(
      CodedID,
      TA.Term.Code_prior < TA.Term.Code_bioe195,
      prior_instructor == bioe195_instructor
    ),
    relationship = "many-to-many"
  ) %>%
  group_by(CodedID, TA.Term.Code_bioe195) %>%
  slice_min(TA.Term.Code_prior, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(CodedID, prior_instructor, TA.Term.Code_prior, prior_course,
         TA.Term.Code_bioe195)

# ————————————————————————————————
# 5) Build the had_prior_course flag (TRUE for a match, FALSE otherwise)
student_match_full <- student_summary %>%
  left_join(prior_match_details, by = "CodedID") %>%
  mutate(
    had_prior_course = !is.na(prior_instructor)
  )

# Quick check
table(student_match_full$had_prior_course, useNA="ifany")

####### STUDENT SPECIFIC MODEL - CORRECT ##########

# 1) Ever‑completed flags + timing of each field course
student_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code=="E") %>%
  group_by(CodedID) %>%
  summarise(
    # binary outcome
    completed_BIOE_195  = as.integer(any(SE.Subject.Catalog.Nbr=="BIOE 195")),
    # ever‑completed each field course
    completed_BIOE_82   = as.integer(any(SE.Subject.Catalog.Nbr=="BIOE 82")),
    completed_BIOE_128L = as.integer(any(SE.Subject.Catalog.Nbr=="BIOE 128L")),
    completed_BIOE_141L = as.integer(any(SE.Subject.Catalog.Nbr=="BIOE 141L")),
    completed_BIOE_151A = as.integer(any(SE.Subject.Catalog.Nbr=="BIOE 151A")),
    completed_BIOE_155L = as.integer(any(SE.Subject.Catalog.Nbr=="BIOE 155L")),
    # timing = when they *first* took each (proportion of degree)
    time_pct_BIOE_82   = ifelse(completed_BIOE_82,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 82"], na.rm=TRUE),
                                NA_real_),
    time_pct_BIOE_128L = ifelse(completed_BIOE_128L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 128L"], na.rm=TRUE),
                                NA_real_),
    time_pct_BIOE_141L = ifelse(completed_BIOE_141L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 141L"], na.rm=TRUE),
                                NA_real_),
    time_pct_BIOE_151A = ifelse(completed_BIOE_151A,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 151A"], na.rm=TRUE),
                                NA_real_),
    time_pct_BIOE_155L = ifelse(completed_BIOE_155L,
                                min(course_percentage_degree[SE.Subject.Catalog.Nbr=="BIOE 155L"], na.rm=TRUE),
                                NA_real_),
    .groups="drop"
  )

# 2) Count how many distinct field courses each took
field_courses   <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 151A","BIOE 155L")
field_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code=="E",
         SE.Subject.Catalog.Nbr %in% field_courses) %>%
  group_by(CodedID) %>%
  summarise(n_field_courses = n_distinct(SE.Subject.Catalog.Nbr),
            .groups="drop") %>%
  right_join(distinct(time_completed_summary, CodedID), by="CodedID") %>%
  mutate(n_field_courses = replace_na(n_field_courses, 0L))

# 3) Prior‑course with the same BIOE 195 instructor
#    (assuming you already have `student_match_full` from your prior‐instructor code)

# 4) Pull in each students’ demographics (one row per student)
demo_vars <- bioe_students %>%
  distinct(CodedID,
           UAD.Admit.Type.Descr,
           PD.Gender...Current,
           SGC.Is.EOP,
           PD.Domestic.International...Current,
           ET.Ethnic.Group.Category,
           UAD.First.Generation,
           TA.GPA...Cumulative)

# ————————————————————————————————
# B) Assemble the final student‐level dataset

model_students <- student_summary %>%           # 1 row per student
  left_join(field_summary,     by = "CodedID") %>%   # # of field courses
  left_join(student_match_full, by = "CodedID") %>%  # had_prior_course (logical)
  left_join(demo_vars,         by = "CodedID") %>%   # demographics
  # drop any stray .x/.y columns
  rename_with(~ sub("\\.y$", "", .), ends_with(".y")) %>%
  dplyr::select(-ends_with(".x")) %>%
  # compute your overall timing summary & scale it
  mutate(
    avg_time_pct_field   = rowMeans(across(starts_with("time_pct_")), na.rm = TRUE),
    avg_time_pct_field_s = as.numeric(scale(avg_time_pct_field))
  ) %>%
  # now make the 3-level prior‐instructor factor
  mutate(
    completed_BIOE_195 = factor(completed_BIOE_195, levels = c(0,1)),
    # make all demog columns factors
    across(c(UAD.Admit.Type.Descr,
             PD.Gender...Current,
             SGC.Is.EOP,
             PD.Domestic.International...Current,
             ET.Ethnic.Group.Category,
             UAD.First.Generation,
             TA.GPA...Cumulative),
           factor)
  ) %>%
  # drop any accidental NAs in the outcome
  filter(!is.na(completed_BIOE_195))

# Quick sanity check
table(model_students$completed_BIOE_195)

# ————————————————————————————————
# C) Scale GPA & fit your model

model_students <- model_students %>%
  mutate(GPA_scaled = scale(as.numeric(TA.GPA...Cumulative))[,1],
         had_prior_course = factor(had_prior_course, levels = c(FALSE, TRUE)))

model_students2 <- model_students %>%
  filter(
    PD.Gender...Current != "X",
    !ET.Ethnic.Group.Category %in% c("Native Hawaiian/Oth Pac Island",
                                     "Unknown/Not Specified")
  )

# 2) Re‐fit the completion model without those levels
glm_student2_clean <- glm(
  completed_BIOE_195 ~ 
    n_field_courses * avg_time_pct_field_s + 
    UAD.Admit.Type.Descr +
    PD.Gender...Current +
    SGC.Is.EOP +
    ET.Ethnic.Group.Category +
    UAD.First.Generation + 
    GPA_scaled,
  data   = model_students2,
  family = binomial(link = "logit")
)
summary(glm_student2_clean)

# 2) Check residuals with DHARMa
sim <- simulateResiduals(glm_student2_clean, n = 1000)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)

# 3) Tidy up Model 1 (Take BIOE 195)  
or1_clean <- broom::tidy(glm_student2_clean, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    model   = "Take BIOE 195",
    term,
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high)
  )
print(or1_clean)

# 3) Check multicollinearity
library(car)
vif(glm_student2_clean)


# 1) Subset to those who completed BIOE 195
takers <- model_students %>%
  filter(completed_BIOE_195 == 1)

# 2) Make sure had_prior_course is a two‐level factor
takers <- takers %>%
  mutate(had_prior_course = factor(had_prior_course, levels = c(FALSE, TRUE)))

# 3) Quick sanity‐check of the outcome
table(takers$had_prior_course, useNA="ifany")

# 4) Fit the “prior-instructor” model
glm_prior <- glm(
  had_prior_course ~ 
    n_field_courses + 
    GPA_scaled,
  data   = takers,
  family = binomial(link = "logit")
)

# 5) Inspect
summary(glm_prior)

sim <- simulateResiduals(glm_prior, n = 1000)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)

# 2) Tidy up Model 2 (prior‐instructor among takers)
or2 <- tidy(glm_prior, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    model   = "Had Prior Instructor",
    term,
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high)
  )
# 5) Combine & humanize term names
or_plot <- bind_rows(or1_clean, or2) %>%
  mutate(
    term = case_when(
      term == "n_field_courses"                      ~ "Number of Field Courses",
      term == "avg_time_pct_field_s"                 ~ "Avg. Timing of Field Courses",
      term == "n_field_courses:avg_time_pct_field_s" ~ "Field Courses × Timing",
      term == "GPA_scaled"                           ~ "GPA (scaled)",
      TRUE                                           ~ term
    ),
    model = factor(model, levels = c("Take BIOE 195","Had Prior Instructor"))
  )

# 6) Plot the two‐panel forest plot
ggplot(or_plot, aes(x = OR, y = term, xmin = OR_low, xmax = OR_high, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10() +
  facet_wrap(~ model, scales = "free_y") +
  labs(
    x     = "Odds Ratio (log scale)",
    y     = NULL,
    title = "Forest Plot of Odds Ratios",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )


### GLM WITH TIMING
# 1) Zero-impute all timing variables
courses <- c("BIOE_82","BIOE_128L","BIOE_141L","BIOE_151A","BIOE_155L")

model_students2 <- model_students %>%
  mutate(across(
    .cols = all_of(paste0("completed_",courses)),
    .fns  = ~ as.integer(. == 1L),
    .names= "flag_{col}"    # creates flag_completed_BIOE_82, etc
  )) %>%
  # now for each timing:
  mutate(across(
    .cols = all_of(paste0("time_pct_",courses)),
    .fns = list(
      zero = ~ ifelse(get(sub("time_pct_","completed_",cur_column()))==1,
                      .x, 0)
    ),
    .names = "{col}"
  ))

# 2) Build the formula
#   for each course, we want: completed_<course>_flag * time_pct_<course>
int_terms <- unlist(lapply(courses, function(crs) {
  flag <- paste0("flag_", "completed_",crs)
  time <- paste0("time_pct_",crs)
  paste0(flag, " + ", time)
}))

#   plus your other covariates
covariates <- c(
  "GPA_scaled"
)

fml_all <- as.formula(paste0(
  "completed_BIOE_195 ~ ",
  paste(c(int_terms, covariates), collapse = " + ")
))

# 3) Fit the big model
glm_all_courses <- glm(
  fml_all,
  data   = model_students2,
  family = binomial(link = "logit")
)

summary(glm_all_courses)

# 1) Tidy and exponentiate
or_all <- broom::tidy(glm_all_courses, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high)
  ) %>%
  # explicitly call dplyr::recode
  mutate(term = dplyr::recode(term,
                              "completed_BIOE_82_flag1"    = "Took BIOE 82",
                              "time_pct_BIOE_82"           = "Timing BIOE 82",
                              "completed_BIOE_128L_flag1"  = "Took BIOE 128L",
                              "time_pct_BIOE_128L"         = "Timing BIOE 128L",
                              "completed_BIOE_141L_flag1"  = "Took BIOE 141L",
                              "time_pct_BIOE_141L"         = "Timing BIOE 141L",
                              "completed_BIOE_151A_flag1"  = "Took BIOE 151A",
                              "time_pct_BIOE_151A"         = "Timing BIOE 151A",
                              "completed_BIOE_155L_flag"   = "Took BIOE 155L",
                              "time_pct_BIOE_155L"         = "Timing BIOE 155L",
                              "GPA_scaled"                 = "GPA (scaled)"
  ))

# 2) Plot on log scale
ggplot(or_all, aes(x = OR, y = term, xmin = OR_low, xmax = OR_high)) +
  geom_point(color = "#1D84B5", size = 3) +
  geom_errorbarh(height = 0.2, color = "#1D84B5") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") +
  scale_x_log10() +
  labs(
    title = "Forest Plot of Odds Ratios",
    x     = "Odds Ratio (log scale)",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", size = 16)
  )


#### Models for Participation Gap ### 

# 0) Filters you use elsewhere
ms2 <- model_students2 %>%
  dplyr::filter(
    PD.Gender...Current != "X",
    !ET.Ethnic.Group.Category %in% c("Native Hawaiian/Oth Pac Island","Unknown/Not Specified")
  )

# 1) One Year per student (prefer grad_year, fallback course_year)
year_by_student <- time_completed_summary %>%
  dplyr::filter(SE.Student.Enrollment.Status.Code == "E") %>%
  dplyr::group_by(CodedID) %>%
  dplyr::summarise(
    Year_grad   = suppressWarnings(max(grad_year,   na.rm = TRUE)),
    Year_course = suppressWarnings(max(course_year, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Year_grad   = ifelse(is.infinite(Year_grad),   NA_integer_, as.integer(Year_grad)),
    Year_course = ifelse(is.infinite(Year_course), NA_integer_, as.integer(Year_course)),
    Year        = dplyr::coalesce(Year_grad, Year_course)
  ) %>%
  dplyr::select(CodedID, Year)

# 2) Join Year (avoid suffixes), ensure Year exists
ms2y <- ms2 %>%
  dplyr::left_join(year_by_student %>% dplyr::rename(Year_join = Year), by = "CodedID") %>%
  { if ("Year" %in% names(.)) dplyr::mutate(., Year = dplyr::coalesce(Year, Year_join)) else dplyr::rename(., Year = Year_join) } %>%
  dplyr::select(-dplyr::any_of("Year_join")) %>%
  dplyr::filter(!is.na(Year))

#### GENDER SPECIFIC GAP ####

# 4) Gender × Year ThesisGap
gender_gap_year <- ms2y %>%
  dplyr::filter(!is.na(PD.Gender...Current)) %>%
  dplyr::count(Value = PD.Gender...Current, Year, completed_BIOE_195, name = "n") %>%
  dplyr::mutate(Thesis = ifelse(as.character(completed_BIOE_195) == "1", "Yes", "No")) %>%
  dplyr::select(-completed_BIOE_195) %>%
  tidyr::pivot_wider(names_from = Thesis, values_from = n, values_fill = list(n = 0)) %>%
  dplyr::mutate(
    ThesisYes = dplyr::coalesce(.data$Yes, 0L),
    ThesisNo  = dplyr::coalesce(.data$No,  0L)
  ) %>%
  dplyr::select(-dplyr::any_of(c("Yes","No"))) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    Total      = ThesisNo + ThesisYes,
    DemRate    = Total / sum(Total),
    ThesisRate = ifelse(sum(ThesisYes) > 0, ThesisYes / sum(ThesisYes), 0),
    ThesisGap  = ThesisRate - DemRate
  ) %>%
  dplyr::ungroup()

# 5) Predictors aggregated to Gender × Year (incl. BIOE 82)
gender_preds_year <- ms2y %>%
  dplyr::filter(!is.na(PD.Gender...Current)) %>%
  dplyr::group_by(Value = PD.Gender...Current, Year) %>%
  dplyr::summarise(
    mean_n_field_courses        = mean(n_field_courses, na.rm = TRUE),
    mean_avg_time_pct_field_s   = mean(avg_time_pct_field_s, na.rm = TRUE),
    mean_GPA_scaled             = mean(GPA_scaled, na.rm = TRUE),
    rate_flag_completed_BIOE_82 = mean(flag_completed_BIOE_82, na.rm = TRUE),
    rate_flag_completed_BIOE_128L = mean(flag_completed_BIOE_128L, na.rm = TRUE), 
    rate_flag_completed_BIOE_155L = mean(flag_completed_BIOE_155L, na.rm = TRUE),
    rate_flag_completed_BIOE_151A = mean(flag_completed_BIOE_151A, na.rm = TRUE), 
    rate_flag_completed_BIOE_141L = mean(flag_completed_BIOE_141L, na.rm = TRUE),
    .groups = "drop"
  )

# 6) Modeling frame
gender_year_df <- gender_gap_year %>%
  dplyr::left_join(gender_preds_year, by = c("Value","Year"))

# 7) Histogram, GLM, DHARMa
ggplot(gender_year_df, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Gender × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

glm_gender_year <- glm(
  ThesisGap ~ mean_avg_time_pct_field_s + mean_GPA_scaled +
    rate_flag_completed_BIOE_82 + rate_flag_completed_BIOE_128L + rate_flag_completed_BIOE_155L + 
    rate_flag_completed_BIOE_155L + rate_flag_completed_BIOE_151A + rate_flag_completed_BIOE_141L,
  data    = gender_year_df,
  weights = Total,
  family  = gaussian("identity")
)
summary(glm_gender_year)

sim_gender_year <- DHARMa::simulateResiduals(glm_gender_year, n = 1000)
plot(sim_gender_year)


#### ETHNICITY SPECIIFIC GAP ####
# ---- Ethnicity × Year ThesisGap
eth_gap_year <- ms2y %>%
  dplyr::filter(!is.na(ET.Ethnic.Group.Category)) %>%
  dplyr::count(Value = ET.Ethnic.Group.Category, Year, completed_BIOE_195, name = "n") %>%
  dplyr::mutate(Thesis = ifelse(as.character(completed_BIOE_195) == "1", "Yes", "No")) %>%
  dplyr::select(-completed_BIOE_195) %>%
  tidyr::pivot_wider(names_from = Thesis, values_from = n, values_fill = list(n = 0)) %>%
  dplyr::mutate(
    ThesisYes = dplyr::coalesce(.data$Yes, 0L),
    ThesisNo  = dplyr::coalesce(.data$No,  0L)
  ) %>%
  dplyr::select(-dplyr::any_of(c("Yes","No"))) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    Total      = ThesisNo + ThesisYes,
    DemRate    = Total / sum(Total),
    ThesisRate = ifelse(sum(ThesisYes) > 0, ThesisYes / sum(ThesisYes), 0),
    ThesisGap  = ThesisRate - DemRate
  ) %>%
  dplyr::ungroup()

# ---- Predictors aggregated to Ethnicity × Year (incl. course rates)
eth_preds_year <- ms2y %>%
  dplyr::filter(!is.na(ET.Ethnic.Group.Category)) %>%
  dplyr::group_by(Value = ET.Ethnic.Group.Category, Year) %>%
  dplyr::summarise(
    mean_n_field_courses        = mean(n_field_courses, na.rm = TRUE),
    mean_avg_time_pct_field_s   = mean(avg_time_pct_field_s, na.rm = TRUE),
    mean_GPA_scaled             = mean(GPA_scaled, na.rm = TRUE),
    rate_flag_completed_BIOE_82   = mean(flag_completed_BIOE_82,   na.rm = TRUE),
    rate_flag_completed_BIOE_128L = mean(flag_completed_BIOE_128L, na.rm = TRUE),
    rate_flag_completed_BIOE_141L = mean(flag_completed_BIOE_141L, na.rm = TRUE),
    rate_flag_completed_BIOE_151A = mean(flag_completed_BIOE_151A, na.rm = TRUE),
    rate_flag_completed_BIOE_155L = mean(flag_completed_BIOE_155L, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Modeling frame
eth_year_df <- eth_gap_year %>%
  dplyr::left_join(eth_preds_year, by = c("Value","Year"))

# ---- Histogram, GLM, DHARMa
ggplot(eth_year_df, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Ethnicity × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

min_gap <- min(eth_year_df$ThesisGap, na.rm = TRUE)
eth_year_df$ThesisGap_log <- log10(eth_year_df$ThesisGap - min_gap + 0.5)

ggplot(eth_year_df, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Ethnicity × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

glm_eth_year <- glm(
  ThesisGap_log ~ mean_avg_time_pct_field_s + mean_GPA_scaled +
    rate_flag_completed_BIOE_82 + rate_flag_completed_BIOE_128L +
    rate_flag_completed_BIOE_141L + rate_flag_completed_BIOE_151A +
    rate_flag_completed_BIOE_155L,
  data    = eth_year_df,
  weights = Total,
  family  = gaussian("identity")
)
summary(glm_eth_year)

sim_eth_year <- DHARMa::simulateResiduals(glm_eth_year, n = 1000)
plot(sim_eth_year)

library(lmtest); library(sandwich)
coeftest(glm_eth_year, vcov. = vcovHC(glm_eth_year, type = "HC3"))


# ---- EOP × Year ThesisGap
eop_gap_year <- ms2y %>%
  dplyr::filter(!is.na(SGC.Is.EOP)) %>%
  dplyr::count(Value = SGC.Is.EOP, Year, completed_BIOE_195, name = "n") %>%
  dplyr::mutate(Thesis = ifelse(as.character(completed_BIOE_195) == "1", "Yes", "No")) %>%
  dplyr::select(-completed_BIOE_195) %>%
  tidyr::pivot_wider(names_from = Thesis, values_from = n, values_fill = list(n = 0)) %>%
  dplyr::mutate(
    ThesisYes = dplyr::coalesce(.data$Yes, 0L),
    ThesisNo  = dplyr::coalesce(.data$No,  0L)
  ) %>%
  dplyr::select(-dplyr::any_of(c("Yes","No"))) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    Total      = ThesisNo + ThesisYes,
    DemRate    = Total / sum(Total),
    ThesisRate = ifelse(sum(ThesisYes) > 0, ThesisYes / sum(ThesisYes), 0),
    ThesisGap  = ThesisRate - DemRate
  ) %>%
  dplyr::ungroup()

# ---- Predictors aggregated to EOP × Year (course rates + summaries)
eop_preds_year <- ms2y %>%
  dplyr::filter(!is.na(SGC.Is.EOP)) %>%
  dplyr::group_by(Value = SGC.Is.EOP, Year) %>%
  dplyr::summarise(
    mean_avg_time_pct_field_s   = mean(avg_time_pct_field_s, na.rm = TRUE),
    mean_GPA_scaled             = mean(GPA_scaled, na.rm = TRUE),
    rate_flag_completed_BIOE_82   = mean(flag_completed_BIOE_82,   na.rm = TRUE),
    rate_flag_completed_BIOE_128L = mean(flag_completed_BIOE_128L, na.rm = TRUE),
    rate_flag_completed_BIOE_141L = mean(flag_completed_BIOE_141L, na.rm = TRUE),
    rate_flag_completed_BIOE_151A = mean(flag_completed_BIOE_151A, na.rm = TRUE),
    rate_flag_completed_BIOE_155L = mean(flag_completed_BIOE_155L, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Modeling frame
eop_year_df <- eop_gap_year %>%
  dplyr::left_join(eop_preds_year, by = c("Value","Year"))

# ---- Histogram, GLM, DHARMa
ggplot(eop_year_df, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — EOP × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

min_gap <- min(eop_year_df$ThesisGap, na.rm = TRUE)
eop_year_df$ThesisGap_log <- log10(eop_year_df$ThesisGap - min_gap + 0.5)

ggplot(eop_year_df, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Ethnicity × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

glm_eop_year <- glm(
  ThesisGap_log ~ mean_avg_time_pct_field_s + mean_GPA_scaled +
    rate_flag_completed_BIOE_82 + rate_flag_completed_BIOE_128L +
    rate_flag_completed_BIOE_141L + rate_flag_completed_BIOE_151A +
    rate_flag_completed_BIOE_155L,
  data    = eop_year_df,
  weights = Total,
  family  = gaussian("identity")
)
summary(glm_eop_year)

sim_eop_year <- DHARMa::simulateResiduals(glm_eop_year, n = 1000)
plot(sim_eop_year)



# ---- First-Gen × Year ThesisGap
fg_gap_year <- ms2y %>%
  dplyr::filter(!is.na(UAD.First.Generation)) %>%
  dplyr::count(Value = UAD.First.Generation, Year, completed_BIOE_195, name = "n") %>%
  dplyr::mutate(Thesis = ifelse(as.character(completed_BIOE_195) == "1", "Yes", "No")) %>%
  dplyr::select(-completed_BIOE_195) %>%
  tidyr::pivot_wider(names_from = Thesis, values_from = n, values_fill = list(n = 0)) %>%
  dplyr::mutate(
    ThesisYes = dplyr::coalesce(.data$Yes, 0L),
    ThesisNo  = dplyr::coalesce(.data$No,  0L)
  ) %>%
  dplyr::select(-dplyr::any_of(c("Yes","No"))) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    Total      = ThesisNo + ThesisYes,
    DemRate    = Total / sum(Total),
    ThesisRate = ifelse(sum(ThesisYes) > 0, ThesisYes / sum(ThesisYes), 0),
    ThesisGap  = ThesisRate - DemRate
  ) %>%
  dplyr::ungroup()


# ---- Predictors aggregated to First-Gen × Year
fg_preds_year <- ms2y %>%
  dplyr::filter(!is.na(UAD.First.Generation)) %>%
  dplyr::group_by(Value = UAD.First.Generation, Year) %>%
  dplyr::summarise(
    mean_avg_time_pct_field_s   = mean(avg_time_pct_field_s, na.rm = TRUE),
    mean_GPA_scaled             = mean(GPA_scaled, na.rm = TRUE),
    rate_flag_completed_BIOE_82   = mean(flag_completed_BIOE_82,   na.rm = TRUE),
    rate_flag_completed_BIOE_128L = mean(flag_completed_BIOE_128L, na.rm = TRUE),
    rate_flag_completed_BIOE_141L = mean(flag_completed_BIOE_141L, na.rm = TRUE),
    rate_flag_completed_BIOE_151A = mean(flag_completed_BIOE_151A, na.rm = TRUE),
    rate_flag_completed_BIOE_155L = mean(flag_completed_BIOE_155L, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Modeling frame
fg_year_df <- fg_gap_year %>%
  dplyr::left_join(fg_preds_year, by = c("Value","Year"))

# ---- Histogram, GLM, DHARMa
ggplot(fg_year_df, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — First-Gen × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

min_gap <- min(fg_year_df$ThesisGap, na.rm = TRUE)
fg_year_df$ThesisGap_log <- log10(fg_year_df$ThesisGap - min_gap + 0.5)

ggplot(fg_year_df, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Ethnicity × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

glm_fg_year <- glm(
  ThesisGap_log ~ mean_avg_time_pct_field_s + mean_GPA_scaled +
    rate_flag_completed_BIOE_82 + rate_flag_completed_BIOE_128L +
    rate_flag_completed_BIOE_141L + rate_flag_completed_BIOE_151A +
    rate_flag_completed_BIOE_155L,
  data    = fg_year_df,
  weights = Total,
  family  = gaussian("identity")
)
summary(glm_fg_year)

sim_fg_year <- DHARMa::simulateResiduals(glm_fg_year, n = 1000)
plot(sim_fg_year)



# ---- Admit Type × Year ThesisGap
admit_gap_year <- ms2y %>%
  dplyr::filter(!is.na(UAD.Admit.Type.Descr)) %>%
  dplyr::count(Value = UAD.Admit.Type.Descr, Year, completed_BIOE_195, name = "n") %>%
  dplyr::mutate(Thesis = ifelse(as.character(completed_BIOE_195) == "1", "Yes", "No")) %>%
  dplyr::select(-completed_BIOE_195) %>%
  tidyr::pivot_wider(names_from = Thesis, values_from = n, values_fill = list(n = 0)) %>%
  dplyr::mutate(
    ThesisYes = dplyr::coalesce(.data$Yes, 0L),
    ThesisNo  = dplyr::coalesce(.data$No,  0L)
  ) %>%
  dplyr::select(-dplyr::any_of(c("Yes","No"))) %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    Total      = ThesisNo + ThesisYes,
    DemRate    = Total / sum(Total),
    ThesisRate = ifelse(sum(ThesisYes) > 0, ThesisYes / sum(ThesisYes), 0),
    ThesisGap  = ThesisRate - DemRate
  ) %>%
  dplyr::ungroup()

# ---- Predictors aggregated to Admit Type × Year
admit_preds_year <- ms2y %>%
  dplyr::filter(!is.na(UAD.Admit.Type.Descr)) %>%
  dplyr::group_by(Value = UAD.Admit.Type.Descr, Year) %>%
  dplyr::summarise(
    mean_avg_time_pct_field_s   = mean(avg_time_pct_field_s, na.rm = TRUE),
    mean_GPA_scaled             = mean(GPA_scaled, na.rm = TRUE),
    rate_flag_completed_BIOE_82   = mean(flag_completed_BIOE_82,   na.rm = TRUE),
    rate_flag_completed_BIOE_128L = mean(flag_completed_BIOE_128L, na.rm = TRUE),
    rate_flag_completed_BIOE_141L = mean(flag_completed_BIOE_141L, na.rm = TRUE),
    rate_flag_completed_BIOE_151A = mean(flag_completed_BIOE_151A, na.rm = TRUE),
    rate_flag_completed_BIOE_155L = mean(flag_completed_BIOE_155L, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Modeling frame
admit_year_df <- admit_gap_year %>%
  dplyr::left_join(admit_preds_year, by = c("Value","Year"))

# ---- Histogram, GLM, DHARMa
ggplot(admit_year_df, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Admit Type × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

min_gap <- min(admit_year_df$ThesisGap, na.rm = TRUE)
admit_year_df$ThesisGap_log <- log10(admit_year_df$ThesisGap - min_gap + 0.5)

ggplot(admit_year_df, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "ThesisGap — Ethnicity × Year", x = "ThesisGap", y = "Count") +
  theme_minimal(13)

glm_admit_year <- glm(
  ThesisGap_log ~ mean_avg_time_pct_field_s + mean_GPA_scaled +
    rate_flag_completed_BIOE_82 + rate_flag_completed_BIOE_128L +
    rate_flag_completed_BIOE_141L + rate_flag_completed_BIOE_151A +
    rate_flag_completed_BIOE_155L,
  data    = admit_year_df,
  weights = Total,
  family  = gaussian("identity")
)
summary(glm_admit_year)

sim_admit_year <- DHARMa::simulateResiduals(glm_admit_year, n = 1000)
plot(sim_admit_year)


