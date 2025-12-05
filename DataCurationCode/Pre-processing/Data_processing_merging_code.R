## =========================
## HOUSEKEEPING
## =========================
# Load required packages for data cleaning, modeling, visualization, and summaries
# Startup messages are suppressed for cleaner output
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(here)
  library(janitor)
  library(broom)
  library(patchwork)
  library(ggthemes)
  library(scales)
  library(forcats)
  library(stringr)
  library(MuMIn)
  library(DHARMa)
  library(brglm)
  library(nnet)
  library(sandwich)
  library(lmtest)
  library(emmeans)
})

# Set a consistent base plotting theme for all figures
theme_set(theme_minimal())

# Establish project working directory
setwd("~/Documents/PhD Documents/Senior Thesis Project")

## =========================
## PATHS & DATA
## =========================
# Load cleaned institutional datasets representing student trajectories,
# academic performance, course-level records, and field course metadata.
degree              <- read.csv("data/Degree.csv", header=TRUE, sep=",")
quarter_performance <- read.csv("data/QuarterPerformance.csv", header=TRUE, sep=",")
quarter_info        <- read.csv("data/QuarterInfo_v2_feb5.csv", header=TRUE, sep=",")
class_performance   <- read.csv("data/ClassPerformance_v2_apr22.csv", header=TRUE, sep=",")
field_course_info   <- read.csv("data/fieldcourse_info_sep10.csv", header=TRUE, sep=",")


## =========================
## CONSTANTS & PALETTES
## =========================
# Define the focal majors included in all downstream analyses.
majors_keep <- c(
  "Ecology and Evolution BS","Marine Biology BS","Biology BS",
  "Plant Sciences BS","Environmental Science BS"
)

# Define the set of field courses used to quantify field-based learning experiences.
# BIOE 195 is excluded from this list because it represents the thesis course.
field_courses_core <- c(
  "BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 151A",
  "BIOE 153A","BIOE 155L","BIOE 157A","BIOE 159A","BIOE 161L",
  "BIOE 150L","ENVS 107A","ENVS 104A"
)

# Define all courses for which per-row completion flags are created.
all_courses <- c(
  "BIOE 20F","BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 158L",
  "BIOE 145L","BIOE 157A","BIOE 161L","BIOE 159A","BIOE 150L","BIOE 151A",
  "ENVS 107A","BIOE 153A","BIOE 195","ENVS 104A"
)

## =========================
## HELPER FUNCTIONS
## =========================
# Convert UCSC term codes to calendar years.
# Handles both 4-digit (legacy) and 5-digit formats.
get_year <- function(tc) {
  s <- as.character(tc)
  n <- nchar(s)
  y4 <- suppressWarnings(as.integer(substr(s, 1, 4)))
  y3 <- suppressWarnings(as.integer(substr(s, 1, 3))) + 1800L
  ifelse(n == 5L, y4, ifelse(n == 4L, y3, NA_integer_))
}

# Convert the final digit of a term code into a quarter index (Fall=0, Winter=1, Spring=2, Summer=3).
# Enables construction of linear time indices.
get_quarter <- function(tc) {
  s <- as.character(tc)
  q <- substr(s, nchar(s), nchar(s))
  dplyr::case_when(
    q == "8" ~ 0L,
    q == "0" ~ 1L,
    q == "2" ~ 2L,
    q == "4" ~ 3L,
    TRUE ~ NA_integer_
  )
}

# Build a linear term index defined by year * 4 + quarter.
# Used for timing and progression analyses.
get_term_index <- function(tc) {
  y <- get_year(tc)
  qi <- get_quarter(tc)
  ifelse(is.na(y) | is.na(qi), NA_integer_, y * 4L + qi)
}

# Percent labels rounded to one decimal place (used in plots).
percent1 <- function(x) scales::percent(x, accuracy = 1)

## =========================
## BUILD PER-ROW COURSE FLAGS & MERGE
## =========================
# Create Yes/No indicators for whether each row corresponds to each course of interest.
# These per-row flags are later aggregated to create student-level "ever completed" indicators.
final_class_performance <- purrr::reduce(
  all_courses,
  .init = class_performance,
  .f = \(df, course) {
    nm <- paste0("completed_", gsub(" ", "_", course))
    mutate(df, !!nm := if_else(SE.Subject.Catalog.Nbr == course, "Yes", "No"))
  }
)

#Normalize before merging 
quarter_info <- quarter_info %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code))
quarter_performance <- quarter_performance %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code))

# Merge quarter metadata with performance data using student ID and term code.
quarter_data <- left_join(
  quarter_info,
  quarter_performance,
  by = c("CodedID","TA.Term.Code")
)

# Merge major/degree data with quarter-level data.
degree_quarter <- left_join(
  degree,
  quarter_data,
  by = c("CodedID","PLNA.Plan.Descr")
) %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code))

# Attach course completion flags to the merged student-term dataset.
bioe_195_final <- left_join(
  degree_quarter,
  final_class_performance %>% mutate(TA.Term.Code = as.character(TA.Term.Code)),
  by = c("CodedID","TA.Term.Code")
)

# Restrict dataset to selected majors and remove zero-unit placeholder rows.
bioe_students <- bioe_195_final %>%
  filter(PLNA.Plan.Descr %in% majors_keep, Term_Units_Earned != 0)

write.csv(bioe_students, "bioe_students.csv", row.names = FALSE)

## =========================
## FIELD-COURSE ATTRIBUTES & JOIN
## =========================
# Standardize field course metadata and prepare for merging.
field_lu <- field_course_info %>%
  transmute(
    SE.Subject.Catalog.Nbr = str_trim(as.character(SE.Subject.Catalog.Nbr)),
    TotalFieldHours        = as.numeric(Total.field.hours),
    Independent.projects   = str_trim(as.character(Independent.projects)),
    TotalUnits             = as.numeric(Units),
    StudentsinClass        = as.numeric(Students.per.Quarter),
    Intensity              = as.factor(Intensity)
  )

# Attach course-level field attributes to student enrollment records.
bioe_students <- bioe_students %>%
  mutate(SE.Subject.Catalog.Nbr = str_trim(as.character(SE.Subject.Catalog.Nbr))) %>%
  left_join(field_lu, by = "SE.Subject.Catalog.Nbr")
