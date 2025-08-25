######################################### HOUSEKEEPING ##########################################

suppressPackageStartupMessages({
  library(tidyverse)   
  library(broom)
  library(nnet)
  library(car)
  library(lme4)
  library(lmerTest)
  library(glmmTMB)
  library(DHARMa)
  library(mediation)
  library(lavaan)
  library(patchwork)
  library(ggrepel)
  library(ggthemes)
  library(ggsankey)
  library(multicool)
  library(circlize)
  library(colorspace) 
  library(ggbreak)
  
})

# Global plot theme
theme_set(theme_minimal())

## =========================
## PATHS & DATA
## =========================
setwd("~/Documents")
#Load in Raw files from Infoview 
degree             <- read.csv("./PhD Documents/Senior Thesis Project/data/Degree.csv",      header=TRUE, sep=",")
non_degree         <- read.csv("./PhD Documents/Senior Thesis Project/data/Non-Degree.csv", header=TRUE, sep=",")
quarter_performance<- read.csv("./PhD Documents/Senior Thesis Project/data/QuarterPerformance.csv", header=TRUE, sep=",")
quarter_info       <- read.csv("./PhD Documents/Senior Thesis Project/data/QuarterInfo_v2_feb5.csv", header=TRUE, sep=",")
class_performance  <- read.csv("./PhD Documents/Senior Thesis Project/data/ClassPerformance_v2_apr22.csv", header=TRUE, sep=",")
offerings          <- read.csv("./PhD Documents/Senior Thesis Project/data/Offerings.csv", header=TRUE, sep=",")


# =========================
# COURSE LIST & QUICK QC
# =========================
## Course list of interest (used for counts + flags)
all_courses <- c(
  "BIOE 20F","BIOE 82","BIOE 128L","BIOE 141L","BIOE 155L","BIOE 158L","BIOE 145L",
  "BIOE 157A","BIOE 161L","BIOE 159A","BIOE 150L","BIOE 151A","ENVS 107A","BIOE 153A",
  "BIOE 245L","BIOE 195","BIOE 85","ENVS 104A"
)

## Quick occurrence check
course_counts <- sapply(all_courses, \(crs) sum(class_performance$SE.Subject.Catalog.Nbr == crs, na.rm = TRUE))
print(course_counts)

# Hand-entered enrollments used to establish plotting order
bioe_courses <- tibble(
  Course   = all_courses,
  Students = c(453,1293,253,260,73,167,157,28,422,429,67,313,555,11,0,189,133,351)
)

# Simple gradient for the bar plot; order by student count (small → large)
plot_order <- bioe_courses %>% arrange(Students) %>% pull(Course)
grad_fn    <- colorRampPalette(c("#7b2cbf","#f28482","#ffd166","#1f78b4"))
manual_cols_course <- setNames(grad_fn(length(plot_order)), plot_order)

bioe_courses_plot <- bioe_courses %>%
  mutate(Course = factor(Course, levels = plot_order))

ggplot(bioe_courses_plot, aes(x = Course, y = Students, fill = Course)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = manual_cols_course) +
  labs(x = "Course", y = "Number of Students") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

# =========================
# BUILD COURSE FLAGS PER ROW & MERGE
# =========================
## Add per-row completion flags (Yes/No) for every course in all_courses
final_class_performance <- reduce(
  all_courses,
  .init = class_performance,
  .f = \(df, course) {
    nm <- paste0("completed_", gsub(" ", "_", course))
    mutate(df, !!nm := if_else(SE.Subject.Catalog.Nbr == course, "Yes", "No"))
  }
)

# Merge quarter info with performance to get per-term attributes on each row
quarter_data    <- left_join(quarter_info, quarter_performance, by = c("CodedID","TA.Term.Code"))

# Link degree to quarter data (per major), keep TA.Term.Code character for joins
degree_quarter  <- left_join(degree, quarter_data, by = c("CodedID","PLNA.Plan.Descr")) %>%
  mutate(TA.Term.Code = as.character(TA.Term.Code))

# Join row-level completion flags onto the degree/term table
bioe_195_final  <- left_join(
  degree_quarter,
  final_class_performance %>% mutate(TA.Term.Code = as.character(TA.Term.Code)),
  by = c("CodedID","TA.Term.Code")
)

write.csv(bioe_195_final, file.path(data_dir, "bioe_195_final.csv"), row.names = FALSE)

## Restrict to majors of interest
bioe_students <- bioe_195_final %>%
  filter(
    PLNA.Plan.Descr %in% c("Ecology and Evolution BS","Marine Biology BS","Biology BS","Plant Sciences BS"),
    Term_Units_Earned != 0
  )

## Quick QC
length(unique(bioe_students$CodedID))
#Check if there are multiple instructors per term
message("Unique instructor per term: ", !any(bioe_students$multiple_instructors))


# =========================
# BASIC DISTRIBUTIONS
# =========================

# Get one record per student with last (graduation) term -> academic year
# TA.Term.Code format: YYYQ where YYY + 1800 = year; Q in {0,2,4,8}
latest_term <- bioe_students %>%
  group_by(CodedID) %>%
  filter(TA.Term.Code == max(TA.Term.Code, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    year_raw  = as.numeric(substr(TA.Term.Code, 1, 3)) + 1800,
    quarter   = as.numeric(substr(TA.Term.Code, 4, 4)),
    # Academic year: count Fall (8) toward the next year (e.g., Fall 2023 -> AY 2024)
    academic_year = ifelse(quarter == 8, year_raw + 1, year_raw)
  )

# Mark who completed a thesis with Yes/No
thesis_yes_ids <- bioe_students %>%
  filter(completed_BIOE_195 == "Yes") %>%
  distinct(CodedID) %>%
  mutate(completed_thesis = "Yes")

# Attach Yes/No thesis status to each student's latest term record
students_with_status <- latest_term %>%
  left_join(thesis_yes_ids, by = "CodedID") %>%
  mutate(completed_thesis = ifelse(is.na(completed_thesis), "No", completed_thesis))

# Stacked share of thesis completion by academic year
yearly_counts <- students_with_status %>%
  count(academic_year, completed_thesis, name = "n") %>%
  group_by(academic_year) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(academic_year, completed_thesis)

ggplot(yearly_counts, aes(x = factor(academic_year), y = n / total, fill = completed_thesis)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("Yes" = "#005f73", "No" = "#94d2bd"),
    labels = c("Yes" = "Thesis Completed", "No" = "No Thesis")
  ) +
  labs(
    x = "Academic Year",
    y = "Percentage of Students",
    fill = "Completed Thesis",
    title = "Senior Thesis Completion by Academic Year",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right"
  )

# =========================
# PARTICIPATION GAP (overall + by year)
# =========================
##OVERALL 
# Demographic columns for gap plots (renamed for clarity)
dems <- c("Gender","Race/Ethnicity","Status","Admit Type","First Gen","EOP")

bioe_students_clean <- latest_term %>%
  filter(!is.na(completed_BIOE_195)) %>%
  rename(
    "Gender"          = "PD.Gender...Current",
    "Race/Ethnicity"  = "ET.Ethnic.Group.Category",
    "Status"          = "PD.Domestic.International...Current",
    "Admit Type"      = "UAD.Admit.Type.Descr",
    "First Gen"       = "UAD.First.Generation",
    "EOP"             = "SGC.Is.EOP"
  ) %>%
  select(all_of(dems), "completed_BIOE_195", "academic_year")

# Aggregate across all years: DemRate (representation), ThesisRate (participation),
# and the gap = ThesisRate - DemRate per demographic level.
agg_all <- purrr::map_dfr(dems, function(var) {
  bioe_students_clean %>%
    group_by(value = .data[[var]], completed_BIOE_195) %>%
    summarize(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = completed_BIOE_195, values_from = n, names_prefix = "Thesis") %>%
    replace_na(list(ThesisYes = 0)) %>%                       # if a group has 0 completers
    mutate(Total = ThesisNo + ThesisYes,
           DemRate = Total / sum(Total),
           ThesisRate = ThesisYes / sum(ThesisYes),
           ThesisGap = ThesisRate - DemRate,
           Demographic = var)
}) %>% rename(Value = value)

# Categorical color palette keyed to *group levels* (used for bars/lines)
color_palette <- c(
  # EOP
  "Y" = "#231942", "N" = "#e0b1cb",
  # Admit Type
  "First-Time Freshman" = "#99e2b4", "Junior Transfer" = "#678d58",
  "Senior Transfer"     = "#a6c48a", "Sophomore Transfer" = "#10451d",
  # First Gen
  "First Generation" = "#184e77", "Not First Generation" = "#52b69a",
  # Citizenship Status
  "Domestic" = "#d76a03", "International" = "#fabc3c",
  # Gender
  "F" = "#bbd0ff", "M" = "#58148e", "X" = "#c18cf9", "U" = "#ffc4ff",
  # Ethnicity
  "African-American/Black" = "#e76f51", "American Indian/Alaska Native" = "#f4a261",
  "Asian" = "#e9c46a", "Hispanic/Latino" = "#a65211",
  "Native Hawaiian/Oth Pac Island" = "#81400e", "Other" = "#2a9d8f",
  "Unknown/Not Specified" = "#76c893", "White/Caucasian" = "#264653"
)

# Side labels data (two columns: Thesis participation vs Demographic representation)
labels_long <- agg_all %>%
  transmute(Demographic, Value, `Thesis` = ThesisRate, `Rep.` = DemRate) %>%
  pivot_longer(c(`Thesis`, `Rep.`), names_to = "Metric", values_to = "pct") %>%
  mutate(Metric = factor(Metric, levels = c("Thesis", "Rep.")))

# Main gap bars (x = participation gap; facet by demographic)
p_gap <- ggplot(agg_all, aes(y = Value, x = ThesisGap, fill = Value)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  labs(x = "Participation Gap", y = "") +
  scale_x_continuous(labels = scales::label_percent(style_positive = "plus")) +
  scale_fill_manual(values = color_palette) +
  ggthemes::theme_few(base_size = 12) +
  theme(strip.background   = element_rect(fill = "grey80", color = NA),
        strip.text.y.left  = element_text(size = 12, face = "bold"),
        axis.text.y        = element_text(size = 12),
        axis.title.x       = element_text(size = 12),
        plot.title         = element_text(size = 13, face = "bold")) +
  facet_grid(Demographic ~ ., scales = "free_y", space = "free", switch = "y")

# Side panel with percentages (shown above the panel)
p_pct <- ggplot(labels_long, aes(y = Value, x = Metric, label = scales::percent(pct, accuracy = 1))) +
  geom_text(size = 5, color = "grey15") +
  scale_x_discrete(position = "top") +
  facet_grid(Demographic ~ ., scales = "free_y", space = "free", switch = "y") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(panel.grid        = element_blank(),
        axis.title        = element_blank(),
        axis.text.y       = element_blank(),
        axis.ticks        = element_blank(),
        strip.text.y.left = element_blank(),
        strip.background  = element_blank(),
        axis.text.x       = element_text(color = "black", face = "bold", size = 12),
        plot.margin       = margin(t = 12, r = 0, b = 5, l = 6))

# Compose: left = gaps; right = participation/representation labels
p_gap + p_pct + patchwork::plot_layout(widths = c(4, 1))

###BY YEAR 

# Create a function to repeat pipeline for each demographic
agg_all_yr <- purrr::map_dfr(dems, function(var) {
  bioe_students_clean %>%
    group_by(value = .data[[var]], completed_BIOE_195,Year) %>%
    summarize(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = completed_BIOE_195, values_from = n, names_prefix = "Thesis") %>%
    replace_na(list(ThesisYes = 0)) %>%  #remove demographic groups that didn't do a senior thesis
    mutate(Total = ThesisNo + ThesisYes,
           DemRate = Total / sum(Total),
           ThesisRate = ThesisYes / sum(ThesisYes),
           ThesisGap = ThesisRate - DemRate,
           Demographic = var)  # Add a column for demographic
}) %>%
  rename(Value = value)  # Rename for clarity

# Labels at the last observed year for each group (optional)
label_df <- agg_all_yr %>%
  group_by(Demographic, Value) %>%
  filter(Year == max(Year)) %>%
  ungroup()

#Plot 
ggplot(agg_all_yr, aes(y = ThesisGap, x = Year, col = Value)) +
  geom_point(show.legend = FALSE, size = 3.5) +
  geom_path(show.legend = FALSE, size = 1.3) +
  #  geom_text_repel(data = label_df,
  #                  aes(label = Value),
  #                  direction = "y",
  #                  nudge_x = 0.5,
  #                  segment.color = "grey50",
  #                  show.legend = FALSE,
  #                  max.overlaps = 100,
  #                  size = 2.5) +
  geom_hline(yintercept = 0) +
  labs(y = "Participation Gap", x = "Year") +
  scale_y_continuous(labels = scales::label_percent(style_positive = "plus")) +
  scale_x_continuous(breaks = 2008:2024) +
  scale_color_manual(values = color_palette) +
  theme_few() +
  theme(strip.background = element_rect(fill = "grey80", color = NA),
        axis.text.x = element_text(angle = 90, hjust = 0.5, size = 12),
        title = element_text(size = 10)) +
  facet_wrap(. ~ Demographic)


# =========================
# FIELD-COURSE PARTICIPATION ASSOCIATIONS
# =========================

# Keep only rows with non-missing thesis indicator for per-student flags
BIOE_students_na_removed <- bioe_students %>%
  filter(!is.na(completed_BIOE_195))

# Course set for student-level flags
mn_courses <- c("BIOE 82","BIOE 85","BIOE 128L","BIOE 141L","BIOE 151A",
                "BIOE 153A","BIOE 155L","BIOE 157A","BIOE 159A","BIOE 161L",
                "BIOE 150L","ENVS 107A","ENVS 104A")
mn_flag_cols <- paste0("completed_", gsub(" ", "_", mn_courses))

# Student-level: "ever completed <course>" (0/1) + thesis outcome (0/1)
student_flags <- BIOE_students_na_removed %>%
  group_by(CodedID) %>%
  summarize(
    completed_BIOE_195 = any(completed_BIOE_195 == "Yes"),
    across(all_of(mn_flag_cols), ~ any(. == "Yes"), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("completed_"), as.integer))

course_cols <- setdiff(grep("^completed_", names(student_flags), value = TRUE), "completed_BIOE_195")

# Proportions of BIOE 195 completion by course flag
students_flags_long <- student_flags %>%
  select(completed_BIOE_195, all_of(course_cols)) %>%
  pivot_longer(cols = -completed_BIOE_195, names_to = "Course", values_to = "Completed")

ggplot(students_flags_long, aes(x = factor(Completed), fill = factor(completed_BIOE_195))) +
  geom_bar(position = "fill") +
  facet_wrap(~ Course, ncol = 4) +
  scale_fill_manual(values = c("0" = "#e0b1cb", "1" = "#231942")) +
  labs(x = "Course Completion (0=No, 1=Yes)", y = "Proportion of BIOE 195 Completion", fill = "BIOE 195 Completion") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))

# Quick contingency tables (thesis × course flag)
contingency_tables <- lapply(course_cols, function(col) {
  cat("\nContingency table for:", col, "\n")
  tbl <- table(student_flags$completed_BIOE_195, student_flags[[col]])
  print(tbl); tbl
})

# =========================
# MULTINOMIAL LOGISTIC REGRESSION & CHORD PLOT
# =========================

# Recode outcome to factor (0/1) 
student_flags <- student_flags %>%
  mutate(completed_BIOE_195 = factor(completed_BIOE_195, levels = c(0,1)))

# Multinomial model: thesis completion ~ set of course flags
model_formula <- as.formula(paste("completed_BIOE_195 ~", paste(course_cols, collapse = " + ")))
multi_mod_all_courses <- nnet::multinom(model_formula, data = student_flags)

summary(multi_mod_all_courses)

# OR table (95% CI), flagged large ORs for review only
or_table <- broom::tidy(multi_mod_all_courses, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), OR_low = exp(conf.low), OR_high = exp(conf.high)) %>%
  select(term, OR, OR_low, OR_high, p.value)
print(or_table)

suspect_ORs <- or_table %>% filter(is.finite(OR), OR >= 20 | OR_low >= 10)
if (nrow(suspect_ORs)) {
  message("⚠️ Very large ORs detected (OR≥20 or lower CI≥10):")
  print(suspect_ORs)
}

# Plot significant effects only (exclude BIOE_153A per your original rule)
significant_results <- or_table %>%
  filter(p.value < 0.05, term != "completed_BIOE_153A")

ggplot(significant_results, aes(x = term, y = OR, ymin = OR_low, ymax = OR_high)) +
  geom_pointrange(color = "#34495E", size = 1.5, shape = 21, fill = "#e0b1cb", fatten = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1.2) +
  coord_flip() +
  labs(x = "Predictor", y = "Odds Ratio", title = "Significant Odds Ratios for Predicting BIOE 195 Completion") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14))

# Chord matrix from significant course terms → {Senior Thesis, No Senior Thesis}
model_coefficients <- tidy(multi_mod_all_courses) %>%
  filter(term != "(Intercept)", p.value < 0.05, term != "completed_BIOE_153A")

chord_matrix <- t(sapply(model_coefficients$term, function(col) {
  if (col %in% names(student_flags)) {
    log_odds_coef <- model_coefficients$estimate[model_coefficients$term == col]
    prob_bioe <- exp(log_odds_coef) / (1 + exp(log_odds_coef))
    prob_no   <- 1 - prob_bioe
    c(BIOE_195 = prob_bioe, No_BIOE_195 = prob_no)
  } else c(BIOE_195 = NA, No_BIOE_195 = NA)
})) %>% na.omit()

# Normalize rows to 100% and label axes
chord_matrix <- chord_matrix / rowSums(chord_matrix) * 100
rownames(chord_matrix) <- gsub("completed_", "", rownames(chord_matrix))
colnames(chord_matrix) <- c("Senior Thesis", "No Senior Thesis")

# Course + outcome colors for chord plot
manual_colors_chord <- c(
  "Senior Thesis" = "#1D84B5", "No Senior Thesis" = "#999999",
  "BIOE_82" = "#17BEBB", "BIOE_128L" = "#747C92",
  "BIOE_141L" = "#CD5334", "BIOE_151A" = "#372772", "BIOE_155L" = "#FCBCB8"
)

# Build link color vector keyed by "SOURCE->TARGET".
link_cols <- sapply(rownames(chord_matrix), function(x) manual_colors_chord[x])
names(link_cols) <- paste(rownames(chord_matrix), "Senior Thesis", sep = "->")
link_cols_no <- link_cols; names(link_cols_no) <- paste(rownames(chord_matrix), "No Senior Thesis", sep = "->")
link_cols <- c(link_cols, link_cols_no)

# Specify colors for all sector grids (rows + columns)
grid_cols <- manual_colors_chord[union(rownames(chord_matrix), colnames(chord_matrix))]

circos.clear() #Clears plotting area from previous plot 

#Plot chord plot 
circos.par(start.degree = 90, #start degree for rotated starting point
           gap.degree = 3, #gap degree for small gaps between sectors
           canvas.xlim = c(-1.3, 1.3), canvas.ylim = c(-1.3, 1.3)) #canvas limits to make sure labels don't cut off
chordDiagram(chord_matrix, grid.col = grid_cols, #colors 
             col = link_cols, 
             transparency = 0.25, #soften overlap for readability 
             annotationTrack = "grid", #draw ring 
             preAllocateTracks = list(track.height = 0.08)) # make more space for labels 

# Add labels on the outer ring 
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  sec <- get.cell.meta.data("sector.index"); xlim <- get.cell.meta.data("xlim")
  label <- sec; font_style <- 1
  if (sec == "No Senior Thesis") { label <- "No\nSenior Thesis"; font_style <- 2 } #Change labels to No Senior thesis w/ break 
  if (sec == "Senior Thesis")    { label <- "Senior\nThesis";     font_style <- 2 } #Change labels for BIOE_195 to Senior Thesis w/ break 
  circos.text(mean(xlim), 1.08, label, facing = "clockwise", niceFacing = TRUE, cex = 1.0, font = font_style, adj = c(0, 0.5))
}, bg.border = NA)

## =========================
## DEGREE-TIMING & FIELD-COURSES
## =========================

# Function to extract year and quarter
get_year <- function(tc) as.integer(substr(tc, 1, 3)) + 1800
get_quarter <- function(tc) as.integer(case_when(
  substr(tc, 4, 4) == "8" ~ 1,
  substr(tc, 4, 4) == "0" ~ 2,
  substr(tc, 4, 4) == "2" ~ 3,
  substr(tc, 4, 4) == "4" ~ 4,
  TRUE ~ NA_integer_
))

# For each enrollment row: compute position in degree (% of total quarters to grad)
time_completed_summary <- bioe_195_final %>%
  mutate(
    admit_year  = get_year(UAD.Admit.Term.Code),
    admit_q     = get_quarter(UAD.Admit.Term.Code),
    course_year = get_year(TA.Term.Code),
    course_q    = get_quarter(TA.Term.Code),
    grad_year   = get_year(DG.Completion.Term.Code),
    grad_q      = get_quarter(DG.Completion.Term.Code),
    quarters_since_admit = (course_year - admit_year) * 4 + (course_q - admit_q),
    total_quarters       = (grad_year   - admit_year) * 4 + (grad_q    - admit_q),
    course_percentage_degree = quarters_since_admit / total_quarters
  )

# Timing summary (global)
avg_course_pct <- time_completed_summary %>%
  group_by(SE.Subject.Catalog.Nbr) %>%
  reframe(
    mean_pct = mean(course_percentage_degree, na.rm = TRUE),
    lq       = quantile(course_percentage_degree, 0.25, na.rm = TRUE),
    uq       = quantile(course_percentage_degree, 0.75, na.rm = TRUE),
    n        = n(),
    Course   = SE.Subject.Catalog.Nbr,
    .groups  = "drop"
  ) %>%
  mutate(Course = reorder(Course, mean_pct))

# Reversed gradient & timing plot limited to course_set
course_set <- bioe_courses %>% arrange(Students) %>% pull(Course)
manual_colors1 <- set_names(rev(colorRampPalette(c("#1f78b4","#ffd166","#f28482","#7b2cbf"))(length(course_set))), course_set)

# Filter to courses of interest and order factor by mean timing for plotting
avg_course_pct_cs <- avg_course_pct %>%
  dplyr::filter(Course %in% course_set) %>%
  dplyr::mutate(Course = forcats::fct_reorder(Course, mean_pct))

#Plot mean timing for each course w/ gradient 
ggplot(avg_course_pct_cs, aes(x = Course, y = mean_pct)) +
  geom_errorbar(aes(ymin = lq, ymax = uq), width = 0.12, linewidth = 0.3, color = "black") +
  geom_point(aes(size = n, color = Course), stroke = 0, alpha = 0.95) +
  scale_color_manual(values = manual_colors1, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(title = "When Students Take High-Impact Courses", x = "Course", y = "% of Degree Completed") +
  theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 11))

# Field course count distribution (restricted to course_set, including zero)
full_counts <- bioe_students %>%
  distinct(CodedID) %>%
  left_join(
    bioe_195_final %>%
      filter(SE.Subject.Catalog.Nbr %in% setdiff(course_set, "BIOE 195")) %>%
      group_by(CodedID) %>%
      summarise(n_field_courses = n_distinct(SE.Subject.Catalog.Nbr), .groups = "drop"),
    by = "CodedID"
  ) %>%
  mutate(n_field_courses = replace_na(n_field_courses, 0L))

#Distribution of counts across students (n, proportion, percent label)
field_dist_cs <- full_counts %>%
  count(n_field_courses, name = "n_students") %>%
  mutate(prop = n_students / sum(n_students), pct = scales::percent(prop))

#Manual color palette 
field_palette_5 <- c(
  "#540b0e",  
  "#bb3e03",  
  "#ca6702",
  "#ee9b00",
  "#fff3b0"   
)
count_levels <- sort(unique(field_dist_cs$n_field_courses)) #Sort based on number of courses 
field_cols <- set_names(rep_len(field_palette_5, length(count_levels)), count_levels)

#Plot: bar heights = number of students; labels = share of all students
ggplot(field_dist_cs, aes(x = factor(n_field_courses), y = n_students, fill = factor(n_field_courses))) +
  geom_col(color = "grey70", linewidth = 0.3) +
  geom_text(aes(label = pct), vjust = -0.5, size = 4, color = "grey20") +
  scale_fill_manual(values = field_cols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "# Field Courses", y = "# Students") +
  theme(panel.grid.minor = element_blank())

## =========================
## PRIOR-COURSE INDICATOR & GLMM
## =========================

# 1) Student-level outcome: ever took BIOE 195 (0/1)
student_summary1 <- time_completed_summary %>%
  dplyr::filter(SE.Student.Enrollment.Status.Code == "E") %>%
  dplyr::group_by(CodedID) %>%
  dplyr::summarise(
    completed_BIOE_195 = as.integer(any(SE.Subject.Catalog.Nbr == "BIOE 195")),
    .groups = "drop"
  )

# 2) BIOE 195 attempts: instructor & term (for timing match)
bioe195_inst <- time_completed_summary %>%
  dplyr::filter(SE.Student.Enrollment.Status.Code == "E",
                SE.Subject.Catalog.Nbr == "BIOE 195") %>%
  dplyr::mutate(
    CodedID              = as.character(CodedID),
    bioe195_instructor   = as.character(CI.Instructor.Name),
    TA.Term.Code_bioe195 = as.integer(TA.Term.Code)
  ) %>%
  dplyr::select(CodedID, bioe195_instructor, TA.Term.Code_bioe195)

# 3) Other enrollments for those students (prep for same-instructor match)
history_all <- time_completed_summary %>%
  dplyr::filter(SE.Student.Enrollment.Status.Code == "E",
                SE.Subject.Catalog.Nbr != "BIOE 195",
                CodedID %in% bioe195_inst$CodedID) %>%
  dplyr::mutate(
    CodedID            = as.character(CodedID),
    prior_instructor   = as.character(CI.Instructor.Name),
    TA.Term.Code_prior = as.integer(TA.Term.Code),
    prior_course       = as.character(SE.Subject.Catalog.Nbr)
  ) %>%
  dplyr::select(CodedID, prior_instructor, TA.Term.Code_prior, prior_course) %>%
  dplyr::distinct()

# 4) For each BIOE 195 attempt, keep the earliest prior course w/ same instructor
prior_match_details <- history_all %>%
  dplyr::inner_join(
    bioe195_inst,
    by = dplyr::join_by(
      CodedID,
      TA.Term.Code_prior < TA.Term.Code_bioe195,
      prior_instructor == bioe195_instructor
    ),
    relationship = "many-to-many"
  ) %>%
  dplyr::group_by(CodedID, TA.Term.Code_bioe195) %>%
  dplyr::slice_min(TA.Term.Code_prior, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(CodedID, prior_instructor, TA.Term.Code_prior, prior_course, TA.Term.Code_bioe195)

# 5) Student-level flag: any prior same-instructor course exists?
student_match_full <- student_summary1 %>%
  dplyr::left_join(prior_match_details, by = "CodedID") %>%
  dplyr::mutate(had_prior_course = !is.na(prior_instructor))

# Quick checkx
table(student_match_full$had_prior_course, useNA = "ifany")

## =========================
## STUDENT-SPECIFIC MODELS  
## =========================

## 1) Ever-completed flags + first-timing of each field course
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
    # timing = when they first took each (proportion of degree)
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

## 2) Count distinct field courses taken
field_courses <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 151A","BIOE 155L")

# Assumes `field_courses` is a character vector of the BIOE field-course codes to count
field_summary <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E",
         SE.Subject.Catalog.Nbr %in% field_courses) %>%
  group_by(CodedID) %>%
  summarise(n_field_courses = n_distinct(SE.Subject.Catalog.Nbr), .groups = "drop") %>%
  right_join(distinct(time_completed_summary, CodedID), by = "CodedID") %>%
  mutate(n_field_courses = replace_na(n_field_courses, 0L)) # fill missing counts with 0

## 3) Demographics (one row per student)
demo_vars <- bioe_students %>%
  distinct(CodedID,
           UAD.Admit.Type.Descr,
           PD.Gender...Current,
           SGC.Is.EOP,
           PD.Domestic.International...Current,
           ET.Ethnic.Group.Category,
           UAD.First.Generation,
           TA.GPA...Cumulative)

## 4) Assemble student-level model frame
model_students <- student_summary %>%
  left_join(field_summary,      by = "CodedID") %>%        # add # field courses
  left_join(student_match_full, by = "CodedID") %>%        # add prior-instructor flag
  left_join(demo_vars,          by = "CodedID") %>%        # add demographics
  rename_with(~ sub("\\.y$", "", .), ends_with(".y")) %>%  # clean join suffixes
  select(-ends_with(".x")) %>%
  mutate(
    avg_time_pct_field   = rowMeans(across(starts_with("time_pct_")), na.rm = TRUE), # mean timing
    avg_time_pct_field_s = as.numeric(scale(avg_time_pct_field)),                    # scaled timing
    completed_BIOE_195   = factor(completed_BIOE_195, levels = c(0, 1)),             # 0/1 outcome as factor
    across(c(UAD.Admit.Type.Descr, PD.Gender...Current, SGC.Is.EOP,
             PD.Domestic.International...Current, ET.Ethnic.Group.Category,
             UAD.First.Generation, TA.GPA...Cumulative), factor)                     # categorical covariates
  ) %>%
  filter(!is.na(completed_BIOE_195))                                                 # keep valid outcome rows


# ========================
# Clean levels + set references
# ========================
model_students2 <- model_students %>%
  filter( # drop unknown/sparse levels that cause instability
    !ET.Ethnic.Group.Category %in% c("Unknown/Not Specified", "Native Hawaiian/Oth Pac Island"),
    !UAD.First.Generation %in% c("Unknown/Other"),
    !PD.Gender...Current %in% c("X")
  ) %>%
  mutate(
    GPA_scaled = scale(as.numeric(TA.GPA...Cumulative))[, 1],                        # scale GPA
    had_prior_course = factor(had_prior_course, levels = c(FALSE, TRUE)),            # 0/1 factor
    # reference (baseline) levels used for contrasts
    UAD.Admit.Type.Descr     = fct_relevel(UAD.Admit.Type.Descr, "First-Time Freshman"),
    PD.Gender...Current      = fct_relevel(PD.Gender...Current,  "F"),
    SGC.Is.EOP               = fct_relevel(SGC.Is.EOP,           "N"),
    ET.Ethnic.Group.Category = fct_relevel(ET.Ethnic.Group.Category, "White/Caucasian"),
    UAD.First.Generation     = fct_relevel(UAD.First.Generation, "Not First Generation")
  )


# ========================
# Main GLM: thesis ~ count × timing + demographics + GPA
# ========================
glm_student2_clean <- glm(
  completed_BIOE_195 ~ n_field_courses * avg_time_pct_field_s +
    UAD.Admit.Type.Descr + PD.Gender...Current + SGC.Is.EOP +
    ET.Ethnic.Group.Category + UAD.First.Generation + GPA_scaled,
  data   = model_students2,
  family = binomial("logit")
)
summary(glm_student2_clean)

# Diagnostics (shape/dispersion/zeros)
sim <- simulateResiduals(glm_student2_clean, n = 1000)
plot(sim); testDispersion(sim); testZeroInflation(sim)

# ORs for readability (labels on key terms)
or1_clean <- broom::tidy(glm_student2_clean, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high)
  ) %>%
  mutate(term = case_when(
    term == "n_field_courses"                      ~ "Number of Field Courses",
    term == "avg_time_pct_field_s"                 ~ "Avg. Timing of Field Courses",
    term == "n_field_courses:avg_time_pct_field_s" ~ "Field Courses × Timing",
    term == "GPA_scaled"                           ~ "GPA (scaled)",
    TRUE ~ term
  ))

ggplot(or1_clean, aes(x = OR, y = term, xmin = OR_low, xmax = OR_high)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  scale_x_log10() +
  labs(x = "Odds Ratio (log scale)", y = NULL, title = "Predictors of BIOE 195 Completion") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

car::vif(glm_student2_clean)  # multicollinearity check


# ========================
# Per-course timing GLM (Took + Timing per course, plus GPA)
# ========================
field_courses_core <- c("BIOE 82","BIOE 128L","BIOE 141L","BIOE 151A","BIOE 155L")
courses_timing     <- str_replace(field_courses_core, " ", "_")

model_students2b <- model_students2 %>%
  mutate(across(
    .cols  = all_of(paste0("completed_", courses_timing)),
    .fns   = ~ as.integer(. == 1L),
    .names = "flag_{col}"                                      # binary “took” flags
  )) %>%
  mutate(across(
    .cols  = all_of(paste0("time_pct_", courses_timing)),
    .fns   = list(zero = ~ ifelse(get(sub("time_pct_","completed_", cur_column())) == 1, .x, 0)),
    .names = "{col}"                                           # timing = 0 if course not taken
  ))

int_terms  <- map_chr(courses_timing, ~ paste0("flag_","completed_",.x," + ","time_pct_",.x))  # took + timing per course
fml_all    <- as.formula(paste("completed_BIOE_195 ~", paste(c(int_terms, "GPA_scaled"), collapse = " + ")))

glm_all_courses <- glm(fml_all, data = model_students2b, family = binomial("logit"))
summary(glm_all_courses)

# Tidy + relabel (Took vs Timing)
or_all <- broom::tidy(glm_all_courses, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high),
    term    = dplyr::recode(term,
                            "flag_completed_BIOE_82"   = "Took BIOE 82",
                            "time_pct_BIOE_82"         = "Timing BIOE 82",
                            "flag_completed_BIOE_128L" = "Took BIOE 128L",
                            "time_pct_BIOE_128L"       = "Timing BIOE 128L",
                            "flag_completed_BIOE_141L" = "Took BIOE 141L",
                            "time_pct_BIOE_141L"       = "Timing BIOE 141L",
                            "flag_completed_BIOE_151A" = "Took BIOE 151A",
                            "time_pct_BIOE_151A"       = "Timing BIOE 151A",
                            "flag_completed_BIOE_155L" = "Took BIOE 155L",
                            "time_pct_BIOE_155L"       = "Timing BIOE 155L",
                            "GPA_scaled"               = "GPA (scaled)")
  )

# Plot (use your manual/timing palettes)
ggplot(or_all, aes(x = OR, y = term, xmin = OR_low, xmax = OR_high)) +
  geom_point(size = 3) +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") +
  scale_x_log10() +
  labs(title = "Course-specific Effects", x = "Odds Ratio (log scale)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold"))


## =========================
## PARTICIPATION GAP MODELS
## =========================
# Filter out unstable/unknown levels to stabilize downstream aggregation/models
ms2 <- model_students2b %>%
  filter(PD.Gender...Current != "X",
         !ET.Ethnic.Group.Category %in% c("Native Hawaiian/Oth Pac Island","Unknown/Not Specified"))

# Derive one "Year" per student (prefer graduation year; otherwise last course year)
year_by_student <- time_completed_summary %>%
  filter(SE.Student.Enrollment.Status.Code == "E") %>%
  group_by(CodedID) %>%
  summarise(
    Year_grad   = suppressWarnings(max(grad_year, na.rm = TRUE)),   # may be NA/Inf
    Year_course = suppressWarnings(max(course_year, na.rm = TRUE)), # fallback
    .groups = "drop"
  ) %>%
  mutate(
    Year_grad   = ifelse(is.infinite(Year_grad), NA_integer_, as.integer(Year_grad)),
    Year_course = ifelse(is.infinite(Year_course), NA_integer_, as.integer(Year_course)),
    Year        = coalesce(Year_grad, Year_course)                  # final Year per student
  ) %>% dplyr::select(CodedID, Year)

# Attach Year to ms2; if a Year col already exists, coalesce with joined Year; drop helper
ms2y <- ms2 %>%
  left_join(year_by_student %>% rename(Year_join = Year), by = "CodedID") %>%
  { if ("Year" %in% names(.)) mutate(., Year = coalesce(Year, Year_join)) else rename(., Year = Year_join) } %>%
  dplyr::select(-any_of("Year_join")) %>%
  filter(!is.na(Year))

# reusable builder for gap frames and predictor joins
build_gap_df <- function(df, group_var) {
  # Build (Value × Year) participation gap table
  gap <- df %>%
    dplyr::filter(!is.na(.data[[group_var]])) %>%
    dplyr::count(Value = .data[[group_var]], Year, completed_BIOE_195, name = "n") %>%   # counts by status
    dplyr::mutate(Thesis = ifelse(as.character(completed_BIOE_195) == "1","Yes","No")) %>% # label yes/no
    dplyr::select(-completed_BIOE_195) %>%
    tidyr::pivot_wider(names_from = Thesis, values_from = n, values_fill = 0) %>%         # wide: Yes/No cols
    dplyr::mutate(ThesisYes = dplyr::coalesce(Yes,0L),
                  ThesisNo  = dplyr::coalesce(No, 0L)) %>%
    dplyr::select(-tidyr::any_of(c("Yes","No"))) %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(
      Total      = ThesisNo + ThesisYes,                         # group size in that year
      DemRate    = Total / sum(Total),                           # representation share
      tot_yes    = sum(ThesisYes),                               # total thesis completers that year
      ThesisRate = ifelse(tot_yes > 0, ThesisYes / tot_yes, 0),  # share among thesis completers
      ThesisGap  = ThesisRate - DemRate                          # over/under-representation
    ) %>%
    dplyr::select(-tot_yes) %>%
    dplyr::ungroup()
  
  # Aggregate predictors to the same Value × Year grain
  preds <- df %>%
    dplyr::filter(!is.na(.data[[group_var]])) %>%
    dplyr::group_by(Value = .data[[group_var]], Year) %>%
    dplyr::summarise(
      mean_n_field_courses      = mean(n_field_courses, na.rm = TRUE),
      mean_avg_time_pct_field_s = mean(avg_time_pct_field_s, na.rm = TRUE),
      mean_GPA_scaled           = mean(GPA_scaled, na.rm = TRUE),
      dplyr::across(starts_with("flag_completed_"),
                    ~ mean(.x, na.rm = TRUE), .names = "rate_{.col}"),   # course completion rates
      .groups = "drop"
    )
  
  # Join gap with predictors for modeling
  dplyr::left_join(gap, preds, by = c("Value","Year"))
}

# 2) Transform ThesisGap (match your old choices)
transform_gap <- function(df, method = c("none","asinh","log10shift")) {
  method <- match.arg(method)
  if (method == "asinh") {
    df$ThesisGap_asinh <- asinh(df$ThesisGap)                 # symmetric stretch for +/- gaps
  } else if (method == "log10shift") {
    min_gap <- min(df$ThesisGap, na.rm = TRUE)
    df$ThesisGap_log <- log10(df$ThesisGap - min_gap + 0.5)   # shift to positive, log-compress
  }
  df
}

# 3) Fit lmer with the same fixed effects you used
fit_gap_lmer <- function(df, response = c("ThesisGap","ThesisGap_asinh","ThesisGap_log"),
                         random = "(1 | Year)") {
  response <- match.arg(response)
  fml_fixed <- paste(
    response, "~",
    paste(c(
      "mean_avg_time_pct_field_s","mean_GPA_scaled",
      "rate_flag_completed_BIOE_82","rate_flag_completed_BIOE_128L",
      "rate_flag_completed_BIOE_141L","rate_flag_completed_BIOE_151A",
      "rate_flag_completed_BIOE_155L"
    ), collapse = " + "),
    "+"
  )
  fml <- as.formula(paste(fml_fixed, random))  # add random intercept for Year
  lme4::lmer(fml, data = df)
}

# 4) One wrapper for each block (build → transform → optional row drop → model)
run_gap_block <- function(df, group_var,
                          transform = c("none","asinh","log10shift"),
                          drop_idx = NULL,
                          response_name = c("ThesisGap","ThesisGap_asinh","ThesisGap_log"),
                          random = "(1 | Year)") {
  
  g <- build_gap_df(df, group_var)                    # construct gap + predictors
  g <- transform_gap(g, match.arg(transform))         # apply requested transform
  if (!is.null(drop_idx) && length(drop_idx)) {
    g <- g[-drop_idx, , drop = FALSE]                 # optional row removal to mirror prior script
  }
  resp <- match.arg(response_name)
  m <- fit_gap_lmer(g, response = resp, random = random)  # fit mixed model
  list(df = g, model = m)
}

### Reproduce models for all demographic group ###

# Gender

# Preview raw distribution
g_gender <- build_gap_df(ms2y, "PD.Gender...Current")
ggplot(g_gender, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "Gender × Year: ThesisGap (raw)",
       x = "Thesis participation gap (ThesisRate − DemRate)", y = "Count") +
  theme_minimal(13)
# Preview transformed distribution (asinh)
g_gender$ThesisGap_asinh <- asinh(g_gender$ThesisGap)
ggplot(g_gender, aes(ThesisGap_asinh)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "Gender × Year: ThesisGap (asinh)",
       x = "asinh(ThesisGap)", y = "Count") +
  theme_minimal(13)

# Fit model
gender_res <- run_gap_block(
  df = ms2y,
  group_var = "PD.Gender...Current",
  transform = "asinh",
  response_name = "ThesisGap_asinh",
  random = "(1 | Year)"
)
gender_year_df <- gender_res$df
m_gender_lmer  <- gender_res$model
summary(m_gender_lmer); plot(DHARMa::simulateResiduals(m_gender_lmer, n = 1000))

# Ethnicity

# Preview raw distribution
g_eth <- build_gap_df(ms2y, "ET.Ethnic.Group.Category")
ggplot(g_eth, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "Ethnicity × Year: ThesisGap (raw)",
       x = "Thesis participation gap (ThesisRate − DemRate)", y = "Count") +
  theme_minimal(13)
# Preview transformed distribution (log10 shift)
min_gap_eth <- min(g_eth$ThesisGap, na.rm = TRUE)
g_eth$ThesisGap_log <- log10(g_eth$ThesisGap - min_gap_eth + 0.5)
ggplot(g_eth, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "Ethnicity × Year: ThesisGap (log10-shifted)",
       x = "log10(ThesisGap − min + 0.5)", y = "Count") +
  theme_minimal(13)

# Fit model 
eth_res <- run_gap_block(
  df = ms2y,
  group_var = "ET.Ethnic.Group.Category",
  transform = "log10shift",
  drop_idx = 19,
  response_name = "ThesisGap_log",
  random = "(1 | Year)"
)
eth_year_df <- eth_res$df
m_eth_lmer  <- eth_res$model
summary(m_eth_lmer); plot(DHARMa::simulateResiduals(m_eth_lmer, n = 1000))

# EOP

# Preview raw distribution
g_eop <- build_gap_df(ms2y, "SGC.Is.EOP")
ggplot(g_eop, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "EOP × Year: ThesisGap (raw)",
       x = "Thesis participation gap (ThesisRate − DemRate)", y = "Count") +
  theme_minimal(13)
# Preview transformed distribution (log10 shift)
min_gap_eop <- min(g_eop$ThesisGap, na.rm = TRUE)
g_eop$ThesisGap_log <- log10(g_eop$ThesisGap - min_gap_eop + 0.5)
ggplot(g_eop, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "EOP × Year: ThesisGap (log10-shifted)",
       x = "log10(ThesisGap − min + 0.5)", y = "Count") +
  theme_minimal(13)

# Fit model 
eop_res <- run_gap_block(
  df = ms2y,
  group_var = "SGC.Is.EOP",
  transform = "log10shift",
  response_name = "ThesisGap_log",
  random = "(1 | Year)"
)
eop_year_df <- eop_res$df
m_eop_lmer  <- eop_res$model
summary(m_eop_lmer); plot(DHARMa::simulateResiduals(m_eop_lmer, n = 1000))


# First-Gen

# Preview raw distribution
g_fg <- build_gap_df(ms2y, "UAD.First.Generation")
ggplot(g_fg, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "First-Gen × Year: ThesisGap (raw)",
       x = "Thesis participation gap (ThesisRate − DemRate)", y = "Count") +
  theme_minimal(13)
# Preview transformed distribution (log10 shift)
min_gap_fg <- min(g_fg$ThesisGap, na.rm = TRUE)
g_fg$ThesisGap_log <- log10(g_fg$ThesisGap - min_gap_fg + 0.5)
ggplot(g_fg, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "First-Gen × Year: ThesisGap (log10-shifted)",
       x = "log10(ThesisGap − min + 0.5)", y = "Count") +
  theme_minimal(13)

# Fit model 
fg_res <- run_gap_block(
  df = ms2y,
  group_var = "UAD.First.Generation",
  transform = "log10shift",
  response_name = "ThesisGap_log",
  random = "(1 | Year)"
)
fg_year_df <- fg_res$df
m_fg_lmer  <- fg_res$model
summary(m_fg_lmer); plot(DHARMa::simulateResiduals(m_fg_lmer, n = 1000))


# Admit type

# Preview raw distribution
g_admit <- build_gap_df(ms2y, "UAD.Admit.Type.Descr")
ggplot(g_admit, aes(ThesisGap)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "Admit Type × Year: ThesisGap (raw)",
       x = "Thesis participation gap (ThesisRate − DemRate)", y = "Count") +
  theme_minimal(13)
# Preview transformed distribution (log10 shift)
min_gap_adm <- min(g_admit$ThesisGap, na.rm = TRUE)
g_admit$ThesisGap_log <- log10(g_admit$ThesisGap - min_gap_adm + 0.5)
ggplot(g_admit, aes(ThesisGap_log)) +
  geom_histogram(bins = 15, color = "white") +
  labs(title = "Admit Type × Year: ThesisGap (log10-shifted)",
       x = "log10(ThesisGap − min + 0.5)", y = "Count") +
  theme_minimal(13)

# Fit model 
admit_res <- run_gap_block(
  df = ms2y,
  group_var = "UAD.Admit.Type.Descr",
  transform = "log10shift",
  response_name = "ThesisGap_log",
  random = "(1 | Year)"
)
admit_year_df <- admit_res$df
m_admit_lmer  <- admit_res$model
summary(m_admit_lmer); plot(DHARMa::simulateResiduals(m_admit_lmer, n = 1000))


## =========================
## SCATTER OVERLAYS
## =========================
# ETHNICITY × YEAR: quick visual checks for linear trends vs GPA/course rates
eth_year_df %>%
  filter(is.finite(ThesisGap), is.finite(mean_GPA_scaled)) %>%
  ggplot(aes(x = mean_GPA_scaled, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Ethnicity × Year: ThesisGap vs Mean GPA (scaled)",
       x = "Mean GPA (scaled)", y = "Thesis participation gap (ThesisRate − DemRate)")

eth_year_df %>%
  filter(is.finite(ThesisGap), is.finite(rate_flag_completed_BIOE_82)) %>%
  ggplot(aes(x = rate_flag_completed_BIOE_82, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Ethnicity × Year: ThesisGap vs BIOE 82 completion rate",
       x = "BIOE 82 completion rate", y = "Thesis participation gap (ThesisRate − DemRate)")

eth_year_df %>%
  filter(is.finite(ThesisGap), is.finite(rate_flag_completed_BIOE_141L)) %>%
  ggplot(aes(x = rate_flag_completed_BIOE_141L, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Ethnicity × Year: ThesisGap vs BIOE 141L completion rate",
       x = "BIOE 141L completion rate", y = "Thesis participation gap (ThesisRate − DemRate)")

eth_year_df %>%
  filter(is.finite(ThesisGap), is.finite(rate_flag_completed_BIOE_151A)) %>%
  ggplot(aes(x = rate_flag_completed_BIOE_151A, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Ethnicity × Year: ThesisGap vs BIOE 151A completion rate",
       x = "BIOE 151A completion rate", y = "Thesis participation gap (ThesisRate − DemRate)")

# EOP × YEAR (GPA): visual association check
eop_year_df %>%
  filter(is.finite(ThesisGap), is.finite(mean_GPA_scaled)) %>%
  ggplot(aes(x = mean_GPA_scaled, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "EOP × Year: ThesisGap vs Mean GPA (scaled)",
       x = "Mean GPA (scaled)", y = "Thesis participation gap (ThesisRate − DemRate)")

# FIRST-GEN × YEAR (BIOE 82 rate): visual association check
fg_year_df %>%
  filter(is.finite(ThesisGap), is.finite(rate_flag_completed_BIOE_82)) %>%
  ggplot(aes(x = rate_flag_completed_BIOE_82, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "First-Gen × Year: ThesisGap vs BIOE 82 completion rate",
       x = "BIOE 82 completion rate", y = "Thesis participation gap (ThesisRate − DemRate)")

# ADMIT TYPE × YEAR (BIOE 151A rate): visual association check
admit_year_df %>%
  filter(is.finite(ThesisGap), is.finite(rate_flag_completed_BIOE_151A)) %>%
  ggplot(aes(x = rate_flag_completed_BIOE_151A, y = ThesisGap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "grey25", alpha = 0.9, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Admit Type × Year: ThesisGap vs BIOE 151A completion rate",
       x = "BIOE 151A completion rate", y = "Thesis participation gap (ThesisRate − DemRate)")