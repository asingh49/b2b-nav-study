library(tidyverse)
library(effsize)
library(car)
library(ggplot2)
library(scales)

cat("========================================\n")
cat("B2B SaaS NAVIGATION MENU VALIDATION\n")

set.seed(6)

# TASKS (B2B enterprise workflows):
# Task 1: Export dashboard to PDF for stakeholder presentation
# Task 2: Create custom metric calculation
# Task 3: Configure automated alert for data threshold
# Task 4: Share dashboard with team (permissions management)
# Task 5: Schedule recurring report delivery

# Design A: Sidebar Navigation
# Hypothesis: Better for complex tasks, frequent switching
design_a <- tibble(
  participant_id = 1:15,
  design = "Sidebar",
  
  # Task-level data (5 tasks per participant)
  task1_completed = rbinom(15, 1, prob = 0.87),  # Easy task
  task2_completed = rbinom(15, 1, prob = 0.73),  # Medium
  task3_completed = rbinom(15, 1, prob = 0.67),  # Hard (alerts)
  task4_completed = rbinom(15, 1, prob = 0.80),  # Medium
  task5_completed = rbinom(15, 1, prob = 0.73),  # Medium
  
  # Time per task (seconds)
  task1_time = rgamma(15, shape = 2, scale = 8),
  task2_time = rgamma(15, shape = 3, scale = 12),
  task3_time = rgamma(15, shape = 4, scale = 15),
  task4_time = rgamma(15, shape = 2.5, scale = 10),
  task5_time = rgamma(15, shape = 3, scale = 11),
  
  # Errors (wrong clicks before success)
  total_errors = rpois(15, lambda = 2.3),
  
  # Post-test measures
  sus_score = rnorm(15, mean = 72, sd = 10),
  ease_rating = rnorm(15, mean = 3.6, sd = 0.7),  # 1-5 scale
  
  # Demographics (B2B users)
  user_role = sample(c("Data Analyst", "Product Manager", "Business Analyst"), 15,
                     replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  experience_level = sample(c("Junior", "Mid-level", "Senior"), 15,
                            replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  platform_tenure = sample(c("<6mo", "6-12mo", ">1yr"), 15,
                           replace = TRUE, prob = c(0.2, 0.3, 0.5))
)


# Design B: Top Bar Navigation
# Hypothesis: Faster for simple tasks, more familiar pattern
design_b <- tibble(
  participant_id = 16:30,
  design = "TopBar",
  
  task1_completed = rbinom(15, 1, prob = 0.93),  # Better on easy
  task2_completed = rbinom(15, 1, prob = 0.87),  # Better on medium
  task3_completed = rbinom(15, 1, prob = 0.60),  # Worse on hard (flat structure)
  task4_completed = rbinom(15, 1, prob = 0.87),  
  task5_completed = rbinom(15, 1, prob = 0.80),
  
  task1_time = rgamma(15, shape = 1.8, scale = 7),   # Faster
  task2_time = rgamma(15, shape = 2.5, scale = 10),
  task3_time = rgamma(15, shape = 4.5, scale = 18),  # Slower on hard task
  task4_time = rgamma(15, shape = 2, scale = 9),
  task5_time = rgamma(15, shape = 2.8, scale = 10),
  
  total_errors = rpois(15, lambda = 1.8),  # Fewer errors
  
  sus_score = rnorm(15, mean = 78, sd = 9),  # Higher perceived usability
  ease_rating = rnorm(15, mean = 4.1, sd = 0.6),
  
  user_role = sample(c("Data Analyst", "Product Manager", "Business Analyst"), 15,
                     replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  experience_level = sample(c("Junior", "Mid-level", "Senior"), 15,
                            replace = TRUE, prob = c(0.2, 0.5, 0.3)),
  platform_tenure = sample(c("<6mo", "6-12mo", ">1yr"), 15,
                           replace = TRUE, prob = c(0.2, 0.3, 0.5))
)

# Combine and calculate aggregate metrics (completion_rate, avg_time, total_completed)
df <- bind_rows(design_a, design_b) %>% 
  mutate(
    # Overall completion rate
    total_completed = task1_completed + task2_completed + task3_completed +
      task4_completed + task5_completed,
    completion_rate = total_completed / 5 * 100,
    
    # Average time across tasks
    avg_time = (task1_time + task2_time +task3_time + task4_time + task5_time) / 5,
    
    #cap scales
    sus_score = pmax(0, pmin(100, sus_score)),
    ease_rating = pmax(1, pmin(5, ease_rating)),
    
    #factor
    design = factor(design,levels = c("Sidebar", "TopBar"))
    
  )

# Save
write_csv(df, "b2b_navigation_study.csv")
cat("✓ Data created: b2b_navigation_study.csv (n=30)\n\n")


# ============================================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================================

completion_rate <- df %>% 
  group_by(design) %>% 
  reframe(
    mean_completion = mean(completion_rate),

total_completed = task1_completed + task2_completed + task3_completed +
  task4_completed + task5_completed,
completion_rate = total_completed / 5 * 100
)

#---


desc_stats <- df %>% 
  group_by(design) %>% 
  summarise(
    n = n(),
  
    # Completion
    mean_completion = mean(completion_rate),
    sd_completion = sd(completion_rate),
    
    # Task-level breakdown
    task1_success = mean(task1_completed) * 100,
    task2_success = mean(task2_completed) * 100,
    task3_success = mean(task3_completed) * 100,
    task4_success = mean(task4_completed) * 100,
    task5_success = mean(task5_completed) * 100,
    
    #Time
    mean_time = mean(avg_time),
    sd_time = sd(avg_time),
    
    #Errors
    mean_errors = mean(total_errors),
    sd_errors = sd(total_errors),
    
    #SUS
    mean_sus = mean(sus_score),
    sd_sus = sd(sus_score),
    
    #Ease
    mean_ease = mean(ease_rating),
    sd_ease = mean(ease_rating),
    
    # Removes the grouping structure after you're done summarizing.
    .groups = "drop"
    
) %>% 
  # Round numeric columns to 2 decimal places
  mutate(across(where(is.numeric), ~round(., 2)))

completion_diff <- desc_stats$mean_completion[2] - desc_stats$mean_completion[1]
sus_diff <- desc_stats$mean_sus[2] - desc_stats$mean_sus[1]
time_diff <- desc_stats$mean_time[1] - desc_stats$mean_time[2]

cat("\n--- Key Differences (TopBar - Sidebar) ---\n")
cat("Completion Rate:  ", 
    ifelse(completion_diff > 0, "+", ""), 
    round(completion_diff, 1), " pp\n", sep = "")
cat("SUS Score:        +", round(sus_diff, 1), " points\n")
cat("Avg Time:         ", 
    ifelse(time_diff > 0, "-", "+"),
    round(abs(time_diff), 1), " seconds\n\n")

# ============================================================================
# 4. ASSUMPTION CHECKS FOR T-TEST
# ============================================================================

cat("\n========================================\n")
cat("ASSUMPTION CHECKS FOR T-TEST\n")
cat("========================================\n\n")

# 4.1 Normality (Shapiro-Wilk test)
cat("--- 1. Normality (Shapiro-Wilk Test) ---\n")
cat("Question: Is the data normally distributed in each group?\n")
cat("Why it matters: t-test assumes normal distribution\n\n")

# 4.1.1 Sidebar Normality
shapiro_sidebar <- shapiro.test(df$completion_rate[df$design == "Sidebar"])
# 4.1.2 Topbar Normality
shapiro_topbar <- shapiro.test(df$completion_rate[df$design == "TopBar"])

#Pick up here 1/26 

#Check Sidebar Normality ✓ Normal (p > .05) p = 0.07
cat("Sidebar:  W =", round(shapiro_sidebar$statistic, 3),
    ", p =", round(shapiro_sidebar$p.value, 3))
if(shapiro_sidebar$p.value > 0.05) {
  cat(" ✓ Normal (p > .05)\n")
} else {
  cat(" ✗ Non-normal (p < .05)\n")
}
# Check TopBar Normality (✗ Non-normal (p < .05)) p = 0.003
cat("TopBar:   W =", round(shapiro_topbar$statistic, 3),
    ", p =", round(shapiro_topbar$p.value, 3))
if(shapiro_topbar$p.value > 0.05) {
  cat(" ✓ Normal (p > .05)\n\n")
} else {
  cat(" ✗ Non-normal (p < .05)\n\n")
}

normality_ok <- shapiro_sidebar$p.value > 0.05 & shapiro_topbar$p.value > 0.05

cat("Interpretation: ")
if(normality_ok) {
  cat("Both groups approximately normal → can use t-test\n\n")
} else {
  cat("Normality violated → should use Mann-Whitney U test instead\n")
  cat("Note: With n=15 per group, t-test is fairly robust to violations\n\n")
}


# 4.2 Homogeneity of variance (Levene's test)
cat("--- 2. Equal Variances (Levene's Test) ---\n")
cat("Question: Do both groups have similar spread/variance?\n")
cat("Why it matters: Standard t-test assumes equal variances\n\n")

levene_result <- leveneTest(completion_rate ~ design, data = df)
levene_p <- levene_result$`Pr(>F)`[1]

cat("F(1, 28) =", round(levene_result$`F value`[1], 3),
    ", p =", round(levene_p, 3))

if(levene_p > 0.05) {
  cat(" ✓ Equal variances (p > .05)\n\n")
} else {
  cat(" ✗ Unequal variances (p < .05)\n\n")
}

variance_ok <- levene_p > 0.05

cat("Interpretation: ")
if(variance_ok) {
  cat("Variances are similar → use standard t-test\n\n")
} else {
  cat("Variances differ → use Welch's t-test (var.equal=FALSE)\n\n")
}

cat("DECISION: Will use ")
if(variance_ok) {
  cat("standard t-test (var.equal=TRUE)\n\n")
} else {
  cat("Welch's t-test (var.equal=FALSE)\n\n")
}


# 5. PRIMARY ANALYSIS: COMPLETION RATE T-TEST

cat("\n========================================\n")
cat("PRIMARY ANALYSIS: COMPLETION RATE\n")
cat("========================================\n\n")

cat("Research Question: Does navigation design affect task completion?\n")
cat("Null Hypothesis (H0): No difference in completion rates\n")
cat("Alternative Hypothesis (H1): Completion rates differ between designs\n")
cat("Significance level: α = 0.05 (two-tailed)\n\n")


completion_test <- t.test(completion_rate ~ design,
                          data = df,
                          var.equal = variance_ok,
                          alternative = "two.sided")

print(completion_test)

t_stat <- completion_test$statistic
df_test <- completion_test$parameter
p_val <- completion_test$p.value
ci_lower <- completion_test$conf.int[1]
ci_upper <- completion_test$conf.int[2]
mean_diff <- diff(completion_test$estimate)

cat("\n--- Statistical Results ---\n")
cat("t(", round(df_test, 0), ") = ", round(t_stat, 3), "\n", sep = "")
cat("p-value = ", format.pval(p_val, digits = 3), "\n", sep = "")
cat("Mean difference: ", round(abs(mean_diff), 2), " percentage points\n", sep = "")
cat("95% Confidence Interval: [", round(ci_lower, 2), ", ", 
    round(ci_upper, 2), "]\n\n", sep = "")

cat("--- Interpretation ---\n")
if(p_val < 0.05) {
  winner <- ifelse(mean_diff < 0, "TopBar", "Sidebar")
  cat("✓ STATISTICALLY SIGNIFICANT (p < .05)\n")
  cat("Conclusion: ", winner, " navigation has significantly ",
      ifelse(mean_diff < 0, "higher", "lower"),
      " completion rate than the alternative.\n", sep = "")
  cat("Decision: Reject null hypothesis\n\n")
} else {
  cat("✗ NOT STATISTICALLY SIGNIFICANT (p ≥ .05)\n")
  cat("Conclusion: No significant difference detected between designs.\n")
  cat("Decision: Fail to reject null hypothesis\n\n")
}

# ============================================================================
# 6. EFFECT SIZE (Cohen's d)
# ============================================================================

cat("========================================\n")
cat("EFFECT SIZE ANALYSIS\n")
cat("========================================\n\n")

cat("Why effect size matters:\n")
cat("• P-value tells us IF there's a difference (statistical significance)\n")
cat("• Effect size tells us HOW BIG the difference is (practical significance)\n")
cat("• Large sample can make tiny differences 'significant'\n")
cat("• Effect size tells us if the difference actually matters\n\n")

cohens_d <- cohen.d(df$completion_rate, df$design)
d_value <- cohens_d$estimate

cat("Cohen's d = ", round(d_value, 3), "\n\n", sep = "")

cat("Interpretation Guidelines:\n")
cat("  |d| < 0.2  → Negligible effect\n")
cat("  |d| < 0.5  → Small effect\n")
cat("  |d| < 0.8  → Medium effect\n")
cat("  |d| ≥ 0.8  → Large effect\n\n")

if(abs(d_value) < 0.2) {
  interpretation <- "Negligible"
} else if(abs(d_value) < 0.5) {
  interpretation <- "Small"
} else if(abs(d_value) < 0.8) {
  interpretation <- "Medium"
} else {
  interpretation <- "Large"
}

cat("This Study: ", interpretation, " effect (d = ", round(d_value, 2), ")\n\n", sep = "")

cat("Practical Meaning:\n")
cat("  • The designs differ by ", round(abs(d_value), 2), 
    " standard deviations\n", sep = "")
cat("  • This is a ", interpretation, " practical difference\n", sep = "")

if(abs(d_value) >= 0.5) {
  cat("  • Large enough to be meaningful in real-world use\n\n")
} else {
  cat("  • May not be noticeable to users in practice\n\n")
}


# ============================================================================
# 7. VISUALIZATIONS
# ============================================================================

cat("========================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("========================================\n\n")

colors <- c("Sidebar" = "#E74C3C", "TopBar" = "#27AE60")

# ============================================================================
# VISUALIZATION 1: MAIN RESULT - COMPLETION RATE COMPARISON
# ============================================================================

cat("Creating main visualization...\n")

# Calculate summary statistics for plotting
plot_data <- df %>%
  group_by(design) %>%
  summarise(
    mean = mean(completion_rate),
    sd = sd(completion_rate),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Create the main chart
p1 <- ggplot(df, aes(x = design, y = completion_rate, fill = design)) +
  
  # Add individual points (jittered for visibility)
  geom_jitter(width = 0.10, height = 0, alpha = 0.5, size = 3.5, color = "gray30") +
  
  # Add boxplot overlay
  geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.5, 
               color = "gray20", linewidth = 0.8) +
  
  # Add mean point (diamond shape)
  stat_summary(fun = mean, geom = "point", shape = 23, size = 7,
               fill = "white", color = "black", stroke = 1.5) +
  
  # Colors
  scale_fill_manual(values = colors) +
  
  # Clean x-axis labels
  scale_x_discrete(labels = c("Sidebar\nNavigation", "Top Bar\nNavigation")) +
  
  # Y-axis with more space at top for annotations
  scale_y_continuous(
    limits = c(0, 115),  # Extended to 115 to make room
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  
  # Add statistical results box (cleaner, no overlap)
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 102, ymax = 115,
           fill = "white", color = "gray75", alpha = 0.8, linewidth = 0.4) +
  
  annotate("text", x = 1.5, y = 110,
           label = paste0("t(28) = ", round(abs(t_stat), 2), 
                          ", p = ", format.pval(p_val, digits = 3)),
           size = 4.5, fontface = "bold", color = "gray20", lineheight = 1.8) +
  
  annotate("text", x = 1.5, y = 106,
           label = paste0("Cohen's d = ", round(abs(d_value), 2), 
                          " (", interpretation, " effect)"),
           size = 4.5, fontface = "bold", color = "gray20") +
  
  # Add significance indicator if p < .05
  {if(p_val < 0.05) 
    annotate("text", x = 1.5, y = 98, 
             label = "* Statistically Significant", 
             size = 4, fontface = "italic", color = "#27AE60")
  } +
  
  # Labels
  labs(
    title = "Task Completion Rate: B2B SaaS Navigation Design Study",
    subtitle = paste0("Top Bar achieved ", round(plot_data$mean[2], 1), 
                      "% vs Sidebar's ", round(plot_data$mean[1], 1),
                      "% completion (+", round(abs(mean_diff), 1), " pp)"),
    x = "",
    y = "Task Completion Rate (%)",
    caption = paste0("n = 30 (15 per design) | Between-subjects usability test | ",
                     "Portfolio project with synthetic data\n",
                     "Diamond = mean, Box = median & quartiles, Points = individual participants")
  ) +
  
  # Professional theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 17, hjust = 0, 
                              margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, color = "gray30", hjust = 0, 
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1, 
                                margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 13, 
                                margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "gray20"),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(25, 25, 20, 25)
  )

# Save high-resolution version for portfolio
ggsave("b2b_navigation_main_result.png", p1, 
       width = 12, height = 9, dpi = 300, bg = "white")

cat("✓ Saved: b2b_navigation_main_result.png\n")


cat("========================================\n")
cat("CREATING VISUALIZATIONS\n")
cat("========================================\n\n")

# Set professional color palette
colors <- c("Sidebar" = "#E74C3C", "TopBar" = "#27AE60")

# Calculate summary statistics for plotting
plot_data <- df %>%
  group_by(design) %>%
  summarise(
    mean = mean(completion_rate),
    sd = sd(completion_rate),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

cat("Creating main visualization...\n")

# ============================================================================
# VISUALIZATION 2: SIMPLE COMPLETION RATE COMPARISON
# ============================================================================


# Create the main chart (SIMPLE VERSION - NO OVERLAP)
p1 <- ggplot(df, aes(x = design, y = completion_rate, fill = design)) +
  
  # Individual points
  geom_jitter(width = 0.20, alpha = 0.5, size = 3.5, color = "gray30") +
  
  # Boxplot
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5, 
               color = "gray20", linewidth = 0.8) +
  
  # Mean point (diamond)
  stat_summary(fun = mean, geom = "point", shape = 23, size = 7,
               fill = "white", color = "black", stroke = 1.5) +
  
  # Colors
  scale_fill_manual(values = colors) +
  
  # X-axis labels
  scale_x_discrete(labels = c("Sidebar\nNavigation", "Top Bar\nNavigation")) +
  
  # Y-axis
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  
  # Labels (all stats in subtitle)
  labs(
    title = "Task Completion Rate Comparison",
    subtitle = paste0("Top Bar: ", round(plot_data$mean[2], 1), "% | ",
                      "Sidebar: ", round(plot_data$mean[1], 1), "% | ",
                      "Difference: +", round(abs(mean_diff), 1), " pp | ",
                      "p = ", format.pval(p_val, digits = 3), " | ",
                      "d = ", round(abs(d_value), 2)),
    x = "",
    y = "Task Completion Rate (%)",
    caption = "n = 30 (15 per design) | Portfolio project with synthetic data\nDiamond = mean, Box = median & IQR, Dots = individual participants"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 17, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0, 
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1, 
                                margin = margin(t = 10), lineheight = 1.2),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11, color = "gray20"),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save
ggsave("b2b_navigation_simple_v2.png", p1, 
       width = 12, height = 8, dpi = 300, bg = "white")

cat("✓ Saved: b2b_navigation_main_result.png\n\n")

completion_summary <- df %>% 
  group_by(design) %>% 
  summarise(
    mean_completion = mean(completion_rate),
    mean_total_completed = mean(task1_completed + task2_completed + 
                                  task3_completed + task4_completed + task5_completed)
  )

# sidebar data only
sidebar_data <- df %>% 
  filter(design == "Sidebar")

# topbar data only
topbar_data <- df %>% 
  filter(design == "TopBar")


hist(df$completion_rate)

summary(df$completion_rate)
