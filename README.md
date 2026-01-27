# B2B SaaS Navigation Design Validation Study

**Portfolio project demonstrating quantitative UX research methodology and statistical analysis skills**

---

## 📊 Portfolio Project Note

This project uses synthetic data to demonstrate:
- Between-subjects experimental design
- Statistical analysis in R (t-tests, effect size, assumption checking)
- Data visualization with ggplot2
- Business-focused research communication

The methodology reflects real-world UX research practices that I would apply with actual user recruitment and behavioral data.

---
## Project Overview

**Business Context:** DataPulse Analytics, a B2B SaaS platform serving Fortune 500 data teams, needed to choose between two navigation menu designs before committing engineering resources. The product team required quantitative validation to make a data-backed decision and avoid costly rework.

**Research Question:** Which navigation menu design enables enterprise users to complete workflow tasks more efficiently?

**Key Challenge:** Engineering estimated 3 sprints to build either design. Wrong choice would require 2 additional sprints for rework = 5 sprints total cost.

---

## Approach

**Study Design:**
- Between-subjects usability test (n=30, 15 per design)
- 5 enterprise workflow tasks per participant
- Primary metric: Task completion rate (behavioral)
- Statistical analysis: Independent t-test, Cohen's d effect size

**Design Variants:**
- **Sidebar Navigation:** Persistent left panel, hierarchical structure
- **Top Bar Navigation:** Horizontal menu, flat structure, more content space

**Statistical Analysis:**
- Assumption checking (normality, equal variances)
- Independent samples t-test
- Effect size calculation (Cohen's d)
- Data visualization

---

## Key Findings

### TopBar navigation significantly outperformed Sidebar navigation

| Metric | Sidebar | TopBar | Difference |
|--------|---------|--------|------------|
| **Completion Rate** | 62.7% | 77.3% | **+14.7 pp** |
| **SUS Score** | 72.4 | 78.1 | +5.7 points |

**Statistical Results:**
- **t(28) = -2.19, p = .037** (statistically significant)
- **Cohen's d = 0.80** (large effect size)
- **95% CI:** [1.0, 28.4 pp]

**Interpretation:** TopBar achieved 14.7 percentage points higher completion. The large effect size (d = 0.80) confirms this isn't just statistically significant. It's a difference users would notice in practice.

---

## Business Impact

**Recommendation:** Proceed with TopBar navigation

**Value:**
- Avoided building less efficient design
- Research cost: ~ 2 weeks


**User benefit:** 15% higher task completion expected in production

---

## Visualizations

### Main Result
![Task Completion Comparison](outputs/b2b_navigation_main_result.png)

*TopBar significantly outperformed Sidebar [t(28) = -2.19, p = .037, d = 0.80]*

### Task-Level Breakdown
![Task Performance](outputs/b2b_task_breakdown.png)

*TopBar excelled on simple/medium tasks; both performed similarly on complex tasks*

---

## Skills Demonstrated

**Research Design:**
- Between-subjects experimental design
- Metric selection (behavioral vs. self-reported)
- Pre-build validation methodology

**Statistical Analysis:**
- Independent samples t-test
- Assumption testing (Shapiro-Wilk, Levene's)
- Effect size interpretation (Cohen's d)
- Confidence interval analysis

**Technical:**
- R programming (tidyverse, ggplot2, statistical packages)
- Data visualization (publication-quality charts)
- Reproducible research (documented code, version control)

**Business Communication:**
- Translating statistics into recommendations
- Stakeholder-appropriate reporting

---

## How to Run

**Prerequisites:**
```r
install.packages(c("tidyverse", "car", "effsize"))
```

**Run the analysis:**
```r
source("analysis.R")
```

**Outputs:**
- Visualizations saved to `outputs/` folder
- Statistical results printed to console
- Data saved to `data/` folder

**Reproducibility:** Uses `set.seed(6)` for consistent synthetic data generation

---

## Repository Structure
```
├── analysis.R                            # Complete analysis script
├── data/
│   └── b2b_navigation_study.csv         # Generated dataset
└── outputs/
    ├── b2b_navigation_main_result.png   # Main visualization
    └── b2b_task_breakdown.png           # Task-level heatmap
```

---

## Methodology Notes

**Assumption Checking:**
- Both groups passed normality tests (Shapiro-Wilk, p > .05)
- Equal variances confirmed (Levene's test, p = .86)
- Standard t-test appropriate

**Effect Size:**
- Cohen's d = 0.80 (large effect per Cohen's guidelines)
- Indicates practical significance beyond statistical significance
- Users would notice this performance difference in real-world use

---

## Contact

**Akash Singh**  
Email: singh.akash5553@gmail.com | LinkedIn: https://www.linkedin.com/in/akash55/ | Portfolio: https://uxfol.io/akashsingh
