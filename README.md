# B2B SaaS Navigation Quantitative Usability Study

**Project demonstrating quantitative UX research methodology and statistical analysis skills in R**

---

## Portfolio Project Note
This analysis demonstrates the research analysis workflow I use for high-stakes product decisions, from study design through statistical validation and stakeholder recommendations. While the data used is synthetic, the methodology mirrors my approach on similar projects.

**What this project demonstrates:**
- Between-subjects experimental design for comparative evaluation
- Statistical analysis in R (t-tests, effect size, assumption checking)
- Translating statistical findings into business impact and recommendations
- Data visualization with ggplot2

The methodology reflects real-world UX research practices that I would apply with actual user recruitment and behavioral data collected via usability testing.

---
## Project Overview

**Business Context:** DataPulse Analytics, a B2B SaaS platform serving Fortune 500 data teams, needed to choose between two navigation menu designs before committing engineering resources. The product team required quantitative validation to make a data-backed decision and avoid costly rework.

**Research Question:** Which navigation menu design enables enterprise users to complete workflow tasks more efficiently?

**Key Challenge:** Engineering estimated 3 sprints to build either design. Wrong choice would require 2 additional sprints for rework = 5 sprints total cost.

---

## Research Design Decisions

**Between-Subjects Design**

- Avoided learning effects: Each participant sees only one design (navigation patterns can be learned quickly)
- Cleaner comparison: No fatigue or order effects from testing both designs

## Metrics Selection

**Primary Metric:** Task Completion Rate
- Direct measure of navigation effectiveness
- Behavioral data > self-reported for validation decisions
- Binary outcome (completed/not completed)


## Approach

**Design Variants:**
- **Sidebar Navigation:** Persistent left panel, hierarchical structure
- **TopBar Navigation:** Horizontal menu, flat structure, more content space

**Study Design:**
- Between-subjects usability test (n=30, 15 per design)
- 5 enterprise workflow tasks per participant
- Randomized task order to control for sequence effects
- Success criteria: Task completed without assistance, correct end state reached

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

**Interpretation:** 
- TopBar achieved 14.7 percentage points (pp) higher completion. The large effect size (d = 0.80) confirms this isn't just statistically significant. It's a difference users would notice in practice.

**What This Means for Users:**
- A 14.7pp improvement translates to approximately 15% more users successfully completing tasks. For DataPulse's 10K enterprise users, this could mean ~1,500 additional successful task completions daily. The large effect size indicates users would experience noticeably smoother workflows, not marginal improvements.

---

## Business Impact

**Recommendation:** Proceed with TopBar navigation

**Rationale:**
- Statistically significant performance advantage (p = .037)
- Large practical effect size (d = 0.80)

**Expected User benefit:** 15% higher task completion expected in production

**Value:**
- Avoided building less efficient Sidebar design and potential rework
- Research investment: ~2 weeks (design + testing + analysis)

**Post-Launch Monitoring Plan**
- Week 1-4: Monitor support tickets for navigation-related issues
- Month 2: Analyze production analytics for task completion patterns

---

## Visualizations

### Main Result
![Task Completion Comparison](outputs/task_completion_comparison.png)

*TopBar significantly outperformed Sidebar [t(28) = -2.19, p = .037, d = 0.80]*

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
b2b-nav-study/
├── .gitignore                           # Git ignore rules
├── README.md                            # Project documentation
├── analysis.R                           # Complete analysis pipeline
├── data/
│   └── b2b_navigation_study.csv        # Generated dataset (n=30)
└── outputs/
    └── task_completion_comparison.png  # Main comparison visualization
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


## Study Limitations & Future Research

**Limitations**
- Sample Size:
    - n=30 is a small sample. Likely need n=300+ for high-confidence design comparison launch decisions using a Between Subjects study. (https://measuringu.com/sample-size-designs/)
- Lab Environment:
    - Testing in controlled conditions doesn't capture real-world interruptions, multitasking, or time pressure that enterprise users face daily.
- Task Coverage:
    - 5 tasks represent core workflows but may not reflect all edge cases or advanced user needs.
- Timeframe:
    - Single-session testing can't assess learnability over time or retention of navigation patterns.

---

## Contact

**Akash Singh**  

Email: singh.akash5553@gmail.com | LinkedIn: https://www.linkedin.com/in/akash55/ | Portfolio: https://uxfol.io/akashsingh
