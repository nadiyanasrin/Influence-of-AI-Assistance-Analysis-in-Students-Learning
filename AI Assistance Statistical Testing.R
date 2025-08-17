install.packages("dplyr")
library("dplyr")

df<-read.csv("C:\\Users\\HP\\Downloads\\ai_assistant_usage_student_life.csv")
View(df)

summary(df)
head(df)
str(df)

# T-test
# Does SatisfactionRating differ based on whether students would Use AI Again?
# Convert 'UsedAgain' to factor
df$UsedAgain <- as.factor(df$UsedAgain)

#run the t-test

t_result1 <- t.test(SatisfactionRating ~ UsedAgain, data = df)
print(t_result1)

# Interpretation

if (t_result1$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}

#Z-test

install.packages("BSDA")  
library(BSDA)

z_result <- z.test(df$SessionLengthMin, mu = 25, sigma.x = 10)
print(z_result)


if (z_result$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}

# F-test

# Convert Discipline column to factor
df$Discipline <- as.factor(data$Discipline)

# Subset data for Computer Science and Psychology
cs_prompts <- subset(df, Discipline == "Computer Science")$TotalPrompts
psy_prompts <- subset(df, Discipline == "Psychology")$TotalPrompts

# Perform F-test to compare variances
f_result <- var.test(cs_prompts, psy_prompts)

# Print result
print(f_result)

if (f_result$p.value < 0.05) {
  print("The difference is statistically significant (p < 0.05)\n")
} else {
  print("The difference is NOT statistically significant (p = 0.05)\n")
}

#Anova Test

# Do different Task Types lead to different levels of student Satisfaction Rating?

# Perform ANOVA: compare Satisfaction Rating across Task Type
anova_result <- aov(SatisfactionRating ~ TaskType, data = df)

# Summary of ANOVA
summary(anova_result)

anova_summary <- summary(anova_result)
p_val <- anova_summary[[1]]["Pr(>F)"][1]

# Now make the decision
  if (t_result1$p.value < 0.05) {
    print("The difference is statistically significant (p < 0.05)\n")
  } else {
    print("The difference is NOT statistically significant (p = 0.05)\n")
  }

# Chi Square Test

# Create contingency table
contingency_table <- table(df$TaskType, df$UsedAgain)

# View the table
print(contingency_table)

# Perform Chi-Square Test
chi_result <- chisq.test(contingency_table)

# Print result
print(chi_result)

# Interpretation
if (chi_result$p.value < 0.05) {
    print("The difference is statistically significant (p < 0.05)\n")
  } else {
    print("The difference is NOT statistically significant (p = 0.05)\n")
  }

