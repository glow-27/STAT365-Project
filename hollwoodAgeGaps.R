# Load necessary libraries
library(tidytuesdayR)
library(dplyr)
library(ggplot2)

# Read in the data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# Create a combined group variable
age_gaps <- age_gaps %>%
  mutate(combined_group = ifelse(character_1_gender == "man" & character_2_gender == "woman", "man_woman",
                                 ifelse(character_1_gender == "woman" & character_2_gender == "man", "man_woman",
                                        ifelse(character_1_gender == "man" & character_2_gender == "man", "man_man",
                                               ifelse(character_1_gender == "woman" & character_2_gender == "woman", "woman_woman", NA)))))

# Create decade breaks and labels
decade_breaks <- seq(1930, 2020, by = 10)
decade_labels <- paste0(decade_breaks, "s")
age_gaps$decade <- cut(age_gaps$release_year, breaks = c(decade_breaks, Inf), labels = decade_labels, include.lowest = TRUE)

# Summarize mean age difference by decade
result <- age_gaps %>%
  group_by(decade) %>%
  summarise(mean_age_difference = mean(age_difference, na.rm = TRUE))

print(result)

# Create a boxplot of age differences by decade
p <- ggplot(age_gaps, aes(x = decade, y = age_difference))
p + geom_boxplot(position = "dodge") +
  labs(title = "Age Difference Distribution by Decade", x = "Decade", y = "Age Difference") +
  theme_minimal()

# Calculate the male-female age difference
age_gaps <- age_gaps |> 
  mutate(maleFemDiff = case_when(character_1_gender == "woman" ~ -1 * age_difference,
                                 character_1_gender == "man" ~ age_difference))

# Perform a t-test on maleFemDiff
t_test_result <- t.test(x = age_gaps$maleFemDiff, mu = 0)
print(t_test_result)

# Calculate Cohen's d for age difference when woman is older vs man is older
# Filter data for the two groups
woman_older <- age_gaps %>%
  filter(character_1_gender == "woman" & character_2_gender == "man")

man_older <- age_gaps %>%
  filter(character_1_gender == "man" & character_2_gender == "woman")

# Calculate means and standard deviations
mean_woman_older <- mean(woman_older$age_difference, na.rm = TRUE)
mean_man_older <- mean(man_older$age_difference, na.rm = TRUE)

sd_woman_older <- sd(woman_older$age_difference, na.rm = TRUE)
sd_man_older <- sd(man_older$age_difference, na.rm = TRUE)

# Sample sizes
n_woman_older <- nrow(woman_older)
n_man_older <- nrow(man_older)

# Calculate pooled standard deviation
s_p <- sqrt(((n_woman_older - 1) * sd_woman_older^2 + (n_man_older - 1) * sd_man_older^2) / (n_woman_older + n_man_older - 2))

# Calculate Cohen's d
(mean_woman_older - mean_man_older) / s_p

# Output the result
print(paste("Cohen's d: ", cohens_d))



library(car) # For Levene's test

# Check normality with Shapiro-Wilk test
shapiro.test(woman_older$age_difference)
shapiro.test(man_older$age_difference)

# Check homogeneity of variances with Levene's test
leveneTest(age_difference ~ character_1_gender, data = age_gaps %>%
             filter(character_1_gender %in% c("man", "woman") & character_2_gender %in% c("man", "woman")))

# Perform the Mann-Whitney U Test
wilcox_test_result <- wilcox.test(age_difference ~ character_1_gender, 
                                  data = age_gaps %>% 
                                    filter(character_1_gender %in% c("man", "woman") & 
                                             character_2_gender %in% c("man", "woman")))

print(wilcox_test_result)

# Visual check of the distribution shapes
ggplot(age_gaps %>% filter(character_1_gender %in% c("man", "woman") & character_2_gender %in% c("man", "woman")),
       aes(x = age_difference, fill = character_1_gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age Differences by Gender",
       x = "Age Difference",
       y = "Density") +
  theme_minimal()

# Q-Q plots to compare shapes of the distributions
qqplot(woman_older$age_difference, man_older$age_difference, 
       main = "Q-Q Plot of Age Differences: Woman Older vs Man Older",
       xlab = "Quantiles of Woman Older Age Differences",
       ylab = "Quantiles of Man Older Age Differences")
abline(0, 1, col = "red")

# Load necessary libraries
library(boot)
library(dplyr)

# Assuming age_gaps is your data frame
# Create subsets for the two groups
woman_older <- age_gaps %>%
  filter(character_1_gender == "woman" & character_2_gender == "man") %>%
  pull(age_difference)

man_older <- age_gaps %>%
  filter(character_1_gender == "man" & character_2_gender == "woman") %>%
  pull(age_difference)

# Compute Hodges-Lehmann estimator for median age difference
hl_est <- median(woman_older) - median(man_older)
hl_est