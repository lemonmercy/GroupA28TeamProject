
# 7COM1079 Final Report – R Code


library(readr)
library(dplyr)   
cwur <- read_csv("cwurData.csv")

# Define region groupings using the full lists
asia_countries <- c(
  "Japan", "Israel", "South Korea", "Singapore", "China",
  "Taiwan", "Hong Kong", "Thailand", "Malaysia", "India",
  "Turkey", "Saudi Arabia", "Iran", "Lebanon", "United Arab Emirates"
)

europe_countries <- c(
  "United Kingdom", "Switzerland", "France", "Sweden", "Italy",
  "Germany", "Netherlands", "Finland", "Norway", "Denmark",
  "Belgium", "Spain", "Ireland", "Austria", "Portugal",
  "Czech Republic", "Greece", "Hungary", "Poland", "Iceland",
  "Slovenia", "Estonia", "Croatia", "Slovak Republic",
  "Bulgaria", "Lithuania", "Romania", "Cyprus", "Serbia"
)

# Add Region column and keep only Asia/Europe with real scores
asia_europe <- cwur %>%
  mutate(
    Region = case_when(
      country %in% asia_countries   ~ "Asia",
      country %in% europe_countries ~ "Europe",
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(Region %in% c("Asia", "Europe"),
         !is.na(score))

# Make Region an ordered factor (Asia, then Europe)
asia_europe$Region <- factor(asia_europe$Region,
                             levels = c("Asia", "Europe"))

print(table(asia_europe$Region))

# Create output folder (if it does not exist)
if (!dir.exists("outputs")) dir.create("outputs")

# ---------------------------
# BOXPLOT
# ---------------------------
png("outputs/boxplot.png", width = 1000, height = 1200, res = 150)
par(mar = c(5, 6, 5, 4))
colors <- c("#ff7771", "#00bfc3");

boxplot(score ~ Region,
        data    = asia_europe,
        main    = "University Rankings: CWUR Scores by Region",
        xlab    = "Region",
        ylab    = "CWUR Score",
        col     = colors,
        notch   = FALSE,
        ylim    = c(40, 100),
        outline = TRUE,
        las     = 1,
        cex.lab = 1.2,
        cex.axis= 1.1,
        cex.main= 1.3)
legend("topright",
       legend = c("Asia", "Europe"),
       fill   = colors)
dev.off()

# ---------------------------
# HISTOGRAM + NORMAL CURVE
# ---------------------------
# Compute mean and sd using the cleaned Asia/Europe data
m <- mean(asia_europe$score)
s <- sd(asia_europe$score)

# Export to PNG (inside outputs folder)
png("outputs/hist_normal_curve.png", width = 1200, height = 900, res = 150)
par(mar = c(5, 6, 5,4))  # margins: bottom, left, top, right

# Histogram of real scores (frequency on y-axis)
h <- hist(asia_europe$score,
          breaks  = 30,
          freq    = TRUE,
          main    = "Histogram of CWUR Scores with Normal Curve Overlay",
          xlab    = "CWUR Scores",
          ylab    = "Frequency",
          col     = "lightblue",
          las     = 1,
          cex.lab = 1.2,
          cex.axis= 1.1,
          cex.main= 1.3)
# drawing the curve overlay
x_vals <- seq(min(asia_europe$score),
              max(asia_europe$score),
              length = 200)
y_vals <- dnorm(x_vals, mean = m, sd = s)
y_vals <- y_vals * diff(h$breaks)[1] * length(asia_europe$score)
lines(x_vals, y_vals, lwd = 2, col = "orange")

dev.off()

# ---------------------------
# Wilcoxon test
# ---------------------------
test_result <- wilcox.test(score ~ Region, data = asia_europe)
print(test_result)

sink("outputs/test_results.txt")
cat("Statistical Test Used: Mann-Whitney U Test (Wilcoxon rank-sum)\n")
cat("Test Statistic:", test_result$statistic, "\n")
cat("P-value:", test_result$p.value, "\n")
sink()