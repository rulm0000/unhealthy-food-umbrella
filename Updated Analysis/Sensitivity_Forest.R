# Sensitivity Analysis Forest Plot
# Visualizes the comparison between Original and Deduplicated (Sensitivity) Models

if (!requireNamespace("forestplot", quietly = TRUE)) install.packages("forestplot")
library(forestplot)
library(dplyr)
library(grid)

# Load Data
comp_file <- "Sensitivity_Comparison.csv"
if (!file.exists(comp_file)) stop("Comparison file not found")

df <- read.csv(comp_file, stringsAsFactors = FALSE)

# Prepare Data for Forestplot
# Forestplot expects vectors/matrices

# We want to group by Outcome to keep pairs together
# The CSV is already sorted by Outcome/Label

# Convert numeric
df$OR <- as.numeric(df$OR)
df$CI_Lower <- as.numeric(df$CI_Lower)
df$CI_Upper <- as.numeric(df$CI_Upper)

# Table Text
# Label | k | OR (95% CI)
es_text <- sprintf("%.2f (%.2f, %.2f)", df$OR, df$CI_Lower, df$CI_Upper)
es_text[is.na(df$OR)] <- ""

tabletext <- cbind(
    c("Outcome / Analysis", df$Label),
    c("k", df$k),
    c("OR (95% CI)", es_text),
    c("I2", paste0(round(as.numeric(df$I2)), "%"))
)

# Means and Limits
means <- c(NA, df$OR)
lowers <- c(NA, df$CI_Lower)
uppers <- c(NA, df$CI_Upper)

# Export Forest Plot - JPEG Format for GitHub (Repo Path)
jpeg_height <- (5 + nrow(df) * 1) * 300
jpeg_width <- 12 * 300

output_file <- "../unhealthy-food-umbrella/Updated Analysis/Sensitivity_Forest_Plot.jpg"
jpeg(output_file, width = jpeg_width, height = jpeg_height, res = 300)

forestplot(
    labeltext = tabletext,
    mean = means,
    lower = lowers,
    upper = uppers,
    xlog = TRUE,
    xlab = "Odds Ratio (log scale)",
    title = "Sensitivity Analysis: Excluding Duplicates",
    col = fpColors(box = "black", line = "black", summary = "black"),
    txt_gp = fpTxtGp(label = gpar(cex = 1.0), ticks = gpar(cex = 0.9), xlab = gpar(cex = 1)),
    new_page = TRUE
)

dev.off()
cat("Plot saved to:", output_file, "\n")
