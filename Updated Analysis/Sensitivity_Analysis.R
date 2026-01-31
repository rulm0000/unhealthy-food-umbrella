# Sensitivity Analysis Script
# Purpose: Re-run specific meta-analyses excluding duplicate studies and compare with original results.

# Load necessary libraries
if (!requireNamespace("metafor", quietly = TRUE)) install.packages("metafor")
library(metafor)

# 1. Load Data
datafile <- "extracted_Rerun_Analyses.csv"
data <- read.csv(datafile, stringsAsFactors = FALSE)

# Preprocessing (same as Rerun_2.R)
# Ensure numeric columns
data$Effect.Size <- as.numeric(data$Effect.Size)
data$X95..CI.Lower <- as.numeric(data$X95..CI.Lower)
data$X95..CI.Upper <- as.numeric(data$X95..CI.Upper)

# 1) log(OR)
data$logOR <- log(data$Effect.Size)
# 2) SE
data$SElogOR <- (log(data$X95..CI.Upper) - log(data$X95..CI.Lower)) / 3.92
# 3) var
data$varlogOR <- data$SElogOR^2

# Helper function to run meta-analysis
run_ma <- function(sub_df, label) {
    if (nrow(sub_df) < 2) {
        if (nrow(sub_df) == 1) {
            return(data.frame(
                Label = label,
                k = 1,
                OR = sub_df$Effect.Size[1],
                CI_Lower = sub_df$X95..CI.Lower[1],
                CI_Upper = sub_df$X95..CI.Upper[1],
                I2 = NA
            ))
        }
        return(data.frame(
            Label = label,
            k = nrow(sub_df),
            OR = NA,
            CI_Lower = NA,
            CI_Upper = NA,
            I2 = NA
        ))
    }

    # Method selection (same heuristic: <5 Fixed, >=5 REML)
    method_ma <- if (nrow(sub_df) < 5) "FE" else "REML"

    res <- tryCatch(
        {
            rma.uni(yi = sub_df$logOR, vi = sub_df$varlogOR, method = method_ma)
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (is.null(res)) {
        return(NULL)
    }

    data.frame(
        Label = label,
        k = res$k,
        OR = exp(res$beta),
        CI_Lower = exp(res$ci.lb),
        CI_Upper = exp(res$ci.ub),
        I2 = res$I2
    )
}

# Define Scenarios
results_list <- list()

# -----------------------------------------------------------------------------
# Scenario 1: Del Pino (Sleep)
# "Original": Adverse Sleep-Related Outcomes (Included)
# Review: Delpino 2023
# Outcome: Adverse Sleep-Related Outcomes
delpino_orig <- subset(data, Review.ID == "Delpino 2023" & Outcome == "Adverse Sleep-Related Outcomes")
res1 <- run_ma(delpino_orig, "Adverse Sleep-Related Outcomes (Original)")
results_list[[length(results_list) + 1]] <- res1

# -----------------------------------------------------------------------------
# Scenario 2: Malamir 2023 (Sleep Dissatisfaction) vs Park et al. 2016
# Context: Remove Park et al. 2016 from Malamir 2023.

malamir_data <- subset(data, Review.ID == "Malmir 2023" & Outcome == "Sleep Dissatisfaction")

# Original
res2 <- run_ma(malamir_data, "Sleep Dissatisfaction (Original)")
results_list[[length(results_list) + 1]] <- res2

# Sensitivity: Exclude "Park et al. 2016" (or similar name)
malamir_sens <- subset(malamir_data, !grepl("Park.*2016", Study.ID))

res2b <- run_ma(malamir_sens, "Sleep Dissatisfaction (Sensitivity)")
results_list[[length(results_list) + 1]] <- res2b


# -----------------------------------------------------------------------------
# Scenario 3: ADHD Diagnosis - Lee 2020 & Park 2012
# Duplicates in "Sweets" and "Junk Food".

# A) Junk Food (JF) -> ADHD Diagnosis
jf_adhd <- subset(data, Exposure == "JF" & Outcome == "ADHD Diagnosis")

# Original
res3 <- run_ma(jf_adhd, "ADHD Diagnosis (JF - Original)")
results_list[[length(results_list) + 1]] <- res3

# Sensitivity: Exclude Lee 2020, Park 2012
jf_adhd_sens <- subset(jf_adhd, !grepl("Lee.*2020|Park.*2012", Study.ID))

res3b <- run_ma(jf_adhd_sens, "ADHD Diagnosis (JF - Sensitivity)")
results_list[[length(results_list) + 1]] <- res3b


# B) Sweets -> ADHD Diagnosis
sw_adhd <- subset(data, Exposure == "Sweets/Candies" & Outcome == "ADHD Diagnosis")

# Original
res4 <- run_ma(sw_adhd, "ADHD Diagnosis (Sweets - Original)")
results_list[[length(results_list) + 1]] <- res4

# Sensitivity: Exclude Lee 2020, Park 2012
sw_adhd_sens <- subset(sw_adhd, !grepl("Lee.*2020|Park.*2012", Study.ID))

res4b <- run_ma(sw_adhd_sens, "ADHD Diagnosis (Sweets - Sensitivity)")
results_list[[length(results_list) + 1]] <- res4b


# Combine Results
final_results <- do.call(rbind, results_list)
write.csv(final_results, "Sensitivity_Comparison.csv", row.names = FALSE)
print(final_results)
