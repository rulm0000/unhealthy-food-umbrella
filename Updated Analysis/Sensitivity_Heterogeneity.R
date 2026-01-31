# Sensitivity Analysis: Outlier Removal for Heterogeneity Reduction (v11 - Filtered High I2 only)

if (!requireNamespace("metafor", quietly = TRUE)) install.packages("metafor")
library(metafor)
library(dplyr)

# 1. Load Data
datafile <- "extracted_Rerun_Analyses.csv"
data <- read.csv(datafile, stringsAsFactors = FALSE)

# Clean Columns
names(data)[5] <- "OR"
names(data)[6] <- "Lower"
names(data)[7] <- "Upper"
names(data)[2] <- "Study_ID"

data$OR <- as.numeric(as.character(data$OR))
data$Lower <- as.numeric(as.character(data$Lower))
data$Upper <- as.numeric(as.character(data$Upper))
data <- data[!is.na(data$OR) & !is.na(data$Lower) & !is.na(data$Upper), ]

data$logOR <- log(data$OR)
data$SElogOR <- (log(data$Upper) - log(data$Lower)) / 3.92
data$varlogOR <- data$SElogOR^2
data$group_label <- paste(data$Exposure, data$Outcome, sep = "_")
all_groups <- unique(data$group_label)

# Results Storage
results_df <- data.frame(
    Group = character(),
    Orig_k = integer(),
    Orig_OR = numeric(),
    Orig_Lower = numeric(),
    Orig_Upper = numeric(),
    Orig_I2 = numeric(),
    Removed_Study = character(),
    New_k = integer(),
    New_OR = numeric(),
    New_Lower = numeric(),
    New_Upper = numeric(),
    New_I2 = numeric(),
    I2_Reduction = numeric(),
    stringsAsFactors = FALSE
)

# 2. Analyze
for (g in all_groups) {
    subset_df <- subset(data, group_label == g)

    if (nrow(subset_df) < 3) next

    # Initial Model
    res <- tryCatch(
        {
            rma.uni(yi = subset_df$logOR, vi = subset_df$varlogOR, method = "REML")
        },
        error = function(e) {
            return(NULL)
        }
    )

    if (is.null(res)) next

    # ONLY proceed if heterogeneity is > 50
    if (res$I2 > 50) {
        orig_k <- res$k
        orig_OR <- exp(res$b)
        orig_Lower <- exp(res$ci.lb)
        orig_Upper <- exp(res$ci.ub)
        orig_I2 <- res$I2

        current_I2 <- orig_I2
        excluded_studies <- c()
        subset_loop <- subset_df

        # Loop until I2 < 50
        while (current_I2 > 50 && nrow(subset_loop) >= 3) {
            res_loop <- tryCatch(
                {
                    rma.uni(yi = subset_loop$logOR, vi = subset_loop$varlogOR, method = "REML")
                },
                error = function(e) {
                    return(NULL)
                }
            )

            if (is.null(res_loop)) break

            l1o <- leave1out(res_loop)
            min_I2_idx <- which.min(l1o$I2)
            min_I2_val <- l1o$I2[min_I2_idx]

            # Minimal gain safety check
            if (min_I2_val >= current_I2 - 0.001) break

            study_to_remove <- subset_loop$Study_ID[min_I2_idx]
            excluded_studies <- c(excluded_studies, study_to_remove)

            subset_loop <- subset_loop[-min_I2_idx, ]
            current_I2 <- min_I2_val
        }

        if (length(excluded_studies) > 0) {
            res_final <- tryCatch(
                {
                    rma.uni(yi = subset_loop$logOR, vi = subset_loop$varlogOR, method = "REML")
                },
                error = function(e) {
                    return(NULL)
                }
            )

            if (!is.null(res_final)) {
                results_df[nrow(results_df) + 1, ] <- list(
                    Group = g,
                    Orig_k = orig_k,
                    Orig_OR = orig_OR,
                    Orig_Lower = orig_Lower,
                    Orig_Upper = orig_Upper,
                    Orig_I2 = orig_I2,
                    Removed_Study = paste(excluded_studies, collapse = "; "),
                    New_k = res_final$k,
                    New_OR = exp(res_final$b),
                    New_Lower = exp(res_final$ci.lb),
                    New_Upper = exp(res_final$ci.ub),
                    New_I2 = res_final$I2,
                    I2_Reduction = orig_I2 - res_final$I2
                )
            }
        }
    }
}

# 3. Save Report
write.csv(results_df, "Heterogeneity_Sensitivity_Report.csv", row.names = FALSE)
cat("Sensitivity report generated (I2 > 50 outcomes only).\n")
cat("Outcomes with high heterogeneity reported:", nrow(results_df), "\n")
