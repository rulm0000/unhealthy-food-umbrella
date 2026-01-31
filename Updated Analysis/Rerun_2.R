### 1) Install the packages if needed (skip if you have them installed)
# install.packages("metafor")
# install.packages("meta")

### 2) Load them
library(metafor)
library(meta)

### 3) Read data
# Use relative path or assume valid working directory
datafile <- "extracted_Rerun_Analyses.csv"
data <- read.csv(datafile, stringsAsFactors = FALSE)

# 1) log(OR)
data$logOR <- log(data$Effect.Size)

# 2) Approximate SE(log(OR)) from 95% CI
data$SElogOR <- (log(data$X95..CI.Upper) - log(data$X95..CI.Lower)) / 3.92

# 3) var(log(OR))
data$varlogOR <- data$SElogOR^2

# group label
data$group_label <- paste(data$Exposure, data$Outcome, sep = "_")

all_groups <- unique(data$group_label)
results_list <- list()

for (g in all_groups) {
  subset_df <- subset(data, group_label == g)
  nStudies <- nrow(subset_df)

  # if fewer than 2 data points, skip
  if (nStudies < 2) next

  # if only 2-3 data points, switch to fixed effect
  if (nStudies >= 4) {
    meta_method <- "REML" # random-effects
  } else {
    meta_method <- "FE" # fixed-effect if just 2-3
  }

  # do the meta-analysis
  res <- rma.uni(
    yi = subset_df$logOR,
    vi = subset_df$varlogOR,
    measure = "GEN",
    method = meta_method
  )

  # transform back to OR
  or_summary <- exp(res$b)
  lower95 <- exp(res$ci.lb)
  upper95 <- exp(res$ci.ub)
  pval <- res$pval
  Q <- res$QE
  Q_df <- res$k - 1
  Q_p <- res$QEp
  I2 <- res$I2
  tau2 <- res$tau2

  # Egger’s test
  egg <- regtest(res, model = "rma")
  egg_p <- egg$pval

  # Begg’s test
  begg <- ranktest(res)
  begg_p <- begg$pval

  # test of excess significance
  exsig <- tes(x = subset_df$logOR, v = subset_df$varlogOR)
  exsig_p <- exsig$pval

  # store
  results_list[[g]] <- data.frame(
    group = g,
    k_studies = res$k,
    model_method = meta_method,
    summary_logOR = as.numeric(res$b),
    summary_OR = as.numeric(or_summary),
    ci95_lower = as.numeric(lower95),
    ci95_upper = as.numeric(upper95),
    pval_summary = pval,
    Q = Q,
    Q_df = Q_df,
    Q_p = Q_p,
    I2 = I2,
    tau2 = tau2,
    Egger_p = egg_p,
    Begg_p = begg_p,
    ExcessSig_p = exsig_p
  )
}

# combine
final_results <- do.call(rbind, results_list)
final_results

# Save output to local directory
write.csv(final_results, "MHUR_Final_Results.csv", row.names = FALSE)


###############################################################################
# Example code without hrzl_lines
###############################################################################

# 1) Install and load required packages
# 1) Install and load required packages
# if (!requireNamespace("forestplot", quietly = TRUE)) install.packages("forestplot")
# if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
library(forestplot)
library(RColorBrewer)

# 2) Suppose 'final_results' is the data frame containing:
#    group, k_studies, summary_logOR, summary_OR, ci95_lower, ci95_upper, pval_summary, etc.

# 3) Prepare columns for the table portion of the forest plot
exposure_outcome <- final_results$group
k_vector <- final_results$k_studies

# Create a nicely formatted effect size column (no header)
effect_size_formatted <- paste0(
  round(final_results$summary_OR, 2),
  " [",
  round(final_results$ci95_lower, 2),
  ", ",
  round(final_results$ci95_upper, 2),
  "]"
)
effect_size_col <- effect_size_formatted

# Format p-values (no header)
p_values_col <- format(final_results$pval_summary, digits = 2, scientific = TRUE)

# Combine into a label-text matrix (no extra row)
labeltext_matrix <- cbind(
  exposure_outcome,
  k_vector,
  effect_size_col,
  p_values_col
)

# 4) Define numeric vectors in the ORIGINAL scale for forestplot
mean_vec <- final_results$summary_OR
lower_vec <- final_results$ci95_lower
upper_vec <- final_results$ci95_upper

# 5) Colors for lines and diamond edges
plot_colors <- fpColors(lines = "navy", zero = "darkred")

# 6) Define the function for drawing diamond-shaped confidence intervals
fn_diamond <- fpDrawDiamondCI

# 7) Create the forest plot — no horizontal lines here
pdf("ForestPlot_Output.pdf", width = 10, height = 8)
forestplot(
  labeltext = labeltext_matrix,
  mean = mean_vec,
  lower = lower_vec,
  upper = upper_vec,
  zero = 1, # Reference line at OR=1
  xlog = TRUE, # forestplot does the log transform internally
  xticks = c(0.5, 1.0, 2.0, 4.0),
  fn.ci_norm = fn_diamond,
  col = plot_colors,
  cex = 0.9,
  lineheight = "auto",
  boxsize = 0.2,
  lwd.ci = 1.5,
  ci.vertices = FALSE,
  graph.pos = 3,
  colgap = unit(6, "mm"),
  title = "Overall Effects by Exposure–Outcome",
  xlab = "Odds Ratio (log scale)"
)
dev.off()

###############################################################################
# By removing 'hrzl_lines', the plot omits any custom horizontal lines.
###############################################################################
