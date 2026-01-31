# Sensitivity Heterogeneity Forest Plot (v15 - Collapsed Rows, No Text Labels)

if (!requireNamespace("forestplot", quietly = TRUE)) install.packages("forestplot")
library(forestplot)
library(dplyr)
library(grid)

# 1. Read Data
sens_file <- "Heterogeneity_Sensitivity_Report.csv"
if (!file.exists(sens_file)) stop("sensitivity report missing")

df <- read.csv(sens_file, stringsAsFactors = FALSE)

# 2. Reshape Data
labels <- c()
means <- c()
lowers <- c()
uppers <- c()
i2s <- c()
ks <- c()
marker_clrs <- c()

for (i in seq_len(nrow(df))) {
    grp <- gsub("_", ": ", df$Group[i])

    # Row 1: Outcome Name + Primary Analysis (Black)
    labels <- c(labels, grp)
    means <- c(means, df$Orig_OR[i])
    lowers <- c(lowers, df$Orig_Lower[i])
    uppers <- c(uppers, df$Orig_Upper[i])
    i2s <- c(i2s, sprintf("%.1f%%", df$Orig_I2[i]))
    ks <- c(ks, df$Orig_k[i])
    marker_clrs <- c(marker_clrs, "black")

    # Row 2: Empty Label + Sensitivity Analysis (Blue)
    labels <- c(labels, "")
    means <- c(means, df$New_OR[i])
    lowers <- c(lowers, df$New_Lower[i])
    uppers <- c(uppers, df$New_Upper[i])
    i2s <- c(i2s, sprintf("%.1f%%", df$New_I2[i]))
    ks <- c(ks, df$New_k[i])
    marker_clrs <- c(marker_clrs, "blue")

    # Spacer
    labels <- c(labels, "")
    means <- c(means, NA)
    lowers <- c(lowers, NA)
    uppers <- c(uppers, NA)
    i2s <- c(i2s, "")
    ks <- c(ks, "")
}

# 3. Assemble Table Text
es_vals <- ifelse(is.na(means), "",
    paste0(
        sprintf("%.2f", means), " (",
        sprintf("%.2f", lowers), ", ",
        sprintf("%.2f", uppers), ")"
    )
)

tabletext <- cbind(
    c("Outcome / Analysis", labels),
    c("Odds Ratio (95% CI)", es_vals),
    c("k", ks)
)

final_means <- c(NA, means)
final_lowers <- c(NA, lowers)
final_uppers <- c(NA, uppers)

# Summary: Only Outcome Header rows (which now have Data) should be Bold?
# Or should we make them normal font but allow the plot to draw?
# If we set is.summary=TRUE, forestplot bolds the text.
# We want the Outcome Name bold.
# But we also want to draw a Custom Marker (Diamond), not a Summary Polygon.
# Our custom function 'fn.ci_norm' overrides standard markers.
# Forestplot usage: fn.ci_norm handles standard CIs. fn.ci_sum handles summary CIs.
# If is.summary=TRUE, it calls fn.ci_sum.
# So we must assign our custom function to `fn.ci_sum` as well if we want summary rows to use Diamonds.

is_summary <- c(TRUE, grepl(":", labels) | labels == "")

fn_custom <- local({
    i <- 0
    clrs <- marker_clrs
    function(..., clr.line, clr.marker) {
        i <<- i + 1
        if (i > length(clrs)) {
            return()
        }
        color <- clrs[i]
        if (is.na(color)) {
            return()
        } # Safety
        fpDrawDiamondCI(..., clr.line = color, clr.marker = color, boxsize = 0.5)
    }
})

# We need a separate counter/function for summary if we want to support both.
# But wait, our 'marker_clrs' matches the length of data rows (non-NA).
# If header rows are summary, they consume an index.
# The `marker_clrs` vector was built assuming every data row gets a color.
# Row 1 (Header+Primary) -> Black.
# Row 2 (Sensitivity) -> Blue.
# This works perfectly.
# EXCEPT: forestplot calls `fn.ci_sum` for summary rows and `fn.ci_norm` for normal rows.
# We must ensure both use our custom painter AND share the same index counter 'i'.
# Or better: simply treat all rows as NORMAL rows (is.summary = FALSE)
# and use a 'txt_gp' argument to Bold the first column if it matches the pattern?
# Forestplot strictly separates summary/normal rows.
# Setting is.summary = FALSE for everything makes the text normal weight. This is safe and clean.
# Use standard 'fn.ci_norm'.

is_summary_safe <- rep(FALSE, length(final_means))
is_summary_safe[1] <- TRUE # Header of table is summary

# Export JPEG
# Height calculation: fewer rows now, but keep spacing
jpeg_height <- (4 + length(labels) * 0.4) * 300
jpeg_width <- 15 * 300

jpeg("Heterogeneity_Sensitivity_Forest.jpg", width = jpeg_width, height = jpeg_height, res = 300)

forestplot(
    labeltext = tabletext,
    graph.pos = 2,
    mean = final_means,
    lower = final_lowers,
    upper = final_uppers,
    fn.ci_norm = fn_custom, # Use custom drawer for ALL rows (since we set is.summary=FALSE)
    is.summary = is_summary_safe,
    xlog = TRUE,
    col = fpColors(lines = "black", zero = "gray"),
    lwd.ci = 3,
    colgap = unit(4, "mm"),
    title = "",
    txt_gp = fpTxtGp(
        label = gpar(cex = 1.6),
        ticks = gpar(cex = 1.5),
        xlab = gpar(cex = 1.6)
    ),
    xlab = "Odds Ratio (log scale)",
    hrzl_lines = list("1" = gpar(lwd = 2, col = "black"))
)

# Draw Legend manually
# Primary
grid.text("Primary Analysis", x = 0.35, y = 0.97, gp = gpar(fontsize = 16, fontface = "bold", col = "black"))
grid.lines(x = c(0.24, 0.28), y = 0.97, gp = gpar(col = "black", lwd = 3))
grid.points(x = 0.26, y = 0.97, pch = 18, size = unit(6, "mm"), gp = gpar(col = "black", fill = "black"))

# Sensitivity
grid.text("Sensitivity Analysis", x = 0.65, y = 0.97, gp = gpar(fontsize = 16, fontface = "bold", col = "black"))
grid.lines(x = c(0.53, 0.57), y = 0.97, gp = gpar(col = "blue", lwd = 3))
grid.points(x = 0.55, y = 0.97, pch = 18, size = unit(6, "mm"), gp = gpar(col = "blue", fill = "blue"))

dev.off()
cat("Plot created: Heterogeneity_Sensitivity_Forest.jpg\n")
