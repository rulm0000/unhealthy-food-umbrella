# Sensitivity Heterogeneity Forest Plot (v14 - Compact, Large Text, Fixed Legend Icons)

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

    # Header
    labels <- c(labels, grp)
    means <- c(means, NA)
    lowers <- c(lowers, NA)
    uppers <- c(uppers, NA)
    i2s <- c(i2s, "")
    ks <- c(ks, "")

    # Primary
    labels <- c(labels, "   Primary Analysis")
    means <- c(means, df$Orig_OR[i])
    lowers <- c(lowers, df$Orig_Lower[i])
    uppers <- c(uppers, df$Orig_Upper[i])
    i2s <- c(i2s, sprintf("%.1f%%", df$Orig_I2[i]))
    ks <- c(ks, df$Orig_k[i])
    marker_clrs <- c(marker_clrs, "black")

    # Sensitivity
    labels <- c(labels, "   Sensitivity Analysis")
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
        fpDrawDiamondCI(..., clr.line = color, clr.marker = color, boxsize = 0.5)
    }
})

# Export JPEG
# Scaled for 300dpi readability
# Compact height: Reduced multiplier from 0.45 to 0.35
jpeg_height <- (4 + length(labels) * 0.35) * 300
jpeg_width <- 15 * 300

jpeg("Heterogeneity_Sensitivity_Forest.jpg", width = jpeg_width, height = jpeg_height, res = 300)

forestplot(
    labeltext = tabletext,
    graph.pos = 2,
    mean = final_means,
    lower = final_lowers,
    upper = final_uppers,
    fn.ci_norm = fn_custom,
    is.summary = is_summary,
    xlog = TRUE,
    col = fpColors(lines = "black", zero = "gray"),
    lwd.ci = 3, # Thicker lines
    colgap = unit(4, "mm"), # Compact column gap
    title = "", # No Title
    txt_gp = fpTxtGp(
        label = gpar(cex = 1.6), # Much Larger Labels
        ticks = gpar(cex = 1.5), # Larger Axis Ticks
        xlab = gpar(cex = 1.6)
    ), # Larger Axis Label
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
