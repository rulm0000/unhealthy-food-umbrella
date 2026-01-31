# Sensitivity Heterogeneity Forest Plot (v18 - Lines Down, Top Removed, Last Removed)

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
ks <- c()
marker_clrs <- c()

# Horizontal lines list
# Top black line removed as requested
hr_lines <- list()

for (i in seq_len(nrow(df))) {
    grp <- gsub("_", ": ", df$Group[i])

    # Row A: Outcome + Primary (Black)
    labels <- c(labels, grp)
    means <- c(means, df$Orig_OR[i])
    lowers <- c(lowers, df$Orig_Lower[i])
    uppers <- c(uppers, df$Orig_Upper[i])
    ks <- c(ks, df$Orig_k[i])
    marker_clrs <- c(marker_clrs, "black")

    # Row B: Sensitivity (Blue)
    labels <- c(labels, "")
    means <- c(means, df$New_OR[i])
    lowers <- c(lowers, df$New_Lower[i])
    uppers <- c(uppers, df$New_Upper[i])
    ks <- c(ks, df$New_k[i])
    marker_clrs <- c(marker_clrs, "blue")

    # Row C: Spacer
    labels <- c(labels, "")
    means <- c(means, NA)
    lowers <- c(lowers, NA)
    uppers <- c(uppers, NA)
    ks <- c(ks, "")
    marker_clrs <- c(marker_clrs, NA)

    # Add Line AFTER Spacer (moved down one spot)
    # BUT only if not the last group
    if (i < nrow(df)) {
        # Current row index is length(labels).
        # Since forestplot header is row 1, and we have a header in tabletext?
        # Wait, 'labels' is part of tabletext.
        # 'tabletext' construction puts 'Outcome / Analysis' as Row 1.
        # 'labels' actually corresponds to rows 2, 3, ...
        # So the row index in the PLOT (where hrzl_lines applies) matches the table rows.
        # length(labels) is the number of data rows.
        # Total rows = 1 (Header) + length(labels).
        # We want line after the Spacer. Spacer is the last added label.
        # So line index = 1 + length(labels).

        idx_line <- 1 + length(labels)
        hr_lines[[as.character(idx_line)]] <- gpar(col = "grey50", lwd = 1)
    }
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

is_summary_safe <- rep(FALSE, length(final_means))
is_summary_safe[1] <- TRUE # Header

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
        }
        fpDrawDiamondCI(..., clr.line = color, clr.marker = color, boxsize = 0.5)
    }
})

# Export JPEG
# Height calculation
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
    hrzl_lines = hr_lines
)

# Draw Legend manually
# Navigate to top viewport to avoid clipping
try(upViewport(0), silent = TRUE)

# Primary
grid.text("Primary Analysis", x = 0.30, y = 0.96, gp = gpar(fontsize = 20, fontface = "bold", col = "black"), just = "left")
grid.lines(x = c(0.24, 0.29), y = 0.96, gp = gpar(col = "black", lwd = 3))
grid.points(x = 0.265, y = 0.96, pch = 18, size = unit(7, "mm"), gp = gpar(col = "black"))

# Sensitivity
grid.text("Sensitivity Analysis", x = 0.60, y = 0.96, gp = gpar(fontsize = 20, fontface = "bold", col = "black"), just = "left")
grid.lines(x = c(0.54, 0.59), y = 0.96, gp = gpar(col = "blue", lwd = 3))
grid.points(x = 0.565, y = 0.96, pch = 18, size = unit(7, "mm"), gp = gpar(col = "blue"))

dev.off()
cat("Plot created: Heterogeneity_Sensitivity_Forest.jpg\n")
