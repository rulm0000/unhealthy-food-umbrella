# Sensitivity Forest Plot - BASE GENERATION
# Output: Sensitivity_Forest_Plot_Base.jpg
# - Base for Python Overlay

if (!requireNamespace("forestplot", quietly = TRUE)) stop("forestplot missing")
library(forestplot)
library(dplyr)
library(grid)

comp_file <- "Sensitivity_Comparison.csv"
if (!file.exists(comp_file)) stop("Comparison file not found")
df <- read.csv(comp_file, stringsAsFactors = FALSE)

row_sleep_orig <- which(grepl("Sleep Dissatisfaction \\(Original\\)", df$Label))
row_sleep_sens <- which(grepl("Sleep Dissatisfaction \\(Sensitivity\\)", df$Label))
row_adhd_orig <- which(grepl("ADHD Diagnosis \\(JF - Original\\)", df$Label))
row_adhd_sens <- which(grepl("ADHD Diagnosis \\(JF - Sensitivity\\)", df$Label))

labels <- c()
means <- c()
lowers <- c()
uppers <- c()
ks <- c()
marker_clrs <- c()

add_data <- function(idx, label_text, color) {
    labels <<- c(labels, label_text)
    means <<- c(means, df$OR[idx])
    lowers <<- c(lowers, df$CI_Lower[idx])
    uppers <<- c(uppers, df$CI_Upper[idx])
    ks <<- c(ks, df$k[idx])
    marker_clrs <<- c(marker_clrs, color)
}
add_spacer <- function() {
    labels <<- c(labels, "")
    means <<- c(means, NA)
    lowers <<- c(lowers, NA)
    uppers <<- c(uppers, NA)
    ks <<- c(ks, "")
}

add_data(row_sleep_orig, "  Sleep Dissatisfaction", "black")
add_data(row_sleep_sens, "", "blue")
add_spacer()
add_data(row_adhd_orig, "  ADHD Diagnosis", "black")
add_data(row_adhd_sens, "", "blue")

hr_line_list <- list()
hr_line_list[["4"]] <- gpar(col = "grey60", lwd = 1)

es_vals <- ifelse(is.na(means), "",
    paste0(
        sprintf("%.2f", means), " (",
        sprintf("%.2f", lowers), ", ",
        sprintf("%.2f", uppers), ")"
    )
)
tabletext <- cbind(
    c("Outcome", labels),
    c("Effect Size (95% CIs)", es_vals),
    c("k", ks)
)
final_means <- c(NA, means)
final_lowers <- c(NA, lowers)
final_uppers <- c(NA, uppers)

is_summary <- rep(FALSE, length(final_means))
is_summary[1] <- TRUE

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

jpeg_height <- (5 + length(labels) * 0.45) * 300
jpeg_width <- 15 * 300
output_file <- "Sensitivity_Forest_Plot_Base.jpg"

jpeg(output_file, width = jpeg_width, height = jpeg_height, res = 300)

forestplot(
    labeltext = tabletext,
    mean = final_means,
    lower = final_lowers,
    upper = final_uppers,
    fn.ci_norm = fn_custom,
    is.summary = is_summary,
    xlog = TRUE,
    col = fpColors(lines = "black", zero = "red"),
    lwd.ci = 3,
    colgap = unit(4, "mm"),
    title = "",
    xlab = "Odds Ratio (log scale)",
    hrzl_lines = hr_line_list,
    txt_gp = fpTxtGp(
        label = gpar(cex = 1.6),
        ticks = gpar(cex = 1.5),
        xlab = gpar(cex = 1.6)
    ),
    graph.pos = 2
)
dev.off()
cat("Base plot saved to", output_file, "\n")
