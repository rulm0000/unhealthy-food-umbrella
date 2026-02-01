# Overlay Legend on Forest Plots
# Reads: _Base.jpg and Forest_Plot_Legend.png
# Writes: Final .jpg

from PIL import Image
import os

def overlay_legend(base_path, legend_path, final_path):
    if not os.path.exists(base_path):
        print(f"Base plot not found: {base_path}")
        return
    if not os.path.exists(legend_path):
        print(f"Legend not found: {legend_path}")
        return

    base = Image.open(base_path).convert("RGBA")
    legend = Image.open(legend_path).convert("RGBA")

    # Resize legend if mostly too big? 
    # Usually standard is fine. Let's keep it.
    
    # Position: Top Center (-ish)
    # The plot has headers. We want it above the plot/headers or just below header?
    # User said "impose... middle centered".
    # Let's put it at y=50 pixels from top, centered X.
    
    # Calculate X
    bg_w, bg_h = base.size
    lg_w, lg_h = legend.size
    
    x = (bg_w - lg_w) // 2
    y = 0  # Align to very top, or slightly down.
    # forestplot leaves some margin.
    
    # Paste (using legend alpha channel as mask)
    base.paste(legend, (x, y), legend)
    
    # Convert back to RGB and save
    final = base.convert("RGB")
    final.save(final_path, quality=95)
    print(f"Saved final plot: {final_path}")

# 1. Overlay Heterogeneity
overlay_legend("Heterogeneity_Sensitivity_Forest_Base.jpg", 
               "Forest_Plot_Legend.png", 
               "Heterogeneity_Sensitivity_Forest.jpg")

# 2. Overlay Sensitivity
overlay_legend("Sensitivity_Forest_Plot_Base.jpg", 
               "Forest_Plot_Legend.png", 
               "Sensitivity_Forest_Plot.jpg")
