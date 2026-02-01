import matplotlib.pyplot as plt
import matplotlib.lines as mlines

# Set font to match R forestplot (usually Arial/Sans on Windows)
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['Arial', 'Helvetica', 'DejaVu Sans']

# Create a figure
# Smaller size to resolve "too large" issue
fig, ax = plt.subplots(figsize=(6, 1.2)) # Reduced from (10, 2)
ax.axis('off')

# Define handles
# 'D' is Diamond marker
# Reduced marker size (18 -> 12)
black_diamond = mlines.Line2D([], [], color='black', marker='D', linestyle='None',
                          markersize=12, label='Primary Analysis')
blue_diamond = mlines.Line2D([], [], color='blue', marker='D', linestyle='None',
                          markersize=12, label='Sensitivity Analysis')

# Create legend
# Reduced fontsize (24 -> 16)
legend = ax.legend(handles=[black_diamond, blue_diamond], 
                   loc='center', 
                   ncol=2, 
                   frameon=False,
                   fontsize=16,
                   handletextpad=0.2,
                   columnspacing=1.5)

# Save as PNG with transparency
output_path = 'Forest_Plot_Legend.png'
plt.savefig(output_path, dpi=300, bbox_inches='tight', transparent=True)
print(f"Legend saved to {output_path}")
