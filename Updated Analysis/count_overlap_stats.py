import pandas as pd
import numpy as np

def analyze_overlap_and_sensitivity():
    # 1. Analyze Overlap
    # ------------------
    try:
        df = pd.read_csv('extracted_Rerun_Analyses.csv')
        
        # Clean Study ID
        if 'Study ID' in df.columns:
            df['Study ID Clean'] = df['Study ID'].astype(str).str.strip().str.lower()
        else:
            print("Error: 'Study ID' column not found.")
            return

        # Define Domains
        domains = {
            'Sleep': ['Adverse Sleep-Related Outcomes', 'Short Sleep Duration', 'Poor Sleep Quality', 'Insomnia', 'Sleep Dissatisfaction', 'Sleep Problems'],
            'Psychological Distress': ['Common Mental Disorders', 'Depression', 'Stress', 'Anxiety', 'Happiness'],
            'ADHD': ['ADHD Diagnosis', 'ADHD Risk']
        }
        
        def get_domain(outcome):
            for domain, outcomes in domains.items():
                if outcome in outcomes:
                    return domain
            return 'Other'

        df['Domain'] = df['Outcome'].apply(get_domain)
        df_domain = df[df['Domain'] != 'Other'].copy()
        
        # We define a "Unique Primary Study" as (Study ID, Effect Size, Domain).
        # We want to see how many times each is duplicated.
        
        grouped = df_domain.groupby(['Study ID Clean', 'Effect Size', 'Domain'])
        
        within_review_overlap_count = 0
        between_review_overlap_count = 0
        
        total_unique_primary_studies = len(grouped)
        total_duplicates_removed = 0

        for name, group in grouped:
            if len(group) > 1:
                # We have duplicates.
                # Check Review IDs
                review_ids = group['Review ID'].unique()
                
                # Total instances - 1 = number of redundant copies
                # E.g. if study appears 3 times, we have 2 overlaps to report?
                # Or just report "X pairs"?
                # User asked for "number of withinreview and between-review primary study overlap"
                
                # Let's count "Overlap Events"
                # If a study is in Review A and Review B -> 1 Between-review overlap
                # If a study is in Review A, B, C -> 2 Between-review overlaps (A-B, B-C)
                # If a study is in Review A twice -> 1 Within-review overlap
                
                # Logic:
                # Count pairs?
                # Or just count raw instances?
                
                # Let's verify if within-review overlap exists
                counts_per_review = group['Review ID'].value_counts()
                
                # Any review with count > 1 implies within-review overlap
                for rid, count in counts_per_review.items():
                    if count > 1:
                        within_review_overlap_count += (count - 1)
                        
                # If there are multiple reviews involved, that's between-review overlap
                unique_reviews = len(counts_per_review)
                if unique_reviews > 1:
                    between_review_overlap_count += (unique_reviews - 1)

        print(f"Overlap Analysis:")
        print(f"  Within-review primary study overlap count: {within_review_overlap_count}")
        print(f"  Between-review primary study overlap count: {between_review_overlap_count}")
        print(f"  (Calculated as excess copies beyond the first instance)")

    except Exception as e:
        print(f"Error analyzing overlap: {e}")

    # 2. Analyze Sensitivity Results
    # ------------------------------
    try:
        report_file = 'Heterogeneity_Sensitivity_Report.csv'
        if not pd.io.common.file_exists(report_file):
            print(f"Error: {report_file} not found.")
            return
            
        res = pd.read_csv(report_file)
        
        # Calculate differences using valid numeric columns
        # Prepare numeric columns
        res['Orig_OR'] = pd.to_numeric(res['Orig_OR'], errors='coerce')
        res['New_OR'] = pd.to_numeric(res['New_OR'], errors='coerce')
        res['Orig_Upper'] = pd.to_numeric(res['Orig_Upper'], errors='coerce')
        res['Orig_Lower'] = pd.to_numeric(res['Orig_Lower'], errors='coerce')
        res['New_Upper'] = pd.to_numeric(res['New_Upper'], errors='coerce')
        res['New_Lower'] = pd.to_numeric(res['New_Lower'], errors='coerce')
        
        # Check robustness
        # 1. Did significance change?
        # Sig if interval does not cross 1.0
        
        def is_sig(lower, upper):
            return (lower > 1.0) or (upper < 1.0)
            
        res['Orig_Sig'] = res.apply(lambda r: is_sig(r['Orig_Lower'], r['Orig_Upper']), axis=1)
        res['New_Sig'] = res.apply(lambda r: is_sig(r['New_Lower'], r['New_Upper']), axis=1)
        
        sig_changes = res[res['Orig_Sig'] != res['New_Sig']]
        
        # Check magnitude change
        res['Pct_Change_OR'] = abs((res['New_OR'] - res['Orig_OR']) / res['Orig_OR']) * 100
        avg_change = res['Pct_Change_OR'].mean()
        
        print(f"\nSensitivity Analysis Results:")
        print(f"  Average change in Odds Ratio: {avg_change:.2f}%")
        print(f"  Significance changes: {len(sig_changes)}")
        if len(sig_changes) > 0:
            print("  Outcomes with changed significance:", sig_changes['Group'].tolist())
        else:
            print("  All outcomes maintained their original statistical significance status.")
            
        # Heterogeneity Impact
        res['Orig_I2'] = pd.to_numeric(res['Orig_I2'], errors='coerce')
        res['New_I2'] = pd.to_numeric(res['New_I2'], errors='coerce')
        
        i2_change = res['Orig_I2'] - res['New_I2']
        print(f"  Average reduction in I2: {i2_change.mean():.2f}% points")
        
    except Exception as e:
        print(f"Error analyzing sensitivity: {e}")

if __name__ == "__main__":
    analyze_overlap_and_sensitivity()
