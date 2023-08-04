import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as stats

from scipy.stats import pearsonr as pear


df = pd.read_excel('dataset_merged_for_plotting.xlsx')

plot_data = df.filter(items=['pleasure', 'sps5_sum', 'reassurance', 'attachment', 'sense_reliable_alliance', 'spectral_centroid.max',
                             'spectral_flux.stdev', 'spectral_complexity.median', 'spectral_complexity.mean',
                             'dynamic_complexity', 'melbands_flatness_db.stdev', 'zerocrossingrate.stdev', 'mfc1',
                             'onset_rate', 'bpm'])

plot_data = plot_data.rename(columns={"pleasure": "PLEASURE", "sps5_sum": "SPS5", "reassurance": "REASSURANCE", "attachment": "ATTACH", "sense_reliable_alliance": "RELIABLE", "spectral_centroid.max": "SPECCENT.max",
                          "spectral_flux.stdev": "SPECFLUX.std", "spectral_complexity.median": "SPECCOMPL.med", "spectral_complexity.mean": "SPECCOMPL.mean",
                          "dynamic_complexity": "DYNCOMPL", "melbands_flatness_db.stdev": "MELFLAT.std", "zerocrossingrate.stdev": "ZEROCROSS.std",
                          "mfc1": "MFC1", "onset_rate": "ONSET", "bpm": "BPM"})


def corr_sig(dataFrame=None):
    p_matrix = np.zeros(shape=(dataFrame.shape[1], dataFrame.shape[1]))
    for col in dataFrame.columns:
        for col2 in dataFrame.drop(col, axis=1).columns:
            _ , p = stats.pearsonr(dataFrame[col], dataFrame[col2])
            p_matrix[dataFrame.columns.to_list().index(col), dataFrame.columns.to_list().index(col2)] = p
    return p_matrix


corr = plot_data.corr(method='pearson')
p_values = corr_sig(plot_data)
# p_mask = np.invert(np.tril(p_values < 0.05))


# Initialize plot
f, ax = plt.subplots(figsize=(12, 10))

# Generate a mask
mask = np.triu(np.ones_like(corr, dtype=bool))

# Define colormap
cmap = sns.diverging_palette(230, 20, as_cmap=True)

# Plot heatmap
sns.heatmap(corr, annot=True, mask=mask, cmap=cmap)

plt.title('Correlation: social provision x acoustic features')
plt.savefig("Correlation.png", bbox_inches='tight')
