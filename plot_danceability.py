import pandas as pd
import os

import seaborn as sb
import matplotlib.pyplot as plt

from pathlib import Path
from matplotlib import rcParams

cwd = Path.cwd()
all_data = pd.read_excel(os.path.join(cwd, './data_frame_pre_post.xlsx'))

# pre = all_data['Year'].unique()[0]
# post = all_data['Year'].unique()[1]

all_data.loc[all_data[all_data['Country_legend'].astype('str').str.contains('Finnland')].index, 'Country_legend'] = 'Finland'
all_data.loc[all_data[all_data['Country_legend'].astype('str').str.contains('Swiss')].index, 'Country_legend'] = 'Switzerland'
all_data.loc[all_data[all_data['Country_legend'].astype('str').str.contains('Great')].index, 'Country_legend'] = 'UK'

rcParams['figure.figsize'] = 20, 11

fig = sb.boxplot(x='Country_legend', y='Danceability_mean', hue='Year', data=all_data, palette="RdBu", linewidth=0.8,
                 showmeans=True, meanprops={"marker": "s", "markerfacecolor": "white", "markeredgecolor": "grey"})

fig.grid(axis="y")
fig.set_ylim(-3, 2.1)


fig.set_ylabel(ylabel='normalized danceability', fontsize=16)
fig.set_xlabel(xlabel='country', fontsize=16)

fig.legend(title='Pre vs Post', fontsize=14)

fig.set_title("Danceability pre- vs post-Covid onset", size=18)

plt.savefig("Danceability.png", bbox_inches='tight')
plt.close()
