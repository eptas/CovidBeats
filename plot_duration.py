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

rcParams['figure.figsize'] = 20, 11

fig, (ax1, ax2) = plt.subplots(ncols=1, nrows=2, sharex=True, figsize=(20, 11))

ax1.grid()
ax2.grid()

ax1 = sb.boxplot(x='Country', y='Streams_total', hue='Year', data=all_data, ax=ax1, palette="RdBu", linewidth=0.8,
                 showmeans=True, meanprops={"marker": "s", "markerfacecolor": "white", "markeredgecolor": "grey"})
ax2 = sb.boxplot(x='Country', y='Streams_total', hue='Year', data=all_data, ax=ax2, palette="RdBu", linewidth=0.8,
                 showmeans=True, meanprops={"marker": "s", "markerfacecolor": "white", "markeredgecolor": "grey"})

ax1.set_ylim(7500000, 41000000)
ax2.set_ylim(900000, 5100000)

ax1.get_xaxis().set_visible(False)
ax2.set_xlabel(xlabel='country', fontsize=16)

ax1.set_ylabel("")
ax2.set_ylabel("")

fig.text(0.12, 0.55, 'absolute listening duration (h)', va='center', rotation='vertical', fontsize=16)

ax2.get_legend().remove()
ax1.legend(title='Pre vs Post', fontsize=14)

ax1.xaxis.tick_top()
ax2.xaxis.tick_bottom()

ax1.set_title("Listening duration pre- vs post-Covid onset", size=18)

ax2.set_xticklabels(['Germany', 'Portugal', 'France', 'Spain', 'Italy', 'Belgium', 'Netherlands', 'Switzerland', 'Denmark', 'Finland', 'UK'])
fig.subplots_adjust(left=0.15, right=0.85, bottom=0.15, top=0.85, hspace=0.05)

plt.savefig("listening_duration.png", bbox_inches='tight')
plt.close()
