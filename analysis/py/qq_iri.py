import glob as gl
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.font_manager as fm

prop = fm.FontProperties(fname='/home/mrrobot/anaconda3/lib/python3.7/'
                               'site-packages/matplotlib/mpl-data/fonts/'
                               'ttf/HelveticaNeue.ttf')

font = {'size': 10}
mpl.rc('font', **font)
path = '/home/mrrobot/Documentos/2019A/' \
       'paper_gen_decrement/analysis/data_results/'
savef_path = '/home/mrrobot/Documentos/2019A/paper_gen_decrement/' \
             'analysis/figures/'
fname = gl.glob(path + 'qq_*.csv')[0]
qqd = pd.read_csv(fname)

h1 = (np.ones_like(qqd['Delay'][qqd['phase'] == 'Baseline']) /
      qqd['Delay'][qqd['phase'] == 'Baseline'].size)

h2 = (np.ones_like(qqd['Delay'][qqd['phase'] == 'Experimental']) /
      qqd['Delay'][qqd['phase'] == 'Experimental'].size)

h3 = (np.ones_like(qqd['Immediate'][qqd['phase'] == 'Baseline']) /
      qqd['Immediate'][qqd['phase'] == 'Baseline'].size)

h4 = (np.ones_like(qqd['Immediate'][qqd['phase'] == 'Experimental']) /
      qqd['Immediate'][qqd['phase'] == 'Experimental'].size)

fig = plt.figure(figsize=(4, 4))
gs = mpl.gridspec.GridSpec(
        2, 2,
        width_ratios=[4, 1],
        height_ratios=[1, 4],
        wspace=0.05, hspace=0.05)

ax0 = fig.add_subplot(gs[1, 0])

ax0.scatter(qqd['Delay'][qqd['phase'] == 'Baseline'],
            qqd['Immediate'][qqd['phase'] == 'Baseline'],
            label='Baseline',
            color='#808080', edgecolor='w', s=25, linewidth=0.07)
ax0.scatter(qqd['Delay'][qqd['phase'] == 'Experimental'],
            qqd['Immediate'][qqd['phase'] == 'Experimental'],
            label='Experimental', marker='x',
            color='k', s=25, linewidth=0.3)
ax0.plot([60, 90], [60, 90], 'k--', linewidth=0.8, zorder=0)

ax0.set_xlabel('Delayed IRI (s)', fontsize=12, fontproperties=prop)
ax0.set_ylabel('Immediate IRI (s)', fontsize=12, fontproperties=prop)
ax0.tick_params(axis='both', direction='in')
ax0.set_xlim(59, 90)
ax0.set_ylim(59, 90)
ax0.legend(frameon=False, prop=prop, handletextpad=0.05)

ax1 = fig.add_subplot(gs[0, 0])

ax1.hist(qqd['Delay'][qqd['phase'] == 'Baseline'], weights=h1,
         bins=30, color='#808080', histtype='step', linewidth=1.5)

ax1.hist(qqd['Delay'][qqd['phase'] == 'Experimental'], weights=h2,
         bins=30, color='k', histtype='step', linewidth=1.5)
ax1.set_ylabel('Density', fontsize=12, fontproperties=prop)
ax2 = fig.add_subplot(gs[1, 1])
ax2.hist(qqd['Immediate'][qqd['phase'] == 'Baseline'],
         orientation="horizontal", bins=30,  weights=h3,
         color='#808080', histtype='step', linewidth=1.5)

ax2.hist(qqd['Immediate'][qqd['phase'] == 'Experimental'],
         orientation="horizontal", bins=30,  weights=h4,
         color='k', histtype='step', linewidth=1.5)

ax1.tick_params(axis='both', direction='in')
ax2.tick_params(axis='both', direction='in')
ax1.yaxis.set_ticks_position('left')
ax2.xaxis.set_ticks_position('bottom')
ax1.spines['right'].set_visible(False)
ax1.spines['top'].set_visible(False)
ax2.spines['right'].set_visible(False)
ax2.spines['top'].set_visible(False)
ax1.set_xticks([])
ax1.set_yticks([0.2, 0.5])
ax2.set_yticks([])
ax2.set_xticks([0.2, 0.5])
ax1.set_xlim(59, 90)
ax2.set_ylim(59, 90)
plt.show()
fig.savefig(savef_path + 'Figure_5.pdf', bbox_inches='tight')
