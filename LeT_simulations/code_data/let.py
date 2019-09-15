"""
Simulations Learning to Time

    lambda ~ N(x, mu, sigma), and lambda > 0; is the transition rate between
    states every j trial a new value of lambda is sampled.
    N(t) is the state active in the t-time since trial, which depends on lambda
    and t, so, N(t) = ceil (lambda * t)

"""

import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib import rc
# rc('text', usetex=True)  # if LaTeX is installed, otherwise comment
rc('figure', figsize=(5.5, 4))
rc('axes', ymargin=0)

"""
Simulate N(t) = ceiling(lambda * t), with lambda normally distributed.
In "Learning to time: a perspective", Appendix, they simulate two 'walks'
with lambda = {0.8,1.2} sampled from N(1,0.2)
"""


def n_t(t, mean, sd):
    """
    lambda admit just positive values
    """

    lmbd = np.random.normal(mean, sd, 1)
    vec_time = np.arange(t)
    nt_iter = np.zeros(t)

    while lmbd < 0:
        lmbd = np.random.normal(mean, sd, 1)

    for j in vec_time:
        nt_iter[j] = np.ceil(lmbd*j)
    return nt_iter


t = 60
time = np.arange(t)+1
mean = 1
sd = 0.2
trials = 100
trial_rf = np.zeros(trials)

gs = gridspec.GridSpec(1, 4, wspace=0.06)
ax1 = plt.subplot(gs[0, 0:3])
ax2 = plt.subplot(gs[0, 3])

for jj in np.arange(trials):
    Nt = n_t(t, mean, sd)
    trial_rf[jj] = Nt[-1]
    ax1.step(time, Nt, c='grey', lw=1, alpha=0.3)
    ax1.scatter(time[-1], Nt[-1], c='black', s=10, alpha=0.5)

ax1.text(20, np.max(trial_rf) - 10,
         r'$\lambda \sim \mathcal{{N}}(\mu=1, \sigma={{{}}})$'.format(sd) + "\n" +
         r'$T = {{{}}}$'.format(t) + ", {} trials".format(trials),
         {'color': 'k', 'fontsize': 10, 'ha': 'center', 'va': 'center',
          'bbox': dict(boxstyle="round", fc="w", ec="k", pad=0.1)})
ax1.set_xlabel('$t$ (time in trial)')
ax1.set_ylabel(r'$N(t) = \lceil \lambda t \rceil$')
ax2.hist(trial_rf, orientation="horizontal", histtype='step', linewidth=0.8,
         facecolor='grey', edgecolor='k', fill=True, alpha=0.5)
ax2.yaxis.tick_right()
ax2.set_ylim(0,np.max(trial_rf))
ax2.yaxis.set_label_position("right")
ax2.axhline(y = np.mean(trial_rf), color='black', linestyle=":")
ax2.set_ylabel(r'$N(t = T)$')
plt.savefig('nt_let.pdf', dpi=120)
plt.show()