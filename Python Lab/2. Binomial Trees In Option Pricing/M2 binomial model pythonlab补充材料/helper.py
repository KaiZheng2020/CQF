# Helper functions to plot binomial tree
# Save the file with '.py' extension on the same location 

import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm
import networkx as nx
import warnings
warnings.filterwarnings('ignore')

# Plot for Value-At-Risk
def plot_var():

    # Hard coded for 95% confidence limit
    
    # Specify confidence level
    confidence_level = 0.95

    # Standard deviation
    z = norm.ppf(1-confidence_level)

    # Specify x ranges for plotting
    x = np.arange(-6,6,0.1)
    x2 = np.arange(-6,z,1/40.)

    # Figure text for annotation
    note1 ='Red area to the left of the'
    note2 ='dotted red line reprsesents'
    note3 ='5% of the total area'
    note4 ='The curve represents a profit/loss'
    note5 ='density function. The 5% VaR is'
    note6 ='1.64 standard deviation from the mean'

    # Set figure size
    plt.figure(figsize=(8, 6))

    # Plot probability density function
    plt.plot(x, norm.pdf(x, 0, 1))

    plt.ylim(0,0.5)

    # Plot vertical line at confidence limit
    plt.axvline(x=z, ymin=0, ymax = 1, linewidth=2, ls='dotted', color='r')

    # Fill the left tail from confidence limit
    plt.fill_between(x2,norm.pdf(x2, 0, 1), color='red')

    # Annotate with notes
    plt.figtext(0.14,0.5,note1)
    plt.figtext(0.14,0.47,note2)
    plt.figtext(0.14,0.44,note3)

    plt.figtext(0.5,0.85,note4)
    plt.figtext(0.5,0.82,note5)
    plt.figtext(0.5,0.79,note6)

    # Annotate and plot title
    plt.annotate("", xy=(-2.5,0.08), xytext=(-2.5,0.18), arrowprops=dict(facecolor='red',shrink=0.001))
    plt.annotate('z = '+str(np.around(z,2)), xy=(-1.5,0.05))
    plt.title("VaR at 95% confidence level")

# Hard coded for Binomial Plot
n = 4 #hard coded

# Plot Binomial Grid
def binomial_grid(n):

    G=nx.Graph()
    for i in range(0,n+1):
        for j in range(1,i+2):
            if i<n:
                G.add_edge((i,j),(i+1,j))
                G.add_edge((i,j),(i+1,j+1))
    posG={}
    for node in G.nodes():
        posG[node]=(node[0],n+2+node[0]-2*node[1])
    
    nx.draw(G,pos=posG)

# Plot Asset Path - Text 
def plot_asset_path():

    plt.figure(figsize=(8, 6))

    # Start
    plt.figtext(0.02,0.49,'S')

    # Step 1
    plt.figtext(0.3,0.66,'uS')
    plt.figtext(0.3,0.3,'vS')

    # Step 2
    plt.figtext(0.5,0.76,'$u^2S$')
    plt.figtext(0.5,0.55,'uvS')
    plt.figtext(0.5,0.22,'$v^2S$')

    # Step 3
    plt.figtext(0.7,0.86,'$u^3S$')
    plt.figtext(0.7,0.66,'$u^2vS$') 
    plt.figtext(0.7,0.45,'$uv^2S$')
    plt.figtext(0.7,0.11,'$v^3S$')

    # Step 4
    plt.figtext(0.9,0.95,'$u^4S$')
    plt.figtext(0.9,0.75,'$u^3vS$')
    plt.figtext(0.9,0.55,'$u^2v^2S$')
    plt.figtext(0.9,0.35,'$uv^3S$')
    plt.figtext(0.9,0.03,'$v^4S$')

    binomial_grid(n)

# Plot Probability - Text
def plot_probability():

    plt.figure(figsize=(8, 6))

    # Start
    plt.figtext(0.02,0.49,'1')

    # Step 1
    plt.figtext(0.3,0.66,'p')
    plt.figtext(0.3,0.3,'1-p')

    # Step 2
    plt.figtext(0.5,0.76,'$p^2$')
    plt.figtext(0.5,0.55,'2p(1-p)')
    plt.figtext(0.5,0.20,'$(1-p)^2$')

    # Step 3
    plt.figtext(0.7,0.86,'$p^3$')
    plt.figtext(0.7,0.66,'$3p^2(1-p)$')
    plt.figtext(0.7,0.45,'$3p(1-p)^2$')
    plt.figtext(0.7,0.11,'$(1-p)^3$')

    # Step 4
    plt.figtext(0.9,0.95,'$p^4$')
    plt.figtext(0.9,0.75,'$4p^3(1-p)$')
    plt.figtext(0.9,0.55,'$6p^2(1-p)^2$')
    plt.figtext(0.9,0.35,'$4p(1-p)^3$')
    plt.figtext(0.9,0.00,'$(1-p)^4$')

    binomial_grid(n)

# Plot Price and Option Tree
def plot_binomial_tree(initial_spot, asset_array, option_array, delta_array):

    s = initial_spot
    px = asset_array
    opx = option_array
    delta = delta_array

    plt.figure(figsize=(8, 6))

    # Start
    plt.figtext(-0.07,0.50, 'S = '+str(s))
    plt.figtext(-0.07,0.47, 'V = '+str(opx[0,0]))
    plt.figtext(-0.07,0.44, '$\Delta$ = '+str(delta[0,0]))

    # Step 1
    plt.figtext(0.27,0.73, 'S = '+str(px[0,1]))
    plt.figtext(0.27,0.70, 'V = '+str(opx[0,1]))
    plt.figtext(0.27,0.67, '$\Delta$ = '+str(delta[0,1]))
    plt.figtext(0.27,0.30, 'S = '+str(px[1,1]))
    plt.figtext(0.27,0.27, 'V = '+str(opx[1,1]))
    plt.figtext(0.27,0.24, '$\Delta$ = '+str(delta[1,1]))

    # Step 2
    plt.figtext(0.5,0.84, 'S = '+str(px[0,2]))
    plt.figtext(0.5,0.81, 'V = '+str(opx[0,2]))
    plt.figtext(0.5,0.78, '$\Delta$ = '+str(delta[0,2]))
    plt.figtext(0.5,0.63, 'S = '+str(px[1,2]))
    plt.figtext(0.5,0.60, 'c = '+str(opx[1,2]))
    plt.figtext(0.5,0.57, '$\Delta$ = '+str(delta[1,2]))
    plt.figtext(0.5,0.20, 'S = '+str(px[2,2]))
    plt.figtext(0.5,0.17, 'c = '+str(opx[2,2]))
    plt.figtext(0.5,0.14, '$\Delta$ = '+str(delta[2,2]))

    # Step 3
    plt.figtext(0.7,0.96, 'S = '+str(px[0,3]))
    plt.figtext(0.7,0.93, 'V = '+str(opx[0,3]))
    plt.figtext(0.7,0.90, '$\Delta$ = '+str(delta[0,3]))
    plt.figtext(0.7,0.73, 'S = '+str(px[1,3]))
    plt.figtext(0.7,0.70, 'V = '+str(opx[1,3]))
    plt.figtext(0.7,0.67, '$\Delta$ = '+str(delta[1,3]))
    plt.figtext(0.7,0.50, 'S = '+str(px[2,3]))
    plt.figtext(0.7,0.47, 'V = '+str(opx[2,3]))
    plt.figtext(0.7,0.43, '$\Delta$ = '+str(delta[2,3]))
    plt.figtext(0.7,0.09, 'S = '+str(px[3,3]))
    plt.figtext(0.7,0.06, 'V = '+str(opx[3,3]))
    plt.figtext(0.7,0.03, '$\Delta$ = '+str(delta[3,3]))

    # Step 4
    plt.figtext(0.9,1.05, 'S = '+str(px[0,4]))
    plt.figtext(0.9,1.02, 'V = '+str(opx[0,4]))
    plt.figtext(0.9,0.99, '$\Delta$ = '+str(delta[0,4]))
    plt.figtext(0.9,0.83, 'S = '+str(px[1,4]))
    plt.figtext(0.9,0.80, 'V = '+str(opx[1,4]))
    plt.figtext(0.9,0.77, '$\Delta$ = '+str(delta[1,4]))
    plt.figtext(0.9,0.60, 'S = '+str(px[2,4]))
    plt.figtext(0.9,0.57, 'V = '+str(opx[2,4]))
    plt.figtext(0.9,0.54, '$\Delta$ = '+str(delta[2,4]))
    plt.figtext(0.9,0.39, 'S = '+str(px[3,4]))
    plt.figtext(0.9,0.36, 'V = '+str(opx[3,4]))
    plt.figtext(0.9,0.33, '$\Delta$ = '+str(delta[3,4]))
    plt.figtext(0.9,-0.01, 'S = '+str(px[4,4]))
    plt.figtext(0.9,-0.04, 'V = '+str(opx[4,4]))
    plt.figtext(0.9,-0.07, '$\Delta$ = '+str(delta[4,4]))

    binomial_grid(n)