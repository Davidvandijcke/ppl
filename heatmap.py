
import pandas as pd 
import numpy as np
import os
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap

# get relative folder
dir_main = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
dir_dataIn = os.path.join(dir_main, "data", "in")
airlong = pd.read_csv(os.path.join(dir_dataIn, "tech_companies_rto.csv"))
airlong = airlong[(airlong.RTO_1 != "missing") & (~airlong.RTO_1.isna())]
airlong['end_dt'] = pd.to_datetime("2024-01-01")
airlong = airlong.rename(columns = {'Company Name': 'region_english', 'RTO_1': 'start_dt'})
airlong = airlong[~(airlong.region_english == "Broadcom")]

def plotHeatMap(airlong):
    """ plot heat map with days on the x axis, regions on the y axis, and the number of alerts as the color scale """

    # duplicate rows for each alert for every day between start_dt and end_dt
    temp = airlong[['region_english', 'start_dt', 'end_dt']].copy()
    temp['start_dt'] = pd.to_datetime(temp['start_dt'])
    temp['end_dt'] = pd.to_datetime(temp['end_dt'])
    temp['days'] = temp.apply(lambda x: pd.date_range(x['start_dt'], x['end_dt']), axis = 1)
    temp = temp.explode('days')
    temp.drop(columns = ['start_dt', 'end_dt'], inplace = True)
    temp['days'] = temp['days'].dt.date
    temp = temp.groupby(['region_english', 'days']).size().reset_index()
    temp.columns = ['region_english', 'days', 'alerts']
    
    max_alerts = temp.alerts.max()

    
    # pivot table
    temp = temp.pivot(index = 'region_english', columns = 'days', values = 'alerts') 

    temp = temp.fillna(0)
    # sort by number of alerts
    temp = temp.reindex(temp.sum(axis = 1).sort_values(ascending = True).index)
    # plot heatmap
    fig, ax = plt.subplots(figsize = (12, 10))


    # create a list of the colors in the preceding comments
    myColors = ['#004B61', '#D9ECF1']
    myColors.reverse()
    #myColors = ["#f7feae","#b7e6a5","#7ccba2","#46aea0","#089099","#00718b","#045275"]
    cmap = LinearSegmentedColormap.from_list('Custom', myColors, max_alerts+1)

    # sort temp by average number of alerts
    temp = temp.reindex(temp.mean(axis = 1).sort_values(ascending = True).index)
    #temp = temp.reindex(sorted(temp.index), axis = 0)
    
    sns.heatmap(temp,  cmap=cmap, ax = ax, 
            cbar_kws = {'label': 'Return to Office', "shrink" : 0.5})
    ax.set_title("")
    
    # drop axis labels
    ax.set_xlabel("")
    ax.set_ylabel("")
    
    # increase font size
    ax.tick_params(axis = 'both', which = 'major', labelsize = 16)

   # ax.tick_params(axis = 'both', which = 'minor', labelsize = 12)
    
    # increase legend font size
    cbar = ax.collections[0].colorbar
    cbar.ax.tick_params(labelsize = 14)
    
    # add a tick for each number
    cbar.set_ticks([i for i in range(0, max_alerts + 1)])
    
    # increase legend title size
    cbar.ax.set_ylabel(cbar.ax.get_ylabel(), fontsize = 18)

    
    # set background white
    ax.set_facecolor("white")
    
    # tight layout
    plt.tight_layout()
    
    fig.autofmt_xdate(rotation=45)
    
    
    # Show only every 4th label
    labels = ax.get_xticklabels()  # Get x labels
    for i, label in enumerate(labels):
        if i % 4 != 0:  # Hide every label that is not a multiple of 4
            label.set_visible(False)
    ax.set_xticklabels(labels)  # Set new label list


    plt.savefig(os.path.join("/Users/davidvandijcke/Dropbox (University of Michigan)/Apps/Overleaf/RTO/figs/", 
                             "heatmap.pdf"), dpi = 600)
    plt.show()
    
