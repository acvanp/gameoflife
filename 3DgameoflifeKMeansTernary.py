# -*- coding: utf-8 -*-
"""
Created on Sat Feb 20 19:03:23 2021

@author: Lenovo
"""
# This approach to 3D game of life relies on dictionaries
# the indices of the dictionary lists of each offset are the same
# so the populations are summed across the indices for each offset
# from a coordinate, giving the neighbor populations

import statistics
import itertools
import random as rn
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401 unused import
import os
from matplotlib.gridspec import GridSpec
from itertools import combinations
import time
import numpy as np
from sklearn.cluster import KMeans
import pandas as pd

os.chdir(r'C:\Users\Lenovo\Demo\gameoflifeImages')

dimensions = 3 # n dimensional space >=2
size = 10
a = [range(size)]*dimensions
coordlist = list(itertools.product(*a))
offsets = [[-1,0,1]]*dimensions
cubelist = list(itertools.product(*offsets))
triallist = []

from time import sleep
from tqdm import tqdm

# populate the space with 1's and 0's
coorddict = {}
for point in coordlist:
    coorddict[point] = [1,3,0,0,0][rn.randint(0,3)]

# make a list of the different cubes or n-dimensional spaces
# by offsetting the points in thecoordlist by each variation of the cubelist
hypercube = []
for cube in cubelist:
    offset_points = []
    for points in coordlist:
        offsetpoint = []
        for pos in range(len(points)):
            offsetpoint.append(points[pos] + cube[pos])
        offset_points.append(tuple(offsetpoint))
    hypercube.append(offset_points)


# have a population count time series to plot in the bottom subplot
poplist = []

# start a forloop that captures each timestep in the game of life
timesteps = 34

for timestep in tqdm(range(timesteps)):    
    
# use dictionaries to capture the populations at each point
# the ordering of the different offsets is the same as the original
# coordinate list, so the index of each point gets summed across each cube
# in the cube list        
    mycube = {}
    cc = 0
    for cube in hypercube:
        tempdict = {}
        for point in cube:
            if point not in coorddict.keys():
                tempdict[point] =  0
            else: 
                tempdict[point] = coorddict[point]
        mycube[cubelist[cc]] = tempdict
        cc = cc + 1
    
    # sum the populations at each point 
    pointpops = []
    for i in range(len(coordlist)):
        pointsum = []
        for k in mycube.keys():
            if k == (0, 0, 0): continue # do not sum the original cube with its neighbors
            pointsum.append( mycube[k][list(mycube[k])[i]] ) # nightmarish data coersion
        pointpops.append(sum(pointsum))
    

    plotcoords = {1:[],2:[]}
    #apply the game of life rule here
    for coord in range(len(coordlist)):

        if (list(coorddict.values())[coord] == 1 and pointpops[coord] in [2,4]) or pointpops[coord] in [5,7]: # survival / generation /death rule here
            plotcoords[1].append(coordlist[coord])
            # repopulate coorddict values
            coorddict[list(coorddict)[coord]] = 1
            
        elif (list(coorddict.values())[coord] == 3 and pointpops[coord] in range(16,22)) or pointpops[coord] in range(18,24): # survival / generation /death rule here
            plotcoords[2].append(coordlist[coord])
            # repopulate coorddict values
            coorddict[list(coorddict)[coord]] = 3
            
        else: coorddict[list(coorddict)[coord]] = 0
    
    poplist.append(sum(coorddict.values()))
        
    x_coords1 = []
    y_coords1 = []
    z_coords1 = []
    
    for x,y,z in plotcoords[1]:
        x_coords1.append(x)
        y_coords1.append(y)
        z_coords1.append(z)
    
    model = KMeans(n_clusters = 3)
    model.fit(pd.DataFrame({'x' : x_coords1, 'y' : y_coords1, 'z' : z_coords1}))
    labels = model.predict(pd.DataFrame({'x' : x_coords1, 'y' : y_coords1, 'z' : z_coords1}))
    centroids1  = model.cluster_centers_
    '''
    pointB1 = centroids1[0]
    pointB2 = centroids1[1]
    pointB3 = centroids1[2]
    
    if timestep > 0:
        distance1 = ((pointA1[0] - pointB1[0])**2 + (pointA1[1] - pointB1[1])**2  + (pointA1[2] - pointB1[2])**2)**0.5 
        distance2 = ((pointA2[0] - pointB2[0])**2 + (pointA2[1] - pointB2[1])**2  + (pointA2[2] - pointB2[2])**2)**0.5 
        distance3 = ((pointA3[0] - pointB3[0])**2 + (pointA3[1] - pointB3[1])**2  + (pointA3[2] - pointB3[2])**2)**0.5 
        print(np.mean([distance1, distance2, distance3]))
    
    pointA1 = centroids1[0]
    pointA2 = centroids1[1]
    pointA3 = centroids1[2]
    '''
    cx1 = []
    cy1 = []
    cz1 = []
    
    
    for x,y,z in centroids1:
        cx1.append(x)
        cy1.append(y)
        cz1.append(z)
        
    
    x_coords2 = []
    y_coords2 = []
    z_coords2 = []
    
    for x,y,z in plotcoords[2]:
        x_coords2.append(x)
        y_coords2.append(y)
        z_coords2.append(z)
    
    model2 = KMeans(n_clusters = 3)
    model2.fit(pd.DataFrame({'x' : x_coords2, 'y' : y_coords2, 'z' : z_coords2}))
    labels2 = model2.predict(pd.DataFrame({'x' : x_coords2, 'y' : y_coords2, 'z' : z_coords2}))
    centroids2  = model2.cluster_centers_
    
    cx2 = []
    cy2 = []
    cz2 = []
    
    for x,y,z in centroids2:
        cx2.append(x)
        cy2.append(y)
        cz2.append(z)
        
    pts_colors = ['red'] * len(x_coords1)
    [pts_colors.append('cyan') for i in x_coords2]
    centroid_colors = ['magenta']*3
    [centroid_colors.append('navy') for i in cx2]
    markers = [u'd'] * len(x_coords1)
    [markers.append(u'o') for i in x_coords2]
    [x_coords1.append(i) for i in x_coords2] 
    [y_coords1.append(i) for i in y_coords2]
    [z_coords1.append(i) for i in z_coords2] 
    [cx1.append(i) for i in cx2]
    [cy1.append(i) for i in cy2]
    [cz1.append(i) for i in cz2]

    fig = plt.figure(figsize = (12,10))
    gs = GridSpec(5,1)
    ax0 = fig.add_subplot(gs[0:3,0], projection = '3d')
    
    for x,y,z,c,m in zip(x_coords1, y_coords1, z_coords1, pts_colors, markers):
        ax0.scatter(x,y,z, s = 40, c = c, marker=m)
    
    for x,y,z,c in zip(cx1, cy1, cz1, centroid_colors):
        ax0.scatter(x,y,z, s = 60, c = c, marker = 's')

    ax0.set_xlim3d(0,size)
    ax0.set_ylim3d(0,size)
    ax0.set_zlim3d(0,size)
    #plt.show()
    ax1 = fig.add_subplot(gs[4,0])
    ax1.plot(poplist)
    #plt.yscale('log')
    plt.xlim([0.1, timesteps + 1])
    plt.ylim([0.1, size ** dimensions])
    plt.ylabel("Population")
    plt.xlabel("Timestep")
    plt.savefig('myplt' + str(timestep) + '.png', bbox_inches="tight", pad_inches = 0)
    plt.close()

# make gif from png files
    
os.system(r'cmd /c "ffmpeg -i myplt%d.png -vf palettegen palette.jpg" ')

os.system(r'cmd /c "ffmpeg -framerate 8 -i C:\Users\Lenovo\Demo\gameoflifeImages\myplt%d.png -i palette.jpg -lavfi paletteuse C:\Users\Lenovo\Demo\gameoflifeImages\output.gif"')

# delete png files 
[os.remove(file) for file in os.listdir(r'C:\Users\Lenovo\Demo\gameoflifeImages') if file.endswith('.png')]





import pandas as pd
import statsmodels.api as sm
import matplotlib.pylab as plt

fig, ax1 = plt.subplots(nrows = 1, ncols = 1)

# Andrews' curves
# multiline plot with group by
for trial in triallist: 
    ax1.plot(range(len(trial)), trial)
ax1.legend(range(len(triallist)))    
plt.show()
