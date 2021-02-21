# -*- coding: utf-8 -*-
"""
Created on Sat Feb 20 19:03:23 2021

@author: Lenovo
"""
# This approach to 3D game of life relies on dictionaries
# the indices of the dictionary lists of each offset are the same
# so the populations are summed across the indices for each offset
# from a coordinate, giving the neighbor populations

import itertools
import random
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401 unused import
import os

os.chdir(r'C:\Users\Lenovo\Demo\gameoflifeImages')

dimensions = 3 # n dimensional spzce >=2
size = 12
a = [range(size)]*dimensions
coordlist = list(itertools.product(*a))
offsets = [[-1,0,1]]*dimensions
cubelist = list(itertools.product(*offsets))

# populate the space with 1's and 0's
coorddict = {}
for point in coordlist:
    coorddict[point] = random.randint(0,1)

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


# start a forloop that captures each timestep in the game of life
timesteps = 15

for timestep in range(timesteps):    
    
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
    

    plotcoords = []
    for coord in range(len(coordlist)):
        if  (list(coorddict.values())[coord] == 1 and pointpops[coord] in [5,6,7]) or pointpops[coord] in [5,7]: # survival / generation /death rule here
            plotcoords.append(coordlist[coord])
            # repopulate coorddict values
            coorddict[list(coorddict)[coord]] = 1
        else: coorddict[list(coorddict)[coord]] = 0
    
    x_coords = []
    y_coords = []
    z_coords = []
    for x,y,z in plotcoords:
        x_coords.append(x)
        y_coords.append(y)
        z_coords.append(z)
    
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(x_coords, y_coords, z_coords, c = 'blue')
    #plt.show()
    plt.savefig('myplt' + str(timestep) + '.png', bbox_inches="tight", pad_inches = 0)
    plt.close()

# make gif from png files
    
os.system(r'cmd /c "ffmpeg -i myplt%d.png -vf palettegen palette.jpg" ')

os.system(r'cmd /c "ffmpeg -framerate 10 -i C:\Users\Lenovo\Demo\gameoflifeImages\myplt%d.png -i palette.jpg -lavfi paletteuse C:\Users\Lenovo\Demo\gameoflifeImages\output.gif"')

# delete png files
[os.remove(file) for file in os.listdir(r'C:\Users\Lenovo\Demo\gameoflifeImages') if file.endswith('.png')]
