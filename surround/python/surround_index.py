
# coding: utf-8

# In[ ]:

import numpy as np
import matplotlib.pyplot as plt


# # Create neighborhood surround index, aka gapiness
# ### Definitions
# - A neighborhood is composed of the trees surrounding an individual within a certain radius.
# - A connected component of the neighborhood is continuous area in the neighborhood this is both occupied by neighbors and at least as large as a specified area.
# 
# ### Parameters
# - Neighborhood size: 9 quadrats (8 quadrats surround targets quadrat)
# - Connected component size: 2 quadrats (8 possible connected components for 9 quadrat neighborhood)
# 

# ## Individual neighbor values (nv): ratio of neighbor BA and distance to target
# ### ${nv_{target,neighbor}} = \frac{BA_{neighbor}^\alpha}{Distance_{target, neighbor}^\beta}$, 
# where $BA$ is the basal area and $Distance$ is the distance between target and neighbor
# 
# - Neighbors are additive within connected components
# - Use $\alpha = 1$ and $\beta = 1$ for now

# ## Calculate Neighbor Surround Index (NSI)
# $nsi = \sum_{i=1}^n \theta_i[ I_i(\sum_{j=1}^{C}\sum_{k=1}^m {nv_{target,k}}) + \sum_{k=1}^m {nv_{target,k}}]$,
# 
# where $n$ is the number of quadrats in the neighborhood, $C$ is the number of quadrats in a connected component of the neighborhood, $m$ is the number of neighbors in a quadrat, $\theta$ is a vector of parameters of length $n$ corresponding to the direction from target to neighbor, and $I\in{\{0,1\}}$ is an indicator variable that evaluates to $1$ if a connected component of the neighborhood is filled.
# 
# - Intuitively, the NSI is the sum of connected components and individual components (i.e. summed neighbor values for each quadrat)

# In[ ]:

#Parameters
nsize = 9
alpha, beta = 1,1
C = 2

# derived parameters
side_len = int(np.sqrt(nsize))

# Definitions
def nv(nbrs, distance, alpha, beta):
    """Calculate neighbor values, nbrs is array of neighbors in a quadrat"""
    return nbrs**alpha / distance**beta

def component_indices(side_length):
    """Returns list of indices of outermost layer of neighbhorhood of 
    size nsize ordered clockwise from (0,0)"""
    inds = [(0,j) for j in range(side_length)]
    inds += [(i,side_length-1) for i in range(1,side_length)]
    inds += [(side_length-1,j) for j in reversed(range(0,side_length-1))]
    inds += [(i,0) for i in reversed(range(1,side_length-1))]
    return inds

def create_nbr_dict(nbrs, inds):
    """Make neighbor dictionary keyed by quadrat, values are neighbor sizes in 
    quadrat"""
    return {inds[i] : np.array([val for ind, val in nbrs                                if ind == inds[i]]) for i in range(len(inds))}
                
def connected_comps(side_length, C):
    """Returns array of connected component indices given neighborhood size.
    Note: currently only works for connected components in outermost layer"""
    num_outer = side_length + 2*(side_length-1) + (side_length-2)
    inds = component_indices(side_length)
    comps = [[inds[(i + j) % len(inds)] for j in range(C)] for i in range(num_outer)]
    return inds, comps

def nsi(nbrs, side_length, C, alpha, beta, theta):
    """Returns neighbor surround index given array of nbrs, neighborhood
    side length, and connected component length.
    Each component of nbrs is of the form ((x, y), size)"""
    inds, comps = connected_comps(side_length, C)
    nbr_dict = create_nbr_dict(nbrs, inds)
    compsval = 0.0
    for comp in comps:
        if all([nbr_dict[comp[i]].size > 0 for i in range(len(comp))]):
            cval = 0.0
            for q in comp:
                dist = np.sqrt((q[0]-1)**2 + (q[1]-1)**2)
                #print dist
                cval += sum(nv(nbr_dict[q], dist, alpha, beta))
            #print str(comp) + ': ' + str(cval)
            compsval += cval
    indval = 0.0
    for q in nbr_dict:
        dist = np.sqrt((q[0]-1)**2 + (q[1]-1)**2)
        indval += sum(nv(nbr_dict[q], dist, alpha, beta))
    return compsval + indval

# Testing
print 'For neighborhood of size ' + str(nsize) +     ' and connected component size ' + str(C) + ':'
print '\tIndices: ' + str(component_indices(side_len))
inds, ccs = connected_comps(side_len, C)
print '\tNumber of connected components: ' + str(len(ccs))
print '\tConnected components: ' + str(ccs)

# Make neighor data
nlocs_no_comp = [(0,0), (2,0), (2,2)]
nbr_no_comps = [(nlocs_no_comp[i], 0.25) for i in range(len(nlocs_no_comp))]
nbr_comps = [((i,j),0.25) for i in [0,1,2] for j in [0,1]]
print 'Disconnected neighbor data: ' + str(nbr_no_comps)
print 'Connected neighbor data: ' + str(nbr_comps)
print 'Neighor dictionary: ' + str(create_nbr_dict(nbr_comps, inds))

theta = 1
nsi(nbr_comps, side_len, C, alpha, beta, theta)


# In[ ]:

# Show test output
inds = component_indices(side_len)
config = np.zeros((side_len, side_len))
for ind in ccs[0]:
    config[ind[0],ind[1]] = 1
fig = plt.figure()
ax1 = fig.add_subplot(1,2,1)
ax1.imshow(config, cmap=plt.cm.bone_r, interpolation='nearest')
ax1.set_title('One connected component')
ax2 = fig.add_subplot(1,2,2)
ax2.axis([-0.5, 2.5, -0.5, 2.5])
ax2.plot([x for x,y in inds],[y for x,y in inds], 'ro')
ax2.set_title('Locations of neighbors around a target at (0,0)')
plt.show()


# ## Neighbor distances

# In[ ]:

# make distance matrix
distances = np.ones((3,3))
np.fill_diagonal(distances, np.sqrt(2))
np.fill_diagonal(np.fliplr(distances), np.sqrt(2))
distances[1,1] = 0

# show
plt.imshow(distances, cmap=plt.cm.bone_r, interpolation='nearest')
plt.title('Neighborhood distances')
plt.colorbar()
plt.show()


# ## Neighbor locations
# - These cases should all be different because distances to corners are greater than distances to sides

# In[ ]:

# test neighbor configurations
# side case
gap_side = np.ones((3,3))
gap_side[0,:] = 0
# corner case
gap_corner = np.ones((3,3))
gap_corner[0,[1, 2]]=0
gap_corner[1, 2]=0
# no continuous gaps: corner cases
gap_corner_broken = np.ones((3,3))
gap_corner_broken[[0,0,2,2],[0,2,0,2]] = 0
# no continuous gaps: side cases
gap_side_broken = np.ones((3,3))
gap_side_broken[[1,2,1,0],[0,1,2,1]] = 0

# show graphics
fig = plt.figure()
ax1 = fig.add_subplot(3,3,1)
ax1.imshow(gap_side, cmap=plt.cm.bone_r, interpolation='nearest')
ax1.set_title('3 quadrat\n side gap')

ax2 = fig.add_subplot(3,3,3)
ax2.imshow(gap_corner, cmap=plt.cm.bone_r, interpolation='nearest')
ax2.set_title('3 quadrat\n corner gap')

ax3 = fig.add_subplot(3,3,7)
ax3.imshow(gap_corner_broken, cmap=plt.cm.bone_r, interpolation='nearest')
ax3.set_title('No continuous gaps:\n corner cases')

ax4 = fig.add_subplot(3,3,9)
ax4.imshow(gap_side_broken, cmap=plt.cm.bone_r, interpolation='nearest')
ax4.set_title('No continuous gaps:\n side cases')

[plt.setp(x.get_yticklabels(), visible=False) for x in [ax1,ax2,ax3,ax4]]
[plt.setp(x.get_xticklabels(), visible=False) for x in [ax1,ax2,ax3,ax4]]
plt.show()


# ## Calculate neighborhood for above configurations
# - Only one neighbor per occupied quardrat
# - All neighbors the same size: $0.25$

# # Test cases
# 1. All neighbors same size
# 2. All neighbors random sizes

# In[ ]:



fig, ax = plt.subplots()

image = np.random.uniform(size=(3, 3))
ax.imshow(image, cmap=plt.cm.gray, interpolation='nearest')
ax.set_title('dropped spines')

# Move left and bottom spines outward by 10 points
ax.spines['left'].set_position(('outward', 10))
ax.spines['bottom'].set_position(('outward', 10))
# Hide the right and top spines
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)
# Only show ticks on the left and bottom spines
ax.yaxis.set_ticks_position('left')
ax.xaxis.set_ticks_position('bottom')

plt.show()

