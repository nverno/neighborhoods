# Create test cases for surroundedness model
import numpy as np
from itertools import combinations, permutations
import matplotlib.pyplot as plt

# Neighborhood dimensions
m = 3
n = 3

# Demo graphic
fig, ax = plt.subplots()

image = np.random.uniform(size=(3, 3))
ax.imshow(image, cmap=plt.cm.gray, interpolation='nearest')
ax.set_title('dropped spines')

# Move left and bottom spines outward by 10 points
# ax.spines['left'].set_position(('outward', 10))
# ax.spines['bottom'].set_position(('outward', 10))
# Hide the right and top spines
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)
# Only show ticks on the left and bottom spines
ax.yaxis.set_ticks_position('left')
ax.xaxis.set_ticks_position('bottom')

plt.show()

####################################################
#
# Test cases
#
#
# Neighbor matrices
# evenly distributed neighbors, all the same size

# distances matrix
distances = np.ones((3,3))
np.fill_diagonal(distances, np.sqrt(2))
np.fill_diagonal(np.fliplr(distances), np.sqrt(2))
distances[1,1] = 0

# No gaps
n1 = np.ones((3,3)) * 0.25

# gap on north
n2 = np.asmatrix(np.ones((3,3)) * 0.25)
n2[0,:] -= 0.25;

# gap NE
n3 = np.ones((3,3)) * 0.25
n3[0, 1] -= 0.25
n3[1,2] -= 0.25


tst1 = np.matrix('1 1 0; 1 1 0; 0 0 0')
tst2 = np.matrix('')
# number of filled neighbor quadrats

num_filled = 6
seven = [0]*(n*m - num_filled) + [1]*num_filled

