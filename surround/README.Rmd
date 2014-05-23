## Neighborhood Surround Index (NSI)

## Create neighborhood surround index, aka gapiness
### Definitions
- A neighborhood is composed of the trees surrounding an individual within a certain radius.
- A connected component of the neighborhood is continuous area in the neighborhood this is both occupied by neighbors and at least as large as a specified area.

### Parameters
- Neighborhood size: 9 quadrats (8 quadrats surround targets quadrat)
- Connected component size: 2 quadrats (8 possible connected components for 9 quadrat neighborhood)


## Individual neighbor values (nv): ratio of neighbor BA and distance to target
### ${nv_{target,neighbor}} = \frac{BA_{neighbor}^\alpha}{Distance_{target, neighbor}^\beta}$, 
where $BA$ is the basal area and $Distance$ is the distance between target and neighbor

- Neighbors are additive within connected components
- Use $\alpha = 1$ and $\beta = 1$ for now

## Calculate Neighbor Surround Index (NSI)
$nsi = \sum_{i=1}^n \theta_i[ I_i(\sum_{j=1}^{C}\sum_{k=1}^m {nv_{target,k}}) + \sum_{k=1}^m {nv_{target,k}}]$,

where $n$ is the number of quadrats in the neighborhood, $C$ is the number of quadrats in a connected component of the neighborhood, $m$ is the number of neighbors in a quadrat, $\theta$ is a vector of parameters of length $n$ corresponding to the direction from target to neighbor, and $I\in{\{0,1\}}$ is an indicator variable that evaluates to $1$ if a connected component of the neighborhood is filled.

- Intuitively, the NSI is the sum of connected components and individual components (i.e. summed neighbor values for each quadrat)
