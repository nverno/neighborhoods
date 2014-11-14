### test_v_rad.R --- 
## Filename: test_v_rad.R
## Description: Tests for v_rad: finds vertical radians for target to neighbor
## Author: Noah Peart
## Created: Fri Nov 14 15:19:01 2014 (-0500)
## Last-Updated: Fri Nov 14 15:43:27 2014 (-0500)
##           By: Noah Peart
######################################################################
test_v_rad <- function() {
    checkEquals(v_rad(0, 0), 0)               # nbr same x, same z
    checkEquals(v_rad(0.123, 0), 0)           # nbr level on +x
    checkEquals(v_rad(1, 0), 0)               # nbr level with target on +(x-axis)
    checkEquals(v_rad(-1, 0), pi)              # nbr level with target on -(x-axis)
    checkEquals(v_rad(1, 1), pi/4)             # nbr above target, 45 degree, +x
    checkEquals(v_rad(-1, 1), 3*pi/4)          # nbr above target 45 degree, -x
    checkEquals(v_rad(-0.5, 0.5), 3*pi/4)      # same as above
    checkEquals(v_rad(-1, -1), pi + pi/4)       # nbr 45 degree below, -x
    checkEquals(v_rad(0.5, -0.5), pi + 3*pi/4)  # nbr 45 degree below, +x
}



