# Code for article 'How to Friends as Influenced People'
# Published on www.significancemagazine.com June 2016
# Nathan Cunningham
# June 2016

# Calculate the euclidean distance between two points x1 and x2
euc_dist <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))  
}

# Calculate new position of an object moving 'v' steps
# in direction 'rot'
# from starting position 'pos'
move_step <- function(pos, rot, v) {
  pos[1] <- pos[1] + sin(rot) * v
  pos[2] <- pos[2] + cos(rot) * v
  pos
}

# Simulate a trajectory for two 'people' moving a maximum 'steps'
# within a square venue of length 'boundary'
# Each moves at pace 'v'
# Change direction with probability rot_prob
# They are considered to find each other when they are within 'thresh'
# steps of each other.
sim_motion <- function(v = c(1, 1), steps, boundary, thresh, rot_prob = c(0.05, 0.05)) {
  # Initialise position and orientation
  pos <- matrix(NA, nrow = steps + 1, ncol = 4)
  pos[1, ] <- runif(4, 0, boundary)
  rot <- rnorm(2, 0, pi / 2)
  # Simulation steps when count reaches the max 'steps' or
  # Both criteria are met:
  # Person 1 finds person 2 and
  # Person 1 finds person 2's starting point
  count <- 1
  criteria <- c(0, 0)
  found_time <- data.frame("Stay put" = NA, "Move" = NA)
  
  while(any(criteria != 1) & count <= steps) {
    move <- 0
    # Change rotation with probability rot_prob
    if(runif(1) <= rot_prob[1]) rot[1] <- rnorm(1, rot[1], pi / 2)
    if(runif(1) <= rot_prob[2]) rot[2] <- rnorm(1, rot[2], pi / 2)
    # If taking a step in that direction means leaving the boundary
    # then you have to change direction
    while(any(0 > move_step(pos[count, 1:2], rot[1], v[1])) ||
       any(move_step(pos[count, 1:2], rot[1], v[1]) > boundary)) {
      rot[1] <- rnorm(1, rot[1], pi / 2)
    }
    
    while(any(0 > move_step(pos[count, 3:4], rot[2], v[2])) ||
       any(move_step(pos[count, 3:4], rot[2], v[2]) > boundary)) {
      rot[2] <- rnorm(1, rot[2], pi / 2)
    }
    
    # Move in your direction
    pos[count + 1, 3:4] <- move_step(pos[count, 3:4], rot[2], v[2])
    pos[count + 1, 1:2] <- move_step(pos[count, 1:2], rot[1], v[1])
    
    # Increment count
    count <- count + 1
    
    # Check if you have found each other
    if(euc_dist(pos[count,1:2], pos[count,3:4]) <= thresh & criteria[1] == 0) { 
      criteria[1] <- 1
      found_time$Move <- count
    }
    
    # Check if your friend found your starting point (stay put strategy)
    if(euc_dist(pos[1,1:2], pos[count,3:4]) <= thresh & criteria[2] == 0) { 
      criteria[2] <- 1
      found_time$Stay.put <- count
    }
    # Return the time it took to find each other
  }
    return(found_time)
}

  