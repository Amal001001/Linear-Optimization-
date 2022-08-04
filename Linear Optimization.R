# ====================================================================================================
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>LINEAR OPTIMIZATIOIN Assignment<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ====================================================================================================
install.packages("lpSolve")
library(lpSolve)


######################################################################################################
#First Task
#----------

# You are the founder of The Sodaz Company and you sell soda of two base products (Strawberry Soda, and Orange Soda.)
# You can sell them for:
#                       a $12/pack
#                     and $24/pack profit respectively.
# Both sodas require water, CO2, and Flavoring as their main ingredients

# and you currently have: 
#                         150 ounces of water,
#                         480 grams of CO2,
#                     and 1190 pounds of flavoring.
# 
# A batch of Strawberry Soda consumes: 5 ounces of water,
#                                      4 grams of CO2,
#                                  and 35 pounds of flavoring.
# 
# A batch of the Orange Soda consumes: 15 ounces of water,
#                                      4 grams of CO2,
#                                  and 20 pounds of flavoring.

# How many batches of Strawberry Soda and Orange Soda should you make to get the maximum profit possible?

#-------------------------------------------solution-----------------------------------------------------
# X1 = Strawberry Soda
# X2 = Orange Soda

# Max (12 X1 + 24 X2)

# Constraints matrix ---> A coefficient matrix, RHS matrix
# 5  X1 + 15 X2 <= 150    ounces of water
# 4  X1 + 4  X2 <= 480    grams of CO2
# 35 X1 + 20 X2 <= 1190   pounds of flavoring 


# A = [5,4,35; 15,4,20]
# B = [150;480;1190]


objective_fn = c (12, 24)
const_mat = matrix( c(5,4,35, 15,4,20), nrow = 3, byrow = FALSE)
const_dir = c("<=", "<=", "<=", "<=")
const_rhs = c(150, 480, 1190)


prob_1 = lp(direction='max', objective_fn, const_mat, const_dir, const_rhs)

ans = prob_1$solution
ans
val = prob_1$objval
val

df <- data.frame(prob_1)
#--------------------------answer---------------------------------------------------------------------

# I should make: 30 batches of Strawberry Soda and
#                zero batches of Orange Soda
# to get the maximum profit possible which is equal to $360.


######################################################################################################
#Second Task
#----------

# The Sodaz Company is coming up with a hot new beverage called Grapye`.
# Because it is a special craft soda, it has a margin of: $30/pack
# It uses: 10 ounces of water,
#          4  grams of CO2,
#      and 15 pounds of flavoring. 

# The addition of this new soda requires that the company has to allocate its personnel to different manufacturing
# processes.

# Each of the batches takes: 5 (Strawberry Soda),
#                            10 (Orange Soda),
#                        and 20 (Grape Soda) hours of labor to make

# and we have only: 5 employees full-time. 

# If this is the production planning for a ((month)) of soda production,
# what is the optimal amount of each soda that must be produced to maximize profit?

#-------------------------------------------solution-----------------------------------------------------
# X1 = Strawberry Soda
# X2 = Orange Soda
# X3 = Grapye Soda

# Max (12 X1 + 24 X2 + 30 X3)

# Constraints matrix ---> A coefficient matrix, RHS matrix
# 5  X1 + 15 X2 + 10 X3 <= 150   ounces of water
# 4  X1 + 4  X2 + 4  X3 <= 480   grams of CO2
# 35 X1 + 20 X2 + 15 X3 <= 1190   pounds of flavoring 
# 5  X1 + 10 X2 + 20 X3 <= 240     hours 

# to calculate the total work hours for the month:
# 30 days * 8 work hours in each work day = 240 hours

# A = [5,15,10; 4,4,4; 35,20,15; 5,10,20]
# B = [150;480;1190;240]


objective_fn2 = c (12, 24, 30)
const_mat2 = matrix( c(5,4,35,5, 15,4,20,10, 10,4,15,20), nrow = 4, byrow = FALSE)
const_dir2 = c("<=", "<=", "<=", "<=")
const_rhs2 = c(150, 480, 1190, 240)


prob_2 = lp(direction='max', objective_fn2, const_mat2, const_dir2, const_rhs2)

ans = prob_2$solution
ans
val = prob_2$objval
val

#--------------------------answer---------------------------------------------------------------------

# I should make: 12    batches of Strawberry Soda,
#                zero  batches of Orange Soda and
#                9     batches of Grapye Soda
# to get the maximum profit possible which is equal to $414.

