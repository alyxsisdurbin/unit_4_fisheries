# Haley Durbin
# 2023-03-14

### Joins and Pivots ###


# Mutating joins add new variables to one table from matching observations in another table
# Filtering joins filter observations from one table based on whether or not they match an observation in the other table

library(tidyverse)

data1 = data.frame(ID = c(1,2),
                   X1 = c("a1", "a2"))
data2 = data.frame(ID = c(2,3),
                   X2 = c("b1", "b2"))

# left_join , begins w data set and adds the second data set to the first
data12 = left_join(data1, data2, by="ID")
data12 = data1 %>% 
  left_join(data2, by="ID")

#right_join 
# preserve everything from the right, or second data frame and bring in anything from the right data frame that relates to the second data frame
data12 = data1 %>% 
  right_join(data2, by="ID")


