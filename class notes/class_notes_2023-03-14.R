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

#inner_join - only brings in data/ ID in which both data frames have this in common
data12 = data1 %>%
  inner_join(data2, by="ID")

# full_join - retains all data regardless of common data
data12 = data1 %>%
  full_join(data2, by="ID")

##### always check the dimensions of new data

# semi_join - not adding any new information
data12_semi = data1 %>%
  semi_join(data2, by="ID")
data12_semi

# anti_join - when you want to know what is missing
data12_anti = data1 %>%
  anti_join(data2, by="ID")
data12_anti

##### wider vs. longer
survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle_n = c(2, 11, 8, 27),
                    chiton_n = c(1, 0, 0, 2),
                    mussel_n = c(0, 1, 1, 4))
long = survey %>% 
  pivot_longer(cols=c("barnacle_n", "chiton_n", "mussel_n"), 
               names_to="taxon", 
               values_to="counts")

wide = long %>%
  pivot_wider(names_from=taxon, 
              values_from=counts)

# exercise 1.2
ggplot(data=survey) +
  geom_point(x=quadrat_id, y=barnacle_n) +
  geom_point(x=quadrat_id, y=chiton_n) +
  geom_point(x=quadrat_id, y=mussel_n)


ggplot(data=long) +
  geom_point(aes(x=quadrat_id, y=counts, color=taxon))

