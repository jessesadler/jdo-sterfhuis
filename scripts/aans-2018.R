library(tidyverse)
library(debkeepr)

# Multiplication: £15 3s. 8d. by 32


# Division: £465 12s. 8d. by 72


# Pounds, shillings, and pence in 15783 pence


# Running account of Jacques and Daniel
# 15 February 1585 to 23 May 1585

l <- c(26, 8, 5, 28, 4, 369, 85, 15, 2, 0, 155, 50, 120, 9, 2, 213, 71, 17, 93, 3)
s <- c(4, 0, 1, 10, 7, 4, 4, 18, 18, 3, 1, 8, 11, 10, 9, 6, 5, 5, 3, 16)
d <- c(0, 0, 10, 0, 8, 4, 11, 10, 6, 4, 4, 0, 0, 8, 3, 8, 0, 10, 1, 11)

dvdm_57_85 <- tibble::tibble(l = l,
                     s = s,
                     d = d)