library(magrittr)
library(dplyr)

tax <- read.csv("tax100ENR.csv")

tax %>% mutate_each(funs(supsmu(tax$X, ., bass=10)$y), starts_with("T")) %>% 
  write.csv("tax100ENRSmooth.csv")