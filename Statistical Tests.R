library(tidyverse)


##Habitat type 

chisq.test(c(43, 6)) #p < 0.001


##Host species 

#Dermacentor andersoni
#cat: 2, dog: 21, human: 47, na: 11, total: 81

#Dermacentor variabilis 
#cat: 1, dog: 92, human: 7, na: 0, total: 100

chisq.test(c(2,1)) #cats, p = 0.5637

chisq.test(c(21, 92)) #dogs, p < 0.001

chisq.test(c(47, 7)) #humans, p < 0.001

andersoni <- c(2, 21, 47, 11)
variabilis <- c(1, 92, 7, 0)
all <- tibble(andersoni, variabilis)
chisq.test(all) #all, p < 0.001
