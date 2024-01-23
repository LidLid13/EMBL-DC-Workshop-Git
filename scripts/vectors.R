weight_g <- c (50, 60, 65, 82)

animals <- c("mouse", "rat", "dog")

length (animals)
class(animals)
class(weight_g)
str(animals)

# how to add an element in the vector
animals <- c (animals,"cincilla")

num_char <- c (1,2,3,"a")
num_logical <- c(1,2,3, TRUE)
char_logical <- c (1,2,3,)
# subsetting a vector
animals [2]
animals <- c("cincilla", "mouse", "rat", "dog")
animals [2]
animals [c (1,2)]
animals [1,2]
more_animals <- animals [c(1, 2,3,2,1,4)]

weight_g
weight_g [c (FALSE, FALSE, TRUE, TRUE)]
weight_g > 63
weight_g [weight_g>63 & weight_g<80]
weight_g [weight_g<58 | weight_g>80]
weight_g==65
animals [animals=="rat" | animals == "frog"]
# %in% helps us find all elements corresponding to a vectors of elements of our choice
animals %in% c ("rat", "frog", "cat", "duck")
# example of vector with missing data 
heigths <- c (2, 3, 3, NA, 6)
mean (heigths)
mean (heigths, na.rm=T)
is.na (heigths)
heigths [!is.na(heigths)]
na.omit(heigths)
heigths [complete.cases (heigths)]

lungh <- c (63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63)
lungh_nona <- lungh [!is.na(lungh)]
median(lungh_nona)
tall <- lungh_nona [lungh_nona > 67]      
length(tall)
sum(tall)
