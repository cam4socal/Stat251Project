##############################
### STAT 251 Final Project ###
####### Dr. Garitt Page ######
#### Fall 2018, Section 1 ####
## A. Hamilton & C. Timpson ##
##############################

library(dplyr)

bakery <- read.csv("C:/Users/cam4s/Documents/STAT251/STAT 251/bakery.csv", header = TRUE)
head(bakery)
tail(bakery)
# Dataset contains transactions from 10/30/2016 - 4/9/2017

# Adjusting the date formatting
class(bakery$Date)
bakery$Date <- format(as.Date(bakery$Date, format = "%Y-%m-%d"), "%m/%d/%Y")
class(bakery$Date)
head(bakery)

# Removing unnecessary items
dontNeed <- c("NONE", "Adjustment", "The BART", "Fairy Doors", "Bowl Nic Pitt", "Siblings", 
              "Christmas common", "Mortimer")
bakery <- subset(bakery, !Item %in% dontNeed)

# List of all unique item types
list_items <- unique(bakery$Item)
list_items

# This vector will not be utilized; it's meerly a reference for all items not removed from dataframe nor are desserts
notDesserts <- c("Bread", "Coffee", "Tea", "Mineral water", "Juice", "Frittata", "Hearty & Seasonal", "Soup", "Mighty protein", 
                 "Chicken sand", "My-5 Fruit Shoot", "Focaccia", "Sandwich", "Eggs", "Granola", "Chimichurri Oil", "Bacon",
                 "Kids biscuit", "Olum & polenta", "Polenta", "The Nomad", "Hack the stack", "Toast", "Bare Popcorn", "Muelsi",
                 "Crisps", "Pintxos", "Brioche and salami", "Afternoon with the baker", "Salad", "Chicken Stew", "Spanish Brunch",
                 "Extra Salami or Feta", "Duck egg", "Baguette", "Tshirt", "Vegan Feast", "Nomad bag", "Coffee granules", 
                 "Drinking chocolate spoons", "Argentina Night", "Half slice Monster", "Gift voucher", "Raw Bars", "Tacos/Fajita",
                 "Farm House", "Scandinavian", "Tartine", "Ella's kitchen pouches", "Hot chocolate", "Basket", "Smoothies", "Coke",
                 "Empanadas", "Art Tray", "Valentine's card", "Postcard")

# Vector of items classified as desserts or holiday treats
desserts <- c("Cake", "Cookies", "Fudge", "Lemon and coconut", "Muffin", "Pastry", "Victorian Sponge", "Alfajores",
              "Brownies", "Dulce de Leche", "Keeping It Local", "Bread Pudding", "Truffles", "Caramel bites",
              "Jammie Dodgers", "Tiffin", "Bakewell", "Scone", "Vegan mincepie", "Raspberry shortbread sandwich", 
              "Panatone", "Chocolates", "Medialuna", "Jam", "Pick and Mix Bowls", "Honey", "Spread", "Crepes", 
              "Gingerbread syrup", "Cherry me Dried fruit")


# Creating array of dessert counts during week before Thanksgiving
bakery_thx_week <- subset(bakery, Date >= "11/18/2016" & Date <= "11/24/2016")
bakery_thx_day1 <- subset(bakery, Date == "11/18/2016")
bakery_thx_day2 <- subset(bakery, Date == "11/19/2016")
bakery_thx_day3 <- subset(bakery, Date == "11/20/2016")
bakery_thx_day4 <- subset(bakery, Date == "11/21/2016")
bakery_thx_day5 <- subset(bakery, Date == "11/22/2016")
bakery_thx_day6 <- subset(bakery, Date == "11/23/2016")
bakery_thx_day7 <- subset(bakery, Date == "11/24/2016")

dessert_count_thx_day1 <- sum(bakery_thx_day1$Item %in% desserts)
dessert_count_thx_day2 <- sum(bakery_thx_day2$Item %in% desserts)
dessert_count_thx_day3 <- sum(bakery_thx_day3$Item %in% desserts)
dessert_count_thx_day4 <- sum(bakery_thx_day4$Item %in% desserts)
dessert_count_thx_day5 <- sum(bakery_thx_day5$Item %in% desserts)
dessert_count_thx_day6 <- sum(bakery_thx_day6$Item %in% desserts)
dessert_count_thx_day7 <- sum(bakery_thx_day7$Item %in% desserts)

thx_week_dessert_count = c(dessert_count_thx_day1, dessert_count_thx_day2, 
                           dessert_count_thx_day3, dessert_count_thx_day4, 
                           dessert_count_thx_day5, dessert_count_thx_day6, 
                           dessert_count_thx_day7)


# Creating array of dessert counts during week before Christmas
bakery_xmas_week <- subset(bakery, Date >= "12/18/2016" & Date <= "12/24/2016")
bakery_xmas_day1 <- subset(bakery, Date == "12/18/2016")
bakery_xmas_day2 <- subset(bakery, Date == "12/19/2016")
bakery_xmas_day3 <- subset(bakery, Date == "12/20/2016")
bakery_xmas_day4 <- subset(bakery, Date == "12/21/2016")
bakery_xmas_day5 <- subset(bakery, Date == "12/22/2016")
bakery_xmas_day6 <- subset(bakery, Date == "12/23/2016")
bakery_xmas_day7 <- subset(bakery, Date == "12/24/2016")


dessert_count_xmas_day1 <- sum(bakery_xmas_day1$Item %in% desserts)
dessert_count_xmas_day2 <- sum(bakery_xmas_day2$Item %in% desserts)
dessert_count_xmas_day3 <- sum(bakery_xmas_day3$Item %in% desserts)
dessert_count_xmas_day4 <- sum(bakery_xmas_day4$Item %in% desserts)
dessert_count_xmas_day5 <- sum(bakery_xmas_day5$Item %in% desserts)
dessert_count_xmas_day6 <- sum(bakery_xmas_day6$Item %in% desserts)
dessert_count_xmas_day7 <- sum(bakery_xmas_day7$Item %in% desserts)

xmas_week_dessert_count = c(dessert_count_xmas_day1, dessert_count_xmas_day2, 
                            dessert_count_xmas_day3, dessert_count_xmas_day4, 
                            dessert_count_xmas_day5, dessert_count_xmas_day6, 
                            dessert_count_xmas_day7)


# Function created for checking item counts
count_check <- function(item_search, day){
  
  all_days <- sum(bakery$Item == item_search) # All days
  first_thx_day <- sum(day$Item == item_search) # One day
  
  return(paste("Store Total: ", all_days, "    On selected day: ", first_thx_day))
}



# Starting the poisson-gamma distributions

a_prior <- 5
b_prior <- 10
prior_mean <- a_prior/b_prior
prior_mean


# Prior distribution information

# NEED TO COME UP WITH

plot(seq(0,100,length.out = 5000),dgamma(seq(0,100,length.out = 5000),5.1,1.25),type = "l", xlim = c(0,15), ylim = c(0,.6), xlab = "theta", ylab = "Probability Density", main = "Posterior Distribution", col = "gray")
lines(seq(0,100,length.out = 5000),dgamma(seq(0,100,length.out = 5000),5.1 + 5,1.25 + 1), col = "black")
legend("topright",legend = c("Prior", "Posterior"), col = c("gray", "black"), lty = c(1,1), cex = .8)

# Looking at the dessert counts during holiday weeks
thx_week_dessert_count
xmas_week_dessert_count

# Lengths of holiday dessert count arrays
accident_sum <- sum(accident)

nThx <- length(thx_week_dessert_count)
nXmas <- length(xmas_week_dessert_count)



a <- 5
b <- .2
nreps <- 10000
before_pot <- c(27,30,25,29,27,23,29,24,27,36,33,34,30,25,29,27,33,33,27,34,28,43,31,24,36,28,26,29,30,20,33)
after_pot <- c(26,32,21,32,21,24,36,26,24,25,30,25,20,14,26,28,21,21)
sum_before <- sum(before_pot)
len_before <- length(before_pot)
sum_after <- sum(after_pot)
len_after <- length(after_pot)
before_theta <- rgamma(nreps,a + sum_before, b + len_before)
after_theta <- rgamma(nreps, a + sum_after, b + len_after)
diffs <- before_theta - after_theta
mean(diffs > 0)


mean(diffs)
median(diffs)

quantile(diffs,c(.025,.975))











