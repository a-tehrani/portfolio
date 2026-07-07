#LOADING DATA
airfareData <- read.csv("Airfares.csv", header = TRUE, as.is = TRUE)

#LOOKING AT DATA
str(airfareData)
summary(airfareData)
nrow(airfareData)
ncol(airfareData)

#CHECKING FOR NULLS
#Count NAs in every column to see which columns have problems
colSums(is.na(airfareData))
#DISTANCE has 8 NAs, and other columns have 1-2 NAs


#IDENTIFYING ROWS WITH NULLS
#Which rows have at least one NA
rows_with_na <- which(rowSums(is.na(airfareData)) > 0)
rows_with_na

# Look at the city names and key columns for those rows
airfareData[rows_with_na, c("S_CITY", "E_CITY", "DISTANCE", "HI")]

#Looking at the output:
#6 rows are New York/Newark routes missing only DISTANCE
#Row 587 is a Phoenix route with NO destination city at all
#Row 612 has no S_CITY or E_CITY (almost completely empty)


#IDENTIFYING CORRUPTED ROWS (those that we can't fill in using research or logic)
#A row is corrupted if it's missing S_CITY or E_CITY
#Without knowing the route, the row is useless
bad_rows <- which(is.na(airfareData$S_CITY) | airfareData$S_CITY == "" | is.na(airfareData$E_CITY) | airfareData$E_CITY == "")
bad_rows

#Look at the corrupted rows
airfareData[bad_rows, ]

#Row 587: Phoenix AZ with no destination, also has #VALUE! in HI
#Row 612: Almost completely empty, no city info at all
#Both are unrecoverable — we need to remove them

#REMOVING CORRUPTED ROWS
nrow(airfareData)  # 640 before removal
airfareData <- airfareData[-bad_rows, ]
nrow(airfareData)  # 638 after removal

#RECHECKING FOR NAs
colSums(is.na(airfareData))
#Only DISTANCE still has 6 NAs

#HANDLING DISTANCE NAs
#Findign which rows are still missing DISTANCE
which(is.na(airfareData$DISTANCE))
airfareData[is.na(airfareData$DISTANCE), c("S_CITY", "E_CITY")]

#All 6 are New York/Newark routes. Since we know the exact city pairs, we looked up the real distances
airfareData$DISTANCE[airfareData$S_CITY == "New York/Newark NY" & airfareData$E_CITY == "Baltimore/Wash Intl MD"] <- 170
airfareData$DISTANCE[airfareData$S_CITY == "New York/Newark NY" & airfareData$E_CITY == "Milwaukee WI"] <- 740
airfareData$DISTANCE[airfareData$S_CITY == "New York/Newark NY" & airfareData$E_CITY == "Nashville TN"] <- 765
airfareData$DISTANCE[airfareData$S_CITY == "New York/Newark NY" & airfareData$E_CITY == "Syracuse NY"] <- 195
airfareData$DISTANCE[airfareData$S_CITY == "New York/Newark NY" & airfareData$E_CITY == "Tampa FL"] <- 1005
airfareData$DISTANCE[airfareData$S_CITY == "New York/Newark NY" & airfareData$E_CITY == "West Palm Beach FL"] <- 1030

#Verify all DISTANCE NAs are fixed
sum(is.na(airfareData$DISTANCE))

#VERIFYING ALL NAs HANDLED
colSums(is.na(airfareData))


#CLEANING STRING WHITESPACE
#Trim whitespace so strings like "Chicago IL" and
#"Chicago IL " don't get treated as different values
#We need to do this BEFORE checking for duplicates

airfareData$S_CITY <- trimws(airfareData$S_CITY)
airfareData$E_CITY <- trimws(airfareData$E_CITY)
airfareData$VACATION <- trimws(airfareData$VACATION)
airfareData$GATE <- trimws(airfareData$GATE)

#FIXING DATA TYPES
#We need to fix data types BEFORE checking for duplicates cause if we
#check duplicates first and then fix types, we can create new duplicates

str(airfareData)

#HI is stored as character instead of numeric
#This happened because the corrupted Phoenix row had "#VALUE!"
#in HI, which forced R to read the whole column as text
#We already removed that row, so now we can safely convert
airfareData$HI <- as.numeric(airfareData$HI)
sum(is.na(airfareData$HI))

#Converting VACATION from "Yes"/"No" to 1/0 for modeling
airfareData$VACATION <- ifelse(airfareData$VACATION == "Yes", 1, 0)
table(airfareData$VACATION)

#GATE has inconsistent casing
table(airfareData$GATE)
#Shows "Free", "free", and "Constrained"
airfareData$GATE <- ifelse(toupper(airfareData$GATE) == "CONSTRAINED", 1, 0)
table(airfareData$GATE)

#CHECKING FOR DUPLICATES
sum(duplicated(airfareData))  # 4 duplicates found

#INSPECTING THE DUPLICATES
#Show both the original AND the duplicate side by side
dup_all <- duplicated(airfareData) | duplicated(airfareData, fromLast = TRUE)
airfareData[dup_all, c("S_CITY", "E_CITY", "COUPON", "SW", "DISTANCE", "FARE")]

#4 pairs of identical rows:
#Los Angeles CA -> Las Vegas NV
#Los Angeles CA -> San Diego CA
#Miami FL -> Orlando FL
#New York/Newark NY -> Pittsburgh PA

#REMOVING DUPLICATES
nrow(airfareData)  # 638 before dedup
airfareData <- unique(airfareData)
nrow(airfareData)  # 634 after dedup

#Verifying
sum(duplicated(airfareData))  # 0

#FINAL VALIDATION
colSums(is.na(airfareData)) 	# All zeros
sum(duplicated(airfareData))	# 0
str(airfareData)
summary(airfareData)


#############################################################
#GRAPHS
#############################################################

plot(airfareData$DISTANCE, airfareData$PAX)
plot(airfareData$FARE, airfareData$PAX)
plot(airfareData$HI, airfareData$PAX) #Does lower HI (more competition) mean higher traffic
plot(airfareData$S_POP+airfareData$E_POP, airfareData$PAX) #Is there a correlation between the starting and ending city populations and the traffic?
boxplot(airfareData$FARE~airfareData$SW) #Does SW presence reduce fares?
boxplot(airfareData$PAX~airfareData$SW) #Does SW presence affect traffic?
boxplot(airfareData$PAX~airfareData$VACATION)
boxplot(airfareData$PAX~airfareData$E_CITY, cex.axis = .5, horizontal = TRUE, Xlab = "", ylab = "", las=2)
boxplot(airfareData$PAX~airfareData$S_CITY, cex.axis = .5, horizontal = TRUE, Xlab = "", ylab = "", las=2)
boxplot(airfareData$PAX, xlab="Boxplot of PAX")
c("min PAX" = min(airfareData$PAX), "avg PAX" = mean(airfareData$PAX), "max PAX" = max(airfareData$PAX))
avg_pax <- mean(airfareData$PAX)
avg_fare <- mean(airfareData$FARE)
avg_hi <- mean(airfareData$HI)
avg_distance <- mean(airfareData$DISTANCE)
c("avg_pax"=avg_pax, "avg_fare"=avg_fare, "avg_hi"=avg_hi, "avg_distance"=avg_distance)
hist(airfareData$FARE)
hist(airfareData$PAX)
hist(airfareData$HI)
hist(airfareData$DISTANCE)
airfareData$E_CITY[which(airfareData$PAX==max(airfareData$PAX))][1] #Ending city with the highest PAX
airfareData$E_CITY[which(airfareData$PAX==min(airfareData$PAX))] #Ending city with the lowest PAX
airfareData$S_CITY[which(airfareData$PAX==max(airfareData$PAX))][1] #Starting city with the highest PAX
airfareData$S_CITY[which(airfareData$PAX==min(airfareData$PAX))] #Starting city with the lowest PAX


#############################################################
#CREATING MODELS
#############################################################

#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(randomForest)

# --- DATA PREP ---
airfareDataM <- airfareData[, !(names(airfareData) %in% c("S_CODE","S_CITY","E_CODE","E_CITY"))]

# --- TRAIN/TEST SPLIT ---
train_size <- 0.6 * nrow(airfareDataM)
set.seed(1)
train_index <- sample(x = 1:nrow(airfareDataM), size = train_size)
train_set <- airfareDataM[train_index, ]
valid_set <- airfareDataM[-train_index, ]

# ============================================================
# Algorithm 1: LINEAR REGRESSION
# ============================================================
#MODEL 1
lm_all <- lm(formula = PAX ~ ., data = train_set)
lm_preds <- predict(object = lm_all, valid_set)
lm_errors <- valid_set$PAX - lm_preds
lm_rmse  <- sqrt(mean((lm_errors)^2))
lm_me   <- mean(lm_errors)
lm_r2	<- cor(lm_preds, valid_set$PAX)^2

#MODEL 2
lm_fare <- lm(PAX ~ FARE + HI + DISTANCE, data = train_set)
lm_fare_preds <- predict(object = lm_fare, newdata = valid_set)
lm_fare_errors <- valid_set$PAX - lm_fare_preds
lm_fare_rmse <- sqrt(mean(lm_fare_errors^2))
lm_fare_me <- mean(lm_fare_errors)
lm_fare_r2   <- cor(lm_fare_preds, valid_set$PAX)^2

#MODEL 3
lm_socio <- lm(PAX ~ S_INCOME + E_INCOME + S_POP + E_POP, data = train_set)
lm_socio_preds <- predict(object = lm_socio, newdata = valid_set)
lm_socio_errors <- valid_set$PAX - lm_socio_preds
lm_socio_rmse <- sqrt(mean(lm_socio_errors^2))
lm_socio_me <- mean(lm_socio_errors)
lm_socio_r2   <- cor(lm_socio_preds, valid_set$PAX)^2

#MODEL 4
lm_interact <- lm(PAX ~ FARE * DISTANCE + S_POP * E_POP, data = train_set)
lm_interact_preds <- predict(object = lm_interact, newdata = valid_set)
lm_interact_errors <- valid_set$PAX - lm_interact_preds
lm_interact_rmse <- sqrt(mean(lm_interact_errors^2))
lm_interact_me <- mean(lm_interact_errors)
lm_interact_r2   <- cor(lm_interact_preds, valid_set$PAX)^2

LM_results <- data.frame(
  Linear_Regression_Model = c("all", "fare", "socio", "interact"),
  RMSE  = c(lm_rmse, lm_fare_rmse, lm_socio_rmse, lm_interact_rmse),
  ME   = round(c(lm_me, lm_fare_me, lm_socio_me, lm_interact_me),  2),
  R2	= round(c(lm_r2, lm_fare_r2, lm_socio_r2, lm_interact_r2),   4)
)

# ============================================================
# Algorithm 2: REGRESSION TREE
# ============================================================
tree_model <- rpart(formula = PAX ~ ., cp=0.001, data = train_set, method = "anova")
options(scipen = 999)
#train set
pred_tree_train <- predict(object = tree_model, newdata = train_set)
errors_tree_train <- train_set$PAX - pred_tree_train
tree_train_rmse <- sqrt(mean((errors_tree_train)^2))
tree_train_me <- mean(errors_tree_train)
tree_train_r2 <- cor(pred_tree_train, train_set$PAX)^2
c(tree_train_rmse, tree_train_me, tree_train_r2)

#valid set
tree_preds <- predict(object = tree_model, newdata = valid_set)

prp(tree_model, type = 1, extra = 1)

errors_tree_valid <- valid_set$PAX - tree_preds
tree_rmse  <- sqrt(mean((errors_tree_valid)^2))
tree_me   <- mean(errors_tree_valid)
tree_r2	<- cor(tree_preds, valid_set$PAX)^2
c(tree_rmse, tree_me, tree_r2)

#cptable
set.seed(50)
reg_tree_huge <- rpart(formula = PAX ~ ., cp = 0.001,
                       data = train_set, method = "anova")
prp(reg_tree_huge, type = 1, extra = 1)

cptable_huge <- printcp(reg_tree_huge)
tree_w_lowestcp <- which(min(cptable_huge[,'xerror']) == cptable_huge[,'xerror']) # step 1:Find the row with the lowest xerror
acceptablecp <- cptable_huge[tree_w_lowestcp,'xerror'] + cptable_huge[tree_w_lowestcp,'xstd'] # step 2: For the row found in Step 1, sum xerror and xstd values
acceptable_tree <- min(which(cptable_huge[,'xerror'] < acceptablecp)) # step 3: Find the rows with the xerror value below the value obtained in Step 2
cp_to_use <- cptable_huge[acceptable_tree,'CP'] # step 4: Pick the CP value for the row chosen in Step 3
cp_to_use

reg_tree_pruned <- prune(reg_tree_huge, cp =  cp_to_use)
prp(reg_tree_pruned, type = 1, extra = 1)

#pruned tree train set
pruned_tree_preds_train <- predict(object = reg_tree_pruned, newdata = train_set)

prp(reg_tree_pruned, type = 1, extra = 1)

pruned_errors_tree_train <- train_set$PAX - pruned_tree_preds_train
pruned_tree_rmse_train  <- sqrt(mean((pruned_errors_tree_train)^2))
pruned_tree_me_train   <- mean(pruned_errors_tree_train)
pruned_tree_r2_train	<- cor(pruned_tree_preds_train, train_set$PAX)^2
c(pruned_tree_rmse_train, pruned_tree_me_train, pruned_tree_r2_train)

#pruned tree valid set
pruned_tree_preds_valid <- predict(object = reg_tree_pruned, newdata = valid_set)

prp(reg_tree_pruned, type = 1, extra = 1)

pruned_errors_tree_valid <- valid_set$PAX - pruned_tree_preds_valid
pruned_tree_rmse  <- sqrt(mean((pruned_errors_tree_valid)^2))
pruned_tree_me   <- mean(pruned_errors_tree_valid)
pruned_tree_r2	<- cor(pruned_tree_preds_valid, valid_set$PAX)^2
c(pruned_tree_rmse, pruned_tree_me, pruned_tree_r2)

RT_results <- data.frame(
  Regression_Tree_Model = c("training set", "valid set", "pruned training set", "pruned valid set"),
  RMSE = c(tree_train_rmse, tree_rmse, pruned_tree_rmse_train, pruned_tree_rmse),
  ME = round(c(tree_train_me, tree_me, pruned_tree_me_train, pruned_tree_me),2),
  R2 = round(c(tree_train_r2, tree_r2, pruned_tree_r2_train, pruned_tree_r2),4)
)

# ============================================================
# Algorithm 3: RANDOM FOREST
# ============================================================
# Baseline model 1

set.seed(500)
rf_model_1 <- randomForest(formula = PAX ~ ., data = train_set, ntree = 500, importance = TRUE)
rf_preds_1 <- predict(object = rf_model_1, valid_set)
errors_rf_1 <- valid_set$PAX - rf_preds_1
rf_me_1 <- mean(errors_rf_1)
rf_rmse_1  <- sqrt(mean((rf_preds_1 - valid_set$PAX)^2))
rf_r2_1	<- cor(rf_preds_1, valid_set$PAX)^2

# More Trees 2

set.seed(500)
rf_model_2 <- randomForest(formula = PAX ~ ., data = train_set, ntree = 1000, importance = TRUE)
rf_preds_2 <- predict(object = rf_model_2, valid_set)
errors_rf_2 <- valid_set$PAX - rf_preds_2
rf_me_2 <- mean(errors_rf_2)
rf_rmse_2  <- sqrt(mean((rf_preds_2 - valid_set$PAX)^2))
rf_r2_2	<- cor(rf_preds_2, valid_set$PAX)^2


# Smaller Nodesize (deeper trees) 3

set.seed(500)
rf_model_3 <- randomForest(formula = PAX ~ ., data = train_set, ntree = 500, nodesize = 3, importance = TRUE)
rf_preds_3 <- predict(object = rf_model_3, valid_set)
errors_rf_3 <- valid_set$PAX - rf_preds_3
rf_me_3 <- mean(errors_rf_3)
rf_rmse_3  <- sqrt(mean((rf_preds_3 - valid_set$PAX)^2))
rf_r2_3	<- cor(rf_preds_3, valid_set$PAX)^2


# Larger Nodesize (shallower trees) 4

set.seed(500)
rf_model_4 <- randomForest(formula = PAX ~ ., data = train_set, ntree = 500, nodesize = 15, importance = TRUE)
rf_preds_4 <- predict(object = rf_model_4, valid_set)
errors_rf_4 <- valid_set$PAX - rf_preds_4
rf_me_4 <- mean(errors_rf_4)
rf_rmse_4 <- sqrt(mean((rf_preds_4 - valid_set$PAX)^2))
rf_r2_4	<- cor(rf_preds_4, valid_set$PAX)^2

# Higher mtry (more features considered per split) 5

set.seed(500)
rf_model_5 <- randomForest(formula = PAX ~ ., data = train_set, ntree = 500, mtry = 8, importance = TRUE)
rf_preds_5 <- predict(object = rf_model_5, valid_set)
errors_rf_5 <- valid_set$PAX - rf_preds_5
rf_me_5 <- mean(errors_rf_5)
rf_rmse_5  <- sqrt(mean((rf_preds_5 - valid_set$PAX)^2))
rf_r2_5	<- cor(rf_preds_5, valid_set$PAX)^2

# Comparing Results

rf_results <- data.frame(
  Random_Forest_Model = c("RF1: Baseline",
                          "RF2: 1000 trees",
                          "RF3: nodesize = 3",
                          "RF4: nodesize = 15",
                          "RF5: mtry = 8"),
  RMSE = round(c(rf_rmse_1, rf_rmse_2, rf_rmse_3, rf_rmse_4, rf_rmse_5), 2),
  ME   = round(c(rf_me_1, rf_me_2, rf_me_3, rf_me_4, rf_me_5), 2),
  R2   = round(c(rf_r2_1, rf_r2_2, rf_r2_3, rf_r2_4, rf_r2_5), 4)
)

rf_results <- rf_results[order(rf_results$RMSE), ]


# ============================================================
# RESULTS TABLE
# ============================================================
print(LM_results)
print(RT_results)
print(rf_results)

# ============================================================
# Optimizing revenue 
# ============================================================

#############################################################
# INSTALL / LOAD PACKAGE
#############################################################

# install.packages("lpSolve")  # Run once if not installed
library(lpSolve)

#############################################################
# CITY DATA
#############################################################

city_data <- data.frame(
  City = c(
    "Albuquerque_NM",
    "Anchorage_AK",
    "Atlanta_GA",
    "Austin_TX",
    "Baltimore_Wash_Intl_MD",
    "Boise_ID",
    "Boston_MA",
    "Burbank_CA",
    "Chicago_IL",
    "Cincinnati_OH",
    "Cleveland_OH",
    "Columbus_OH",
    "Corpus_Christi_TX",
    "Dallas_Fort_Worth_TX",
    "Denver_CO",
    "Detroit_MI",
    "El_Paso_TX",
    "Fort_Lauderdale_FL",
    "Fort_Meyers_FL",
    "Greenville_Sprtnbg_SC",
    "Hartford_CT",
    "Honolulu_Intl_HI",
    "Houston_TX",
    "Jacksonville_FL",
    "Kansas_City_MO",
    "Las_Vegas_NV",
    "Los_Angeles_CA",
    "Memphis_TN",
    "Miami_FL",
    "Minneapolis_St_Paul_MN",
    "Nashville_TN",
    "New_Orleans_LA",
    "New_York_Newark_NY",
    "Norfolk_Va_B_Pt_Ch_VA",
    "Oakland_CA",
    "Omaha_NE",
    "Orlando_FL",
    "Philadelphia_Camden_PA",
    "Phoenix_AZ",
    "Pittsburgh_PA",
    "Portland_OR",
    "Sacramento_CA",
    "Salt_Lake_City_UT",
    "San_Diego_CA",
    "San_Francisco_CA",
    "San_Jose_CA",
    "Seattle_Tacoma_WA",
    "Spokane_WA",
    "St_Louis_MO",
    "Tampa_FL",
    "Washington_DC"
  ),
  
  Routes = c(
    9, 1, 41, 10, 2, 1, 31, 7, 90, 6,
    10, 10, 2, 36, 22, 22, 1, 11, 4, 3,
    5, 1, 20, 3, 11, 26, 33, 3, 11, 12,
    1, 7, 88, 3, 6, 1, 15, 6, 16, 1,
    1, 2, 2, 9, 8, 2, 12, 1, 4, 4,
    6
  ),
  
  Revenue = c(
    7278217,
    2067321,
    82255302,
    8313817,
    640724,
    394525,
    84605160,
    6002825,
    246608072,
    9163499,
    10965232,
    10047865,
    770767,
    69348658,
    40499983,
    33339055,
    377194,
    20027575,
    4420976,
    1651043,
    4163836,
    564330,
    35018834,
    3905958,
    11067561,
    31015604,
    118470609,
    3243319,
    26415452,
    23809944,
    447627,
    9442866,
    245190207,
    2835205,
    4069227,
    484098,
    14683704,
    7415899,
    13951108,
    565046,
    420530,
    766797,
    960529,
    7820529,
    21129583,
    1034836,
    13815558,
    340569,
    4229525,
    3506052,
    4403932
  )
)

#############################################################
# CALCULATE REVENUE PER ROUTE
#############################################################

city_data$Rev_Per_Route <- city_data$Revenue / city_data$Routes

#############################################################
# OBJECTIVE FUNCTION
#############################################################

# We are maximizing revenue per route
objective <- city_data$Rev_Per_Route

#############################################################
# CONSTRAINTS
#############################################################

# Constraint 1: Choose at most 23 cities
# x1 + x2 + ... + x51 <= 23

max_cities_constraint <- rep(1, nrow(city_data))

constraint_matrix <- matrix(
  max_cities_constraint,
  nrow = 1
)

constraint_direction <- "<="
constraint_rhs <- 23

#############################################################
# SOLVE OPTIMIZATION MODEL
#############################################################

result <- lp(
  direction = "max",
  objective.in = objective,
  const.mat = constraint_matrix,
  const.dir = constraint_direction,
  const.rhs = constraint_rhs,
  all.bin = TRUE
)

#############################################################
# VIEW RESULTS
#############################################################

# Optimization status
result$status

# Objective value
result$objval

# 0/1 decision variables
result$solution

#############################################################
# CREATE FINAL OUTPUT TABLE
#############################################################

city_data$Selected <- result$solution

selected_cities <- city_data[city_data$Selected == 1, ]

selected_cities <- selected_cities[order(-selected_cities$Rev_Per_Route), ]

print(selected_cities)

#############################################################
# SUMMARY OUTPUT
#############################################################

cat("Number of selected cities:", sum(city_data$Selected), "\n")
cat("Total selected routes:", sum(city_data$Routes * city_data$Selected), "\n")
cat("Total selected revenue:", sum(city_data$Revenue * city_data$Selected), "\n")
cat("Total selected revenue per route score:", result$objval, "\n")




