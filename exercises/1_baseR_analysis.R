#########################################
# Base R Analysis                       #
#                                       #
# Korn Ferry Institute: Automation Team #
# 2020-04-24                            #
#########################################

# 1. Required Packages =========================================================

# repo to use
options(repos = "https://cran.rstudio.com/")

# can also set other options here, like stringsAsFactors = FALSE

# 2. Loading Data ==============================================================

# get project directory (SPECIFY MANUALLY???)
proj_dir    <- "/Users/nydicks/OneDrive - Korn Ferry/Documents/Projects/Workshops and Training/Workshops/2020/siop-2020-big-data-systems"
data_dir    <- file.path(proj_dir, "data")

# read in data

# - demos to merge with scores
baser_demos  <- read.csv(file             = file.path(data_dir, "demos_to_merge.csv"),
                         header           = TRUE,
                         stringsAsFactors = FALSE,
                         row.names        = 1)

# - scores to merge with demos
baser_scores <- read.csv(file             = file.path(data_dir, "scores_to_merge.csv"),
                         header           = TRUE,
                         stringsAsFactors = FALSE,
                         row.names        = 1)

# data to merge with previous data once previous data have been merged !
baser_comb_1 <- read.csv(file             = file.path(data_dir, "data_to_rowbind.csv"),
                         header           = TRUE,
                         stringsAsFactors = FALSE,
                         row.names        = 1)

# note: can set stringsAsFactors = FALSE as a global statement
#  options(stringsAsFactors = FALSE)

# take a look at the data
head(baser_demos)
head(baser_scores)
head(baser_comb)

# if we didn't know to specify `row.names` above, this variable (a hold-over that
# shows row position of the original data) would be read in as a separate column
# `X`
baser_demos_2    <- read.csv(file             = file.path(data_dir, "demos_to_merge.csv"),
                             header           = TRUE,
                             stringsAsFactors = FALSE)

head(baser_demos_2)

# we could have easily removed this variable
baser_demos_2    <- baser_demos_2[ , !(colnames(baser_demos_2) %in% "X")]


# 3. Merging ===================================================================

#   A. Combine Variables -------------------------------------------------------

# we will merge demos and scores together using the merge() function
# - in base-R, everything all column-wise merging uses the "merge" function

# lets arrange by guid (so that merging doesn't change the order, this is for
# demonstration only and is not necessary for merging)
baser_demos   <- baser_demos[order(baser_demos$guid), ]
baser_scores  <- baser_scores[order(baser_scores$guid), ]

# because all of the guids are in both tables (the variable used to merge cases),
# regardless of how we merge, the result is exactly the same

# merge known as an inner join
merge_ex_1 <- merge(x     = baser_demos,
                    y     = baser_scores,
                    by    = "guid",
                    all   = FALSE)

# merge known as a left outer join
merge_ex_2 <- merge(x     = baser_demos,
                    y     = baser_scores,
                    by    = "guid",
                    all.x = TRUE)

# merge known as a right outer join
merge_ex_3 <- merge(x     = baser_demos,
                    y     = baser_scores,
                    by    = "guid",
                    all.y = TRUE)

# merge known as a full outer join
merge_ex_4 <- merge(x     = baser_demos,
                    y     = baser_scores,
                    by    = "guid",
                    all   = TRUE)

# all of these should be exactly the same
identical(merge_ex_1, merge_ex_2)
identical(merge_ex_1, merge_ex_3)
identical(merge_ex_1, merge_ex_4)

# let's remove some cases and see how the behavior changes with merging
baser_demos_1  <- baser_demos[-c(1, 3, 5), ]
baser_scores_1 <- baser_scores[-c(2, 4), ]

head(baser_demos_1)
head(baser_scores_1)

# inner join on reduced data
merge_ex_5 <- merge(x     = baser_demos_1,
                    y     = baser_scores_1,
                    by    = "guid",
                    all   = FALSE)

dim(merge_ex_5)
dim(merge_ex_1)

merge_ex_5[1:5, 1:4]
merge_ex_1[1:5, 1:4]

# left outer join on reduced data
merge_ex_6 <- merge(x     = baser_demos_1,
                    y     = baser_scores_1,
                    by    = "guid",
                    all.x = TRUE)

dim(merge_ex_6)
dim(merge_ex_2)

merge_ex_6[1:5, c(1, 7:8)]
merge_ex_2[1:5, c(1, 7:8)]

# right outer join on reduced data
merge_ex_7 <- merge(x     = baser_demos_1,
                    y     = baser_scores_1,
                    by    = "guid",
                    all.y = TRUE)

dim(merge_ex_7)
dim(merge_ex_3)

merge_ex_7[1:5, c(1,2, 7)]
merge_ex_3[1:5, c(1,2, 7)]

# full outer join on reduced data
merge_ex_8 <- merge(x     = baser_demos_1,
                    y     = baser_scores_1,
                    by    = "guid",
                    all   = TRUE)

dim(merge_ex_8)
dim(merge_ex_4)

merge_ex_8[1:5, c(1,2, 7:8)]
merge_ex_4[1:5, c(1,2, 7:8)]

# we will use the FULL merge as our combination data moving forward
baser_comb_2  <- merge_ex_1

#   B. Combine People ----------------------------------------------------------

# if all columns match, we can use rbind
baser_comb    <- rbind(baser_comb_1,
                      baser_comb_2)

# if at least one column doesn't match ...
baser_comb_ugh <- baser_comb_2[ , -3]

tryCatch(
  expr  = rbind(baser_comb_ugh,
                baser_comb_1),
  error = function(e) message(e)
)

# we have an error ... to be able to do this in baseR, we kind of have to
# hack a solution ...

# a. find unique columns of dataset to merge
all_columns    <- unique(c(colnames(baser_comb_ugh),
                           colnames(baser_comb_1)))

# grab sequence of rows from first dataset
seq_1          <- seq_len(nrow(baser_comb_ugh))
seq_2          <- seq_len(nrow(baser_comb_1))

# build an empty data.frame with both sets of columns and all_columns as names
baser_comb_3   <- as.data.frame(
  matrix(data = NA,
         nrow = length(seq_1) + length(seq_2),
         ncol = length(all_columns))
)

colnames(baser_comb_3) <- all_columns

# add back the rows into the empty data frame
baser_comb_3[seq_1, colnames(baser_comb_ugh)] <- baser_comb_ugh
baser_comb_3[seq_2, colnames(baser_comb_1)]   <- baser_comb_1

baser_comb_3[544:550, 1:4]

# the above method is cumbersome, especially if you are combining multiple datasets
# with different missing columns ... there are better functions.


# 4. Reshaping =================================================================

# the data is currently in wide format with each item (`X1`, ... , `X10`) a column
# of the data - we might want to make the data so that each variable forms a
# column and each observation forms a row.

# the base R reshape function is pretty slow and cumbersome
var_name        <- "trait"
val_name        <- "normed_score"

# - wide to long (don't ever do this), but we need to use the following vars:
#   - varying: variables where the entries turn into the "values"
#   - v.names: what to name the final "values"
#   - timevar: what to name the variable (names of variables that turn into the "values")
#              column
#   - idvar:   what uniquely identifies a person/record
baser_comb_long <- reshape(baser_comb,
                           varying   = paste0("X", 1:10),
                           v.names   = val_name,
                           timevar   = var_name,
                           idvar     = names(baser_demos),
                           direction = "long")

# - if we've done wide-to-long that way, we can just use reshape again to get
#   from long to wide
baser_comb_wide   <- reshape(baser_comb_long,
                             direction = "wide")

# - we could also specify variables explicitly
baser_comb_wide_2 <- reshape(baser_comb_long,
                             v.names   = val_name,
                             timevar   = var_name,
                             idvar     = names(baser_demos),
                             direction = "wide",
                             sep       = "")

head(baser_comb_wide)
head(baser_comb_wide_2)

# note that this reshape function pretty much assumes that our reshaped data is
# time-series so does some pretty annoying things
#  - keeps just the numbers when moving from wide-to-long (even though we didn't
#    tell it to do this)
#  - pastes the name of the v.name ONTO the values of the timevar when reshaping
#    (if we are using not-already pre-reshaped data)
#  - adds rownames that we can't get rid of in the command itself

# we can fix some of that behavior by specifying the times variable (which is
# very cumbersome because we have to specifying varying and times SEPARATELY)
baser_comb_long_2 <- reshape(baser_comb,
                             varying   = paste0("X", 1:10),
                             v.names   = val_name,
                             timevar   = var_name,
                             times     = paste0("X", 1:10),
                             idvar     = names(baser_demos),
                             direction = "long")

head(baser_comb_long_2)

# 5. Aggregating ===============================================================

#   A. Long Format Data --------------------------------------------------------

# if your data is in long format, you can simply do "tapply" (which turns everything
# into wide format already)
with(baser_comb_long, {
  tapply(normed_score, list(data_level, trait), mean)
})

# you can also use "aggregate" if you want your data to be in the same format
aggr_comb_1_mn <- aggregate(normed_score ~ data_level + trait,
                            FUN  = mean,
                            data = baser_comb_long)
aggr_comb_1_sd <- aggregate(normed_score ~ data_level + trait,
                            FUN  = sd,
                            data = baser_comb_long)

# you can rename normed_score to "mean" and "sd" and then merge everything
# together
merge(x  = aggr_comb_1_mn,
      y  = aggr_comb_1_sd,
      by = c("data_level", "trait"),
      suffix = c("_mean", "_sd"))

# because X10 should come at the end, you can re-order X1-X10 based on the required
# order (which will be covered a bit in the next section)
                                  
#   B. Wide Format Data --------------------------------------------------------

# if your data is in wide format, you aggregate by specifying the columns that
# you want to aggregate
aggr_comb_2_mn <- aggregate(x   = baser_comb_wide[paste0("X", 1:10)],
                            by  = baser_comb_wide[c("data_level")],
                            FUN = mean)
aggr_comb_2_sd <- aggregate(x   = baser_comb_wide[paste0("X", 1:10)],
                            by  = baser_comb_wide[c("data_level")],
                            FUN = sd)

# you can rename normed_score to "mean" and "sd" and then merge everything
# together
merge(x  = aggr_comb_2_mn,
      y  = aggr_comb_2_sd,
      by = c("data_level"),
      suffix = c("_mean", "_sd"))

# this is a bit cumbersome and can be made easier by saving the intermediate
# variables (such as the names of the grouping variable and the aggregation
# variables)