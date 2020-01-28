
setwd('/Users/jonesj/Desktop/Projects/Active/SIOP_2020/siop-2020-big-data-systems')

## read in data !
# demos to merge with scores
demos   <- read.csv(file             = 'data/demos_to_merge.csv',
                    header           = TRUE,
                    stringsAsFactors = FALSE)

# scores to merge with demos
scores  <- read.csv(file             = 'data/scores_to_merge.csv',
                    header           = TRUE,
                    stringsAsFactors = FALSE)

# data to merge with previous data once previous data have been merged !
row_dat <- read.csv(file             = 'data/data_to_rowbind.csv',
                    header           = TRUE,
                    stringsAsFactors = FALSE)

## note! can set stringsAsFactors = FALSE as a global statement:
## options(stringsAsFactors = FALSE)

head(demos)
head(scores)
head(row_dat)

## investigating the data shows that there is a variable that we can get rid of:
## X; this variable is basically a hold-over that shows row position of the 
## original data that was created for this demo.

demos   <- demos[, !colnames(demos) %in% 'X']
scores  <- scores[, !colnames(scores) %in% 'X']
row_dat <- row_dat[, !colnames(row_dat) %in% 'X']

# we will now merge demos and scores together using the merge() function
# If you inpsect this function, you will see quite a bit of confuing arguments

# to start, we are going to order both demos and scores by the guid variable
# note - this is for demonstration only, and is not necessary for merging

demos  <- demos[order(demos$guid), ]
scores <- scores[order(scores$guid), ]

# merge known as an inner join
merge_ex_1 <- merge(x     = demos,
                    y     = scores,
                    by    = 'guid',
                    all   = FALSE)

# merge known as a left outer join
merge_ex_2 <- merge(x     = demos,
                    y     = scores,
                    by    = 'guid',
                    all.x = TRUE)

# merge known as a right outer join
merge_ex_3 <- merge(x     = demos,
                    y     = scores,
                    by    = 'guid',
                    all.y = TRUE)

# merge known as a full outer join
merge_ex_4 <- merge(x     = demos,
                    y     = scores,
                    by    = 'guid',
                    all   = TRUE)

# all of these merges give the exact same final table - this behavior does not
# always happen, and only occurs b/c we have 1 to 1 matching guids (the variable
# used to merge cases). Lets remove some cases and see how the behavior changes 
# with merges

demos_rm  <- demos[-c(1, 3, 5), ]
scores_rm <- scores[-c(2, 4), ]

head(demos_rm)
head(scores_rm)

# inner join on reduced data
merge_ex_5 <- merge(x     = demos_rm,
                    y     = scores_rm,
                    by    = 'guid',
                    all   = FALSE)

dim(merge_ex_5)
dim(merge_ex_1)

merge_ex_5[1:5, 1:4]
merge_ex_1[1:5, 1:4]

# left outer join on reduced data
merge_ex_6 <- merge(x     = demos_rm,
                    y     = scores_rm,
                    by    = 'guid',
                    all.x = TRUE)

dim(merge_ex_6)
dim(merge_ex_2)

merge_ex_6[1:5, c(1, 7:8)]
merge_ex_2[1:5, c(1, 7:8)]

# right outer join on reduced data
merge_ex_7 <- merge(x     = demos_rm,
                    y     = scores_rm,
                    by    = 'guid',
                    all.y = TRUE)

dim(merge_ex_7)
dim(merge_ex_3)

merge_ex_7[1:5, c(1,2, 7)]
merge_ex_3[1:5, c(1,2, 7)]

# full outer join on reduced data
merge_ex_8 <- merge(x     = demos_rm,
                    y     = scores_rm,
                    by    = 'guid',
                    all   = TRUE)

dim(merge_ex_8)
dim(merge_ex_4)

merge_ex_8[1:5, c(1,2, 7:8)]
merge_ex_4[1:5, c(1,2, 7:8)]

## now that we have merged cases on an ID - how do we merge data with similar
## columns?

## if all columns match, we can use rbind()
all_data_ex_1 <- rbind(merge_ex_1, row_dat)

## however, if at least one column doesn't match...
row_dat_rm <- row_dat[, -3]
all_data_ex_2 <- rbind(merge_ex_1, row_dat_rm)

# error :( 
# one way to deal with this... not pretty
# grab the unique columne names of the dataset to merge
all_columns <- unique(c(colnames(merge_ex_1), 
                        colnames(row_dat_rm)))

# grab sequence of rows from first dataset
seq_1 <- 1:nrow(merge_ex_1)

# grab sequence of rows from second dataset and total length of seq_1 
seq_2 <- 1:nrow(row_dat_rm) + length(seq_1)

# create empty data.frame()
crazy_merge <- as.data.frame(matrix(data = NA, 
                                    nrow = length(seq_1) + length(seq_2),
                                    ncol = length(all_columns)))
colnames(crazy_merge) <- all_columns

# fill out the empty data frame 
crazy_merge[seq_1, colnames(merge_ex_1)] <- merge_ex_1
crazy_merge[seq_2, colnames(row_dat_rm)] <- row_dat_rm

crazy_merge[544:550, 1:4]

# the above method is cumbersome.. especially when you have multiple datasets
# with many differing columns missing.. there are much nicer functions that
# we will show later

## summary statistics
## how to compute the means of a numeric score by catgorical variable level
X1_by_level_mn <- tapply(X     = all_data_ex_1$X1,
                         INDEX = all_data_ex_1$data_level,
                         FUN   = mean)

# how to do all numeric vars by level? wrap teh tapply in a lapply and combine
# little tricky..
numeric_var_nms   <- paste0("X", 1:10)
level_by_score_mn <- lapply(X   = as.list(numeric_var_nms),
                            FUN = function(x) {
                              tapply(X     = all_data_ex_1[, x],
                                     INDEX = all_data_ex_1$data_level,
                                     FUN   = mean)
                            })

level_by_score_mn <- do.call(rbind, level_by_score_mn)
rownames(level_by_score_mn) <- numeric_var_nms

round(level_by_score_mn, 3)

# if you want a different summary stat, just replace 'mean' ('var', 'sd', etc)















