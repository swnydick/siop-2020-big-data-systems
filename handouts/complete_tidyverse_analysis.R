## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo       = TRUE,
                      out.width  = '1600px',
                      out.height = '900px',
                      dpi        = 200)


## ------------------------------------------------------------------------------------------------
options(repos = "https://cran.rstudio.com/")

cur_pkgs <- rownames(installed.packages())
req_pkgs <- c("tidyverse",
              "here",
              "vctrs",
              "fs",
              "vroom",
              "magrittr")

# determine packages that are missing
miss_pkgs <- setdiff(x = req_pkgs,
                     y = cur_pkgs)

# installing missing packages
if(length(miss_pkgs)){
  install.packages(miss_pkgs)
}


## ------------------------------------------------------------------------------------------------
# load the tidyverse packages
library(magrittr)
library(tidyverse)


## ------------------------------------------------------------------------------------------------
# check to see if any dependencies are out of date
tidyverse::tidyverse_update(recursive = TRUE)


## ------------------------------------------------------------------------------------------------
tidyverse::tidyverse_packages(include_self = FALSE)


## ------------------------------------------------------------------------------------------------
# pull project directory
project_dir <- here::here()

# combine file names to pull paths
data_files  <- vctrs::vec_c(
   demos  = "demos_to_merge",
   scores = "scores_to_merge",
   comb   = "data_to_rowbind"
)

# bind directory with file names to create file paths
data_paths  <- rlang::set_names(
   x  = fs::path(project_dir, "data", data_files, ext = "csv"),
   nm = names(data_files)
)

# can check to make sure the files exist before reading them in
print(fs::file_exists(data_paths))


## ------------------------------------------------------------------------------------------------
# method one of reading files
tidy_data_1 <- map(.x = data_paths,
                   .f = read_csv)

# this is equilvalent to:
# tidy_data_1 <- list(
#   demos  = read_csv(data_files[1]),
#   scores = read_csv(data_files[2]),
#   comb   = read_csv(data_files[3])
# )


## ------------------------------------------------------------------------------------------------
tidy_data_1[[1]]


## ------------------------------------------------------------------------------------------------
# a function to remove the first column and rename any X1_1 column as X1
remove_rowname_column <- function(tbl){
   tbl <- tbl %>%
          select(-1)
   
   # there are better ways of doing this that are more general
   if("X1_1" %in% names(tbl)){
      tbl <- tbl %>%
             rename(X1 = X1_1)
   }
   
   tbl
}

# removing the rownames column
tidy_data_1 <- map(.x = tidy_data_1,
                   .f = remove_rowname_column)


## ------------------------------------------------------------------------------------------------
# method two of reading files
tidy_data_2 <- map(.x = data_paths,
                   .f = vroom::vroom,
                   col_select = -1)

# this is equilvalent to:
# tidy_data_2 <- list(
#   demos  = vroom::vroom(data_files[1], col_select = -1),
#   scores = vroom::vroom(data_files[2], col_select = -1),
#   comb   = vroom::vroom(data_files[3], col_select = -1)
# )


## ------------------------------------------------------------------------------------------------
tidy_data_2[[1]]


## ------------------------------------------------------------------------------------------------
tidy_demos  <- pluck(tidy_data_1, "demos")
tidy_scores <- pluck(tidy_data_1, "scores")
tidy_comb_1 <- pluck(tidy_data_1, "comb")


## ------------------------------------------------------------------------------------------------
tidy_demos


## ------------------------------------------------------------------------------------------------
tidy_scores


## ------------------------------------------------------------------------------------------------
tidy_comb_1


## ------------------------------------------------------------------------------------------------
# sort dfs by guid
tidy_demos    <- tidy_demos %>%
                 arrange(guid)
tidy_scores   <- tidy_scores %>%
                 arrange(guid)

# remove a few odd rows from tidy_demos and even rows from tidy_scores
tidy_demos_1  <- tidy_demos %>%
                 mutate(row_n = row_number()) %>%
                 slice(-vctrs::vec_c(1, 3, 5)) %>%
                 select(row_n, everything())

tidy_scores_1 <- tidy_scores %>%
                 mutate(row_n = row_number()) %>%
                 slice(-vctrs::vec_c(2, 4)) %>%
                 select(row_n, everything())


## ------------------------------------------------------------------------------------------------
tidy_demos_1
tidy_scores_1


## ------------------------------------------------------------------------------------------------
id_var <- vctrs::vec_c("row_n", "guid")
inner_join(x  = tidy_demos_1,
           y  = tidy_scores_1,
           by = id_var) %>%
   arrange(row_n)

left_join(x  = tidy_demos_1,
          y  = tidy_scores_1,
          by = id_var) %>%
   arrange(row_n)

right_join(x  = tidy_demos_1,
           y  = tidy_scores_1,
           by = id_var) %>%
   arrange(row_n)

full_join(x  = tidy_demos_1,
          y  = tidy_scores_1,
          by = id_var) %>%
   arrange(row_n)


## ------------------------------------------------------------------------------------------------
inner_join(x = tidy_demos_1,
           y = tidy_scores_1)


## ------------------------------------------------------------------------------------------------
# keep all tidy_demos_1 rows with equivalents in tidy_scores_1
out <- semi_join(x  = tidy_demos_1,
                 y  = tidy_scores_1,
                 by = id_var)
out
names(out)

# keep all tidy_demos_1 rows without equivalents in tidy_scores_1
out <- anti_join(x  = tidy_demos_1,
                 y  = tidy_scores_1,
                 by = id_var)
out
names(out)


## ------------------------------------------------------------------------------------------------
id_var      <- "guid"
tidy_comb_2 <- inner_join(x  = tidy_demos,
                          y  = tidy_scores,
                          by = id_var)


## ------------------------------------------------------------------------------------------------
tidy_comb   <- bind_rows(tidy_comb_1,
                         tidy_comb_2)
tidy_comb


## ------------------------------------------------------------------------------------------------
tidy_comb <- list(tidy_comb_1,
                  tidy_comb_2) %>%
             bind_rows()
tidy_comb


## ------------------------------------------------------------------------------------------------
bind_rows(list(tidy_comb_1,
               tidy_comb_2),
          tidy_comb_1)


## ------------------------------------------------------------------------------------------------
bind_rows(tidy_comb_1,
          tidy_comb_2,
          .id = "source")
bind_rows(blah = tidy_comb_1,
          blee = tidy_comb_2,
          .id  = "source")


## ------------------------------------------------------------------------------------------------
df_1 <- select(tidy_comb_1,
               1, 3, 5) %>%
        slice(1, 2)
df_1
df_2 <- select(tidy_comb_2,
               1, 2, 4, 6, 7) %>%
        slice(1, 2)
df_2


## ------------------------------------------------------------------------------------------------
bind_rows(df_1,
          df_2)


## ------------------------------------------------------------------------------------------------
# change the type of data
df_1 <- tidy_comb_1 %>%
        mutate(X1 = as.character(X1))
df_2 <- tidy_comb_2

# try to bind rows together
safely(.f = bind_rows)(df_1, df_2)$error


## ------------------------------------------------------------------------------------------------
var_name       <- "trait"        # the category variable made from the columns
val_name       <- "normed_score" # the numeric variable made from the values
tidy_comb_long <- gather(tidy_comb,
                         key   = !!var_name,
                         value = !!val_name,
                         matches("^X[0-9]+$"))
tidy_comb_wide <- spread(tidy_comb_long,
                         key    = !!var_name,
                         value  = !!val_name)

# check if we've reversed everything
all_equal(target  = tidy_comb_wide,
          current = tidy_comb)


## ------------------------------------------------------------------------------------------------
tidy_comb_long
tidy_comb_wide


## ------------------------------------------------------------------------------------------------
gather(tidy_comb,
       key   = !!var_name,
       value = !!val_name,
       X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)


## ------------------------------------------------------------------------------------------------
gather(tidy_comb,
       key   = !!var_name,
       value = !!val_name,
       -guid, -data_level, -data_industry, -data_function)


## ------------------------------------------------------------------------------------------------
gather(tidy_comb,
       key   = !!var_name,
       value = !!val_name,
       starts_with("X"))


## ------------------------------------------------------------------------------------------------
tidy_comb_long_2 <- pivot_longer(tidy_comb,
                                 cols      = matches("^X[0-9]+$"),
                                 names_to  = var_name,
                                 values_to = val_name)
tidy_comb_wide_2 <- pivot_wider(tidy_comb_long_2,
                                names_from  = all_of(var_name),
                                values_from = all_of(val_name))

# check if we've reversed everything
all_equal(target  = tidy_comb_wide_2,
          current = tidy_comb_wide)
all_equal(target  = tidy_comb_long_2,
          current = tidy_comb_long)


## ------------------------------------------------------------------------------------------------
pivot_longer(tidy_comb,
             cols      = matches("^X[0-9]+$"),
             names_to  = var_name,
             values_to = val_name,
             
             # remove the "X" in front of the variable names
             names_prefix = "^X")


## ------------------------------------------------------------------------------------------------
pivot_longer(tidy_comb,
             cols      = matches("^X[0-9]+$"),
             values_to = val_name,
             
             # put the "X" in front of the variable names into a separate variable
             names_to  = c("prefix", var_name),
             names_sep = 1)


## ------------------------------------------------------------------------------------------------
tidy_comb_spec <- build_longer_spec(tidy_comb,
                                    cols      = matches("^X[0-9]+$"),
                                    names_to  = var_name,
                                    values_to = val_name)
tidy_comb_spec


## ------------------------------------------------------------------------------------------------
tidy_comb_spec <- tidy_comb_spec %>%
                  separate(col  = "trait",
                           into = vctrs::vec_c("prefix", "trait_number"),
                           sep  = 1) %>%
                  mutate(.value = inset(.value,
                                        2,
                                        value = "whoops"),
                         random = letters[vctrs::vec_seq_along(prefix)])
tidy_comb_spec


## ------------------------------------------------------------------------------------------------
pivot_longer_spec(data = tidy_comb,
                  spec = tidy_comb_spec) %>%
   select(-(guid:data_function))


## ------------------------------------------------------------------------------------------------
all_equal(target  = tidy_comb,
          current = tidy_comb %>%
                    pivot_longer_spec(tidy_comb_spec) %>%
                    pivot_wider_spec(tidy_comb_spec,
                                     values_fn = list(whoops = first)))


## ------------------------------------------------------------------------------------------------
tidy_comb_long %>%
   group_by(data_level, trait) %>%
   summarize(mean_score = mean(normed_score),
             sd_score   = sd(normed_score))


## ------------------------------------------------------------------------------------------------
# function to sort levels by integer buried in string
pull_out_number <- function(x){
   stringr::str_extract(string = x,
                        pattern = "[0-9]+") %>%
   as.integer() %>%
   unique()
}

# updating levels of trait to be in right order
tidy_comb_long <- 
   tidy_comb_long %>%
   mutate(
     trait = forcats::as_factor(trait) %>%
             forcats::fct_reorder(.f   = .,
                                  .x   = .,
                                  .fun = pull_out_number)
   )

# rerun
tidy_comb_long %>%
   group_by(data_level, trait) %>%
   summarize(mean_score = mean(normed_score),
             sd_score   = sd(normed_score))


## ------------------------------------------------------------------------------------------------
# repeat this for all 10 variables ... ugh
tidy_comb_wide %>%
   group_by(data_level) %>%
   summarize(X1_mean = mean(X1),
             X1_sd   = sd(X1),
             X2_mean = mean(X2),
             X2_sd   = sd(X2),
             X3_mean = mean(X3),
             X3_sd   = sd(X3))


## ------------------------------------------------------------------------------------------------
tidy_comb_wide %>%
   group_by(data_level) %>%
   summarize(across(.cols = matches("^X[0-9]+$"),
                    .fns  = list(mean = mean,
                                 sd   = sd)))


## ------------------------------------------------------------------------------------------------
tidy_comb_wide %>%
   group_by(data_level) %>%
   summarize(across(.cols = -c("guid", "data_industry", "data_function"),
                    .fns  = list(mean = mean,
                                 sd   = sd)))

