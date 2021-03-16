
rm(list = ls())
options(stringsAsFactors = FALSE)

library(MASS)

N <- 100000
p <- 10

num_cor <- p * (p-1) / 2

set.seed(1234)

rs <- runif(num_cor, 0, 0.40)

R <- matrix(data = 0,
            nrow = 10,
            ncol = 10)

R[upper.tri(R)] <- rs
R <- R + t(R)
diag(R) <- 1

data_numeric <- mvrnorm(n         = N,
                        mu        = rep(0, p),
                        Sigma     = R,
                        empirical = TRUE)

# gen categorical data
levels   <- c('Individual Contributor', 'Supervisor', 'Mid-level Manager',
              'Functional Leader')
industry <- c('Advanced Technology', 'Consumer Goods', 'Financial Services',
              'Government', 'Retail', 'Communications', 'Healthcare')
functio  <- c('Legal', 'Sales', 'Operations', 'Marketing', 'Human Resources',
              'General Management', 'Administrative Services')

data_level <- sample(x       = levels,
                     size    = N,
                     replace = TRUE,
                     prob    = c(0.65, 0.15, 0.1, 0.1))

data_industry <- sample(x       = industry,
                        size    = N,
                        replace = TRUE)

data_function <- sample(x       = functio,
                        size    = N,
                        replace = TRUE)

guid <- vector(length = N)
for(i in seq_len(N)) guid[i] <- paste(sample(c(LETTERS, letters, 1:9), 10), collapse = '')

master_data <- data.frame(guid, data_level, data_industry, data_function, data_numeric)

write.csv(master_data, '~/Desktop/Master_Data_20210315.csv')

##
rbind_1 <- master_data[1:250, ]
write.csv(rbind_1, '~/Desktop/data_to_rowbind.csv')


demos <- master_data[23456:24000, 1:4]
scors <- master_data[23456:24000, c(1, 5:ncol(master_data))]

write.csv(demos, '~/Desktop/demos_to_merge.csv')
write.csv(scors, '~/Desktop/scores_to_merge.csv')



