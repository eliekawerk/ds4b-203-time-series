# Moving Average Example

library(tidyverse)
library(timetk)

1:10

1:10 %>% slidify_vec(.f = mean, .period = 2, .align = "right", .partial = FALSE)
