# example template

# run if libraries are not installed
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("reshape2")

library(readxl)  # to read excel files -- not needed if data are in plain text (e.g., csv)
library(ggplot2)
library(reshape2)

# set your working directory to where your data live
# be sure to use forward slashes or double backslashes
setwd("C:/git_repositories/data_template/data")


# read data ----

# if your data are in excel format you can read them with read_excel
# read_excel returns a tibble which is a data frame with a couple extra features
# read_excel will generally read dates in as dates reather than stings
# in this example, we increased guess_max to 2000 (the number rows it checkes
# before setting the data type) so that read_excel correctly guessed the
# "comments" column
sp <- read_excel(path = "LeaveTree_2018_Masterfile.xlsx", sheet = "Bird",
                 guess_max = 2000)

env <- read_excel("LeaveTree_2018_Masterfile.xlsx", "Site")

# if your data are in csv format you can read them with read.csv
# this is the tried and true method of reading data into R
# "as.is = TRUE" stops read.csv from formating strings as factors
# it's often easier to do this and then make factors of any collomns needed
# sp <- read.csv("LeaveTree_2018_Masterfile_bird.csv", as.is = TRUE)
# env <- read.csv("LeaveTree_2018_Masterfile_site.csv", as.is = TRUE)

# formate date as a date
# dates may also be formated as "%Y-%m-%d"
# date_form <- "%m/%d/%Y"
# sp$date <- as.Date(sp$date , date_form)
# env$date <- as.Date(env$date , date_form)


# limite to singing obs ----

# optionally, one can limit the analysis to observations of singing birds
# sp <- sp[sp$type == "S", ]


# aggregate by count and species ----

# not that this will drop information about what type of observations was recorded

# this will result in one row per species/count for each minute/distance that
# species was detected. To aggretate such that there is only one record for each
# species per point count, add "minute" and "distance" to "drop".
drop <- c("type", "comments")
sp <- sp[, setdiff(names(sp), drop)]
sp <- aggregate(howmany ~ ., sp, FUN = sum)


# check dates ----

# check that survey dates are within the expected range
range(env$date)

# check survey date distribution
ggplot(data = env, mapping = aes(x = date)) +
  geom_bar()


# check for large values ----

# only species counts greater than cutoff will be labeled
cutoff <- 20

# sum number of individuals per species by point count
sp_lv <- aggregate(howmany ~ sppcode + site + ptcount + date, sp, FUN = sum)
sp_lv$rnum <- as.numeric(rownames(sp_lv))
sp_labs <- sp_lv[sp_lv$howmany > cutoff, ]

# vjust may need some tweaking to plot in a useful manor (moves labels up and down)
ggplot(data = sp_lv, mapping = aes(x = rnum, y = howmany)) +
  geom_point() +
  geom_text(aes(x = rnum, y = howmany, label = sppcode), data = sp_labs,
            vjust = 1.2)


# species frequency distribution ----

sp_bar <- aggregate(howmany ~ sppcode, sp, FUN = length)
sp_bar <- sp_bar[order(sp_bar$howmany, decreasing = TRUE), ]
sp_bar$sppcode <- factor(sp_bar$sppcode, levels = sp_bar$sppcode)

# expand removes white space between labels and bars
# limits maintains white space above bars
limits <- c(0, max(sp_bar$howmany * 1.03))

# this will often need to be a very large figure
# in RStudio, click "Zoom", then maximize the window
ggplot(data = sp_bar, mapping = aes(x = sppcode, y = howmany)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = limits) +
  ylab("count frequency")


# side-by-side bar plot ----

# here, we predend that there are two treatments in the study Y and N.
env$treatment <- sample(c("Y", "N"), size = nrow(env), replace = TRUE)

# check how many of each there are
table(env$treatment)

# this is only needed if you want control over the order that treatment is
# plotted in. it will order treatment alphabetically by default. one could do
# the same thing with species if desired
env$treatment <- factor(env$treatment, levels = c("Y", "N"))

sp_side <- aggregate(howmany ~ site + ptcount + date + sppcode, sp, FUN = sum)
by <- c("site", "ptcount", "date")
sp_side <- merge(sp, env, by = by)

# just look at the 10 most common species
top10 <- table(sp_side$sppcode)
top10 <- names(sort(top10, decreasing = TRUE))[1:10]
sp_side <- sp_side[sp_side$sppcode %in% top10, ]

ggplot(sp_side, aes(x = sppcode, fill = treatment)) +
  # dodge places the bars next to each other instead of the default on top of
  # each other
  geom_bar(position = "dodge") +
  # the rest is just making it look nice
  # not needed in this example, but usefull for long names
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("n observations") +
  xlab("Species") +
  scale_fill_manual(values = c("blue3", "firebrick3"))


# wide formate ----

# often, our data are stored such that a given survey has multiple records
# associated with it (e.g., one row per species). this is often refered to as
# long formate. for some analyses, we need one row per survey and one column per
# species. this is often refered to as wide format. the following converts long
# format to wide format.

# in the formula, place all columns that identify a single survey (site, ptcount,
# and date in this case) on the left of the tilde and the column that designates
# the new column names on the right.
sp_wide <- dcast(sp, formula = site + ptcount + date ~ sppcode,
                 fun.aggregate = sum, fill = 0, value.var = "howmany")
