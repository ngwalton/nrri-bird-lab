# this is a template to provide examples of error checking, figeures,
# etc. that our lab commonly uses when first looking at count data.

# list of examples to add:
# show example of adding common names?


# uncomment and run the following if libraries were not previously installed;
# only needs to be run once
# install.packages("readxl")
# install.packages("here")
# install.packages("ggplot2")
# install.packages("reshape2")

library(readxl)  # to read excel files; not needed if data are plain text (e.g., csv)
library(here)    # to auto-find working directory
library(ggplot2)
library(reshape2)

# set your working directory to where your project lives. using here() from the
# here package makes this easy and repetable accross computerers. here() will
# look for an RStudio file (among other things) to guess your project root.
setwd(here())

# one can also hard-code the location like so (be sure to use forward slashes or
# double backslashes)
# setwd("/data_template")  # if project is in the root (usually C on Windows)
# setwd("~/data_template") # if project is in your home directory (My Docs on Widows)


# read data ----

# if your data are in excel format you can read them with read_excel
# read_excel returns a tibble which is a data frame with a couple extra features
# read_excel will generally read dates in as dates rather than stings
# in this example, we increased guess_max to 2000 (the number rows it checks
# before setting the data type) so that read_excel correctly guessed the
# "comments" column
data_file <- "./data/example_data_2018_masterfile.xlsx"
sp <- read_excel(path = data_file, sheet = "bird",
                 guess_max = 2000)

env <- read_excel(data_file, "site")

# if your data are in csv format you can read them with read.csv
# this is the tried and true method of reading data into R
# "as.is = TRUE" stops read.csv from formatting strings as factors
# it's often easier to do this and then make factors of any columns needed
# sp <- read.csv("example_data_2018_masterfile_bird.csv", as.is = TRUE)
# env <- read.csv("example_data_2018_masterfile_site.csv", as.is = TRUE)

# format date as a date; another common date format is "%Y-%m-%d"
# date_form <- "%m/%d/%Y"
# sp$date <- as.Date(sp$date , date_form)
# env$date <- as.Date(env$date , date_form)


# basic error checking ----

# error checking will vary by dataset, but there are some basic things you can
# expect to check. if this is a data cleaning exersize, you will want to insure
# all useful fields contain expected/resonable values, and that there are no
# missing values from required fields.

# there should be no duplicates of sites/ptcount combinations in this data set.
# if there was more than one year's worth of data for sites, one could add date
# to the values to check there.
anyDuplicated(env[, c("site", "ptcount")]) # any value other than 0 indicates dupicates

# some columns are not optional -- check these for NA values
check <- c("site", "ptcount", "date", "time")
anyNA(env[, check])

# there are NAs -- check which column and how many
sapply(env[, check], function(x) sum(is.na(x)))  # one time value is missing

# this may not be important in this case, but checking unique values is often
# useful to make sure they are as expected
sort(unique(env$wind))


# check dates ----

# check that survey dates are within the expected range
range(env$date)

# check survey date distribution
ggplot(data = env, mapping = aes(x = date)) +
  geom_bar()


# check for large values ----

# plotting the number of individuals for each species from all point counts is
# good way to spot values that are outside the norm.

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
            vjust = 1.2) +
  xlab("row number")


# limit to singing obs ----

# optionally, one can limit the analysis to observations of singing birds
# sp <- sp[sp$type == "S", ]


# aggregate by count and species ----

# not that this will drop information about what type of observations was recorded

# this will result in one row per species/count for each minute/distance that
# species was detected. To aggregate such that there is only one record for each
# species per point count, add "minute" and "distance" to "drop".
drop <- c("type", "comments")
sp <- sp[, setdiff(names(sp), drop)]
sp <- aggregate(howmany ~ ., sp, FUN = sum)


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


# side-by-side bar plot by treament ----

# check how many of each there are
table(env$treatment)

# this is only needed if you want control over the order that treatment is
# plotted in. it will order treatment alphabetically by default. one could do
# the same thing with species if desired. levels must contain all levels in
# treatment, and should be in the order you want them to appear in the figure
# legend.
env$treatment <- factor(env$treatment, levels = c("group", "strip", "control"))

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
  # not needed in this example, but useful for long names
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("n observations") +
  xlab("Species")


# wide format ----

# often, our data are stored such that a given survey has multiple records
# associated with it (e.g., one row per species). this is often referred to as
# long format. for some analyses, we need one row per survey and one column per
# species. this is often referred to as wide format. the following converts long
# format to wide format.

# in the formula, place all columns that identify a single survey (site, ptcount,
# and date in this case) on the left of the tilde and the column that designates
# the new column names on the right.
sp_wide <- dcast(sp, formula = site + ptcount + date ~ sppcode,
                 fun.aggregate = sum, fill = 0, value.var = "howmany")
