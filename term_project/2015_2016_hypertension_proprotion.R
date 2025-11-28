# In this script, the importance of using weight is showed.
# 
# this is to replicate the second sample of using weights in this link.
# https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx

# data source
# https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&Cycle=2015-2016


library(haven)
demo <- read_xpt("BPX_2015_2016.XPT")   # demographics
head(demo)
