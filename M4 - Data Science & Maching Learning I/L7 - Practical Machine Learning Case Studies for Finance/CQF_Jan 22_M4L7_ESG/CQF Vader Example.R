# This file illustrates the results of Vader for different text input.

library(vader)

setwd("C:/My Path")

# For a good introduction to Vader:
# https://t-redactyl.io/blog/2017/04/using-vader-to-handle-sentiment-analysis-with-social-media-text.html

# Look at the compound sentiment values:
get_vader("The food here is great.")
get_vader("The food here is great!")
get_vader("The food here is great!!")
get_vader("The food here is great, but the service is horrible.")

