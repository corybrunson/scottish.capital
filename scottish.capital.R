# Read the data as a set of bipartite graphs with node attributes
source('scripts/read.scottish.capital.R')

# Clean the data
# Note: Several director names are manually identified in order to achieve
# agreement with basic statistics reported in the book.
source('scripts/clean.scottish.capital.R')

# Test the data; reproduce results from the book
source('scripts/test.scottish.capital.R')
