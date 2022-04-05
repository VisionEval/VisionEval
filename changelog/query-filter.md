## Filter Queries from the query spec

Rather than apply query filters to fully built results, have the filtering take place in the query
spec. So we add a "Filter" that names one of the geographies or a break point like this:

Filter = list(
  Marea = "RVMPO",   # a vector of Mareas - or adapt to some other geography
  Income = "min",    # a BreakName, or alternatively a numeric index
  # Income = 0       # a numeric index into the breaks associated with Index
)

Just ignore the Filter (with a warning) if the geography is wrong, or the breaks don't match
with either what is being "broken" or the names applied to it.

Reduce the results to a vector or an array. The filter can be more than one value in each list
element (so we can do a subset of geographies or breaks). Reduce the dimensionality accordingly.

For Function specs, the dimension of the resulting item must be "one". So when we check a spec,
we're going to look at the breaks and the filters, and just presume the filter specifies
something useful - the key is do we have either no filter or filter length more than one for
any of the Function's Symbols/Operands.