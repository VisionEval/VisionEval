## Filter Queries from the query spec

Rather than apply query filters to fully built results, have the filtering take place in the query
spec. So we add a "Filter" that names one (or a list) of the geographies or breakpoints like this:

Filter = list(
  Marea = "RVMPO",   # a vector of Mareas - or adapt to some other geography
  Income = c("min","20000"),    # a BreakName, or alternatively a numeric index
  # Income = 0       # a numeric index into the breaks associated with Index
)

Just ignore the Filter (with a warning) if the geography is wrong, or the breaks don't match
with either what is being "broken" or the names applied to it.

Reduce the results to a vector or an array. The filter can be more than one value in each list
element (so we can do a subset of geographies or breaks). Reduce the dimensionality accordingly.

## Clean up function specs so they are dimensionally correct.

Still cleaning up function specs so they will work on vector or array measures (need to check
the filtered dimensions on each operand to ensure all geographies or breaks are the same). Then
add proper dimensions and names to the results.

Need to get the PBOT model in place so we can use its multiple Azones and Mareas to further
test the full filtering.
See: C:/Git-Repos/VE-Partners/Portland Full Model