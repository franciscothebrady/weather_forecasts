First generate a random ID for each row in the events_processed.csv file.
You can do this with

```
pop_data <- events_processed %>% mutate(id = sample(nrow(.), size = nrow(.)))
```

This will put a random number in a new column called `id`. We will need this
to later merge the results back in and tracking each of the events.

To the `pop_data` data frame, append the POP values. (If you can, also add the actual precipitation amounts in mm.)

Next, arrange `pop_data` as such:

id | GHCND.prcp_cat | GHCND.prcp_amt | Q24.f2 | ... | P24.f2 | ...

You can remove all the other columns.

Install the `verification` package and you can test the result with the `value` function.

```
value(pop_data$GHCND.prcp_amt, pop_data$P24.f2)
```
