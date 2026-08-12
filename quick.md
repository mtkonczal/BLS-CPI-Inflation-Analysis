# The swimwear comma

Two files in the same BLS directory disagree about what an item is called.
Found 2026-08-11, against the June 2026 CPI release.

---

## Part 1: The technical version

### What happened

`scripts/01c_replicate_bls_tables.R` reconciles our CPI pipeline against the
published news release tables. It joins the scraped table to our data on item
name, purely as a diagnostic, and reports anything it cannot match. Two rows
came back unmatched every time:

```
Table 6 rows with no item_code match: 2
  Men's underwear, nightwear, swimwear, and accessories
  Women's underwear, nightwear, swimwear, and accessories
```

### The disagreement

Both files live in `https://download.bls.gov/pub/time.series/cu/`. Both key on
`item_code`. They do not agree on `item_name`.

| Source | `SEAA02` |
|---|---|
| `cu.item` | `Men's underwear, nightwear, swimwear and accessories` |
| `cu.series` | `Men's underwear, nightwear, swimwear, and accessories` |
| `cpi.t06.htm` | `Men's underwear, nightwear, swimwear, and accessories` |

One serial comma. Same for `SEAC04` (women's). `cw.item`, the CPI-W lookup,
sides with `cu.item` and omits it.

### It is not just swimwear

Comparing the item name in `cu.item` against the name embedded in
`cu.series`'s `series_title` field, for all 400 U.S. city average item codes,
gives five disagreements:

| Item code | `cu.item` | `cu.series` | Difference |
|---|---|---|---|
| `SA0L5` | `All items less medical care` | `All items  less medical care` | double space |
| `SEAA02` | `...swimwear and accessories` | `...swimwear, and accessories` | serial comma |
| `SEAC04` | `...swimwear and accessories` | `...swimwear, and accessories` | serial comma |
| `SEFT05` | `Baby food and formula` | `Baby Food and Formula` | capitalization |
| `SS62052` | `Photo Processing` | `Photo processing` | capitalization |

Note that the last two disagree in opposite directions. Whatever produced these
files, it was not one style rule applied inconsistently. It was two.

The news release is a third surface and splits its vote: it uses the `cu.item`
spelling for `All items less medical care` and `Baby food and formula`, and the
`cu.series` spelling for both underwear items. No file is authoritative.

### Reproduction

```r
library(tidyverse)

# BLS requires a contact email in the user agent
options(HTTPUserAgent = "R CPI research (you@example.com)")
rd <- function(f) readr::read_tsv(paste0("https://download.bls.gov/pub/time.series/cu/", f),
                                  col_types = readr::cols(.default = readr::col_character()))

items <- rd("cu.item") %>%
  transmute(item_code = str_trim(item_code), from_item = str_trim(item_name))

series <- rd("cu.series") %>%
  filter(str_trim(area_code) == "0000") %>%
  transmute(item_code = str_trim(item_code),
            from_series = str_match(series_title, "^(.*?) in U\\.S\\. city average")[, 2]) %>%
  distinct()

inner_join(items, series, by = "item_code") %>%
  filter(from_item != from_series)
```

Returns five rows.

### Why it matters, and why it does not

It does not matter for the price data. The indexes are identical whichever file
you read the label from, and nothing about measurement is affected.

It matters if you join on names. Our old weights pipeline did exactly that:
`left_join(cpi_data, weights, by = "item_name")`, against a hand-maintained CSV.
A name join fails silently. You get `NA` in a weight column, the affected item
drops out of a weighted aggregate, and the total still looks plausible because
the item was 0.03 percent of the basket. Nothing errors. Nothing warns.

Two items at 0.03 and 0.06 percent of the CPI is a rounding error. But the
failure mode generalizes, and BLS renames items for real from time to time,
which is the same bug with a bigger coefficient.

### The fix

Join on `item_code`. Codes are stable across files, across surveys, and across
renames. Names are prose, and prose drifts.

Our pipeline does this now. `01c_replicate_bls_tables.R` still joins on name,
because its whole job is to compare against a scraped HTML table that has no
codes in it, so it normalizes the serial comma before joining and reports
anything still unmatched. As of the June 2026 release that count is zero.

---

## Part 2: The blog post version

### A comma walks into the Consumer Price Index

I have been rebuilding how I pull CPI weights, and I wrote a script whose only
job is to check my numbers against the tables BLS actually publishes. Every
column matched. Relative importance, percent changes, contributions, standard
errors, hundreds of line items, exact to the last decimal.

Except two rows would not match at all, and both of them were underwear.

Here is `SEAA02`, the item code for men's underwear, nightwear, swimwear and
accessories, as spelled in `cu.item`, the file whose entire purpose is to tell
you what items are called:

> Men's underwear, nightwear, swimwear and accessories

And here it is in `cu.series`, which sits in the same directory, downloads from
the same server, and refers to the same item code:

> Men's underwear, nightwear, swimwear, and accessories

One comma. The Oxford comma, specifically, the one that has been litigated in
dairy-driver overtime cases and will apparently now be litigated in apparel
price indexes.

I went looking for company and found four more. `Baby food and formula` is
capitalized in one file and not the other. `Photo Processing` is also
capitalized in one file and not the other, in the opposite direction. And `All
items less medical care` has two spaces in the middle of it in `cu.series`,
which is the sort of thing that happens when a string gets assembled by
concatenation and nobody looks at it again for twenty years.

Five out of four hundred. BLS's own news release, a third version of the same
names, agrees with the first file twice and the second file twice.

Now, the honest part. None of this touches a single price. The index values are
the same no matter which file you read the label off of, and if you take one
thing from this post it should not be "BLS data is sloppy." The measurement is
fine. What is frayed is the plumbing around it: the lookup tables, the
documentation, the metadata that tells you how to assemble the thing.

That distinction matters more than usual right now. There is a real and
well-funded argument going around that federal statistics cannot be trusted, and
"an economist found a typo" is exactly the kind of thing that gets flattened into
that argument by people who did not read past the headline. So: this is a typo.
It is a typo in a filename-adjacent metadata field. The CPI is fine.

But there is a real lesson, and it is the reason I am writing this up rather than
just fixing it and moving on.

**Join on codes, not names.** My old pipeline matched CPI weights to CPI data on
the item's name, against a spreadsheet I maintained by hand. If I had kept doing
that, these two items would have silently gotten no weight at all. Not an error.
Not a warning. Just an `NA` quietly propagating into a weighted average, dropping
0.09 percent of the consumption basket on the floor, and producing a number that
looked entirely reasonable.

That is the thing about name joins. They do not fail loudly, they fail
invisibly, and they fail on exactly the rows where somebody's style guide
changed. Codes do not have style guides.

I found this one because I wrote a script that compares my output to BLS's
published output and prints everything it cannot reconcile. That script is now
the most valuable file in the repository, and it has never once found a problem
with BLS's arithmetic. It found a comma.

I will take it.

*If you want to check my work: the reproduction code is in Part 1 above. Files
read 2026-08-11, against the June 2026 CPI release.*
