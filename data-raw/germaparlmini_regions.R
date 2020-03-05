library(GermaNER)
library(polmineR)
library(magrittr)
library(cwbtools)
library(stringi)

am <- corpus("GERMAPARLMINI") %>%
  subset(speaker == "Angela Dorothea Merkel" & date == "2009-11-10")
am_ne <- germaner_get_named_entities(am)

germaparl_ne_regions <- conll_get_regions(am_ne)
germaparl_ne_regions[, "annotated" := stringi::stri_escape_unicode(germaparl_ne_regions[["annotated"]])]

save(germaparl_ne_regions, file = "~/Lab/github/GermaNER/data/regions.RData")
