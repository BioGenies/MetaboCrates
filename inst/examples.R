# adding data

path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
attributes(dat)

dat <- add_group(dat, "group")
attributes(dat)

na_info <- attr(dat, "NA_info")
metabolites_to_remove <- get_LOD_to_remove(na_info, 0.8)

dat <- remove_metabolites(dat, metabolites_to_remove, type = "LOD")
attributes(dat)

dat <- unremove_metabolites(dat, type = "LOD")
attributes(dat)

dat <- complete_data(dat, LOD_method = "limit", LLOQ_method = "limit")


dat <- calculate_CV(dat)
attributes(dat)

