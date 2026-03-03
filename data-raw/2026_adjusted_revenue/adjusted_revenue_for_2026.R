# Fix for the 2026 report since SPPVALUE has many missing values for the years 2020 - 2024
# Apply price per pound to landings of species with missing value
#  using Price per pound data from oracle
#
# NOTE: commercial data rds files not saved in repo.
# They will need to be copied from internal servers
#
# 1. Base pull comlandr using the create_commercial_data workflow found in the
# READ_SOE_Workflows repo
# 2. Apply price per pound
# 3. deflate price
# 4. use this to create comdat and bennet

channel <- dbutils::connect_to_database("server", "user")
## read in comdat pull. This data file was created from the SOE-Workflows repo
landings <- readRDS(here::here(
  "data-raw/2026_adjusted_revenue/commercial_bennet.rds"
))

lbstomt <- 0.000453592
# get price per pound from oracle
sql <- "select * from NEFSC_GARFO.CFDRS_AVG_PRICE_TST"
prices <- DBI::dbGetQuery(channel, sql)
#######################################################
# turns out not all months of each year have a price.
#######################################################
# get species itis
sql <- "select * from CAMS_GARFO.CFG_ITIS"
itiscodes <- DBI::dbGetQuery(channel, sql)

# create a table with itis number, name and price by month
# join itis names with price
main_table <- prices |>
  dplyr::left_join(itiscodes, by = c("VALID_ITIS" = "ITIS_TSN")) |>
  dplyr::as_tibble() |>
  dplyr::select(
    YEAR,
    MONTH_IN_YEAR,
    VALID_ITIS,
    AVERAGE_PRICE,
    ITIS_SCI_NAME,
    ITIS_NAME,
    DLR_NESPP3
  ) |>
  dplyr::rename(
    MONTH = MONTH_IN_YEAR,
    ITIS = VALID_ITIS,
    PRICEPP = AVERAGE_PRICE,
    SCI_NAME = ITIS_SCI_NAME,
    COMNAME = ITIS_NAME,
    NESPP3 = DLR_NESPP3
  ) |>
  dplyr::mutate(
    NESPP3 = as.numeric(NESPP3),
    MONTH = as.numeric(MONTH),
    ITIS = as.numeric(ITIS)
  )

# find itis codes with no nespp3
missing_nespp3 <- main_table |>
  dplyr::filter(is.na(NESPP3)) |>
  dplyr::distinct(ITIS)

# remove all na for nespp3 in the main species lookup table
main_table2 <- main_table |>
  dplyr::filter(!is.na(NESPP3))


# find species nespp3 codes with no value data post 2019
sp_no_value <- landings$comland |>
  dplyr::filter((YEAR > 2019) & (is.na(SPPVALUE))) |>
  dplyr::distinct(NESPP3) |>
  dplyr::pull()

# which species are these
sp_no_value

# see if all of these has an associated price
find_out <- dplyr::setdiff(sp_no_value, unique(main_table$NESPP3))

# which species dont have a price
find_out
# print the names of these species
to_add <- ecodata::species_groupings |>
  dplyr::select(COMNAME, ITISSPP, NESPP3) |>
  dplyr::distinct() |>
  dplyr::filter(NESPP3 %in% find_out)

# how much landings from these to_add species for which we have no price
## If not much then just ignore!!
landings$comland |>
  dplyr::filter((YEAR > 2019) & (is.na(SPPVALUE)) & (NESPP3 %in% find_out)) |>
  dplyr::group_by(YEAR, NESPP3) |>
  dplyr::summarise(landings = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(to_add, by = "NESPP3") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = YEAR, y = landings)) +
  ggplot2::facet_wrap(~COMNAME)

## So now lets assign SPPVALUE to these missing records
early_data <- landings$comland |>
  dplyr::filter(YEAR <= 2019)
# convert to price per metric tons by nespp3 so we can join with comlandr data pull
price_mt <- main_table |>
  dplyr::group_by(YEAR, MONTH, NESPP3) |>
  dplyr::summarise(pricepmt = mean(PRICEPP) / lbstomt, .groups = "drop")

######### Now we should deflate these prices to 2024 dollar values
# for the 2026 report

# to bring all prices to base year reference value
# deflation = base year reference / index(yr, month) of data point
deflateData <- readRDS(system.file(
  "extdata/fred/fred.rds",
  package = "comlandr"
))
reference <- deflateData |>
  dplyr::filter(YEAR == 2024, MONTH == 1) |>
  dplyr::pull(value)

#################################################################

# updated sppvalue using ssb price per pound
fix_data <- landings$comland |>
  dplyr::as_tibble() |>
  dplyr::filter(YEAR > 2019) |>
  dplyr::left_join(price_mt, by = c("YEAR", "MONTH", "NESPP3")) |>
  dplyr::left_join(deflateData, by = c("YEAR", "MONTH")) |>
  dplyr::mutate(
    newVALUE = dplyr::case_when(
      is.na(SPPVALUE) ~ SPPLIVMT * pricepmt * reference / value,
      .default = SPPVALUE
    )
  ) |>
  dplyr::select(-SPPVALUE, -pricepmt, -value) |>
  dplyr::rename(SPPVALUE = newVALUE)

# still some revenue not assigned due to missing monthly prices in the Oracle table
# Just removing these. Mostly little/winter/thorny skates
fix_data_omit <- fix_data |> dplyr::filter(!is.na(SPPVALUE))

# updated data
commercial_data <- list()
commercial_data$comland <- rbind(early_data, fix_data_omit)


# save it
# This data file is a replacement for the original commerical data pull and is used as the input for
# both the comdat and bennet workflows
saveRDS(
  commercial_data,
  here::here(
    "data-raw/2026_adjusted_revenue/commercial_bennet_sppvalue_fixed.rds"
  )
)

# now run this file though comdat and bennet workflows

######### Done
