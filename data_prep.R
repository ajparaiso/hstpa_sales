# Header ----------------------------------------------------------------------

# GitHub/rent-stab-explore/sales_map

# PROJECT: Rent Reform Evaluation
# AUTHOR: Andrew Paraiso
# DATE: 2022-10-12

# PURPOSE: Map sales tk

# DETAILS: tk


# Setup -----------------------------------------------------------------------

library(tidyverse)
library(DBI)
library(lubridate)
library(janitor)
library(glue)
library(fs)
library(sf)
library(leaflet)
library(fcthemes)

con <- furmanr::fcdb_connect()

# Load data -------------------------------------------------------------------

# load prime cd shapes
prime_cds <- c(108, 107, 106, 105, 104, 103, 102, 101, 401, 301, 302, 306)

prime_cd_geo <- read_sf(con, query = "SELECT cd,
                                       geom
                  FROM nyc_cds") %>%
  filter(as.numeric(cd) %in% prime_cds) %>%
  st_transform(4326) %>%
  st_as_sf() 

# load acs indicators 
acs_vars <- dbGetQuery(con, "SELECT fc_geoid,
                                    geoid_full,
                                    hh_inc_med,
                                    pop_race_white_pct,
                                    pop_race_black_pct
                       FROM acs_indicators_nyc AS acs
                       WHERE year = 2018
                        AND span = 5
                        AND fc_geo_type = 'tract'") 

# load tract geometry 
nyc_tracts <- read_sf(con, query = "SELECT tract_2010 AS fc_geoid,
                                           geom
                      FROM nyc_tracts_2010_clipped") %>%
  st_transform(4326) %>%
  st_as_sf()

# load sales data
sales_nyc_data <- dbGetQuery(con, "WITH anti_join_df AS
                             (SELECT DISTINCT j.bbl 
                              FROM fc_sandbox.shd_property_june2022 AS j)
                             SELECT *
                             FROM fc_sandbox.rre_sales_extended_thr_22q2 AS sales_nyc
                             LEFT JOIN anti_join_df AS bbls_drop
                              ON sales_nyc.bbl = bbls_drop.bbl
                             WHERE bbls_drop.bbl IS NULL
                              AND sales_nyc.year_built < 1974
                              AND sales_nyc.res_units > 5
                              AND sales_nyc.deed_date BETWEEN '2014/01/01' AND '2022/06/30'
                              AND sales_nyc.deed_date NOT BETWEEN '2020/04/01' AND '2021/03/31'") 

# load sales geomtery
bbls_geo <- read_sf(con, query = "SELECT bbl,
                                         geom
                    FROM rre_bbls_new") %>%
  st_transform(4326) %>%
  st_as_sf()


# Prep data -------------------------------------------------------------------

# merge acs and geometry data 
acs_vars_geo <- acs_vars %>%
  left_join(nyc_tracts, by = "fc_geoid") %>%
  st_as_sf()

# find percentiles for sale price/unit filters 
ppu_low <- quantile(sales_nyc_data$rpricepu,0.01)[1]
ppu_hi <- quantile(sales_nyc_data$rpricepu,0.99)[1]

pct_trans <- 100

# clean sales data 
sales_clean <- sales_nyc_data %>%
  filter(rpricepu >= ppu_low, rpricepu <= ppu_hi) %>%
  filter(pct_transferred >= pct_trans | is.na(pct_transferred)) %>%
  mutate(
    rs_group_ariel = case_when(
      sale_stab_units_pct >= 0.0 &
        sale_stab_units_pct <= 0.25 ~ "0-25% RS",
      sale_stab_units_pct > 0.25 &
        sale_stab_units_pct <= 0.75 ~ "26-75% RS",
      sale_stab_units_pct > 0.75 &
        sale_stab_units_pct <= 1.0 ~ "76-100% RS"
    ),
    rs_group_ariel = fct_relevel(rs_group_ariel, c(
      "0-25% RS", "26-75% RS", "76-100% RS"
    )),
    sale_period = case_when(deed_date < '2019-06-14' ~ "Pre-HSTPA",
                            deed_date >= '2019-06-14' ~ "Post-HSTPA"),
    sale_period = fct_relevel(sale_period, c(
      "Pre-HSTPA", "Post-HSTPA"
    )),
    deed_year = lubridate::year(deed_date),
    deed_quarter = lubridate::quarter(deed_date),
    comarea = as.numeric(comarea),
    comarea_exist = ifelse(comarea > 0 , "Yes", "No"),
    comarea_exist = fct_relevel(comarea_exist, c("Yes", "No")),
    sale_numbldgs = as.numeric(numbldgs),
    sale_res_units = as.numeric(sale_res_units),
    sale_age = (deed_year - sale_year_built),
    sale_retail_sqft_pct = sale_retailarea / sale_bldgarea,
    prime_neighborhood = ifelse(cd %in% prime_cds, 1, 0)
  ) %>%
  mutate_if(is.integer, as.numeric) %>%
  select(
    bbl,
    tract_2010,
    sale_period,
    sale_stab_units_pct,
    rs_group_ariel,
    deed_year,
    deed_quarter,
    sale_lotarea,
    sale_bldgarea,
    comarea,
    comarea_exist,
    sale_numbldgs,
    rpricepsqft,
    prime_neighborhood
  ) %>%
  drop_na() 

# merge sales with spatial data
sales_geo <- bbls_geo %>%
  right_join(sales_clean, by = "bbl")


