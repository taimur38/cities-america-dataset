library(tidyverse)
library(fredr)

apikey <- "423518c0b80952381dda1a2367425540"

fredr_set_key(apikey)

natl_sector_ids <- tribble(
                           ~series_id, ~title,
                           "PAYNSA", "Total Nonfarm",
                           "CEU1000000001", "Mining and Logging",
                           "CEU2000000001", "Construction", 
                           "CEU3000000001", "Manufacturing",
                           "CEU4000000001", "Trade, Transportation, and Utilities",
                           "CEU4142000001", "Wholesale Trade",
                           "CEU4200000001", "Retail Trade",
                           "CEU4300000001", "Transportation and Warehousing",
                           "CEU5000000001", "Information",
                           "CEU5500000001", "Financial Activities",
                           "CEU6000000001", "Professional and Business Services",
                           "CEU6500000001", "Education and Health Services", # technicall Private Education and Health Services
                           "CEU7000000001", "Leisure and Hospitality",
                           # missing 'Other Services
                           # need to create this from
                           "CEU7072200001", "Food Services and Drinking Places",
                           "TEMPHELPN", "Temporary Help Services",
                           ## end 

                           "CEU9000000001", "Government", 
                           "CEU9091000001", "Federal Government",
                        )

# so sometimes they dont put transportation and warehousing together! 
# so we get trade,transportation and utitilies ; wholesale, retail ; no warehousing ;
# this comes from here: https://www.bls.gov/sae/additional-resources/naics-supersectors-for-ces-program.htm
sectors <- tribble(
                   ~code, ~title,
                   "00", "Total Nonfarm",
                   "15", "Mining, Logging and Construction",
                   "30", "Manufacturing",
                   "40", "Trade, Transportation, and Utilities",
                   "41", "Wholesale Trade",
                   "42", "Retail Trade",
                   "43", "Transportation and Warehousing",
                   "50", "Information",
                   "55", "Financial Activities",
                   "60", "Professional and Business Services",
                   "65", "Education and Health Services",
                   "70", "Leisure and Hospitality",
                   "80", "Other Services",
                   "90", "Government"
)

# go through all natl sector ids, grab the data, group them into annual data, combine sectors for services and mining logging and concstruction

all_natl_results <- NULL

for(i in 1:nrow(natl_sector_ids)) {
    sector <- natl_sector_ids[i, ]
    print(sector)
    code <- sector$series_id
    title <- sector$title
    series_id <- code

    res <- fredr::fredr_series_observations(series_id = series_id, observation_start = as.Date("1990-01-01"))

    if(nrow(res) == 0) {
        print(paste("No data for", title, "with series id", series_id))
        next
    }

    res <- res |>
        mutate(title = title) 

    all_natl_results <- rbind(all_natl_results, res)

}

annual_natl_results <- all_natl_results |>
    mutate(
           year = year(date),
    ) |>
    group_by(year, title) |>
    summarise(
              value = mean(value, na.rm=T)
    ) 

# we need to create a few combined series
merged_annual_natl_results <- all_natl_results |>
    mutate(
           year = year(date),
           title = case_when(
               title == "Mining and Logging" ~ "Mining, Logging and Construction",
               title == "Construction" ~ "Mining, Logging and Construction",
               title == "Temporary Help Services" ~ "Other Services",
               TRUE ~ title
           )
    ) |>
    group_by(year, title) |>
    summarise(value = mean(value, na.rm=T)) 

merged_annual_natl_results |>
    write_parquet('data/national_sector_employment.parquet')

merged_annual_natl_results <-
    read_parquet('data/national_sector_employment.parquet')


#msa_id <- "32820"  # memphis
#code <- "30"
fetch_city_dat <- function(msa_id) {

    results <- NULL
    # loop though rows of sectors
    for(i in 1:nrow(sectors)) {
        sector <- sectors[i, ]
        print(sector)
        code <- sector$code
        title <- sector$title
        series_id <- paste0("SMU47", msa_id, code, "0000000", "1A")
        print(series_id)

        res <- fredr::fredr_series_observations(series_id = series_id, observation_start = as.Date("1990-01-01"))

        results <- rbind(results, res |> 
                         mutate(sector = title))

    }

    results
}

fredr_series_search_text("Boston", tag_names="sae;msa;bls;annual;employment") 
fredr::fredr_series_tags(series_id="SMU25144603000000001")

cityname <- "Boston"
fetch_city_dat_generic <- function(cityname) {

    search_results <- fredr_series_search_text(cityname, tag_names="sae;msa;bls;annual;employment") 

    if(nrow(search_results) == 0) {
        print(paste("No search results for city", cityname))
        return(NULL)
    }

    search_results <- search_results |>
        filter(units == "Thousands of Persons") |>
        # remove anything that has a more than one colon 
        filter(str_count(title, ":") <= 1) |> 
        # make sure it starts with All Employees:
        filter(str_starts(title, "All Employees:"))  |>
        # make sure DISCONTINUIED is not in the title
        filter(!grepl("DISCONTINUED", title, ignore.case=TRUE)) 

    # create a map of sector to series id 
    # based on text matches
    results <- NULL 
    for(i in 1:nrow(sectors)) {
        sector <- sectors[i, ]
        sector_title <- sector$title

        # remove commas and 'and', replace with * so that the regex works
        sector_title_fuzzy <- sector_title |>
            str_replace_all(",", " ") |>
            str_replace_all(" and ", " ") |>
            str_squish() |>
            str_replace_all(" ", ".*")

        matched_series <- search_results |>
            filter(grepl(sector_title_fuzzy, title, ignore.case=TRUE))

        if(nrow(matched_series) == 0) {
            print(paste("No match for sector", sector_title, 'in city', cityname))
            # print(search_results$title) 
            # print(sector_title_fuzzy)

            next
        } else if(nrow(matched_series) > 1) {

            # so the actual sector name is between All Employees: and in 
            # so lets that first 
            if(sector_title == "Government") {
                matched_series <- matched_series |>
                    mutate(
                        mini_title = str_remove_all(title, "All Employees:"),
                        mini_title = str_remove_all(mini_title, "in .*"),
                        mini_title  = str_squish(mini_title)
                    ) |> 
                    filter(mini_title == sector_title)
            }
            else {
                print(paste("Multiple matches for sector", sector_title, "taking first"))
                print(matched_series$title)
            }
        }

        series_id <- matched_series$id[1]
        print(series_id)

        res <- fredr::fredr_series_observations(series_id = series_id, observation_start = as.Date("1990-01-01"))

        results <- rbind(results, res |> 
                         mutate(
                                sector = sector_title,
                                full_sector_title = matched_series$title[1]

                        ))
    }

    results

}

chicaggo_dat <- fetch_city_dat_generic("Chicago")
la_dat <- fetch_city_dat_generic("Los Angeles")

memphis_dat_test2 <- fetch_city_dat_generic("Memphis")
nyc_dat_test2 <- fetch_city_dat_generic("New York City")
san_antonio_dat_test2 <- fetch_city_dat_generic("San Antonio")

boston_dat <- fetch_city_dat_generic("Boston")


msa_names
# can we do it for all MSAs.. 

msa_names <- mega_dataset |>
    select(GeoFIPS, GeoName) |>
    unique() |>
    mutate(
        short_name = str_remove_all(GeoName, "\\(Metropolitan Statistical Area\\)"),
    )

msa_names

all_msa_data <- NULL

for(i in 1:nrow(msa_names)) {
    msa <- msa_names[i, ]
    print(msa)
    cityname <- msa$short_name
    msa_id <- msa$GeoFIPS
    res <- fetch_city_dat_generic(cityname) 

    if(is.null(res)) {
        next
    }

    res <- res |>
        mutate(
               city = cityname, 
               msa_id = msa_id
        )

    all_msa_data <- rbind(all_msa_data, res)
    # pause for a second to avoid hitting rate limits
    Sys.sleep(10)
}

all_msa_data |>
    write_parquet('data/all_msa_sector_employment_v1.parquet')

# resume loop
next_range <- i:nrow(msa_names)
msa_names[i, ]

for(i in next_range) {
    msa <- msa_names[i, ]
    print(msa)
    cityname <- msa$short_name
    msa_id <- msa$GeoFIPS
    res <- fetch_city_dat_generic(cityname) 

    if(is.null(res)) {
        next
    }

    res <- res |>
        mutate(
               city = cityname, 
               msa_id = msa_id
        )


    all_msa_data <- rbind(all_msa_data, res)
    # pause for a second to avoid hitting rate limits
    Sys.sleep(10)
}

# tag = sae, group_id = rls
# groupd = geot, name = msa
# src = bls
fredr::fredr_series_tags(series_id=series_id)

fredr_tags_series("sae;msa;bls;annual")

fredr_series_search_text("New York City", tag_names="sae;msa;bls;annual;employment") |>
    select(id, title, units) |>
    filter(units == "Thousands of Persons") |>
    distinct()

fredr_tags_series("sae;msa;bls;annual")


memphis_msa <- "32820"

memphis_dat_test <- fetch_city_dat(memphis_msa)


memphis_merged <- memphis_dat_test |>
    mutate(year = year(date)) |>
    left_join(merged_annual_natl_results %>%
              rename(natl_emps = value), by = c("year", "sector" = "title"))

memphis_merged |>
    filter(sector != "Total Nonfarm") |>
    #filter(!sector %in% c("Retail Trade", "Wholesale Trade")) |>
    filter(sector != "Trade, Transportation, and Utilities") |>
    mutate(
           market_share = value / natl_emps,
    ) |>
    filter(sector != "Other Services") |>
    #filter(year %in% c(1990, 2000, 2010, 2020)) |>
    filter(year %in% seq(1990, 2020, by=10)) |>
    arrange(year) |>
    ggplot(aes(x=value, y=market_share, color=sector, label=str_wrap(sector, 15))) +
    geom_path(arrow = arrow(length = unit(0.2, "cm"))) +
    geom_point(data = . %>% filter(year != 2020), pch=21, fill='white') +
    geom_text_repel(data = . %>% filter(year == 2020)) +
    theme(legend.position = 'none') +
    scale_y_continuous(labels = scales::percent) +
    labs(
         title = "Memphis Sectoral Employment from 1990 to 2020",
         y = "Share of National Employment",
         x = "Employment in Memphis (Thousands)",
         caption = "Source: FRED, Bureau of Labor Statistics"
    )

ggsave('imgs/memphis-labor-market-share.png', width=9, height=6)

write_parquet(memphis_merged, 'data/memphis_sector_employment.parquet')


# how many jobs did memphis gain / lose overall due to national effect, sector effect, local effect


san_antonio_msa <- "41700"

san_antonio_dat_test <- fetch_city_dat(san_antonio_msa)

fetch_city_dat2 <- function(msa_id) {

    results <- NULL
    # loop though rows of sectors
    for(i in 1:nrow(sectors)) {
        sector <- sectors[i, ]
        print(sector)
        code <- sector$code
        title <- sector$title
        series_id <- paste0("SMU48", msa_id, code, "0000000", "1A")
        print(series_id)

        res <- fredr::fredr_series_observations(series_id = series_id, observation_start = as.Date("1990-01-01"))

        results <- rbind(results, res |> 
                         mutate(sector = title))

    }

    results
}

san_antonio_dat_test <- fetch_city_dat2(san_antonio_msa)

san_antonio_dat_test |>
    filter(grepl("Mining", sector))

san_antonio_merged <- san_antonio_dat_test |>
    mutate(year = year(date)) |>
    left_join(merged_annual_natl_results %>%
              rename(natl_emps = value), by = c("year", "sector" = "title"))

write_parquet(san_antonio_merged, 'data/san_antonio_sector_employment.parquet')

san_antonio_merged |>
    filter(sector != "Total Nonfarm") |>
    #filter(!sector %in% c("Retail Trade", "Wholesale Trade")) |>
    filter(sector != "Trade, Transportation, and Utilities") |>
    mutate(
           market_share = value / natl_emps,
    ) |>
    filter(sector != "Other Services") |>
    #filter(year %in% c(1990, 2000, 2010, 2020)) |>
    filter(year %in% seq(1990, 2020, by=10)) |>
    arrange(year) |>
    ggplot(aes(x=value, y=market_share, color=sector, label=str_wrap(sector, 15))) +
    geom_path(arrow = arrow(length = unit(0.2, "cm"))) +
    geom_point(data = . %>% filter(year != 2020), pch=21, fill='white') +
    geom_text_repel(data = . %>% filter(year == 2020)) +
    theme(legend.position = 'none') +
    scale_y_continuous(labels = scales::percent) +
    labs(
         title = "San Antonio Sectoral Employment from 1990 to 2020",
         y = "Share of National Employment",
         x = "Employment in San Antonio (Thousands)",
         caption = "Source: FRED, Bureau of Labor Statistics"
    )

ggsave('imgs/san-antonio-labor-market-share.png', width=9, height=6)


# new york is a totally different system for some reason
# they have a code like NEWY636FIREN


# ok so lets build a 2 sector database of all the MSAs using this fredr stuff
# already we know it wont work because nyc is totally different code
# so we need a new appraoch
# once we get the dataset we can do the shift share stuff everywhere and put that into a =



msa_names

merged_annual_natl_results

nyc_merged <- all_msa_data |>
    filter(msa_id == 35620) |>
    mutate(year = year(date)) |>
    left_join(merged_annual_natl_results %>%
              rename(natl_emps = value), by = c("year", "sector" = "title"))

    library(ggrepel)

nyc_merged |>
    filter(sector != "Total Nonfarm") |>
    #filter(!sector %in% c("Retail Trade", "Wholesale Trade")) |>
    filter(sector != "Trade, Transportation, and Utilities") |>
    mutate(
           market_share = value / natl_emps,
    ) |>
    filter(sector != "Other Services") |>
    #filter(year %in% c(1990, 2000, 2010, 2020)) |>
    filter(year %in% seq(1990, 2020, by=10)) |>
    arrange(year) |>
    ggplot(aes(x=value, y=market_share, color=sector, label=str_wrap(sector, 15))) +
    geom_path(arrow = arrow(length = unit(0.2, "cm"))) +
    geom_point(data = . %>% filter(year != 2020), pch=21, fill='white') +
    geom_text_repel(data = . %>% filter(year == 2020)) +
    theme(legend.position = 'none') +
    scale_y_continuous(labels = scales::percent) +
    labs(
         title = "New York Sectoral Employment from 1990 to 2020",
         y = "Share of National Employment",
         x = "Employment in New York (Thousands)",
         caption = "Source: FRED, Bureau of Labor Statistics"
    )
