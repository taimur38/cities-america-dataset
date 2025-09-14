# this s a file that can be sourced to load utility functions, and the usual datasets.


library(tidyverse)
library(readxl)
library(arrow)

# first we want our mega dataset 

mega_dataset <- read_parquet('data/mega_dataset2.parquet') |>
    mutate(
           short_name = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           short_name = str_extract(short_name, "^[^,]+"),
           short_name = str_extract(short_name, "^[^-]+"),

           real_mean_wage = mean_wage / (`RPPs: All items` / 100),

           real_mean_wage_housing = mean_wage / (`RPPs: Services: Housing` / 100),
           )

# then ACS
acs <- open_dataset('data/shared-data/ipums/acs-cities.parquet')

# the usual scatter df
scatter_df <- mega_dataset |>
    mutate(
           nominal_pc_income = mean_wage,

           fuzzy_wage = ifelse(is.na(mean_wage), `Real per capita personal income (constant (2017) dollars) 2/` * (`RPPs: All items`/100), mean_wage),

    ) |>
    group_by(GeoFIPS) |>
    mutate(
           net_migrant_rate = net_migrants_2010s / implied_population[year == 2010],

        net_migrant_cagr = ((implied_population[year == 2010] + net_migrants_2010s) / implied_population[year == 2010])^(1/10) - 1,

           population_cagr = (implied_population[year == 2020] / implied_population[year == 2010])^(1/10) - 1,

           nominal_cagr = (nominal_pc_income[year == 2020] / nominal_pc_income[year == 2010])^(1/10) - 1,

           fuzzy_cagr = (fuzzy_wage[year == 2020] / fuzzy_wage[year == 2010])^(1/10) - 1,

           real_wage_cagr = (real_mean_wage[year == 2020] / real_mean_wage[year == 2010])^(1/10) - 1,
    ) |>
    ungroup() |>
    filter(year == 2020)
    #filter(year %in% c(2010, 2020)) 

avg_nom_cagr = median(scatter_df$nominal_cagr, na.rm=T)
avg_net_migrant_rate = median(scatter_df$net_migrant_rate, na.rm=T)
avg_net_migrant_cagr = median(scatter_df$net_migrant_cagr, na.rm=T)
avg_pop_cagr = median(scatter_df$population_cagr, na.rm=T)

gen_city_patches <- function(cname) {

    full_name <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(GeoName) |>
        first() |>
        pull()

    scatter1 <- mega_dataset |>
        group_by(GeoFIPS) |>
        mutate(
            real_wage = `Real per capita personal income (constant (2017) dollars) 2/`,
                #A_MEDIAN / (`RPPs: All items` / 100),
            real_mean_wage = mean_wage / (`RPPs: All items` / 100),
            migrant_rate = net_migrants_2010s / implied_population[year == 2010],
            pop_cagr_2010 = (implied_population / implied_population[year == 2010])^(1/10) - 1,
        ) |>
        ungroup() |>
        filter(year == 2020) |>
        arrange(desc(implied_population)) |>
        #head(50) |>
        filter(migrant_rate < 0.4) |>
        ggplot(aes(y=real_mean_wage, x=migrant_rate, label=short_name)) +
        geom_vline(aes(xintercept = median(migrant_rate, na.rm=T)), linetype='dashed') + 
        geom_hline(aes(yintercept = median(real_mean_wage, na.rm=T)), linetype='dashed') + 
        geom_point(alpha = 0.5) +
        geom_point(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)), color='red', size=3) + 
        # geom_label_repel(data = . %>% filter(grepl(cname, GeoName, ignore.case = T))) +
        scale_y_continuous(labels = scales::dollar_format()) +
        scale_x_continuous(labels = scales::percent_format()) +
        labs(
            title = "Real Wage vs Net Migration Rate",
            subtitle = "2010-2020",
            x = "Net Migration Rate",
            y = "Real Wage",
        )

    scatter2 <- scatter_df |>
        filter(net_migrant_rate < 0.4) |>
        ggplot(aes(x=net_migrant_rate, y=nominal_cagr, label=short_name)) +
        geom_hline(yintercept = avg_nom_cagr, linetype='dashed') +
        geom_vline(xintercept = avg_net_migrant_rate, linetype='dashed') +
        geom_point(alpha = 0.5) +
        geom_point(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)), color='red', size=3) +
        scale_x_continuous(labels = scales::percent_format()) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
            x = "Net Migration Rate (2010-2020)",
            y = "Average Wage Nominal CAGR",
            title = "Changes in Nominal Income vs Net Migration",
            subtitle = "2010-2020",
        ) 

        # first lets plot the implied_population
        # we keep a v-line for covid in 2019
        pop_chart <- mega_dataset |>
            filter(grepl(cname, GeoName, ignore.case = T)) |>
            select(year, 
                implied_population
            ) |>
            ggplot(aes(x=year, y=implied_population)) +
            geom_line() +
            geom_point() + 
            geom_vline(xintercept = 2020, linetype='dashed') +
            scale_y_log10(labels = scales::comma_format()) +
            labs(
                title = "Population",
                x = "Year",
                y = ""
            )


        pop_growths_df <- mega_dataset |>
            group_by(GeoFIPS, GeoName) |>
            arrange(year) |>
            mutate(
                pop_growth = (implied_population - lag(implied_population)) / lag(implied_population),
            ) |>
ungroup() |>
            group_by(year) |>
            mutate(
                mdn_pop_growth = median(pop_growth, na.rm=T),
            ) |>
            ungroup() 

        yrange <- pop_growths_df |>
            group_by(year) |>
            summarise(
                      tenth = quantile(pop_growth, 0.01, na.rm=T),
                      nintyeth = quantile(pop_growth, 0.99, na.rm=T),
            ) |>
            summarise(
                      mt = min(tenth, na.rm=T),
                      mn = max(nintyeth, na.rm=T),
            ) |>
            pull(mt, mn)


        ann_pop_growths <- pop_growths_df |>
            ggplot(aes(x=year, y=pop_growth)) +
            geom_hline(yintercept = 0, linetype='dashed') +
            # geom_line(aes(y=mdn_pop_growth), color='red', linetype='dashed') +
            # geom_point(aes(y=mdn_pop_growth), color='red') +
            geom_boxplot(aes(group=year), outlier.shape = NA) +
            geom_line(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)) ) +
            geom_point(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)) ) +
            scale_y_continuous(
                               labels = scales::percent_format(),
                               limits = quantile(pop_growths_df$pop_growth, c(0.001, 0.999), na.rm=T),
            ) +
            labs(
                title = "YoY Population Growth vs Median MSA",
                x = "Year",
                y = ""
            )

    # then we look at real per-capita income 
    income_chart <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(year, real_mean_wage) |>
        ggplot(aes(x=year, y=real_mean_wage)) +
        geom_line() +
        geom_point() + 
        geom_vline(xintercept = 2020, linetype='dashed') +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(
             title = "Real Average Wage",
             x = "Year",
             y = ""
        )

        # do a YoY Per Capita Income Chart 

        inc_growths_df <- mega_dataset |>
            group_by(GeoFIPS, GeoName) |>
            arrange(year) |>
            mutate(
                pop_growth = (implied_population - lag(implied_population)) / lag(implied_population),
                #inc_growth = (`Real per capita personal income (constant (2017) dollars) 2/` - lag(`Real per capita personal income (constant (2017) dollars) 2/`)) / lag(`Real per capita personal income (constant (2017) dollars) 2/`),
                inc_growth = (real_mean_wage - lag(real_mean_wage)) / lag(real_mean_wage),

                nom_inc_growth = (mean_wage - lag(mean_wage)) / lag(mean_wage),
            ) |>
            ungroup() |>
            group_by(year) |>
            mutate(
                mdn_pop_growth = median(pop_growth, na.rm=T),
                mdn_inc_growth = median(inc_growth, na.rm=T),
            ) |>
            ungroup() 

    yoy_pcinc_chart <- inc_growths_df |>
        ggplot(aes(x=year, y=inc_growth)) +
        geom_hline(yintercept = 0, linetype='dashed') +
        # geom_line(aes(y=mdn_inc_growth), color='red', linetype='dashed') 
        geom_boxplot(aes(group=year), outlier.shape = NA) +
        geom_line(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)) ) +
        geom_point(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)) ) +
        scale_y_continuous(
                           labels = scales::percent_format(),
                           limits = quantile(inc_growths_df$inc_growth, c(0.001, 0.999), na.rm=T),
        ) +
        labs(
            title = "YoY Real Average Wage Growth vs Median MSA",
            x = "Year",
            y = ""
        )

    yoy_nom_wage_chart <- inc_growths_df |>
        ggplot(aes(x=year, y=nom_inc_growth)) +
        geom_hline(yintercept = 0, linetype='dashed') +
        # geom_line(aes(y=mdn_inc_growth), color='red', linetype='dashed') 
        geom_boxplot(aes(group=year), outlier.shape = NA) +
        geom_line(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)) ) +
        geom_point(data = . %>% filter(grepl(cname, GeoName, ignore.case = T)) ) +
        scale_y_continuous(
                           labels = scales::percent_format(),
                           limits = quantile(inc_growths_df$nom_inc_growth, c(0.001, 0.999), na.rm=T),
        ) +
        labs(
            title = "YoY Nominal Average Wage Growth vs Median MSA",
            x = "Year",
            y = ""
        )


        total_income_chart <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(year, 
               `Real personal income (millions of constant (2017) dollars)`, 
        ) |>
        ggplot(aes(x=year, y=`Real personal income (millions of constant (2017) dollars)`)) +
        geom_line() +
        geom_point() +
        geom_vline(xintercept = 2020, linetype='dashed') +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(
             title = "Total Real Income",
             x = "Year",
             y = ""
        )

    wage_dist_chart <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(year, median_wage, wage_10th, wage_25th, wage_75th, wage_90th) |>
        ggplot(aes(x=year)) +
        geom_segment(aes(x=year, xend=year, y=wage_10th, yend=wage_90th)) +
        #geom_linerange(aes(ymin=A_PCT25, ymax=A_PCT75)) +
        geom_rect(aes(ymin=wage_25th, ymax=wage_75th, xmin=year-0.25, xmax=year+0.25), fill='white', color='black') +
        #geom_point(aes(y=A_MEDIAN)) +
        geom_segment(aes(x=year-0.25, xend=year+0.25, y=median_wage, yend=median_wage)) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(
             title = "Wage Distribution",
             x = "Year",
             y = ""
        )

    # lets plot the RPPs
    rpp_overall <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(year, 
               `RPPs: All items`
        ) |>
        ggplot(aes(x=year, y=`RPPs: All items`)) +
        geom_line() +
        geom_point() + 
        geom_vline(xintercept = 2020, linetype='dashed') +
        labs(
             title = "RPPs: All Items",
             x = "Year",
             y = ""
        )

        # now RPPs for Housing

    rpp_housing <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(year, 
               `RPPs: Services: Housing`
        ) |>
        ggplot(aes(x=year, y=`RPPs: Services: Housing`)) +
        geom_line() +
        geom_point() + 
        geom_vline(xintercept = 2020, linetype='dashed') +
        labs(
             title = "RPPs: Housing",
             x = "Year",
             y = ""
        )

    # what's a chart that would show whether population is responding to housing? you could show the growth of population vs the previous year
    # against the RPPs for housing

    pop_growth_yearly <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        arrange(year) |>
        mutate(
               pop_growth = (implied_population - lag(implied_population)) / lag(implied_population),
        ) |>
        ggplot(aes(x=`RPPs: Services: Housing`, y=pop_growth)) +
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_path(arrow=arrow(length=unit(0.2, "inches"), type="closed", ends="last")) +
        geom_point(aes(label=year)) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
             title = "Population Growth vs Cost of Living",
             y = "Change in Population",
             x = "Cost of Housing, relative to National Average",
        )

    # lets do pop_growth_yearly and the bottom 10pct income as 
    pop_growth_bottom <- mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        arrange(year) |>
        mutate(
               pop_growth = (implied_population - lag(implied_population)) / lag(implied_population),
               real_pct10 = wage_10th / (`RPPs: All items` / 100),
        ) |>
        ggplot(aes(x=real_pct10, y=pop_growth)) +
        geom_hline(yintercept = 0, linetype='dashed') +
        geom_path(arrow=arrow(length=unit(0.2, "inches"), type="closed", ends="last")) +
        geom_point(aes(label=year)) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
             title = "Population Growth vs Wages of Bottom 10%",
             y = "Change in Population",
             x = "Real Income of 10th Percentile of Residents",
        )


    # put them together in patchwork and return
    # lay out as a grid
    p <- scatter1 + scatter2 +
        pop_chart + ann_pop_growths +
        #total_income_chart + 
        income_chart + yoy_nom_wage_chart +
        #yoy_pcinc_chart +
        wage_dist_chart + rpp_housing +
        #yoy_pcinc_chart + wage_dist_chart +
        #rpp_overall + 
        #pop_growth_yearly + pop_growth_bottom +
        plot_layout(
            ncol = 2,
            guides = "collect",
        ) +
        plot_annotation(
            title = paste0(full_name, ": Summary Statistics"),
            caption = "Source: BEA, OES, US Census"
        )

    p
    ggsave(paste0("imgs/", cname, "_summary.png"), width=12, height=12)

    p
}


# FREDR SECTION 

library(fredr)

fredr_apikey <- "423518c0b80952381dda1a2367425540"

fredr_set_key(fredr_apikey)

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

natl_emp_data <- read_parquet('data/national_sector_employment.parquet')
all_msa_data <- read_parquet('data/all_msa_sector_employment_v1.parquet')

fetch_city_dat <- function(cityname) {

    search_results <- fredr_series_search_text(cityname, tag_names="sae;msa;bls;annual;employment") |>
        filter(units == "Thousands of Persons") |>
        # remove anything that has a more than one colon 
        filter(str_count(title, ":") <= 1) 

    # create a map of sector to series id 
    # based on text matches
    results <- NULL 
    for(i in 1:nrow(sectors)) {
        sector <- sectors[i, ]
        print(sector)
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
            print(paste("No match for sector", title))
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
                         mutate(sector = sector_title))
    }

    results

}

create_shift_chart <- function(cityname) {

    city_merged <- all_msa_data |>
        filter(grepl(cityname, city, ignore.case = T)) |>
        mutate(year = year(date)) |>
        left_join(merged_annual_natl_results %>%
                rename(natl_emps = value), by = c("year", "sector" = "title"))

    city_name_full <- city_merged$city[1]
    #city_name_short <- city_merged$short_name[1]

    city_merged |>
        filter(sector != "Total Nonfarm") |>
        #filter(!sector %in% c("Retail Trade", "Wholesale Trade")) |>
        filter(sector != "Trade, Transportation, and Utilities") |>
        mutate(
            market_share = value / natl_emps,
        ) |>
        filter(sector != "Other Services") |>
        filter(year %in% seq(1990, 2020, by=10)) |>
        arrange(year) |>
        ggplot(aes(x=value, y=market_share, color=sector, label=str_wrap(sector, 15))) +
        geom_path(arrow = arrow(length = unit(0.2, "cm"))) +
        geom_point(data = . %>% filter(year != 2020), pch=21, fill='white') +
        geom_text_repel(data = . %>% filter(year == 2020)) +
        theme(legend.position = 'none') +
        scale_y_continuous(labels = scales::percent) +
        labs(
            title = paste(city_name_full, "Sectoral Employment from 1990 to 2020"),
            y = "Share of National Employment",
            x = paste("Employment in", city_name_full, "(Thousands"),
            caption = "Source: FRED, Bureau of Labor Statistics"
        )

}
