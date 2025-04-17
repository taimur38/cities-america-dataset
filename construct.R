library(tidyverse)
library(ggthemes)
library(ggrepel)
library(readxl)
library(arrow)

theme_set(theme_few())

# this file is to put together data on housing, net migration, income levels, and population together for MSAs in the US. 
# can we do relative price levels? yes I had found something on PPP in cities...
# wages vs cost of living (real wage)
# vs net migration 

msa_names <- read_csv('data/msa-names.csv')


# migration: https://netmigration.wisc.edu/ 

# take a look at income and prices 

# MAIRPD is implicit regional price deflators by msa
# MARPI is Real personal income by MSA (1 is personal, 2 is per capita personal income
# MARPP is regional price parities by MSA. all items vs goods vs housing vs utilities vs other
# weighted average 
 

# https://apps.bea.gov/itable/?ReqID=70&step=1#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOV0sImRhdGEiOltbIlRhYmxlSWQiLCI1MzMiXV19

# code folding for this sections...

rpp_dat <- read_csv('data/rpp2/MARPP_MSA_2008_2023.csv')

names(rpp_dat)

# set columns 2008-2023 as doubles
rpp_long <- rpp_dat |>
    select(-Region, -LineCode, -IndustryClassification)  |>
    mutate(across(c(`2008`:`2023`), as.numeric)) |>
    pivot_longer(-c(GeoFIPS, GeoName, TableName, Description, Unit), names_to='year', values_to='value') |>
    mutate(year = as.numeric(year)) 


rpp_long |>
    select(Description, Unit) |> unique()

rpp_long |>
    filter(Description == "Real per capita personal income (constant (2017) dollars) 2/") |>
    filter(year == 2023) |>
    # take (Metropolitan Statistical Area) out of GeoName
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
    ) |>
    arrange(desc(value)) |>
    head(50) |>
    ggplot(aes(x=value, y=reorder(GeoName, value))) +
    geom_bar(stat='identity') +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(
         y = ""
    )

msa_implied_pops <- rpp_long |>
    filter(Description %in% c("Real per capita personal income (constant (2017) dollars) 2/", "Real personal income (millions of constant (2017) dollars)")) |>
    select(year, GeoFIPS, GeoName, Description, value) |>
    pivot_wider(names_from = Description, values_from = value) |>
    mutate(
           implied_population = `Real personal income (millions of constant (2017) dollars)` * 1E6 / `Real per capita personal income (constant (2017) dollars) 2/`,
    )

rpp_long |>
    filter(Description %in% c("Real per capita personal income (constant (2017) dollars) 2/", "Real personal income (millions of constant (2017) dollars)")) |>
    filter(year == 2023) |>
    select(GeoFIPS, GeoName, Description, value) |>
    pivot_wider(names_from = Description, values_from = value) |>
    # take (Metropolitan Statistical Area) out of GeoName
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
    ) |>
    filter(GeoName != "United States") |>
    filter(GeoName != "United States (Nonmetropolitan Portion)") |>
    ggplot(aes(x=`Real personal income (millions of constant (2017) dollars)`, y=`Real per capita personal income (constant (2017) dollars) 2/`, label=GeoName)) +
    geom_point() +
    geom_text_repel() +
    labs(
         title = "Total vs per capita Income",
         subtitle = "2023",
         caption = "Source: BEA",
    )

ggsave('imgs/total_vs_per_capita_income.png', width=10, height=6)

rpp_long |>
    filter(Description == "RPPs: Services: Housing") |> 
    filter(year == 2023) |>
    filter(GeoName != "United States") |>
    filter(GeoName != "United States (Nonmetropolitan Portion)") |>
    arrange(desc(value)) |>
    # take (Metropolitan Statistical Area) out of GeoName
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
    ) |>
    head(50) |>
    ggplot(aes(x=value, y=reorder(GeoName, value))) +
    geom_bar(stat='identity') 

rpp_long |>
    filter(grepl("Midland, TX", GeoName)) |>
    filter(year == 2023) 


# MIGRATION 

msa_county_ref <- read_csv('data/msa_county_reference17.txt') |>
    mutate(fips_str2020 = paste0(fipstate, fipscty)) 

migration_dat <- read_csv('data/netmigration/NME_1020_data.csv')

# f1tttt,"Population 2020, total"
# m1tttt,"Net Migrants 2010s, total"
# r1tttt,"Net Migration Rate 2010s, total"

msa_migration <- migration_dat |>
    select(STNAME, STFIPS, fips_str2020, CoName2020, f1tttt, m1tttt, r1tttt) |>
    right_join(msa_county_ref, by=c('fips_str2020'='fips_str2020')) |>
    group_by(msa, name_msa) |>
    summarise(
              total_pop_2020 = sum(f1tttt, na.rm=T),
              net_migrants_2010s = sum(m1tttt, na.rm=T),
    ) |>
    arrange(desc(net_migrants_2010s)) 

msa_migration |>
    arrange(desc(total_pop_2020)) |>
    head(150) |>
    mutate(name_msa = str_remove(name_msa, "Metro Area")) |>
    ggplot(aes(x=net_migrants_2010s, y=total_pop_2020, label=name_msa)) +
    geom_vline(xintercept = 0) +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_log10() +
    labs(
         title = "Net Migration per MSA over 2010s",
         x = "Net Migrants",
         y = "Population in 2020",
         caption = "Source: Applied Population Laboratory, University of Wisconsin - Madison, 2024"
)

ggsave('imgs/net_migration.png', width=10, height=6)

# Egan-Robertson, David, Katherine J. Curtis, Richelle L. Winkler, Kenneth M. Johnson, and Caitlin Bourbeau, Age-Specific Net Migration Estimates for US Counties, 1950-2020. Applied Population Laboratory, University of Wisconsin - Madison, 2024. Web. [Date of access.] https://netmigration.wisc.edu/.

# SECTION: Putting the data together
# databuild 

wages_2024 <- read_excel('data/OES_Report.xlsx', skip = 5)
names(wages_2024)

wages_min <- wages_2024 |>
    mutate(
           msa_code = str_extract(`Area Name`, "\\d{5}"),
    ) |>
    select(msa_code, `Area Name`, `Employment (1)`, `Annual mean wage (2)`, `Annual median wage (2)`) |>
    mutate(
           year = 2024
    )


msa_names 

migrate_jn <- msa_migration |>
    #pivot_longer(c(total_pop_2020, net_migrants_2010s), names_to='metric', values_to='value') |>
    rename(GeoFIPS = msa) |>
    mutate(GeoFIPS = as.character(GeoFIPS)) |>
    select(-name_msa)

median_wages_time <- read_csv('data/median_wages_estimatedmsas.csv') |>
    mutate(
           AREA = as.character(AREA),
    )

median_wages_time |>
    arrange(AREA, year)

mega_dataset <- rpp_long |>
    filter(GeoFIPS != "00000" & GeoFIPS != "00999") |>
    select(GeoFIPS, GeoName, Description, year, value)  |>
    rename(metric = Description) |>
    select(GeoFIPS, GeoName, metric, year, value) |>
    pivot_wider(names_from=metric, values_from=value) |>
    left_join(migrate_jn) |>
    mutate(
           implied_population = `Real personal income (millions of constant (2017) dollars)` * 1E6 / `Real per capita personal income (constant (2017) dollars) 2/`,
    ) |>
    arrange(desc(implied_population)) |>
    left_join(median_wages_time, by=c('GeoFIPS'='AREA', 'year')) 

write_parquet(mega_dataset, 'data/mega_dataset.parquet')

# add wages.. can use oews using all occupations from 2008 - 2023 ideally.

names(mega_dataset)

mega_dataset |> 
    filter(GeoFIPS %in% c("35620", '31080')) |>
    select(
           GeoName,
           year, 
           `Real per capita personal income (constant (2017) dollars) 2/`, 
           A_MEDIAN, 
           `RPPs: All items`, 
           `RPPs: Services: Housing`, 
           net_migrants_2010s,
           implied_population
    ) |>
    filter(year %in% c(2009, 2010, 2020)) 



mega_dataset |>
    group_by(GeoFIPS) |>
    mutate(
           real_wage = A_MEDIAN / (`RPPs: All items` / 100),
           migrant_rate = net_migrants_2010s / implied_population[year == 2010],
    ) |>
    ungroup() |>
    filter(year == 2020) |>
    arrange(desc(implied_population)) |>
    head(50) |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           # take only the first name of the city before the ,
           GeoName = str_extract(GeoName, "^[^,]+"),
           # take only the first word before the dash
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(y=real_wage, x=migrant_rate, label=GeoName)) +
    geom_point() +
    geom_text_repel()  +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
         title = "Real Wage vs Net Migration Rate",
         subtitle = "2010-2020, top 50 cities in USA",
         x = "Net Migration Rate (net migration 2010-2020 / population 2010)",
         y = "PPP Adjusted Wage in 2020 (2017 dollars)",
    )
ggsave('imgs/real_wage_vs_migration.png', width=10, height=6)

mega_dataset |>
    group_by(GeoFIPS) |>
    mutate(
           real_wage = `Real per capita personal income (constant (2017) dollars) 2/`,
               #A_MEDIAN / (`RPPs: All items` / 100),
           migrant_rate = net_migrants_2010s / implied_population[year == 2010],
    ) |>
    ungroup() |>
    filter(year == 2020) |>
    arrange(desc(implied_population)) |>
    head(50) |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           # take only the first name of the city before the ,
           GeoName = str_extract(GeoName, "^[^,]+"),
           # take only the first word before the dash
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(y=real_wage, x=migrant_rate, label=GeoName)) +
    geom_point() +
    geom_text_repel()  +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
         title = "Real Wage vs Net Migration Rate",
         subtitle = "2010-2020, top 50 cities in USA",
         x = "Net Migration Rate (net migration 2010-2020 / population 2010)",
         y = "PPP Adjusted Wage in 2020 (2017 dollars)",
    )

ggsave('imgs/real_wage_vs_migration_2.png', width=10, height=6)

mega_dataset |>
    group_by(GeoFIPS) |>
    mutate(
           real_wage = `Real per capita personal income (constant (2017) dollars) 2/`,
               #A_MEDIAN / (`RPPs: All items` / 100),
           migrant_rate = net_migrants_2010s / implied_population[year == 2010],
    ) |>
    ungroup() |>
    filter(year == 2020) |>
    arrange(desc(implied_population)) |>
    #head(50) |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           # take only the first name of the city before the ,
           GeoName = str_extract(GeoName, "^[^,]+"),
           # take only the first word before the dash
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(y=real_wage, x=migrant_rate, label=GeoName)) +
    geom_vline(aes(xintercept = median(migrant_rate, na.rm=T)), linetype='dashed') + 
    geom_hline(aes(yintercept = median(real_wage, na.rm=T)), linetype='dashed') + 
    geom_point() +
    geom_text_repel()  +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
         title = "Real Wage vs Net Migration Rate",
         subtitle = "2010-2020 all MSAs in USA",
         x = "Net Migration Rate (net migration 2010-2020 / population 2010)",
         y = "PPP Adjusted Wage in 2020 (2017 dollars)",
    )

ggsave('imgs/real_wage_vs_migration_3.png', width=10, height=6)


mega_dataset |>
    group_by(GeoFIPS) |>
    mutate(
           real_wage = `Real per capita personal income (constant (2017) dollars) 2/`,
               #A_MEDIAN / (`RPPs: All items` / 100),
           migrant_rate = net_migrants_2010s / implied_population[year == 2010],
    ) |>
    ungroup() |>
    filter(year == 2020) |>
    arrange(desc(implied_population)) |>
    #head(50) |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           # take only the first name of the city before the ,
           GeoName = str_extract(GeoName, "^[^,]+"),
           # take only the first word before the dash
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(y=real_wage, x=migrant_rate, label=GeoName)) +
    geom_vline(aes(xintercept = median(migrant_rate, na.rm=T)), linetype='dashed') + 
    geom_hline(aes(yintercept = median(real_wage, na.rm=T)), linetype='dashed') + 
    geom_point() +
    geom_text_repel( data = . %>% filter(GeoFIPS %in% top_cities_2020)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
         title = "Real Wage vs Net Migration Rate",
         subtitle = "2010-2020 all MSAs in USA",
         x = "Net Migration Rate (net migration 2010-2020 / population 2010)",
         y = "PPP Adjusted Wage in 2020 (2017 dollars)",
    )

ggsave('imgs/real_wage_vs_migration_4.png', width=10, height=6)

# lets look at Miami

mega_dataset |>
    filter(grepl("Miami", GeoName)) |>
    select(year, `Real personal income (millions of constant (2017) dollars)`, `RPPs: All items`, `RPPs: Services: Housing`, A_MEDIAN, implied_population) |>
    pivot_longer(-year) |>
    ggplot(aes(x=year, y=value)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~name, scales="free_y") +
    labs(
         title = "Miami: Statistics"
    )

ggsave("imgs/miami_stats.png", width=10, height=6)


gen_city_chart <- function(cname) {
    mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        select(year, 
               `Real personal income (millions of constant (2017) dollars)`, 
               `Real per capita personal income (constant (2017) dollars) 2/`, 
               `RPPs: All items`, 
               `RPPs: Services: Housing`, 
               A_MEDIAN, 
               implied_population
        ) |>
        pivot_longer(-year) |>
        ggplot(aes(x=year, y=value)) +
        geom_line() +
        geom_point() + 
        facet_wrap(~name, scales="free_y") +
        labs(
             title = paste0(cname, ": Statistics")
        )

    ggsave(paste0("imgs/", cname, "_stats.png"), width=10, height=6)

}


gen_city_chart('Orlando')
gen_city_chart('Houston')

gen_city_chart('Boston')

gen_city_chart('Los Angeles')

gen_city_chart('New York')

gen_city_chart('Miami')


# lets do a metric that is 
# change in real per capita income 
# change in population 
# from 2010-2020

changes_df <- mega_dataset |>
    #filter(grepl(cname, GeoName, ignore.case = T)) |>
    filter(year %in% c(2010, 2020)) |>
    select(
           GeoFIPS,
           GeoName,
           year, 
            `Real per capita personal income (constant (2017) dollars) 2/`, 
            implied_population,
    ) |>
    rename(
            real_pc_income = `Real per capita personal income (constant (2017) dollars) 2/`,
            pop = implied_population,
    ) |>
    pivot_longer(c(-year, -GeoFIPS, -GeoName)) |>
    pivot_wider(names_from=year, values_from=value) |>
    mutate(
            cagr = ((`2020` / `2010`)^(1/10) - 1)
    ) |>
    select(-`2010`, -`2020`) |>
    pivot_wider(names_from=name, values_from=cagr) 

top_cities_2020 <- mega_dataset |>
    filter(year == 2020) |>
    arrange(desc(implied_population)) |>
    head(50) |>
    pull(GeoFIPS)

top_cities_2010 <- mega_dataset |>
    filter(year == 2010) |>
    arrange(desc(implied_population)) |>
    head(100) |>
    pull(GeoFIPS)

mean_pop_cagr <- mean(changes_df$pop, na.rm=T)
mean_income_cagr <- mean(changes_df$real_pc_income, na.rm=T)

changes_df |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           GeoName = str_extract(GeoName, "^[^,]+"),
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(x=pop, y=real_pc_income, label=GeoName)) +
    geom_hline(yintercept = mean_income_cagr, linetype='dashed') +
    geom_vline(xintercept = mean_pop_cagr, linetype='dashed') +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
         x = "Population CAGR (2010-2020)",
         y = "Real per capita income CAGR (2010-2020)",
         title = "Changes in Real Income vs Population Growth",
    )

ggsave('imgs/changes_real_income_vs_population_growth.png', width=10, height=6)

changes_df |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           GeoName = str_extract(GeoName, "^[^,]+"),
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(x=pop, y=real_pc_income, label=GeoName)) +
    geom_hline(yintercept = mean_income_cagr, linetype='dashed') +
    geom_vline(xintercept = mean_pop_cagr, linetype='dashed') +
    geom_point() +
    geom_text_repel(data = . %>% filter(GeoFIPS %in% top_cities_2020)) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
         x = "Population CAGR (2010-2020)",
         y = "Real per capita income CAGR (2010-2020)",
         title = "Changes in Real Income vs Population Growth",
    )

ggsave('imgs/changes_real_income_vs_population_growth_biglabel.png', width=10, height=6)


income_growth_vs_netmigrants <- mega_dataset |>
    #filter(grepl(cname, GeoName, ignore.case = T)) |>
    filter(year %in% c(2010, 2020)) |>
    select(
           GeoFIPS,
           GeoName,
           year, 
            `Real per capita personal income (constant (2017) dollars) 2/`
    ) |>
    rename(
            real_pc_income = `Real per capita personal income (constant (2017) dollars) 2/`,
    ) |>
    pivot_longer(c(-year, -GeoFIPS, -GeoName)) |>
    pivot_wider(names_from=year, values_from=value) |>
    mutate(
            cagr = ((`2020` / `2010`)^(1/10) - 1)
    ) |>
    select(-`2010`, -`2020`) |>
    pivot_wider(names_from=name, values_from=cagr) |>
    left_join(mega_dataset |> 
              filter(year == 2010) |>
              select(GeoFIPS, GeoName, net_migrants_2010s, implied_population) ,
          by=c('GeoFIPS', 'GeoName')
    ) |>
    mutate(
           migrant_rate = net_migrants_2010s / implied_population,
    ) 

avg_migrant_rate = mean(income_growth_vs_netmigrants$migrant_rate, na.rm=T)
avg_income_growth = mean(income_growth_vs_netmigrants$real_pc_income, na.rm=T)
    
income_growth_vs_netmigrants |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           GeoName = str_extract(GeoName, "^[^,]+"),
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(x=migrant_rate, y=real_pc_income, label=GeoName)) +
    geom_hline(yintercept = avg_income_growth, linetype='dashed') +
    geom_vline(xintercept = avg_migrant_rate, linetype='dashed') +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
         x = "Net Migration Rate (2010-2020)",
         y = "Real per capita income CAGR (2010-2020)",
         title = "Changes in Real Income vs Net Migration",
    ) 

ggsave('imgs/changes_real_income_vs_net_migration.png', width=10, height=6)

    
income_growth_vs_netmigrants |>
    mutate(
           GeoName = str_remove(GeoName, "(Metropolitan Statistical Area)"),
           GeoName = str_extract(GeoName, "^[^,]+"),
           GeoName = str_extract(GeoName, "^[^-]+"),
    ) |>
    ggplot(aes(x=migrant_rate, y=real_pc_income, label=GeoName)) +
    geom_hline(yintercept = avg_income_growth, linetype='dashed') +
    geom_vline(xintercept = avg_migrant_rate, linetype='dashed') +
    geom_point() +
    geom_text_repel(data = . %>% filter(GeoFIPS %in% top_cities_2020)) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
         x = "Net Migration Rate (2010-2020)",
         y = "Real per capita income CAGR (2010-2020)",
         title = "Changes in Real Income vs Net Migration",
    ) 

ggsave('imgs/changes_real_income_vs_net_migration_biglabel.png', width=10, height=6)

gen_changes_chart <- function(cname) {

    mega_dataset |>
        filter(grepl(cname, GeoName, ignore.case = T)) |>
        filter(year %in% c(2010, 2020)) |>
        select(year, 
               `Real per capita personal income (constant (2017) dollars) 2/`, 
               implied_population,
        ) |>
        rename(
               real_pc_income = `Real per capita personal income (constant (2017) dollars) 2/`,
               pop = implied_population,
        ) |>
        pivot_longer(-year) |>
        pivot_wider(names_from=year, values_from=value) |>
        mutate(
               cagr = ((`2020` / `2010`)^(1/10) - 1)
        ) 

