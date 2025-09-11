library(tidyverse)
library(arrow)
library(ggthemes)
library(ggrepel)
library(broom)
library(fixest)

theme_set(theme_few())

acs <- read_parquet('acs-cities.parquet')

# ok i want to get the wages per year per city 

metro_wages <- acs %>%
  filter(INCWAGE > 0 & INCWAGE != 999999) %>%
  group_by(MET2013, YEAR) %>%
  summarise(
    mean_wage = sum(INCWAGE * PERWT, na.rm=T) / sum(PERWT, na.rm=T),
    median_wage = median(INCWAGE, na.rm=T),
    wage_10th = quantile(INCWAGE, 0.1, na.rm=T),
    wage_90th = quantile(INCWAGE, 0.9, na.rm=T),
    wage_25th = quantile(INCWAGE, 0.25, na.rm=T),
    wage_75th = quantile(INCWAGE, 0.75, na.rm=T),
  ) %>%
  collect()

# plot the wages as a line graph using mean 

write_csv(metro_wages, 'metro_wages.csv')

# next to do is to regress mincer and get wage premium...
# and then plot the wage premium by metro area
reg_dat <- acs %>%
  filter(INCWAGE > 0 & INCWAGE != 999999) %>%
  mutate(
    has_bach = EDUCD >= 101 & EDUCD != 999,
    race_desc = case_when(
      RACHSING == 1 ~ "White",
      RACHSING == 2 ~ "Black/African American",
      RACHSING == 3 ~ "American Indian/Alaska Native",
      RACHSING == 4 ~ "Asian/Pacific Islander",
      RACHSING == 5 ~ "Hispanic/Latino",
      TRUE ~ "Other"
    ),
    race_reg =factor(race_desc, 
                     levels = c("White", "Black/African American", 
                                "American Indian/Alaska Native", 
                                "Asian/Pacific Islander", 
                                "Hispanic/Latino", "Other")),
  )

# this is the wrong regression.. I need to capture the FE per MET2013

reg_res <- reg_dat %>%
  mutate(
    msa = factor(MET2013),
    age2 = AGE ^ 2
  ) %>%
  group_by(YEAR) %>%
  group_modify(~tidy(feols(
    log(INCWAGE) ~ msa | age2 + OCCSOC + has_bach + SEX,
    data = .x,
    weights = ~PERWT
  )))


reg_res |>
  mutate(
    msa = str_replace(term, "msa", ""),
  ) |>
  group_by(YEAR) |>
  mutate(
    demeaned = estimate - mean(estimate, na.rm = TRUE),
  ) |>
  ungroup() |>
  ggplot(aes(x=YEAR, y=demeaned)) +
  geom_boxplot(aes(group=YEAR)) +
  geom_line(
    data = . %>%
      filter(msa == '41700'),
    color = 'red'
  )



# lets add more controls...

reg_res2 <- reg_dat %>%
  mutate(
    msa = factor(MET2013),
    age2 = AGE ^ 2
  ) %>%
  group_by(YEAR) %>%
  group_modify(~tidy(feols(
    log(INCWAGE) ~ msa | age2 + OCCSOC + INDNAICS + has_bach + SEX,
    data = .x,
    weights = ~PERWT
  )))
  
reg_res2 |>
  mutate(
    msa = str_replace(term, "msa", ""),
  ) |>
  group_by(YEAR) |>
  mutate(
    demeaned = estimate - mean(estimate, na.rm = TRUE),
  ) |>
  ungroup() |>
  write_parquet('metro_wage_premiums.parquet')

reg_res2 <- read_parquet('metro_wage_premiums.parquet')

reg_res2 |>
  mutate(
    msa = str_replace(term, "msa", ""),
  ) |>
  group_by(YEAR) |>
  mutate(
    demeaned = estimate - mean(estimate, na.rm = TRUE),
  ) |>
  ungroup() |>
  ggplot(aes(x=YEAR, y=demeaned)) +
  geom_boxplot(aes(group=YEAR)) +
  geom_line(
    data = . %>%
      filter(msa == '41700'),
    color = 'red'
  )

#lets bring in some ind shares 

#Sector	Definition
# 11	Agriculture, Forestry, Fishing and Hunting
# 21	Mining, Quarrying, and Oil and Gas Extraction
# 22	Utilities
# 23	Construction
# 31-33	Manufacturing
# 42	Wholesale Trade
# 44-45	Retail Trade
# 48-49	Transportation and Warehousing
# 51	Information
# 52	Finance and Insurance
# 53	Real Estate and Rental and Leasing
# 54	Professional, Scientific, and Technical Services
# 55	Management of Companies and Enterprises
# 56	Administrative and Support and Waste Management and Remediation Services
# 61	Educational Services
# 62	Health Care and Social Assistance
# 71	Arts, Entertainment, and Recreation
# 72	Accommodation and Food Services
# 81	Other Services (except Public Administration)
# 92	Public Administration

natl_ind_stats <- acs |>
  mutate(
    naics_2digit = str_sub(INDNAICS, 1, 2),
    naics_2_desc = case_when(
      naics_2digit == '11' ~ 'Agriculture, Forestry, Fishing and Hunting',
      naics_2digit == '21' ~ 'Mining, Quarrying, and Oil and Gas Extraction',
      naics_2digit == '22' ~ 'Utilities',
      naics_2digit == '23' ~ 'Construction',
      naics_2digit %in% c('31', '32', '33') ~ 'Manufacturing',
      naics_2digit == '42' ~ 'Wholesale Trade',
      naics_2digit %in% c('44', '45') ~ 'Retail Trade',
      naics_2digit %in% c('48', '49') ~ 'Transportation and Warehousing',
      naics_2digit == '51' ~ 'Information',
      naics_2digit == '52' ~ 'Finance and Insurance',
      naics_2digit == '53' ~ 'Real Estate and Rental and Leasing',
      naics_2digit == '54' ~ 'Professional, Scientific, and Technical Services',
      naics_2digit == '55' ~ 'Management of Companies and Enterprises',
      naics_2digit == '56' ~ 'Administrative and Support and Waste Management and Remediation Services',
      naics_2digit == '61' ~ 'Educational Services',
      naics_2digit == '62' ~ 'Health Care and Social Assistance',
      naics_2digit == '71' ~ 'Arts, Entertainment, and Recreation',
      naics_2digit == '72' ~ 'Accommodation and Food Services',
      naics_2digit == '81' ~ 'Other Services (except Public Administration)',
      naics_2digit == '92' ~ 'Public Administration'
    )
  ) %>%
  filter(EMPSTAT == 1) %>%
  group_by(YEAR, naics_2_desc) |>
  summarise(
    natl_ind_emp = sum(PERWT, na.rm=T),
  ) |>
  ungroup() |>
  group_by(YEAR) |>
  mutate(
    total_emp = sum(natl_ind_emp)
  ) |>
  ungroup() |>
  mutate(
    natl_ind_share = natl_ind_emp / total_emp
  )

acs_short_industry_shares <- acs |>
  filter(EMPSTAT == 1) |>
  mutate(
    naics_2digit = str_sub(INDNAICS, 1, 2),
    naics_2_desc = case_when(
      naics_2digit == '11' ~ 'Agriculture, Forestry, Fishing and Hunting',
      naics_2digit == '21' ~ 'Mining, Quarrying, and Oil and Gas Extraction',
      naics_2digit == '22' ~ 'Utilities',
      naics_2digit == '23' ~ 'Construction',
      naics_2digit %in% c('31', '32', '33') ~ 'Manufacturing',
      naics_2digit == '42' ~ 'Wholesale Trade',
      naics_2digit %in% c('44', '45') ~ 'Retail Trade',
      naics_2digit %in% c('48', '49') ~ 'Transportation and Warehousing',
      naics_2digit == '51' ~ 'Information',
      naics_2digit == '52' ~ 'Finance and Insurance',
      naics_2digit == '53' ~ 'Real Estate and Rental and Leasing',
      naics_2digit == '54' ~ 'Professional, Scientific, and Technical Services',
      naics_2digit == '55' ~ 'Management of Companies and Enterprises',
      naics_2digit == '56' ~ 'Administrative and Support and Waste Management and Remediation Services',
      naics_2digit == '61' ~ 'Educational Services',
      naics_2digit == '62' ~ 'Health Care and Social Assistance',
      naics_2digit == '71' ~ 'Arts, Entertainment, and Recreation',
      naics_2digit == '72' ~ 'Accommodation and Food Services',
      naics_2digit == '81' ~ 'Other Services (except Public Administration)',
      naics_2digit == '92' ~ 'Public Administration'
    )
  ) %>%
  group_by(MET2013, YEAR, naics_2_desc) |>
  summarise(
    city_ind_emp = sum(PERWT, na.rm=T)
  ) |>
  ungroup() |>
  group_by(MET2013, YEAR) |>
  mutate(
      city_total_emp = sum(city_ind_emp, na.rm=T)
  ) |>
  ungroup() |>
  left_join(natl_ind_stats, by = c("YEAR", "naics_2_desc")) |>
  mutate(
    city_ind_share = city_ind_emp / city_total_emp,
    city_ind_markeshare = city_ind_emp / natl_ind_emp,
    city_ind_rca = city_ind_share / natl_ind_share
  )

acs_short_industry_shares |>
  filter(MET2013 == 32820) |>
  filter(!is.na(naics_2_desc)) |>
  #filter(city_ind_share > 0.005) |>
  ggplot(aes(x=YEAR, y=city_ind_emp, color=naics_2_desc)) +
  geom_line(alpha = 0.5) +
  geom_line(data = . %>% filter(naics_2_desc == "Manufacturing")) +
  geom_text_repel(data = . %>%
                    filter(YEAR == 2022) %>%
                    filter(city_ind_emp > 40000),
    aes(label=naics_2_desc)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.position = 'none') +
  labs(
    title = "Employment by Sector, Memphis",
    y = "Estimated Employment",
    x = "",
    caption = "Source: American Community Survey"
  )

ggsave('imgs/memphis_ind_shares.png', width=9, height=6)

acs_short_industry_shares |>
  filter(MET2013 == 32820) |>
  #filter(YEAR >= 2010 & YEAR <= 2020) |>
  filter(!is.na(naics_2_desc)) |>
  group_by(naics_2_desc) |>
  mutate(
    emp_2022 = first(city_ind_emp[YEAR == 2022]),
    emp_2010 = first(city_ind_emp[YEAR == 2010])
  ) |>
  ungroup() |>
  mutate(
    naics_2_desc = fct_reorder(naics_2_desc, emp_2010, .desc = TRUE)
  ) |>
  ggplot(aes(x=YEAR, y=city_ind_markeshare, color=naics_2_desc)) +
  geom_line(alpha = 0.5) +
  geom_line(data = . %>% filter(naics_2_desc == "Manufacturing")) +
  # geom_text_repel(data = . %>%
  #                   filter(YEAR == 2022),
  #                 aes(label=naics_2_desc)) +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(labels=scales::percent) +
  geom_vline(xintercept = 2020) +
  theme(legend.position = 'none') +
  labs(
    title = "Employment by Sector, Memphis",
    y = "Share of National Employment in Sector",
    x = "",
    caption = "Source: American Community Survey"
  ) +
  # label each facet max length is 25 
  facet_wrap(~ naics_2_desc, scales = 'free_y', labeller = label_wrap_gen(width = 20))

ggsave('imgs/memphis_ind_shares_marketshare_allyears.png', width=9, height=9)

acs_short_industry_shares |>
  filter(MET2013 == 32820) |>
  filter(naics_2_desc == "Manufacturing") |>
  ggplot(aes(x=YEAR, y=city_ind_markeshare)) +
  geom_line()

acs_short_industry_shares |>
  filter(MET2013 == 32820) |>
  filter(!is.na(naics_2_desc)) |>
  filter(YEAR %in% c(2010, 2020)) |>
  select(YEAR, naics_2_desc, city_ind_emp) |>
  pivot_wider(names_from=YEAR, values_from=city_ind_emp) |>
  mutate(
    diff = `2020` - `2010`,
    cagr = ((`2020` / `2010`) ^ (1 / 10)) - 1
  ) |>
  arrange(desc(diff))2
  
  geom_area() +
  geom_vline(xintercept = 2012) +
  geom_text_repel(data = . %>%
                    filter(YEAR == 2022),
                  aes(label=naics_2_desc)) +
  theme(legend.position = 'none') 



acs_industry_shares <- acs |>
  group_by(YEAR, INDNAICS) |>
  mutate(
    natl_ind_emp = sum(PERWT, na.rm=T),
    natl_ind_share = natl_ind_emp / sum(natl_ind_emp),
  ) |>
  ungroup() |>
  group_by(MET2013, YEAR, INDNAICS) |>
  summarise(
    
    natl_ind_emp = first(natl_ind_emp),
    natl_ind_share = first(natl_ind_share),
    
    city_ind_emp = sum(PERWT, na.rm=T),
    
    city_ind_share = city_ind_emp / natl_ind_emp,
    
    city_ind_rca = city_ind_share / natl_ind_share
  ) 

acs_industry_shares |>
  mutate(
    naics_2digit = str_sub(INDNAICS, 1, 2),
    naics_2_desc = case_when(
      naics_2digit == '11' ~ 'Agriculture, Forestry, Fishing and Hunting',
      naics_2digit == '21' ~ 'Mining, Quarrying, and Oil and Gas Extraction',
      naics_2digit == '22' ~ 'Utilities',
      naics_2digit == '23' ~ 'Construction',
      naics_2digit %in% c('31', '32', '33') ~ 'Manufacturing',
      naics_2digit == '42' ~ 'Wholesale Trade',
      naics_2digit %in% c('44', '45') ~ 'Retail Trade',
      naics_2digit %in% c('48', '49') ~ 'Transportation and Warehousing',
      naics_2digit == '51' ~ 'Information',
      naics_2digit == '52' ~ 'Finance and Insurance',
      naics_2digit == '53' ~ 'Real Estate and Rental and Leasing',
      naics_2digit == '54' ~ 'Professional, Scientific, and Technical Services',
      naics_2digit == '55' ~ 'Management of Companies and Enterprises',
      naics_2digit == '56' ~ 'Administrative and Support and Waste Management and Remediation Services',
      naics_2digit == '61' ~ 'Educational Services',
      naics_2digit == '62' ~ 'Health Care and Social Assistance',
      naics_2digit == '71' ~ 'Arts, Entertainment, and Recreation',
      naics_2digit == '72' ~ 'Accommodation and Food Services',
      naics_2digit == '81' ~ 'Other Services (except Public Administration)',
      naics_2digit == '92' ~ 'Public Administration'
    )
  ) |>
  filter(naics_2_desc == "Accommodation and Food Services") |>
  filter(MET2013 == 32820) |>
  ggplot(aes(x=YEAR, y=city_ind_emp, color=INDNAICS)) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  geom_text_repel(data = . %>%
                    filter(YEAR == 2022),
    aes(label=INDNAICS)) +
  theme(legend.position = 'none') 





acs_short_industry_shares |>
  filter(MET2013 == 41700) |>
  filter(!is.na(naics_2_desc)) |>
  #filter(city_ind_share > 0.005) |>
  ggplot(aes(x=YEAR, y=city_ind_emp, color=naics_2_desc)) +
  geom_line(alpha = 0.5) +
  geom_line(data = . %>% filter(naics_2_desc == "Manufacturing")) +
  geom_text_repel(data = . %>%
                    filter(YEAR == 2022) %>%
                    filter(city_ind_emp > 40000),
                  aes(label=naics_2_desc)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.position = 'none') +
  labs(
    title = "Employment by Sector, San Antonio",
    y = "Estimated Employment",
    x = "",
    caption = "Source: American Community Survey"
  )


acs_short_industry_shares |>
  filter(MET2013 == 41700) |>
  filter(YEAR >= 2010 & YEAR <= 2020) |>
  filter(!is.na(naics_2_desc)) |>
  group_by(naics_2_desc) |>
  mutate(
    emp_2022 = first(city_ind_emp[YEAR == 2022]),
    emp_2010 = first(city_ind_emp[YEAR == 2010])
  ) |>
  ungroup() |>
  mutate(
    naics_2_desc = fct_reorder(naics_2_desc, emp_2010, .desc = TRUE)
  ) |>
  ggplot(aes(x=YEAR, y=city_ind_rca, color=naics_2_desc)) +
  geom_line() +
  # geom_text_repel(data = . %>%
  #                   filter(YEAR == 2022),
  #                 aes(label=naics_2_desc)) +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.position = 'none') +
  labs(
    title = "Employment by Sector, San Antonio",
    y = "Share of National Employment in Sector",
    x = "",
    caption = "Source: American Community Survey"
  ) +
  # label each facet max length is 25 
  facet_wrap(~ naics_2_desc, scales = 'free_y', labeller = label_wrap_gen(width = 20))

ggsave('imgs/san_antonio_ind_shares_marketshare.png', width=9, height=9)
