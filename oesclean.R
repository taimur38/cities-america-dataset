library(tidyverse)
library(readxl)


# 2008

m2008_1 <- read_excel('data/oews/MSA_M2008_dl_2.xls')
m2008_2 <- read_excel('data/oews/MSA_M2008_dl_3.xls')

df_2008 <- m2008_1 |>
    filter(OCC_CODE == "00-0000") |>
    rbind(
          m2008_2 |> filter(OCC_CODE == "00-0000")
    ) |>
    mutate(
           year = 2008
    ) |>
    select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, year, A_PCT10, A_PCT25, A_PCT75, A_PCT90)

df_2008

# 2009

m2009 <- read_excel('data/oews/aMSA_M2009_dl.xls')

df_2009 <- m2009 |>
    filter(OCC_CODE == '00-0000') |>
    select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, A_PCT10, A_PCT25, A_PCT75, A_PCT90) |>
    mutate(year = 2009) 


# 2010... skip these lets go to 2020
m2010_1 <- read_excel('data/oews/MSA_M2010_dl_1.xls')
m2010_2 <- read_excel('data/oews/MSA_M2010_dl_2.xls')
m2010_3 <- read_excel('data/oews/MSA_M2010_dl_3.xls')

m2010 <- rbind(
    m2010_1,
    m2010_2,
    m2010_3
)

m2010 |>
    filter(OCC_CODE == '00-0000') |>
    filter(grepl('(los angeles)|(anaheim)', AREA_NAME, ignore.case = TRUE)) 

df_2010 <- m2010 |>
    filter(OCC_CODE == '00-0000') |>
    select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, A_PCT10, A_PCT25, A_PCT75, A_PCT90) |>
    mutate(year = 2010) 

# 2011

m2011_1 <- read_excel('data/oews/MSA_M2011_dl_1_AK_IN.xls')
m2011_2 <- read_excel('data/oews/MSA_M2011_dl_2_KS_NY.xls')
m2011_3 <- read_excel('data/oews/MSA_M2011_dl_3_OH_WY.xls')

m2011 <- rbind(
    m2011_1,
    m2011_2,
    m2011_3
)

df_2011 <- m2011 |>
    filter(OCC_CODE == '00-0000') |>
    select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, A_PCT10, A_PCT25, A_PCT75, A_PCT90) |>
    mutate(year = 2011)

# 2012
m2012_1 <- read_excel('data/oews/MSA_M2012_dl_1_AK_IN.xls')
m2012_2 <- read_excel('data/oews/MSA_M2012_dl_2_KS_NY.xls')
m2012_3 <- read_excel('data/oews/MSA_M2012_dl_3_OH_WY.xls')

m2012 <- rbind(
    m2012_1,
    m2012_2,
    m2012_3
)

df_2012 <- m2012 |>
    filter(OCC_CODE == '00-0000') |>
    select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, A_PCT10, A_PCT25, A_PCT75, A_PCT90) |>
    mutate(year = 2012)

# 2013

m2013_1 <- read_excel('data/oews/MSA_M2013_dl_1_AK_IN.xls')
m2013_2 <- read_excel('data/oews/MSA_M2013_dl_2_KS_NY.xls')
m2013_3 <- read_excel('data/oews/MSA_M2013_dl_3_OH_WY.xls')

m2013 <- rbind(
    m2013_1,
    m2013_2,
    m2013_3
)

df_2013 <- m2013 |>
    filter(OCC_CODE == '00-0000') |>
    select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, A_PCT10, A_PCT25, A_PCT75, A_PCT90) |>
    mutate(year = 2013)


# 2014-2024

years <- c(2014:2024)

years_df <- tibble(
                   AREA = "",
                   AREA_NAME = "",
                   TOT_EMP = as.numeric(),
                   A_MEAN = as.numeric(),
                   A_MEDIAN = as.numeric(),
                   A_PCT10 = as.numeric(),
                   A_PCT25 = as.numeric(),
                   A_PCT75 = as.numeric(),
                   A_PCT90 = as.numeric(),
        )

for(y in years) {

    print(y)
    yy <- y %% 2000
    fname <- paste0('data/oews/oesm', yy, 'ma/MSA_M', y, '_dl.xlsx')

    myear <- read_excel(fname)

    names(myear) <- str_to_upper(names(myear))
    print(names(myear))

    if ('AREA_TITLE' %in% names(myear)) {
        myear <- myear |>
            rename(AREA_NAME = AREA_TITLE)
    }

    df_year <- myear |>
        filter(OCC_CODE == '00-0000') |>
        select(AREA, AREA_NAME, TOT_EMP, A_MEAN, A_MEDIAN, A_PCT10, A_PCT25, A_PCT75, A_PCT90) |>
        mutate(year = y)

    print(df_year)
    years_df <- rbind(years_df, df_year)
}


median_wages_time <- rbind(
        df_2008,
        df_2009,
        df_2010,
        df_2011,
        df_2012,
        df_2013,
        years_df
) 

median_wages_time <- median_wages_time |>
    mutate(
           TOT_EMP = as.numeric(TOT_EMP),
           A_MEAN = as.numeric(A_MEAN),
           A_MEDIAN = as.numeric(A_MEDIAN),
           A_PCT10 = as.numeric(A_PCT10),
           A_PCT25 = as.numeric(A_PCT25),
           A_PCT75 = as.numeric(A_PCT75),
           A_PCT90 = as.numeric(A_PCT90)
    )

msa_metrodivision_map <- read_csv('data/msa-metrodivision-map.csv') |>
    mutate(
           msa_id = as.character(msa_id),
           division_id = as.character(division_id)
    )

median_wages_time |>
    filter(grepl("boston", AREA_NAME, ignore.case = TRUE)) 

median_wages_time |>
    filter(grepl("los angeles", AREA_NAME, ignore.case = TRUE)) 

median_wages_time |>
    filter(grepl("new york", AREA_NAME, ignore.case = TRUE)) 

# if ive been given a metro division it means
missing_msas_estimated <- median_wages_time |>
    left_join(msa_metrodivision_map, by=c('AREA' = 'division_id')) |>
    filter(!is.na(msa_id)) |>
    group_by(msa_id, year) |>
    summarise(
              AREA = first(msa_id),
              AREA_NAME = first(msa_name),
              TOT_EMP = sum(TOT_EMP),
              A_MEAN = sum(A_MEAN * TOT_EMP) / sum(TOT_EMP),
              A_MEDIAN = sum(A_MEDIAN * TOT_EMP) / sum(TOT_EMP),
              A_PCT10 = sum(A_PCT10 * TOT_EMP) / sum(TOT_EMP),
              A_PCT25 = sum(A_PCT25 * TOT_EMP) / sum(TOT_EMP),
              A_PCT75 = sum(A_PCT75 * TOT_EMP) / sum(TOT_EMP),
              A_PCT90 = sum(A_PCT90 * TOT_EMP) / sum(TOT_EMP),
    ) |>
    ungroup()

    names(median_wages_time)
    names(missing_msas

median_wages_estimatedmsas <- median_wages_time |>
    rbind(select(missing_msas_estimated, -msa_id)) 


write_csv(median_wages_time, 'data/median_wages_time.csv')

write_csv(median_wages_estimatedmsas, 'data/median_wages_estimatedmsas.csv')
