library(arrow)
library(ipumsr)
#
ddi <- read_ipums_ddi('data/shared-data/ipums/usa_00011.xml')
acs <- read_ipums_micro(ddi)
#
zap_labels(acs)

#acs <- arrow::read_csv_arrow('data/shared-data/ipums/usa_00012.csv')

write_parquet(acs, 'data/shared-data/ipums/acs-cities.parquet')


