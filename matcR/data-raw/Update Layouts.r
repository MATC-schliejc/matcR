#Updates fwf_layout.rda

library(readxl)

fwf_layouts <- read_excel('C:/local/matcR/data-raw/_WTCS Record Layouts.xlsx')
#copy the existing rda file in /data to /data-raw/history and rename with last version #
usethis::use_data(fwf_layouts)
