library(curl)
library(dplyr)
library(tidyr)
library(readxl)

# function to fill in na from next row
fillNa <- function(x) {
  lstval <- NA
  for (i in seq_along(x)) {
    ifelse(is.na(x[i]), x[i] <- lstval, lstval <- x[i])
  }
  x
}

# Download Files (need to download 2015 separately as it's in xlsx)
for(i in 2007:2014) {
  curl::curl_download(paste0("http://ec.europa.eu/budget/remote/fts/dl/export_",i,"_en.xls"),
                      destfile = paste0("euFunds/funds",i,".xls"))
}

curl::curl_download("http://ec.europa.eu/budget/remote/fts/dl/export_2015_en.xlsx", 
                    destfile = "euFunds/funds2015.xlsx")

files = list.files("euFunds/", full.names = T)

# merge files (use fillNA to fill missings for shared contracts)
df1 <- readxl::read_excel(files[1])
names(df1)[12] <- "Total amount"  
df1 <- df1 %>% dplyr::select(-Coordinator, -`Geographical Zone`, -`Co-financing rate`) %>% 
  dplyr::mutate(Amount = as.numeric(Amount))

df2 <- readxl::read_excel(files[2])
names(df2)[12] <- "Total amount"  
df2 <- df2 %>% dplyr::select(-Coordinator, -`Geographical Zone`, -`Co-financing rate`) %>% 
  dplyr::mutate(Amount = as.numeric(Amount))

df3 <- readxl::read_excel(files[3])
names(df3)[12] <- "Total amount"  
df3 <- df3 %>% dplyr::select(-Coordinator, -`Geographical Zone`, -`Co-financing rate`) %>% 
  dplyr::mutate(Amount = as.numeric(Amount))

df4 <- readxl::read_excel(files[4]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -`Geographical Zone`, -`Co-financing rate`, -`Funding Type`) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount)
  
  
df5 <- readxl::read_excel(files[5]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -`Geographical Zone`, -`Co-financing rate`, -`Funding Type`) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount)

df6 <- readxl::read_excel(files[6]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -NUTS2, -`Geographical Zone`, -`Co-financing rate`, -`Funding Type`) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount)

df7 <- readxl::read_excel(files[7]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -`Geographical Zone`,-NUTS2, -`Funding Type`) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount)

df8 <- readxl::read_excel(files[8]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`,
         -`Geographical Zone`,-NUTS2, -`Funding Type`) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount)

df9 <- readxl::read_excel(files[9]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`,
         -`Geographical Zone`,-NUTS2, -`Funding Type`) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>% 
  dplyr::group_by(`Commitment position key`) %>% 
  dplyr::mutate(`Total amount` = replace(`Total amount`, `Total amount`==0, NA)) %>% 
  dplyr::mutate(Amount = replace(Amount, is.na(Amount) & !is.na(`Total amount`),
                                                `Total amount`)) %>% 
  dplyr::mutate(`Total amount` = replace(`Total amount`, `Total amount`==0, NA)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount)

df <- dplyr::full_join(df1, df2) %>% 
  dplyr::full_join(., df3) %>% 
  dplyr::full_join(., df4) %>% 
  dplyr::full_join(., df5) %>% 
  dplyr::full_join(., df6) %>% 
  dplyr::full_join(., df7) %>% 
  dplyr::full_join(., df8) %>% 
  dplyr::full_join(., df9) 
  
rm(df1, df2, df3, df4, df5, df6, df7, df8, df9)

# Get Numbers for UK
terms <- c("UNIVERSITY", "SCHOOL", "COLLEGE")

dfUK <- df %>% 
  dplyr::filter(`Country / Territory`=="United Kingdom") %>% 
  dplyr::filter(grepl(paste(terms, collapse = "|"),
                      `Name of beneficiary`,ignore.case = T)) %>% 
  dplyr::filter(!duplicated(`Subject of grant or contract`)) %>% 
  dplyr::filter(!duplicated(`Commitment position key`))
  
