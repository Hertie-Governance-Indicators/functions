library(curl)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
# function to fill in na from next row
fillNa <- function(x) {
  lstval <- NA
  for (i in seq_along(x)) {
    ifelse(is.na(x[i]), x[i] <- lstval, lstval <- x[i])
  }
  x
}



# Download Files ----------------------------------------------------------

# # (need to download 2015 separately as it's in xlsx)
# for(i in 2007:2014) {
#   curl::curl_download(paste0("http://ec.europa.eu/budget/remote/fts/dl/export_",i,"_en.xls"),
#                       destfile = paste0("euFunds/funds",i,".xls"))
# }
# 
# curl::curl_download("http://ec.europa.eu/budget/remote/fts/dl/export_2015_en.xlsx", 
#                     destfile = "euFunds/funds2015.xlsx")
# 
files = list.files("euFunds/", full.names = T)



# merge files -------------------------------------------------------------

# (use fillNA to fill missings for shared contracts)


# df1 <- readxl::read_excel(files[1])
# names(df1)[12] <- "Total amount"  
# df1 <- df1 %>% dplyr::select(-Coordinator, -`Geographical Zone`, -`Co-financing rate`) %>% 
#   dplyr::mutate(Amount = as.numeric(Amount))
# 
# df2 <- readxl::read_excel(files[2])
# names(df2)[12] <- "Total amount"  
# df2 <- df2 %>% dplyr::select(-Coordinator, -`Geographical Zone`, -`Co-financing rate`) %>% 
#   dplyr::mutate(Amount = as.numeric(Amount))
# 
# df3 <- readxl::read_excel(files[3])
# names(df3)[12] <- "Total amount"  
# df3 <- df3 %>% dplyr::select(-Coordinator, -`Geographical Zone`, -`Co-financing rate`) %>% 
#   dplyr::mutate(Amount = as.numeric(Amount))

df4 <- readxl::read_excel(files[4]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -`Geographical Zone`, -`Co-financing rate`) %>% 
  dplyr::mutate(Amount = ifelse(is.na(Amount), `Total amount`, Amount)) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount, -`Total amount`)
  
df5 <- readxl::read_excel(files[5]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -`Geographical Zone`, -`Co-financing rate`) %>% 
  dplyr::mutate(Amount = ifelse(is.na(Amount), `Total amount`, Amount)) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount, -`Total amount`)

df6 <- readxl::read_excel(files[6]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -NUTS2, -`Geographical Zone`, -`Co-financing rate`) %>% 
  dplyr::mutate(Amount = ifelse(is.na(Amount), `Total amount`, Amount)) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount, -`Total amount`)

df7 <- readxl::read_excel(files[7]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`, 
         -`Geographical Zone`,-NUTS2) %>% 
  dplyr::mutate(Amount = ifelse(Amount==0, `Total amount`, Amount)) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount, -`Total amount`)

df8 <- readxl::read_excel(files[8]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`,
         -`Geographical Zone`,-NUTS2) %>% 
  dplyr::mutate(Amount = ifelse(is.na(Amount), `Total amount`, Amount)) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount, -`Total amount`)

df9 <- readxl::read_excel(files[9]) %>% 
  dplyr::select(-Coordinator, -`VAT Number of beneficiary`,
         -`Geographical Zone`,-NUTS2) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>% 
  dplyr::mutate(Amount = ifelse(is.na(Amount), `Total amount`, Amount)) %>% 
  dplyr::mutate_each(funs(fillNa(.)), -`Name of beneficiary`, -Address,
                     -City, -`Postal code`, -`Country / Territory`, -Amount, -`Total amount`)

df <- dplyr::full_join(df4, df5) %>% 
  dplyr::full_join(., df6) %>% 
  dplyr::full_join(., df7) %>% 
  dplyr::full_join(., df8) %>% 
  dplyr::full_join(., df9) 



# Tables and Figures ------------------------------------------------------

# dictionaries
schools <- c("UNIVERSITY", "UNIVERSITE", "SCHOOL", "COLLEGE", 
             "UNIVERSIT", "SCHULE")
medicine <- c("HOSPITAL","Medical","Medicine", "KLINIKUM", "MEDIZIN",
              "PHARMA", "MEDISCH")
department <- c("Research", "Education", "Innovation")

# Subsets
universities <- df %>% 
  dplyr::filter(grepl("Grant",
                      `Funding Type`, ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(department, collapse = "|"),
                      `Responsible Department`,ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(schools, collapse = "|"),
                      `Name of beneficiary`,ignore.case = T)) %>% 
  dplyr::filter(!grepl(paste(medicine, collapse = "|"),
                       `Name of beneficiary`,ignore.case = T))


germany <-  df %>% 
  dplyr::filter(`Country / Territory`=="Germany") %>% 
  dplyr::filter(grepl("Grant",
                      `Funding Type`, ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(department, collapse = "|"),
                      `Responsible Department`,ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(schools, collapse = "|"),
                      `Name of beneficiary`,ignore.case = T)) %>% 
  dplyr::filter(!grepl(paste(medicine, collapse = "|"),
                      `Name of beneficiary`,ignore.case = T))

uk <- df %>% 
  dplyr::filter(`Country / Territory`=="United Kingdom") %>% 
  dplyr::filter(grepl("Grant",
                      `Funding Type`, ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(department, collapse = "|"),
                      `Responsible Department`,ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(schools, collapse = "|"),
                      `Name of beneficiary`,ignore.case = T)) %>% 
  dplyr::filter(!grepl(paste(medicine, collapse = "|"),
                       `Name of beneficiary`,ignore.case = T))

hertie <- df %>% 
  dplyr::filter(grepl("Grant",
                      `Funding Type`, ignore.case = T)) %>% 
  dplyr::filter(grepl("Hertie School",
                      `Name of beneficiary`, ignore.case = T))

# Top German Universities 
topgerFund <- germany %>% dplyr::group_by(`Name of beneficiary`) %>% 
  dplyr::summarise(round(sum(Amount))) %>%
  dplyr::arrange(desc(`round(sum(Amount))`))
topgerCase <- germany %>% dplyr::group_by(`Name of beneficiary`) %>% 
  dplyr::summarise(n())
topger <- dplyr::full_join(topgerFund, topgerCase) %>% 
  dplyr::arrange(desc(`round(sum(Amount))`))
write.csv(topger, file = "topger.csv")

# Top UK Universities 
topukFund <- uk %>% dplyr::group_by(`Name of beneficiary`) %>% 
  dplyr::summarise(round(sum(Amount))) %>%
  dplyr::arrange(desc(`round(sum(Amount))`))
topukCase <- uk %>% dplyr::group_by(`Name of beneficiary`) %>% 
  dplyr::summarise(n())
topuk <- dplyr::full_join(topukFund, topukCase) %>% 
  dplyr::arrange(desc(`round(sum(Amount))`))
write.csv(topuk, file = "topuk.csv")

# Compare Ger to UK
gerYears <- germany %>% dplyr::group_by(Year) %>% 
  dplyr::summarise(sum(Amount))
gerCases <- germany %>% dplyr::group_by(Year) %>% 
  dplyr::summarise(n())
gerAmount <- dplyr::full_join(gerYears, gerCases)

ukYears <- uk %>% dplyr::group_by(Year) %>% 
  dplyr::summarise(sum(Amount))
ukCases <- uk %>% dplyr::group_by(Year) %>% 
  dplyr::summarise(n())
ukAmount <- dplyr::full_join(ukYears, ukCases)

hertieAmount <- hertie %>% 
  dplyr::mutate(`EU Grants` = n()) %>% 
  dplyr::mutate(`EU Funding` = sum(Amount)) %>% 
  dplyr::mutate(Type = "Hertie") %>% 
  select(`EU Grants`, `EU Funding`, Type) %>% 
  dplyr::filter(!duplicated(.))

# Top Business Schools

Copenhagen <- universities %>% 
  dplyr::filter(grepl("COPENHAGEN BUSINESS SCHOOL",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

LUISS <- universities %>% 
  dplyr::filter(grepl("LUISS",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

Maastricht <- universities %>% 
  dplyr::filter(grepl("Maastricht",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

Corvinus <- universities %>% 
  dplyr::filter(grepl("Corvinus",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

CEU <- universities %>% 
  dplyr::filter(grepl("BUDAPESTI KOZEP",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

Carlo <- df %>% 
  dplyr::filter(grepl("COLLEGIO CARLO ALBERTO",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 

Essex <- df %>% 
  dplyr::filter(grepl("Grant",
                      `Funding Type`, ignore.case = T)) %>% 
  dplyr::filter(grepl("University of Essex",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 

#  Think Tanks
OpenEvidence <- df %>% 
  dplyr::filter(grepl("Open Evidence",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))
Wuppertal <- df %>% 
  dplyr::filter(grepl("WUPPERTAL INSTITUT FUR KLIMA",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

ispi <- df %>% 
  dplyr::filter(grepl("ISTITUTO PER GLI STUDI",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount))

ids <-  df %>% 
  dplyr::filter(grepl("Institute of Development Studies",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 

pse <- df %>% 
  dplyr::filter(grepl("FONDATION DE COOPERATION SCIENTIFIQUE ECOLE D",
                      `Name of beneficiary`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 
  

# Data Projects
data <- df %>% 
  dplyr::filter(grepl("Grant",
                      `Funding Type`, ignore.case = T)) %>% 
  dplyr::filter(grepl(paste(department, collapse = "|"),
                      `Responsible Department`,ignore.case = T)) %>%
  dplyr::filter(grepl("data",`Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::filter(!duplicated(`Subject of grant or contract`))
  
sobigdata <- df %>% 
  dplyr::filter(grepl("SOBIGDATA",
                      `Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 

sense4us <- df %>% 
  dplyr::filter(grepl("Data Insights for Policy Makers",
                      `Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 

quantess <- df %>% 
  dplyr::filter(grepl("Quantitative Analysis of Textual Data",
                      `Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 
cessda <- df %>% 
  dplyr::filter(grepl("cessda",
                      `Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 

datactive <-df %>% 
  dplyr::filter(grepl("DATACTIVE",
                      `Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 
  

DELECA <- df %>% 
  dplyr::filter(grepl("leadership capacity",
                      `Subject of grant or contract`, ignore.case = T)) %>% 
  dplyr::summarise(sum(Amount)) 
  

