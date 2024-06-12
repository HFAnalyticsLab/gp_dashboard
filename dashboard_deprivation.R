#########################################
### GP dashboard deprivation analysis ###
#########################################

library(tidyverse)
library(readxl)

# load IMD data and calculated a re-weighted IMD for 2021 LSOA codes using the 2011-2021 lookup

imd_raw <- read_csv(file="data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv", name_repair = make.names)

#colnames(imd_raw)
imd <- imd_raw %>%
  rename(
    LSOA_CODE = LSOA.code..2011.,
    IMD_SCORE = Index.of.Multiple.Deprivation..IMD..Score,
    IMD_DECILE = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
    IMD_HEALTH = Health.Deprivation.and.Disability.Score) %>%
  mutate(
    IMD_QUINTILE = case_when(IMD_DECILE %in% c(1:2) ~ 1,
                             IMD_DECILE %in% c(3:4) ~ 2,
                             IMD_DECILE %in% c(5:6) ~ 3,
                             IMD_DECILE %in% c(7:8) ~ 4,
                             IMD_DECILE %in% c(9:10) ~ 5),
    LSOA_DATE = 2011) %>%
  select(LSOA_CODE, IMD_SCORE, IMD_HEALTH, IMD_QUINTILE, LSOA_DATE)

mapping <- read_csv(file = "data/LSOA_2011_to_LSOA_2021_Lookup_E_W.csv", name_repair = make.names)

# convert 2021 LSOA groups if the LSOA CHNGID M (merged in 2021) or X (unclear mapping)

weighted_mapping = mapping %>% left_join(
  mapping %>% filter(CHGIND=="M" | CHGIND=="X") %>%
    group_by(LSOA21CD) %>%
    summarise(WEIGHT=1/n())) %>%
  mutate(WEIGHT=if_else(CHGIND %in% c("S","U"),1,WEIGHT))


weighted_imd <- weighted_mapping  %>%
  left_join(select(imd, LSOA_CODE, IMD_HEALTH, IMD_SCORE), by=c("LSOA11CD" = "LSOA_CODE")) %>%
  mutate(
    weighted_imd = IMD_SCORE*WEIGHT,
    weighted_imd_health = IMD_HEALTH*WEIGHT
  ) %>%
  group_by(LSOA21CD) %>%
  summarise(
    IMD_SCORE = sum(weighted_imd),
    IMD_HEALTH = sum(weighted_imd_health)
  )  %>%
  mutate(IMD_QUINTILE = ntile(-IMD_SCORE,5)) %>%
  mutate(LSOA_DATE = 2021) %>%
  rename(LSOA_CODE = LSOA21CD)

imd_2011_2021 <- rbind(imd, weighted_imd)


year_list <- c(2017:2023)


load_ons_data <- function(year, sex) {
  
  #print(year)
  
  if(year == 2017) {
    object_url = "data/population/SAPE20DT2-mid-2017-lsoa-syoa-estimates-unformatted.xls"
  } else if(year == 2018) {
    object_url = "data/population/SAPE21DT2-mid-2018-lsoa-syoa-estimates-unformatted.xlsx"
  } else if(year == 2019) {
    object_url = "data/population/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"
  } else if(year == 2020) {
    object_url = "data/population/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
  } else if(year == 2021) {
    object_url = "data/population/sapelsoasyoatablefinal.xlsx"
    excel_sheet ="Mid-2021 LSOA 2021"
  } else if(year %in% c(2022:2023)) {
    object_url = "data/population/sapelsoasyoatablefinal.xlsx"
    excel_sheet ="Mid-2022 LSOA 2021"
  } # end setting the object url
  
  print(object_url)
  
  if(year == 2017) {
    
    lsoa_date = 2011
    
    data <- read_xls(
      path = object_url,
      sheet=paste0("Mid-",year," ",sex),
      skip = 4,
      trim_ws=TRUE,
      .name_repair = "universal"
    )
    
    # data <- aws.s3::s3read_using(
    #   FUN = readxl::read_xls,
    #   object = object_url,
    #   bucket = s3_bucket,
    #   sheet=paste0("Mid-",year," ",sex),
    #   skip = 4,
    #   trim_ws=TRUE,
    #   .name_repair = "universal")
    
  } else if(year %in% c(2018:2020)) {
    
    lsoa_date = 2011
    
    data <- read_xlsx(
      path = object_url,
      sheet = paste0("Mid-",year," ",sex),
      skip = 4,
      trim_ws=TRUE,
      .name_repair = "universal"
    )
    
    # data <- aws.s3::s3read_using(
    #   FUN = readxl::read_xlsx,
    #   object = object_url,
    #   bucket = s3_bucket,
    #   sheet=paste0("Mid-",year," ",sex),
    #   skip = 4,
    #   trim_ws=TRUE,
    #   .name_repair = "universal")
    
  } else if(year %in% c(2021:2023)) {
    
    lsoa_date = 2021
    
    data <- read_xlsx(
      path = object_url,
      sheet = excel_sheet,
      skip = 3,
      trim_ws=TRUE,
      .name_repair = "universal"
    )
    
    # data <- aws.s3::s3read_using(
    #   FUN = readxl::read_xlsx,
    #   object = object_url,
    #   bucket = s3_bucket,
    #   sheet = "Mid-2021 LSOA 2021",
    #   skip = 3,
    #   trim_ws=TRUE,
    #   .name_repair = "universal")
    
  } # end loading the data
  
  if(year %in% c(2017:2018)) {
    
    data1 <- data %>%
      select(-All.Ages) %>%
      pivot_longer(cols=!c('Area.Codes', 'Area.Names'), names_to='AGE', values_to='POP') %>%
      mutate(AGE = as.numeric(str_remove_all(AGE, '[^[:alnum:]]')),
             SEX = sex) %>%
      rename(LSOA_CODE = Area.Codes)
    
  } else if(year %in% c(2019:2020)) {
    
    data1 <- data %>%
      select(-contains('Boundaries'), -All.Ages) %>%
      pivot_longer(cols=!c('LSOA.Code', 'LSOA.Name'), names_to='AGE', values_to='POP') %>%
      mutate(AGE = as.numeric(str_remove_all(AGE, '[^[:alnum:]]')),
             SEX = sex) %>%
      rename(LSOA_CODE = LSOA.Code)
    
  } else if (year %in% c(2021:2023)) {
    
    data1 <- data %>%
      select(-c(LAD.2021.Code, LAD.2021.Name, LSOA.2021.Name, Total)) %>%
      pivot_longer(!LSOA.2021.Code, names_to="AGE", values_to="POP") %>%
      separate(AGE, into = c("SEX", "AGE"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
      mutate(SEX = case_when(SEX == "F" ~ "Females",SEX == "M" ~ "Males")
      ) %>%
      filter(SEX == sex) %>%
      rename(LSOA_CODE = LSOA.2021.Code)
    
  } # end cleaning the data by year
  
  # group into age groups once here
  data2 <- data1 %>%
    mutate(
      AGE = as.numeric(AGE),
      AGE = case_when(
        AGE %in% c(0:4) ~ "0_4",
        AGE %in% c(5:14) ~ "5_14",
        AGE %in% c(15:44) ~ "15_44",
        AGE %in% c(45:64) ~ "45_64",
        AGE %in% c(65:74) ~ "65_74",
        AGE %in% c(75:84) ~ "75_84",
        AGE >85  ~ "85_PLUS")
    ) %>%
    group_by(LSOA_CODE, AGE) %>%
    summarise(Total_POP = sum(POP)) %>%
    mutate(
      YEAR = year,
      SEX = sex,
      LSOA_DATE = lsoa_date
    )
  
}

ons_data_M <- lapply(year_list, load_ons_data, sex="Males")
ons_data_F <- lapply(year_list, load_ons_data, sex="Females")

ons_both <- c(ons_data_M, ons_data_F)
ons_df <- as.data.frame(do.call(rbind, ons_both))

## calculate weighted LSOA populations using Carr Hill formula values from 'Level or not?'

adj_pop_df <- ons_df %>%
  mutate(
    SEX = case_when(
      SEX == "Males" ~ "MALE",
      SEX == "Females" ~ "FEMALE"
    ),
    YEAR = as.numeric(YEAR)
  ) %>%
  pivot_wider(
    names_from = c(SEX, AGE),
    values_from = Total_POP
  ) %>%
  filter(!str_detect(LSOA_CODE, 'W')) %>%
  left_join(imd_2011_2021, by=c("LSOA_CODE"="LSOA_CODE", "LSOA_DATE"="LSOA_DATE")) %>%
  group_by(YEAR, LSOA_CODE) %>%
  mutate(TOTAL_POP = MALE_0_4 + 
           MALE_5_14 + 
           MALE_15_44 + 
           MALE_45_64 +
           MALE_65_74 +
           MALE_75_84 +
           MALE_85_PLUS +
           FEMALE_0_4 +
           FEMALE_5_14 +
           FEMALE_15_44 +
           FEMALE_45_64 +
           FEMALE_65_74 +
           FEMALE_75_84 +
           FEMALE_85_PLUS,
         ADJUSTED_POP = (2.354*MALE_0_4 + 
                           1*MALE_5_14 + 
                           0.913*MALE_15_44 + 
                           1.373*MALE_45_64 +
                           2.531*MALE_65_74 +
                           3.254*MALE_75_84 +
                           3.193*MALE_85_PLUS +
                           2.241*FEMALE_0_4 +
                           1.030*FEMALE_5_14 +
                           1.885*FEMALE_15_44 +
                           2.115*FEMALE_45_64 +
                           2.820*FEMALE_65_74 +
                           3.301*FEMALE_75_84 +
                           3.090*FEMALE_85_PLUS)* 1.054^IMD_HEALTH
         
  )

# normalise the adjusted population

adj_pop_norm = adj_pop_df %>% 
  group_by(YEAR) %>%
  summarise(TOTAL = sum(TOTAL_POP), ADJ=sum(ADJUSTED_POP)) %>%
  mutate(AF=TOTAL/ADJ) %>%
  select(YEAR,AF) %>%
  inner_join(adj_pop_df) %>%
  mutate(NORMALISED_ADJ_POP = ADJUSTED_POP*AF) %>%
  select(YEAR,LSOA_CODE,NEED_ADJ_POP=NORMALISED_ADJ_POP,TOTAL_POP)

### load the attribution datasets ###

temp = list.files(path = "data/attribution", pattern="\\.csv$", full.names = TRUE)

lsoa_attributions <- lapply(temp, read.csv)

#lsoa_attributions <- bulk_import_csv_files(iau_bucket, attribution_prefix)

for (i in seq_along(lsoa_attributions)) {
  names(lsoa_attributions[[i]]) <- c("PUBLICATION","EXTRACT_DATE","PRACTICE_CODE","PRACTICE_NAME","LSOA_CODE","SEX","NUMBER_OF_PATIENTS")
}

lsoa_attributions_df <- as.data.frame(do.call(rbind, lsoa_attributions))

lsoa_prac_pc = lsoa_attributions_df %>%
  mutate(YEAR = as.numeric(str_sub(EXTRACT_DATE, start = -4))) %>%
  select(YEAR, PRACTICE_CODE, PRACTICE_NAME, LSOA_CODE, NUMBER_OF_PATIENTS) %>%
  group_by(PRACTICE_CODE, YEAR) %>%
  mutate(total_pat_in_practice = sum(NUMBER_OF_PATIENTS),
         pc = NUMBER_OF_PATIENTS / total_pat_in_practice) %>%
  select(-NUMBER_OF_PATIENTS, -total_pat_in_practice) %>%
  ungroup() %>%
  mutate(LSOA_DATE = case_when(YEAR<2021 ~ 2011,
                               YEAR>2020 ~2021))

# assign IMD at the practice level using the attributions dataset
prac_imd = lsoa_prac_pc %>% 
  inner_join(imd_2011_2021, by=c("LSOA_CODE", "LSOA_DATE")) %>% 
  inner_join(adj_pop_norm, by=c("LSOA_CODE", "YEAR")) %>%
  group_by(YEAR,PRACTICE_CODE) %>%
  summarise(IMD_SCORE_PROP=sum(pc*TOTAL_POP*IMD_SCORE), TOTAL_POP=sum(pc*TOTAL_POP)) %>% #calculates number of people with each IMD score
  mutate(IMD_SCORE=IMD_SCORE_PROP/TOTAL_POP) %>% #takes the average IMD score of all the people
  ungroup() %>%
  select(YEAR,PRACTICE_CODE,IMD_SCORE,TOTAL_POP)

prac_imd = prac_imd %>% 
  group_by(YEAR) %>%
  arrange(-IMD_SCORE) %>%
  mutate(CUM_POP = cumsum(TOTAL_POP), PROP = CUM_POP/max(CUM_POP),
         #IMD_DECILE=cut(PROP,10,labels=FALSE),
         IMD_QUINTILE = ntile(PROP,5)) %>%
  select(YEAR,PRACTICE_CODE,IMD_SCORE,IMD_QUINTILE)


### load workforce datasets

temp_workforce = list.files(path = "data/workforce", pattern="\\.csv$", full.names = TRUE)

september_records <- temp_workforce %>% str_subset(pattern = "September")


workforce_datasets <-   lapply(
  september_records, function(x) {
    print(x)
    split_path = strsplit(x, " ")
    year = split_path[[1]][[6]]
    workforce <- read.csv(x)
    
    workkforce_relevant_cols <- workforce %>%
      select(PRAC_CODE, PRAC_NAME, TOTAL_GP_FTE, TOTAL_GP_EXTGL_FTE, TOTAL_GP_EXL_FTE, TOTAL_GP_RET_FTE, GP_SOURCE) %>%
      mutate(YEAR = as.numeric(year))
    
  }
)

workforce_df <- as.data.frame(do.call(rbind, workforce_datasets))


attribute_gps_to_lsoa <- lsoa_prac_pc %>%
  filter(!str_detect(LSOA_CODE, 'W|NO2011')) %>%
  inner_join(workforce_df, by=c("PRACTICE_CODE" = "PRAC_CODE", "YEAR" = "YEAR")) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    TOTAL_GP_EXTGL_FTE = as.numeric(TOTAL_GP_EXTGL_FTE),
    TOTAL_GP_RET_FTE = as.numeric(TOTAL_GP_RET_FTE),
    pc_fte_gps = pc*(TOTAL_GP_EXTGL_FTE-TOTAL_GP_RET_FTE)
  ) %>%
  group_by(LSOA_CODE, YEAR) %>%
  summarise(gps_lsoa = sum(pc_fte_gps, na.rm=T)) %>%
  left_join(adj_pop_norm, by=c("LSOA_CODE"="LSOA_CODE", "YEAR"="YEAR")) %>%
  mutate(LSOA_DATE = case_when(YEAR %in% c(2017:2020) ~ 2011,
                               YEAR %in% c(2021:2023) ~ 2021)) %>%
  left_join(imd_2011_2021, by=c("LSOA_CODE"="LSOA_CODE", "LSOA_DATE"= "LSOA_DATE"))


workforce_imd <- attribute_gps_to_lsoa %>%
  group_by(IMD_QUINTILE, YEAR) %>%
  summarise(total_gps = sum(gps_lsoa, na.rm=T),
            total_population = sum(NEED_ADJ_POP, na.rm=T)) %>%
  mutate(gps_per_pop = 100000*total_gps/total_population) %>%
  na.omit()

write.csv(workforce_imd, "workforce.csv")

### QOF graphs

temp_qof = list.files(path = "data/qof", pattern="\\.csv$", full.names = TRUE)

# extract only the csv file names into a list

qof_datasets <-   lapply(
  temp_qof, function(x) {
    print(x)
    
    split_path = strsplit(x, "-")
    path_year = split_path[[1]][[1]]
    year = as.numeric(gsub(".*?([0-9]+).*", "\\1", path_year))
    
    qof_scores <- read.csv(x)
    
    if(year == 2017) {
      
      keep <- qof_scores[, c("Practice.code", "Achievement.score..max..559.")]
      keep$Achievement.score..max..559. <- as.numeric(keep$Achievement.score..max..559.)
      colnames(keep) <- c("PRACTICE_CODE", "TOTAL_POINTS")
      keep$YEAR = as.numeric(year)
      
    } else if(year %in% c(2018:2019)) {
      
      keep <- qof_scores[, c("Practice.code", "Achievement.score..559.max.")]
      keep$Achievement.score..559.max. <- as.numeric(keep$Achievement.score..559.max.)
      colnames(keep) <- c("PRACTICE_CODE", "TOTAL_POINTS")
      keep$YEAR = as.numeric(year)
      
    } else if(year %in% c(2021:2022)) {
      
      keep <- qof_scores[, c("Practice.code", "Achievement.score..635.max.")]
      keep$Achievement.score..635.max. <- as.numeric(keep$Achievement.score..635.max.)
      colnames(keep) <- c("PRACTICE_CODE", "TOTAL_POINTS")
      keep$YEAR = as.numeric(year)
      
    }
    return(keep)
  }
)

qof_df <- as.data.frame(do.call(rbind, qof_datasets))

qof_imd <- inner_join(qof_df, prac_imd, by=c("PRACTICE_CODE", "YEAR")) %>%
  group_by(YEAR, IMD_QUINTILE) %>%
  summarise(POINTS=mean(TOTAL_POINTS, na.rm=T)) %>%
  mutate(
    percentage = case_when(
      YEAR %in% c(2017:2020) ~ round((POINTS/559)*100,2),
      YEAR %in% c(2021:2023) ~ round((POINTS/635)*100,2)
    )
  )

write.csv(qof_imd, "qof.csv")

## finance datasets

temp_finance = list.files(path = "data/payments", pattern="\\.csv$", full.names = TRUE)

finance_datasets <-   lapply(
  temp_finance, function(x) {
    print(x)
    
    year <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x))
    
    finances <- read.csv(x)
    
    PRACTICE_CODE <- finances[, "Practice.Code"]
    WEIGHTED_PATIENTS  <- finances[, grep("number.of.weighted.patients", tolower(names(finances)))]
    PAYMENTS <- finances[, grep("Total.NHS.Payments.to.General.Practice.Minus.Deductions", names(finances))]
    print(length(PAYMENTS))
    
    if(length(PAYMENTS) < 10) {
      PAYMENTS <- finances[, "Total.NHS.Payments.to.General.Practice.Minus.Deductions"]
    }
    
    YEAR <- 2000+year
    print(YEAR)
    
    keep <- as.data.frame(cbind(PRACTICE_CODE, WEIGHTED_PATIENTS, PAYMENTS))
    keep$YEAR <- YEAR
    
    return(keep)
  }
)

finances_df <- as.data.frame(do.call(rbind, finance_datasets))

finance_imd <- finances_df %>%
  mutate(YEAR = as.numeric(YEAR),
         WEIGHTED_PATIENTS = as.numeric(WEIGHTED_PATIENTS),
         PAYMENTS = as.numeric(PAYMENTS)) %>%
  left_join(prac_imd, by=c("YEAR", "PRACTICE_CODE")) %>%
  mutate(IMD_QUINTILE = as.factor(IMD_QUINTILE)) %>%
  group_by(IMD_QUINTILE, YEAR) %>%
  summarise(TOTAL_PAYMENTS = sum(PAYMENTS, na.rm=T),
            TOTAL_WEIGHTED = sum(WEIGHTED_PATIENTS, na.rm=T)) %>%
  mutate(PAYMENT_PER_WEIGHTED_PATIENT = TOTAL_PAYMENTS/TOTAL_WEIGHTED) %>%
  na.omit()

write.csv(finance_imd, "finance.csv")

