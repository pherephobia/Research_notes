setwd("/Users/sanghoonpark/Library/CloudStorage/Dropbox/Dataset")
library(estimatr);library(geomtextpath); library(tidyverse)
theme_set(theme_bw())
patent <- ezpickr::pick("INPACT-S.dta")
#patent_citation <- ezpickr::pick("INPACT-S-Citations.dta")
names(patent)
# appln_auth - 특허 출원을 제출한 국가의 2자리 코드
# appln_auth_iso3 - 특허 출원을 제출한 국가의 3자리 코드
# appln_auth_name - 특허 출원을 제출한 국가의 표준화된 이름
# person_ctry_code - 특허 출원인이 거주하는 국가의 2자리 코드
# person_ctry_code_iso3 - 특허 출원인이 거주하는 국가의 3자리 코드
# person_ctry_code_name - 특허 출원인이 거주하는 국가의 표준화된 이름
# applyn_filing_year - 계산 대상인 특정 출원이 제출된 연도
# (참고: 하나의 기술이 서로 다른 연도에 서로 다른 관할권에 출원되는 것은 가능하며 아마도 흔한 일이다.
#        이 경우 각각은 첫 번째 출원 연도가 아닌 특정 출원이 제출된 연도에 계산된다.)
# isic_rev3_2 - ISIC 개정 3에 따른 특허의 2자리 산업 코드.
## Tabulation category C: Mining and Quarrying
### 10 Mining of Coal and Lignite; Extraction of Peat
### 11 Extraction of Crude Petroleum and Natural Gas; Service activities incidental to Oil and Gas extraction, excluding surveying
### 12 Mining of Uranium and Thorium Ores
### 13 Mining of Metal Ores
### 14 Other Mining and Quarrying
## Tabulation category D: Manufacturing
### 15 Manufacture of Food Products and Beverages
### 16 Manufacture of Tobacco Products
### 17 Manufacture of Textiles
### 18 Manufacture of Wearing Apparel; Dressing and Dyeing of Fur
### 19 Tanning and Dressing of Leather; Manufacture of Luggage, Handbags, Saddlery, Harness and Footwear
### 20 Manufacture of Wood and of Products of Wood and Cork, except Furniture; Manufacture of articles of Straw and Plaiting Materials
### 21 Manufacture of Paper and Paper Products
### 22 Publishing, Printing and Reproduction of Recorded Media
### 23 Manufacture of Coke, Refined Petroleum Products and Nuclear Fuel
### 24 Manufacture of Chemicals and Chemical Products
### 25 Manufacture of Rubber and Plastics Products
### 26 Manufacture of Other Non-Metallic Mineral Products
### 27 Manufacture of Basic Metals
### 28 Manufacture of Fabricated Metal Products, except Machinery and Equipment
### 29 Manufacture of Machinery and Equipment NEC **
### 30 Manufacture of Office, Accounting and Computing Machinery
### 31 Manufacture of Electrical Machinery and Apparatus NEC **
### 32 Manufacture of Radio, Television and Communication Equipment and Apparatus
### 33 Manufacture of Medical, Precision and Optical Instruments, Watches and Clocks
### 34 Manufacture of Motor Vehicles, Trailers and Semi-Trailers
### 35 Manufacture of other Transport Equipment
### 36 Manufacture of Furniture; Manufacturing NEC **
### 37 Recycling
## Tabulation category E: Electricity, Gas and Water Supply
### 40 Electricity, Gas, Steam and Hot Water Supply
### 41 Collection. Purification and Distribution of Water
## Tabulation category F: Construction
### 45 Construction
# patents_applt - 출원인 국가별로 계산된 양자 간, 산업별, 연도별 특허 수 (발명자 국가와는 반대)

patent |> mutate(COWcode = countrycode::countrycode(appln_auth_iso3, "iso3c", "cown")) ->
  patent

patent |> 
  group_by(appln_auth_iso3, appln_filing_year) |> 
  summarize(patent_n = n()) |> ungroup() -> patent_number

patent |> group_by(appln_auth_iso3, appln_filing_year, isic_rev3_2) |> count() |> 
  mutate(isic_rev3_2_fct = 
           factor(isic_rev3_2,
                  levels = c(sort(unique(patent$isic_rev3_2))))) -> patent_diverse

patent_diverse |> ungroup() -> patent_diverse

patent_diverse |> dplyr::filter(!n == 0L) -> patent_diverse

create_dummies <- function(df, a_factor) {
  
  if (!is.data.frame(df) | !is_tibble(df)) stop('first arg must be data.frame')
  
  new_df <- df
  new_df <- new_df[ , a_factor] %>%
    select(my_factor = 1)
  
  if (!is.factor(new_df$my_factor)) stop('second arg must be a factor')
  
  factor_levels <- levels(new_df$my_factor)
  
  for (i in 1:length(factor_levels)) {
    var <- new_df$my_factor == factor_levels[i]
    new_df[[paste0(factor_levels[i], '_', i)]] <- var
  }
  
  new_df <- new_df %>% select(-my_factor) %>%
    mutate_all(as.integer)
  df <- bind_cols(df, new_df)
  
  return(df)
}

create_dummies(patent_diverse, "isic_rev3_2_fct") -> patent_diverse_wide

names(patent_diverse_wide)
patent_diverse_wide |> 
  rowwise() |>  
  mutate(diverse = sum(c_across(6:36))) |> 
  dplyr::select(c(1, 2, 3, 5, 37)) |> 
  ungroup() -> patent_diverse_short

patent_diverse_short

patent_diverse_short |> 
  group_by(appln_auth_iso3, appln_filing_year) |> 
  summarize(diverse = length(isic_rev3_2_fct)) -> patent_diverse_sum

patent_number |> 
  drop_na(appln_auth_iso3, appln_filing_year) |> 
  left_join(patent_diverse_sum |> drop_na(appln_auth_iso3, appln_filing_year), 
            by = c("appln_auth_iso3", "appln_filing_year")) ->
  patent_diverse_number

vdemdata::vdem -> vdem

vdem |> 
  mutate(iso3 = countrycode::countrycode(country_name, "country.name", "iso3c")) ->
  vdem

vindoc_cy_full <- readRDS("~/Library/CloudStorage/Dropbox/Dataset/vindoc_cy_full.rds")

vdem |> drop_na(iso3) |> left_join(
  vindoc_cy_full |> 
    mutate(iso3 = countrycode::countrycode(country_name, "country.name", "iso3c")) |> 
    dplyr::select(iso3, year, 
                  contains("v2xed_ed_inpt"), contains("v2xed_ed_poed"), contains("v2xed_ed_inco"),
                  contains("v2xed_ed_con"),
                  contains("v2edpoledprim"), contains("v2edpoledsec"), contains("v2edideol"),
                  contains("v2xed_ed_cent"), contains("v2xed_ed_ctag"), contains("v2edcentcurrlm"),
                  contains("v2edcenttxbooks"), contains("v2edteautonomy"), contains("v2edteunionindp"),
                  contains("v2edtehire"), contains("v2edtefire"), contains("v2edmath")) |> 
    drop_na(iso3), 
  by = c("iso3", "year")
) -> vdem

vdem_indoc |> 
  left_join(patent_diverse_number, 
            by = c("iso3" = "appln_auth_iso3", 
                   "year" = "appln_filing_year")) ->
  vdem_patent
gc()

#remotes::install_github("ropengov/rqog")
library(rqog)
qog <- read_qog(which_data = "standard", data_type = "time-series")
qog |> names()
qog$ccodealp
qog |> dplyr::select(ccodealp, year, wdi_fdiin, wdi_expstup, wdi_expstus, wdi_expstut) -> subqog
meta_std_ts_2023[grepl("wdi_eduprp", meta_std_ts_2023$code, ignore.case = TRUE),]

vdem_patent |> left_join(
  subqog |> drop_na(ccodealp, year),
  by = c("iso3" = "ccodealp", "year")
) -> vdem_patent

vdem_patent |> dplyr::filter(!COWcode %in% c(365, 710)) |> 
  group_by(e_boix_regime, year) |> 
  summarize(mean = mean(patent_n, na.rm = T)) |> dplyr::filter(year > 1979) |> 
  mutate(regime = factor(
    e_boix_regime, levels = c(0, 1),
    labels = c("Autocracy", "Democracy")
  )) ->
  sum_regime_patent

vdem_patent |> dplyr::filter(!COWcode %in% c(365, 710)) |> 
  group_by(e_boix_regime, year) |> 
  summarize(mean = mean(diverse, na.rm = T)) |> dplyr::filter(year > 1979) |> 
  mutate(regime = factor(
    e_boix_regime, levels = c(0, 1),
    labels = c("Autocracy", "Democracy")
  ))->
  sum_regime_diverse

end_patent_n <- sum_regime_patent |> 
  drop_na(mean, regime) |> 
  dplyr::filter(year == max(year, na.rm = T))

sum_regime_patent |> drop_na() |> 
  mutate(regime = factor(
    e_boix_regime, levels = c(0, 1),
    labels = c("Autocracy", "Democracy")  
    )) |> 
  ggplot(aes(x = year, y = mean, color = regime, group = regime)) +
  geom_line(show.legend = F) + 
  ggrepel::geom_label_repel(
    data = end_patent_n,
    aes(label = regime, x = year, y = mean), show.legend = F) +
  geom_point(shape = 21, fill = "white", show.legend = F) +
  scale_x_continuous(breaks = c(seq(1980, 2020, 2))) +
  scale_y_continuous(breaks = c(seq(0, 1600, 100))) +
  theme_bw()

end_diverse_n <- sum_regime_diverse |> 
  drop_na(mean, regime) |> 
  dplyr::filter(year == max(year, na.rm = T))

sum_regime_diverse |> drop_na() |> 
  mutate(regime = factor(
    e_boix_regime, levels = c(0, 1),
    labels = c("Autocracy", "Democracy")
  )) |> 
  ggplot(aes(x = year, y = mean, color = regime, group = regime)) +
  geom_line(show.legend = F) + 
  ggrepel::geom_label_repel(
    data = end_diverse_n,
    aes(label = regime, x = year, y = mean), show.legend = F) +
  geom_point(shape = 21, fill = "white", show.legend = F) +
  scale_x_continuous(breaks = c(seq(1980, 2020, 2))) +
  scale_y_continuous(breaks = c(seq(0, 36, 2)), limits = c(20, 30)) +
  theme_bw()

### Industry categorization -------

vdem_patent |> 
  mutate(
    industry_codes = 
      case_when(
        isic_rev3_2 > 9 & isic_rev3_2 < 15 ~ 1L,
        isic_rev3_2 > 14 & isic_rev3_2 < 38 ~ 2L,
        isic_rev3_2 > 39 & isic_rev3_2 < 42 ~ 3L,
        isic_rev3_2 > 44 ~ 4L,
        T ~ NA_integer_),
    industry_codes = factor(industry_codes,
                            levels = c(1, 2, 3, 4),
                            labels = c("Mining", "Manufacturing", 
                                       "Energy", "Construction"))) -> vdem_patent

### 10 Mining of Coal and Lignite; Extraction of Peat
### 11 Extraction of Crude Petroleum and Natural Gas; Service activities incidental to Oil and Gas extraction, excluding surveying
### 12 Mining of Uranium and Thorium Ores
### 13 Mining of Metal Ores
### 14 Other Mining and Quarrying
## Tabulation category D: Manufacturing
### 15 Manufacture of Food Products and Beverages
### 16 Manufacture of Tobacco Products
### 17 Manufacture of Textiles
### 18 Manufacture of Wearing Apparel; Dressing and Dyeing of Fur
### 19 Tanning and Dressing of Leather; Manufacture of Luggage, Handbags, Saddlery, Harness and Footwear
### 20 Manufacture of Wood and of Products of Wood and Cork, except Furniture; Manufacture of articles of Straw and Plaiting Materials
### 21 Manufacture of Paper and Paper Products
### 22 Publishing, Printing and Reproduction of Recorded Media
### 23 Manufacture of Coke, Refined Petroleum Products and Nuclear Fuel
### 24 Manufacture of Chemicals and Chemical Products
### 25 Manufacture of Rubber and Plastics Products
### 26 Manufacture of Other Non-Metallic Mineral Products
### 27 Manufacture of Basic Metals
### 28 Manufacture of Fabricated Metal Products, except Machinery and Equipment
### 29 Manufacture of Machinery and Equipment NEC **
### 30 Manufacture of Office, Accounting and Computing Machinery
### 31 Manufacture of Electrical Machinery and Apparatus NEC **
### 32 Manufacture of Radio, Television and Communication Equipment and Apparatus
### 33 Manufacture of Medical, Precision and Optical Instruments, Watches and Clocks
### 34 Manufacture of Motor Vehicles, Trailers and Semi-Trailers
### 35 Manufacture of other Transport Equipment
### 36 Manufacture of Furniture; Manufacturing NEC **
### 37 Recycling
## Tabulation category E: Electricity, Gas and Water Supply
### 40 Electricity, Gas, Steam and Hot Water Supply
### 41 Collection. Purification and Distribution of Water
## Tabulation category F: Construction
### 45 Construction

### DVs Distributions ---------

vdem_patent |> 
  mutate(
    regime = if_else(e_boix_regime == 1L, "Democracy",
                     if_else(e_boix_regime == 0L, "Autocracy", NA_character_)),
    regime = factor(regime,
                    levels = c("Autocracy", "Democracy"))) -> vdem_patent
vdem_patent |>   
  drop_na(regime) |> 
  ggplot(aes(x = year, y = log(patent_n+1), group = year, color = regime)) +
  geom_boxplot(show.legend = F) + facet_wrap(~regime)

vdem_patent |>   
  drop_na(regime) |> 
  ggplot(aes(x = year, y = diverse, group = year, color = regime)) +
  geom_boxplot(show.legend = F) + facet_wrap(~regime)

vdem_patent |> dplyr::filter(regime %in% "Autocracy") |> 
  pull(diverse) |> hist()

vdem_patent |> dplyr::filter(regime %in% "Democracy") |> 
  pull(diverse) |> hist()

vdem_patent |> #dplyr::filter(regime %in% "Autocracy") |> 
  ggplot(aes(x = diverse, fill = regime, color = regime)) +
  geom_histogram(alpha = 0.5)

vdem_patent |> group_by(year, regime) |> count() |> drop_na(regime) |> 
  dplyr::filter(year > 1980) |> print(n = Inf)

vdem_patent |> group_by(year, regime) |> 
  summarize(
    mean_n = mean(patent_n, na.rm = T),
    mean_diverse = mean(diverse, na.rm = T)) |>
  mutate(
    demo_auto_n = mean_n - dplyr::lag(mean_n, n = 1, order_by = year),
    demo_auto_diverse = mean_diverse - dplyr::lag(mean_diverse, n = 1, order_by = year)) |> 
  drop_na(regime, mean_n, mean_diverse) |> 
  dplyr::filter(year > 1980) |> print(n = Inf) -> summary_dvs

summary_dvs |> ungroup() |> drop_na(demo_auto_n) |> 
  ggplot(aes(x = year, y = demo_auto_n)) +
  geom_path() + geom_point()

summary_dvs |> ungroup() |> drop_na(demo_auto_diverse) |> 
  ggplot(aes(x = year, y = demo_auto_diverse)) +
  geom_path() + geom_point()

### Bivariate Relationship -----------------------------------------------------

vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                       if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                      regime = factor(regime,
                                      levels = c("Autocracy", "Democracy")),
                      iso3 = factor(iso3),
                      year_fa = factor(year),
                      year = as.numeric(as.character(year)),
                      lnpatent = log(patent_n+1)) -> vdem_patent

vdem_patent |> ungroup() |> dplyr::filter(year > "1979") |> 
  dplyr::filter(regime %in% "Democracy") |> 
  drop_na(year, iso3, v2xed_ed_cent.x, regime) |> 
  group_by(iso3) |> mutate(n = n()) |> 
  dplyr::filter(n == 42) |> 
  ggplot(aes(x = year, y = v2xed_ed_cent.x)) + geom_line() + geom_point() +
  facet_wrap(~iso3)

library(estimatr)

vdem_patent |> 
  mutate(lnpatent = log(patent_n+1)) |> 
  dplyr::select(lnpatent , v2xed_ed_cent.x , v2xed_ed_ctag.x , e_gdppc , e_pop , 
                v2peprisch , v2pesecsch , v2petersch , #v2edcenttxbooks.x, v2edcentcurrlm.x,
                #v2edteautonomy.x , v2edteunionindp.x , v2edtehire.x , v2edtefire.x ,
                v2edmath.x, v2stfisccap, v2svstterr ) |> drop_na() |> 
  cor()
vdem_patent$v2xed

lm_robust(
  lnpatent ~ (v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + wdi_fdiin + 
    
    #
    v2edmath.x +
    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)) |> dplyr::filter(regime == "Democracy"),
  se = "stata") -> number_demo

lm_robust(
  lnpatent ~ (v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + wdi_fdiin + 
    
    #
    v2edmath.x +
    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)) |> dplyr::filter(regime == "Autocracy"),
  se = "stata") -> number_auto

lm_robust(
  lnpatent ~ regime*(v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + wdi_fdiin + 
    
    #
    v2edmath.x +
    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)),
  se = "stata") -> number_full

lm_robust(
  diverse ~ (v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + wdi_fdiin + 
    
    #
    v2edmath.x +
    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)) |> 
    dplyr::filter(regime %in% "Democracy"),
  se = "stata") -> diverse_demo

lm_robust(
  diverse ~ (v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + wdi_fdiin + 
    
    #
    v2edmath.x +
    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)) |> 
    dplyr::filter(regime %in% "Autocracy"),
  se = "stata") -> diverse_auto

lm_robust(
  diverse ~ regime*(v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + wdi_fdiin + 
    
    #
    v2edmath.x +
    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)),
  se = "stata") -> diverse_full
coefnames <- c("(Constant)", "Centralized curriculum", 
               "Centralized textbook approval", 
               "Political education effort in education", "GDPpc",
               "Population", "Primary school enrolment", 
               "Secondary school enrolment", "Tertiary school enrolment",
               "State fiscal capacity",
               "Foreign Direct Investment Inflow",
               "Math and Science", "Democracy",
               "Democracy$\\times$Centralized curriculum",
               "Democracy$\\times$Centralized textbook approval",
               "Democracy$\\times$Political education effort in education")
texreg::screenreg(list(number_demo, number_auto, number_full, 
                       diverse_demo, diverse_auto, diverse_full),
                  omit.coef = "as.factor|year|iso3", include.ci = F, digits = 3,
                  custom.model.names = c("M1: Number Democracy",
                                         "M2: Number Autocracy",
                                         "M3: Number Full",
                                         "M4: Diverse Democracy",
                                         "M5: Diverse Autocracy",
                                         "M6: Diverse Full"),
                  custom.coef.names = coefnames,
                  reorder.coef = c(2, 3, 4, 12, 13, 14, 15, 16, 5, 6, 7, 8, 9, 10, 11, 1),
                  custom.gof.rows = list(
                    "Country-fixed" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                    "Year-fixed" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
                  ))

lm_robust(
  lnpatent ~ regime*v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap +
    #
    v2edmath.x,
   # iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)),
  se = "stata") -> number_full

number_full |> 
  marginaleffects::plot_predictions(condition = list("v2edcentcurrlm.x", "regime"))  + 
  labs(x = "\nCentralized curriculum", y = "Ln(Numbers of Patent + 1)\n") +
  #scale_x_continuous(breaks = c(seq(0, 1, 0.25))) +
  theme_bw()

lm_robust(
  lnpatent ~ v2edcentcurrlm.x + v2edcenttxbooks.x + regime*v2xed_ed_poed.x + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap +
    #
    v2edmath.x,# +
#    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)),
  se = "stata") -> number_full




number_full |> 
  marginaleffects::plot_predictions(condition = list("v2xed_ed_poed.x", "regime")) + 
  labs(x = "\nPolitical education effort in education", y = "Ln(Numbers of Patent + 1)\n") +
  scale_x_continuous(breaks = c(seq(0, 1, 0.25))) +
  theme_bw()

lm_robust(
  lnpatent ~ v2edcentcurrlm.x + regime*v2edcenttxbooks.x + v2xed_ed_poed.x + 
    e_gdppc + e_pop + 
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap +
    #
    v2edmath.x,# +
#    iso3 + year,
  data = vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                                if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                               regime = factor(regime,
                                               levels = c("Autocracy", "Democracy")),
                               iso3 = factor(iso3),
                               year = factor(year),
                               lnpatent = log(patent_n+1)),
  se = "stata") -> number_full

number_full |> 
  marginaleffects::plot_predictions(condition = list("v2edcenttxbooks.x", "regime"))  + 
  labs(x = "\nCentralized textbook approval", y = "Ln(Numbers of Patent + 1)\n") +
  #scale_x_continuous(breaks = c(seq(0, 1, 0.25))) +
  theme_bw()


diverse_full |> 
  marginaleffects::plot_predictions(condition = list("v2edcentcurrlm.x", "regime"))  + 
  labs(x = "\nCentralized curriculum", y = "Ln(Numbers of Patent + 1)\n") +
  #scale_x_continuous(breaks = c(seq(0, 1, 0.25))) +
  theme_bw()

diverse_full |> 
  marginaleffects::plot_predictions(condition = list("v2edcenttxbooks.x", "regime"))  + 
  labs(x = "\nCentralized textbook approval", y = "Ln(Numbers of Patent + 1)\n") +
  #scale_x_continuous(breaks = c(seq(0, 1, 0.25))) +
  theme_bw()



lm(
  lnpatent ~ regime + (v2edcentcurrlm.x + v2edcenttxbooks.x + v2xed_ed_poed.x) + 
    e_gdppc +  
    v2peprisch + v2pesecsch + v2petersch + v2stfisccap + v2svstterr +
    #
    v2edmath.x,
    #iso3, # + year,
  data = vdem_patent |>
    mutate(regime = if_else(v2x_regime > 1, "Democracy",
                            if_else(v2x_regime < 2, "Autocracy", NA_character_)),
           regime = factor(regime,
                           levels = c("Autocracy", "Democracy")),
           iso3 = factor(iso3), year = factor(year),
           lnpatent = log(patent_n+1))) |> 
  DAAG::vif()

vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                       if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                      regime = factor(regime,
                                      levels = c("Autocracy", "Democracy"))) |> 
  group_by(iso3, regime) |> 
  summarize(mean_currim = mean(v2edcentcurrlm.x, na.rm = T)) |> drop_na() |> 
  ggplot(aes(x = reorder(iso3, mean_currim), y = mean_currim, fill = regime, color = regime)) + geom_col() +
  coord_flip() + facet_wrap(~regime)

vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                       if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                      regime = factor(regime,
                                      levels = c("Autocracy", "Democracy"))) |> 
  group_by(regime) |> drop_na(regime) |> 
  summarize(mean_currim = mean(v2edcentcurrlm.x, na.rm = T))


vdem_patent |> ggplot(aes(x = v2stfisccap, y = log(e_gdppc + 1))) +
  geom_point() + geom_smooth()



  
  #texreg::screenreg(omit.coef = "as.factor",
  #                     digits = 3, include.ci = F)

vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                       if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                      regime = factor(regime,
                                      levels = c("Autocracy", "Democracy")),
                      iso3 = factor(iso3),
                      year = factor(year),
                      lnpatent = log(patent_n+1)) |> 
  group_by(regime) |> 
  summarize(
    mean_centcurrlm = mean(v2edcentcurrlm.x, na.rm = T),
    mean_centtext = mean(v2edcenttxbooks.x, na.rm = T),
    mean_poed = mean(v2xed_ed_poed.x, na.rm = T),
    median_centcurrlm = median(v2edcentcurrlm.x, na.rm = T),
    median_centtext = median(v2edcenttxbooks.x, na.rm = T),
    median_poed = median(v2xed_ed_poed.x, na.rm = T),
    n = n()
  ) |> drop_na()



vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                       if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                      regime = factor(regime,
                                      levels = c("Autocracy", "Democracy")),
                      iso3 = factor(iso3),
                      year = factor(year),
                      lnpatent = log(patent_n+1)) |> 
  ggplot(aes(x = year, y = lnpatent)) + geom_point() + geom_line() +
  facet_wrap(~iso3)



vdem_patent |> mutate(regime = if_else(v2x_regime > 1, "Democracy",
                                       if_else(v2x_regime < 2, "Autocracy", NA_character_)),
                      regime = factor(regime,
                                      levels = c("Autocracy", "Democracy")),
                      iso3 = factor(iso3),
                      year = factor(year),
                      lnpatent = log(patent_n+1)) |> 
  dplyr::select(v2edcentcurrlm.x, v2edcenttxbooks.x, v2xed_ed_poed.x, regime) |> 
  tidyr::gather(indicators, values, -regime) |> 
  drop_na(regime) |> 
  ggplot(aes(x = values, fill = regime)) + geom_density(alpha = 0.4) +
  facet_wrap(~indicators) + theme_bw() + theme(legend.position = "top",
                                               legend.title = element_blank())
library(ggplot2)
vdem_patent |> ggplot(aes(x = year, y = diverse, group = year)) +
  geom_boxplot()
