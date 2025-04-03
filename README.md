
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pf

<!-- badges: start -->
<!-- badges: end -->

The goal of pf_app is to provide tools for working with ProjectFacts
data and easy reporting of the data.

## Installation

You can install `pf` with `remotes` via

``` r
remotes::install_github("CTU-Bern/pf")
```

Once it’s installed, it can be updated via

``` r
pf::update_pf()
```

## Example

``` r
library(pf)
```

The following code is used to update the data on the R drive.

``` r
# load the data from R
all_tabs <- getPFData()
# download data via ODBC
all_tabs <- getPFData(NULL)

all_tabs <- decodeAllCustomFields(all_tabs)

# save the data to R
savePFdata(all_tabs)
```

Regular usage would be e.g.:

``` r
# load the data from R
all_tabs2 <- getPFData()

dat <- prepTime(all_tabs)
```

Hours spent by DM for set up and STA for analysis:

``` r
library(tidyverse)
d <- dat %>% 
  dplyr::mutate(
    tokeep = stringr::str_detect(.$finart_Name, "Database [Ss]et-up|Data [Aa]nalysis|Statist") |
      stringr::str_detect(.$finart_Name1, "Stat")) %>% 
  dplyr::select(ctu_projectName, starts_with("finart"), starts_with("ProjectName"), tokeep) |> View()
  dplyr::filter(tokeep) %>% 
  dplyr::mutate(div = stringr::word(finart_Name, end = 2),
                div = stringr::str_to_sentence(div), 
                div = case_when(div == "Regular reports" ~ "Data analysis",
                                div == "Study planning" ~ "Data analysis",
                                TRUE ~ div)) %>% 
  dplyr::group_by(ctu_projectName, div) %>% 
  dplyr::summarize(billable_time = sum(billable_time),
            nonbillable_time = sum(nonbillable_time)) %>% 
  dplyr::mutate(billable_time = billable_time/60,
                nonbillable_time = nonbillable_time/60)

writexl::write_xlsx(d, "../DBsetup_STA_time_may2022.xlsx")
d %>% 
  ggplot(aes(x = billable_time)) +
  geom_histogram() +
  facet_wrap(~div)
```

### Reports

#### CTU quarterly report

``` r
rmarkdown::render(input = "R:/Projectfacts/ODBC/pf_app/vignettes/quarterly.Rmd", 
                  output_file=file.path('R:/Projectfacts/ODBC/reports', 
                                        paste0("CTUQuarterly_",
                                               Sys.Date(),
                                               ".html")))
```

### Miscellaneous other things

#### Email addresses of project contacts

``` r
library(tidyverse)
View(all_tabs$project)
View(all_tabs$contactfield)

tmp <- all_tabs$project %>% 
  filter(!is.na(FK_CUSTOMERCONTACT)) %>% 
  left_join(all_tabs$crmkontakt, 
            by = c("FK_CUSTOMERCONTACT" = "PK_CRMKONTAKT")) %>% 
  left_join(all_tabs$customer %>% 
              select(PK_CUSTOMER, Path) %>% 
              rename(customer_name = Path), 
            by = c("FK_CUSTOMER.x" = "PK_CUSTOMER")) %>% 
  left_join(all_tabs$contactfield %>% 
              filter(TYPE == 30),
            by = c("FK_CUSTOMERCONTACT" = "FK_CONTACT")) %>% 
  select(Vorname, Nachname, VALUE, customer_name) %>% 
  rename(Email = VALUE) %>% 
  distinct(Vorname, Nachname, Email, .keep_all = TRUE) 

writexl::write_xlsx(tmp, "../reports/emailaddresses.xlsx")
```
