#' Function to get the booked hours for projects in total and in each year
#' 
#' This code has been adapted for the purpose of producing the graphs for the QMR Yearly Report and CTU Annual Report. 
#' The code gets (among others) customer info (top_CustomerName, CustomerName, CustomerName2, Organization), default project of customer,
#' center info (mono/multi), ordinance info, archiving date of the project, runs prepTime to get total time bookings and time bookings for each year, 
#' categorizes sum of time bookings (< 8h, 9-20h,...).
#' 
#' @param all_tabs result from e.g. getPFData
#' 
#' @return projects
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' x <- projects_ctu_report(all_tabs)



projects_ctu_report <- function(all_tabs){


# get customer info
tmp1 <- pf::constructCustomerParents(all_tabs$customer) %>% 
  dplyr::left_join(all_tabs$customer %>% 
                     dplyr::select(PK_CUSTOMER, FK_DEFAULTPRICELIST) %>%
                     dplyr::left_join(all_tabs$pricerecord %>%
                                        dplyr::select(PK_PRICERECORD, NAME), 
                                      by=c("FK_DEFAULTPRICELIST"="PK_PRICERECORD")),
                   by="PK_CUSTOMER") %>% 
  dplyr::rename(Organization = NAME) %>% 
  # get default project (for for/non profit)
  dplyr::left_join(all_tabs$customer %>% 
                     dplyr::select(PK_CUSTOMER,FK_DEFAULTPROJECT) %>% 
                     dplyr::left_join(all_tabs$project %>% 
                                        dplyr::select(PK_Project,Name),
                                      by=c("FK_DEFAULTPROJECT"="PK_Project")),
                   by="PK_CUSTOMER") %>% 
  dplyr::rename(default_projname = Name) %>% 
  # get project info
  dplyr::left_join(all_tabs$project %>% 
              dplyr::select(PK_Project, FK_CUSTOMER, Path, CaseId, cf_applicable_ordinance_2, cf_study_category,
                     cf_snf, cf_eu, cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, 
                     cf_other_ctu_cro, cf_location, cf_setup_number_of_sites,CreateDate), 
            by=c("PK_CUSTOMER"="FK_CUSTOMER"))  %>% 
  # get archiving date
  dplyr::left_join(all_tabs$projectactivity %>% 
                     dplyr::select("FK_PROJECT","FK_PROJECTSTATEDEFINITION","ClosedDate") %>% 
                     dplyr::left_join(all_tabs$projectstatedefinition %>% 
                                        dplyr::select("PK_PROJECTSTATEDEFINITION","NAME"),
                                      by = c("FK_PROJECTSTATEDEFINITION" = "PK_PROJECTSTATEDEFINITION")
                                      ) %>% 
                     dplyr::filter(NAME == "Study: Archived"),
                   by=(c("PK_Project" = "FK_PROJECT")))  %>%
  # clean up
  dplyr::filter(grepl("P-|FTE", CaseId)) %>% 
  dplyr::select(top_CustomerName, CustomerName, CustomerName2, Organization, default_projname, Path, CaseId, cf_applicable_ordinance_2, cf_study_category,
                cf_snf, cf_eu, cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, 
                cf_other_ctu_cro, cf_location, cf_setup_number_of_sites,CreateDate,ArchivingDate=ClosedDate) %>% 
  dplyr::mutate(projnum = readr::parse_number(gsub('[-]', ' ', CaseId))) %>%
  dplyr::arrange(projnum) %>%
  dplyr::distinct(projnum, Path, .keep_all=TRUE) %>%
  dplyr::mutate(across(projnum, as.character)) %>%
  dplyr::mutate(projnum = stringr::str_pad(projnum, 4, pad = "0")) %>%
  dplyr::rename(Project.name = Path) %>%
  dplyr::relocate(projnum, Project.name, .before = top_CustomerName) 
  

### Select projects and sum up booked time overall
tmp2 <- pf::prepTime(all_tabs)   %>% 
  dplyr::mutate(BookedYear = lubridate::year(BookedDate)) %>% 
  dplyr::filter(grepl("P-|FTE", proj)) %>% 
  dplyr::select(BookedDate, BookedYear, Timespent, ctu_projectName, ctu_division, proj, projnum, projecttype ) %>% 
  # make sure all projectnbrs have 4 digits:
  dplyr::mutate(projnum = formatC(as.integer(projnum),width = 4, format = "d", flag = "0")) %>% 
  dplyr::group_by(projnum, ctu_division) %>% 
  dplyr::summarise(total_time = sum(Timespent, na.rm = TRUE),
                   ProjectName = dplyr::first(ctu_projectName)) %>% 
  dplyr::group_by(projnum) %>%
  dplyr::summarise(total_divisions = paste0(ctu_division, collapse = ","),
                   total_time = sum(total_time, na.rm = TRUE)) %>% 
  dplyr::mutate(total_hrs = total_time/60) %>% 
  dplyr::mutate(total_hrs_cat = cut(total_hrs, 
                                    breaks = c(0,7.9999,20.49,40.49,60.49,100.49,500.49,Inf),
                                    labels = c("< 8h", "8-20h", "21-40h", "41-60h", "61-100h", "101-500h", "> 500h")
                                    )
                )


# Select projects and sum up booked times per year
tmp3 <- pf::prepTime(all_tabs) %>% 
  dplyr::mutate(BookedYear = lubridate::year(BookedDate)) %>% 
  dplyr::filter(grepl("P-|FTE", proj)) %>% 
  dplyr::select(BookedDate, BookedYear, Timespent, ctu_projectName, ctu_division, proj, projnum, projecttype ) %>% 
  # make sure all projectnbrs have 4 digits:
  dplyr::mutate(projnum = formatC(as.integer(projnum),width = 4, format = "d", flag = "0")) %>% 
  dplyr::group_by(projnum, ctu_division,BookedYear) %>%
  dplyr::summarise(time_per_year = sum(Timespent, na.rm = TRUE),
                 ProjectName = dplyr::first(ctu_projectName)) %>% 
  dplyr::group_by(projnum,BookedYear) %>%
  dplyr::summarise(divisions_per_year = paste0(ctu_division, collapse = ","),
                   time_per_year = sum(time_per_year, na.rm = TRUE)) %>% 
  dplyr::mutate(hrs_per_year = time_per_year/60) %>% 
  dplyr::mutate(hrs_per_year_cat = cut(hrs_per_year, 
                                       breaks = c(0,7.9999,20.49,40.49,60.49,100.49,500.49,Inf),
                                       labels = c("< 8h", "8-20h", "21-40h", "41-60h", "61-100h", "101-500h", "> 500h")
                                       )
                )

### join dataframes, create variable centers, ordinance, customer, institution, cat for reports

projects <- tmp1 %>%
  dplyr::left_join(tmp2,by="projnum")  %>% 
  dplyr::right_join(tmp3,by= "projnum",multiple = "all") %>% 
  dplyr::filter(projnum != 9999) %>%
  # get center info
  dplyr::mutate(Centers = dplyr::case_when(
    stringr::str_detect(cf_location, stringr::regex("International", ignore_case = TRUE)) ~ "multicenter (international)",
    stringr::str_detect(cf_setup_number_of_sites, stringr::regex("Single center", ignore_case = TRUE)) ~ "monocenter",
    cf_location=="Swiss" & cf_setup_number_of_sites=="Multicenter" ~ "multicenter (national only)" ) ) %>% 
  # get ordinance info
  dplyr::mutate(
    Ordinance = dplyr::case_when(
      stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("ClinO", ignore_case = TRUE)) ~ "ClinO",    
      stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("HRO", ignore_case = TRUE)) ~ "HRO",
      stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("HRA", ignore_case = TRUE)) ~ "Non-HRA"),
    Ordinance_sub = dplyr::case_when(
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("IMP", ignore_case = TRUE)) ~ "IMP",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Medical Device", ignore_case = TRUE)) ~ "MD",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Other health", ignore_case = TRUE)) ~ "Other",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Further", ignore_case = TRUE)) ~ "Further Use",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Observational", ignore_case = TRUE)) ~ "Sample Data Collection")
    ) %>% 
  dplyr::select(projnum:default_projname,
                Centers:Ordinance_sub,
                CreateDate:hrs_per_year_cat) %>% 
  dplyr::mutate(dplyr::across(c(Centers, Ordinance, Ordinance_sub), 
                              as.factor),
                Centers = factor(Centers, 
                                 levels(Centers)[c(1,3,2)])) 

return(projects)
}

