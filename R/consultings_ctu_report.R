#' Function to get the booked hours for consultings in total and in each year
#' 
#' This code has been adapted for the purpose of producing the graphs for the QMR Yearly Report and CTU Annual Report.
#' The code gets (among others) the createYear of the ticket, customer (top_CustomerName, CustomerName, CustomerName2, Organization), default project of customer,
#' extracts whether it is a grant application consulting, runs prepTime to get total time bookings and time bookings for each year.
#' 
#' @param all_tabs result from e.g. getPFData
#' 
#' @return consultings
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' x <- consultings_ctu_report(all_tabs)



consultings_ctu_report <- function(all_tabs){
  
  consultings <- all_tabs$ticket %>% # table ticket
    dplyr::mutate(CreateYear = lubridate::year(CreateDate)
    ) %>% 
    dplyr::filter(grepl("C-", TICKETID)) %>% 
    
    dplyr::left_join(
      all_tabs$customer %>% # join with table 'customer'
        dplyr::left_join(pf::constructCustomerParents(all_tabs$customer)) %>%
        dplyr::left_join(all_tabs$pricerecord %>% 
                           dplyr::select(PK_PRICERECORD, NAME),
                         by=c("FK_DEFAULTPRICELIST"="PK_PRICERECORD")
                         ) %>% 
        dplyr::select(PK_CUSTOMER, top_CustomerName, CustomerName, CustomerName2,Organization = NAME),
      by = c(FK_CUSTOMER = "PK_CUSTOMER")
    ) %>% 
    
    dplyr::left_join(
      all_tabs$project %>% # join with table 'projects' to get consulting project
        dplyr::select(PK_Project, Name) %>%
        dplyr::rename(default_projname = Name),
      by = c(FK_PROJECT = "PK_Project")
    ) %>% 
    
    dplyr::left_join(
      pf::prepTime(all_tabs) %>% # join with total time bookings (see prepTime.R)
        dplyr::filter(!is.na(FK_TICKET)) %>% 
        dplyr::group_by(FK_TICKET) %>%
        dplyr::summarise(total_time = sum(Timespent, na.rm = TRUE)) %>%
        dplyr::mutate(total_hrs = total_time/60),
      by = c(PK_TICKET = "FK_TICKET")
    ) %>% 
    
    dplyr::right_join(
      pf::prepTime(all_tabs) %>% # join with time bookings per year (see prepTime.R)
        dplyr::filter(!is.na(FK_TICKET)) %>% 
        dplyr::mutate(BookedYear = lubridate::year(BookedDate)) %>% 
        dplyr::group_by(FK_TICKET,BookedYear) %>% 
        dplyr::summarise(time_per_year = sum(Timespent, na.rm = TRUE)) %>% 
        dplyr::mutate(hrs_per_year = time_per_year/60),
      by = c(PK_TICKET = "FK_TICKET"),
      multiple = "all") %>% 
    
    dplyr::select(TICKETID,
                  SUBJECT,
                  Categories,
                  GrantApplication = cf_grant_application_or_submission_to_snf_eu_charity,
                  CreateYear:hrs_per_year)
  
  return(consultings)
}
