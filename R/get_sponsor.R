#' Function to get sponsoring institution and organization for projects/consultings
#' 
#' This code has been adapted for the purpose of producing the graphs for the QMR Yearly Report and CTU Annual Report.
#' The code generates the variable SponsorOrganization (Insel, MedFac, UniBe Other, External Non/For Profit),
#' SponsorInstitution (e.g., Department at Inselspital), and SponsorInst_short (abbreviated intitution-name to be used for plotting)
#' Note: All institutes of the medical faculty have been hard-coded acccording to 
#' https://www.medizin.unibe.ch/ueber_uns/fakultaet/index_ger.html (last check-date: 2023-01-27)
#' All other institutes of UniBe are allocated to "UniBe other".
#' 
#' @param input_data consultings/project data table with minimum variables top_CustomerName, CustomerName, CustomerName2, Organization, default_projectname
#' 
#' @return output_data
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' input_data <-  consultings_ctu_report(all_tabs)
#' x <- get_sponsor(all_tabs)


get_sponsor <- function(input_data){
  
  output_data <- input_data %>% 
    dplyr::mutate(
      SponsorOrganization = dplyr::case_when(top_CustomerName == "Insel Gruppe AG" ~ "Insel",
                                            # medical faculty 
                                            # retrieved from https://www.medizin.unibe.ch/ueber_uns/fakultaet/index_ger.html 2023-01-27
                                            # names compared with projectfacts
                                            CustomerName == "Dekanat Medizinische Fakultät" |
                                            CustomerName == "Forensisch-Psychiatrischer Dienst (FPD)" |
                                            CustomerName == "Institut für Infektionskrankheiten" |
                                            CustomerName == "Institut für Pathologie" |
                                            CustomerName == "Institut für Rechtsmedizin" |
                                            CustomerName == "Institut für Anatomie" |
                                            CustomerName == "Institut für Biochemie und Molekulare Medizin" |
                                            CustomerName == "Institute of Biochemistry  and Molecular Medicine" |
                                            CustomerName == "Institut für Medizingeschichte" |
                                            CustomerName == "Institut für Pharmakologie" |
                                            CustomerName == "Institut für Physiologie" |
                                            CustomerName == "Institute of Social and Preventive Medicine (ISPM)" |
                                            CustomerName == "Theodor Kocher Institut (TKI)" |
                                            CustomerName == "Department for BioMedical Research" |
                                            CustomerName == "Zahnmedizinische Kliniken" |
                                            CustomerName == "ARTORG Center for Biomedical Engineering Research" |
                                            CustomerName == "Bern Center for Precision Medicine (BCPM)" | # not yet in PF
                                            CustomerName == "sitem Center for Translational Medicine and Biomedical Entrepreneurship" |
                                            CustomerName == "Zentrum für Künstliche Intelligenz in der Medizin" | # not yet in PF
                                            CustomerName == "Berner Institut für Hausarztmedizin (BIHAM)" |
                                            CustomerName == "Institut für Komplementäre und Integrative Medizin" |
                                            CustomerName == "Institut für Medizinische Lehre" |
                                            top_CustomerName == "Universitäre Psychiatrische Dienste" |
                                            CustomerName2 == "CTU Bern" 
                                            ~ "UniBe Medical Faculty",
                                            # UNIBE other (all other institutes)
                                            top_CustomerName == "Universität Bern" ~ "UniBe Other",
                                            # external non-profit
                                            top_CustomerName == "External Non-Profit" |
                                            Organization == "External non-profit" |
                                            top_CustomerName == "Lindenhofgruppe" |
                                            default_projname == "Consulting External Non-Profit" 
                                            ~ "External Non-Profit",
                                            # external for-profit
                                            top_CustomerName == "External For-Profit" |
                                            Organization == "External for-profit" |
                                            default_projname == "Consulting External For-Profit" 
                                            ~ "External For-Profit"
                                            ),
      SponsorInstitution = dplyr::case_when(SponsorOrganization == "Insel" ~ CustomerName,
                                            SponsorOrganization == "UniBe Medical Faculty" ~ CustomerName,
                                            SponsorOrganization == "UniBe Other" ~ CustomerName,
                                            SponsorOrganization == "External Non-Profit" ~ SponsorOrganization,
                                            SponsorOrganization == "External For-Profit" ~ SponsorOrganization),
      SponsorInstitution = stringr::str_remove(SponsorInstitution,"^Universitätsklinik für "),
      SponsorInstitution = stringr::str_remove(SponsorInstitution, "^Universitätsinstitut für "),
      SponsorInst_short = dplyr::case_when(SponsorInstitution == "Anästhesiologie und Schmerztherapie" ~ "Anästh. und Schmerztherapie",
                                    SponsorInstitution == "ARTORG Center for Biomedical Engineering Research" ~ "ARTORG",
                                    SponsorInstitution == "Berner Institut für Hausarztmedizin (BIHAM)" ~ "BIHAM",
                                    SponsorInstitution == "Center for the Study of Language and Society (CSLS)" ~ "CSLS",
                                    SponsorInstitution == "Centre for Development and Environment (CDE)" ~ "CDE",
                                    SponsorInstitution == "Department for BioMedical Research" ~ "Dep. for BioMedical Research",
                                    SponsorInstitution == "Diagnostische, Interventionelle und Pädiatrische Radiologie" ~ "Diagn., Interv. und Päd. Radiologie",
                                    SponsorInstitution == "Diagnostische und Interventionelle Neuroradiologie" ~ "Diagn. und Interv. Neuroradiologie",
                                    SponsorInstitution == "Endokrinologie, Diabetologie und Klinische Ernährung" ~ "Endokr., Diabet. und Klin. Ernährung",
                                    SponsorInstitution == "Forensisch-Psychiatrischer Dienst (FPD)" ~ "FPD",
                                    SponsorInstitution == "Hals, Nasen- und Ohrenkrankheiten (HNO), Kopf- und Halschirurgie" ~ "HNO, Kopf- und Halschirurgie",
                                    SponsorInstitution == "Hämatologie und Hämatologisches Zentrallabor" ~ "Hämatologie und Hämat. Zentrallabor",
                                    SponsorInstitution == "Institute of Biochemistry  and Molecular Medicine" ~ "Inst. of Biochem. and Mol. Med.", 
                                    SponsorInstitution == "Institute of Social and Preventive Medicine (ISPM)" ~ "ISPM",
                                    SponsorInstitution == "Institut für Infektionskrankheiten" ~ "Inst. für Infektionskrankheiten",
                                    SponsorInstitution == "Institut für Komplementäre und Integrative Medizin" ~ "Inst. für Kompl. und Integ. Medizin",
                                    SponsorInstitution == "Klinische Psychologie und Psychotherapie" ~ "Klin. Psychologie und Psychoth.",
                                    SponsorInstitution == "Philosophisch-humanwissenschaftliche Fakultät" ~ "Phil.-humanw. Fakultät",
                                    SponsorInstitution == "Rheumatologie und Immunologie" ~ "Rheumato- und Immunologie",
                                    SponsorInstitution == "Schädel-, Kiefer- und Gesichtschirurgie" ~ "Schä.-, Kief.- und Ges.-chirurgie",
                                    SponsorInstitution == "sitem Center for Translational Medicine and Biomedical Entrepreneurship" ~ "sitem Center for Transl. Medicine...",
                                    SponsorInstitution == "Universitäres Zentrum für Palliative Care" ~ "Univ. Zentrum für Palliative Care",
                                    SponsorInstitution == "Universitäre Psychiatrische Dienste" ~ "UPD",
                                    TRUE ~ SponsorInstitution
      ),
      SponsorOrganization = as.factor(SponsorOrganization),
      SponsorInstitution = as.factor(SponsorInstitution),
      SponsorInst_short = as.factor(SponsorInst_short)
    )
    
  return(output_data)
}
