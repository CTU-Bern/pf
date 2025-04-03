#' Add custom fields to projects data
#'
#' @param dat e.g. \code{all_tabs$project}
#' @param customfields e.g. \code{all_tabs$customfields}
#' @param customfieldvalue e.g. \code{all_tabs$customfieldvalue}
#'
#' @return data
#' @import dplyr stringr stringi
#' @importFrom janitor make_clean_names
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' dat <- all_tabs$project
#' customfields <- all_tabs$customfields
#' customfieldvalue <- all_tabs$customfieldvalue
#' project2 <- decodeCustomFields(project, customfields, customfieldvalue)
#' @importFrom magrittr %>%
 


decodeCustomFields <- function(dat, customfields, customfieldvalue){
  
  STOREKEY <- NAME <- PK_CUSTOMFIELD <- AKTIV <- AREA <- NULL
  
  tags <- customfields %>%
    dplyr::mutate(
      name = janitor::make_clean_names(NAME),
      name = stringr::str_c("cf_", name)
    ) %>%
    dplyr::select(PK_CUSTOMFIELD, NAME, AKTIV, AREA, name)
  
  # merge with customfieldvalue
  new_cfs <- merge(x=customfieldvalue, y=tags, by.x="FK_CUSTOMFIELD", by.y="PK_CUSTOMFIELD")
 
  # to convert back to the format after combining and then pivoting, record which cf is which class
  nums <- which(!is.na(new_cfs$NUMBERVALUE))
  dates <- which(!is.na(new_cfs$DATEVALUE))
  chars <- which(!is.na(new_cfs$TEXTVALUE))
  idens <- which(!is.na(new_cfs$IDENTITYVALUE))
  bools <- which(!is.na(new_cfs$BOOLEANVALUE))
  num_cols <- cbind(unique(new_cfs$name[nums]), "num")
  date_cols <- cbind(unique(new_cfs$name[dates]), "date")
  char_cols <- cbind(unique(new_cfs$name[chars]), "char")
  iden_cols <- cbind(unique(new_cfs$name[idens]), "iden")
  bool_cols <- cbind(unique(new_cfs$name[bools]), "bool")
  
  # what formats are being used? want a long list to contain the value classes
  list_colclasses <- list(num_cols, date_cols, char_cols, iden_cols, bool_cols)
  
  # Filter out any that didn't have values in them (i.e. that have fewer than 2 columns)
  valid_colclasses <- list_colclasses[sapply(list_colclasses, function(x) ncol(x) == 2)]
  
  # Use do.call to rbind the valid objects together
  colclass_mat <- do.call(rbind, valid_colclasses)  # combine to a matrix
  
  # just get the custom field values and ignore the NAs
  cf_values <- new_cfs[c("NUMBERVALUE", "DATEVALUE", "TEXTVALUE", "IDENTITYVALUE", "BOOLEANVALUE")]
  
  # can double check that there is only one value per row, but it is the case, and there is no empty row as also checked here
  ##num_nas_if_four_empty <- 4*dim(cf_values)[1]
  ##na_count <- sum(is.na(cf_values))
  
  cf_singlecolumn_value <- apply(cf_values, 1, function(x){ na.omit(x) })
  new_cfs$cf_singlecolumn_value <- cf_singlecolumn_value
  
  # take only the columns we need 
  new_cfs <- new_cfs %>%
    select(FK_ITEM, name, cf_singlecolumn_value) %>%
    mutate(cf_singlecolumn_value = as.character(cf_singlecolumn_value)
    )
  
  # reshape the single columns into their own columns for each cf
  cfs_reshaped <- new_cfs %>%
    pivot_wider(
      names_from = name,    # New column names come from the 'name' column
      values_from = cf_singlecolumn_value
    )
  
  
  # Convert columns to the desired types based on the colclass_mat matrix
  for (i in seq_along(colclass_mat[,1])) {
    col_name <- colclass_mat[i,1]
    class_type <- colclass_mat[i,2]
    
    if (class_type == "bool") {
      cfs_reshaped[[col_name]] <- as.logical(as.numeric(trimws(cfs_reshaped[[col_name]])))
    } else if (class_type == "date") {
      cfs_reshaped[[col_name]] <- as.Date(cfs_reshaped[[col_name]])
    } else if (class_type == "char") {
      cfs_reshaped[[col_name]] <- as.character(trimws(iconv(cfs_reshaped[[col_name]], from = "latin1", to = "UTF-8"))) # to solve the encoding issue
    } else if (class_type == "num") {
      cfs_reshaped[[col_name]] <- as.numeric(cfs_reshaped[[col_name]])
    } else if (class_type == "iden") {
      cfs_reshaped[[col_name]] <- as.character(trimws(cfs_reshaped[[col_name]])) # I guess an identity value would be a character - so far we don't have any so not sure
    }
  }
  
  # find the "PK_" column to merge with in the data given
  pk_column <- grep("PK_", names(dat), value = TRUE) 
  if(length(pk_column) > 1) stop("There is more than one column with a PK identifier that could be used to merge, please resolve.")
  
  # now merge with the dataframe of choice, e.g. tickets, projects, etc using FK_ITEM
  out <- dat %>%
    left_join(cfs_reshaped, by = setNames("FK_ITEM", pk_column))
  
  return(out)
}




#' @describeIn decodeCustomFields
#' Wrapper around decodeCustomFields to extract custom fields for all four dataframes known to use them
#' 
#' list = all_tabs
#' the direct backend download from projectfacts
#' @param list  \code{all_tabs}
#' 
#' @export


decodeAllCustomFields <- function(list){
  cfs <- list$customfields
  cfvs <- list$customfieldvalue
  list$customer <- decodeCustomFields(dat=list$customer, customfields=cfs, customfieldvalue=cfvs)
  list$project <- decodeCustomFields(dat=list$project, customfields=cfs, customfieldvalue=cfvs)
  list$ticket <- decodeCustomFields(dat=list$ticket, customfields=cfs, customfieldvalue=cfvs)
  list$worker <- decodeCustomFields(dat=list$worker, customfields=cfs, customfieldvalue=cfvs)
  
  return(list)
}


# microbenchmark(
#   decodeCustomFields(all_tabs$ticket, all_tabs$customfields),
#   decodeCustomFields2(all_tabs$ticket, all_tabs$customfields),
#   decodeCustomFields3(all_tabs$ticket, all_tabs$customfields),
#   times = 10
# )
# 
# microbenchmark(
# all_tabs$project$CUSTOMFIELDVALUES |> stringi::stri_detect_regex(tags$regex[1]),
# all_tabs$project$CUSTOMFIELDVALUES |> stringi::stri_extract_first_regex(tags$regex[1])
# , times = 10)


# z <- lapply(1:nrow(tags), function(x){
#   name <- tags$name[x]
#   regex <- tags$regex[x]
#   storekey <- tags$storekey[x]
#   rm <- regmatches(xcf, regexpr(regex, xcf, perl = TRUE))
#   list(regex = regex,
#        storekey = stringi::stri_detect_regex(xcf, storekey),
#        full_regex = grepl(regex, xcf, perl = TRUE),
#        regmatch = ifelse(length(rm) > 0, rm, NA)
#        )
# })
# 
# do.call(rbind, z) |> View()


