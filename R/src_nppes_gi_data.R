#' National Plan & Provider Enumeration System (NPPES)
#'
#' @description
#' `src_nppes_gi_data()` returns a dataframe of GI Providers (Gastroenterology and 
#' Colon & Recal Surgeons) from the NPPES API.
#' 
#' @details
#' Centers for Medicare & Medicaid Services CMS has developed the NPPES to assign 
#' unique identifiers to health care providers. The National Provider Indentifier (NPI) 
#' has been the standard identifier for all HIPAA-covered entities (health care providers) 
#' since May 23, 2007. Small health plans were required to obtain and use 
#' an NPI by May 23, 2008.
#' 
#' @import lgr
#' @import jsonlite
#' @importFrom dplyr mutate select filter
#' @importFrom tidyr unnest
#' @importFrom stringr str_to_title
#'   
#' @param state Character string or vector of character strings of state postal abbreviations
#' 
#' @return A data frame (data.frame) containing data pulled from the NPPES API.
#'
#' @source - [Data Source](https://npiregistry.cms.hhs.gov/api-page)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' gi = src_nppes_gi_data(state  = c('KY', 'TN'))
#'
#' colnames(gi)
#'
#' dplyr::glimpse(gi)
#'
#' @export

src_nppes_gi_data = function(state = c(state.abb, 'DC')){
    lg <- get_carddealr_logger()
    
    lg$info('Starting nppes_gi_data')
    
    prov = data.frame()
    
    taxonomy = data.frame(
        term = c('Gastroenterology', 'colon'), 
        provider = c('Gastroenterology', 'Colon & Rectal Surgery')
    )
    
    for (s in state){
        cat("Collecting data for", s, "\n")
        for (i in 1:nrow(taxonomy)){
            count = 1
            result_count = 200
            
            while (result_count == 200 & count < 7){
                url = paste0('https://npiregistry.cms.hhs.gov/api/?version=2.1&',
                             'address_purpose=LOCATION',
                             '&enumeration_type=NPI-1',
                             '&number=',
                             '&state=', s,
                             '&taxonomy_description=', taxonomy$term[i],
                             '&skip=', 200*(count -1),
                             '&limit=200')
                    
                resp = jsonlite::fromJSON(url, flatten=T)
                result_count = resp$result_count
                
                if (length(resp$results) != 0){
                    df = resp$results |> 
                        tidyr::unnest(addresses) |> 
                        dplyr::filter(address_purpose == 'LOCATION') |>
                        dplyr::mutate(
                            basic.middle_name = ifelse('basic.middle_name' %in% colnames(df), 
                                                       basic.middle_name, 
                                                       NA),
                            Name = dplyr::case_when(
                                !is.na(basic.middle_name) ~ stringr::str_to_title(paste0(basic.first_name, ' ', 
                                                                                         basic.middle_name, ' ', 
                                                                                         basic.last_name)),
                                is.na(basic.middle_name) ~ stringr::str_to_title(paste0(basic.first_name, ' ', 
                                                                                        basic.last_name))
                            ),
                            Phone_number = telephone_number,
                            zip = substr(postal_code, 1, 5),
                            Address = paste0(stringr::str_to_title(address_1), ', ', 
                                             stringr::str_to_title(city), ', ', 
                                             state, ' ', 
                                             zip),
                            Type = 'GI Provider',
                            latitude = '',
                            longitude = '',
                            Notes = taxonomy$provider[i]
                        ) |> 
                        dplyr::rename(State = state) |> 
                        dplyr::select(Type, Name, Address, State, Phone_number, Notes, latitude, longitude)
                    
                    prov = rbind(prov, df)
                    count = count + 1
                }
            }
        }
    }
    
    prov = distinct(prov)
    
    lg$info('Completing nppes_gi_data')
    
    return(prov)
}