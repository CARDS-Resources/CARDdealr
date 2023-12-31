#' American College of Radiology Lung Cancer Screening Registry (LCSR) dataset
#'
#' @description
#' `src_lung_cancer_screening_data()` returns a dataframe of actively reporting sites in the 
#' lung cancer screening registry (LCSR).
#' 
#' @details
#' The ACR Lung Cancer Screening Registry (LCSR) is designed to systematically audit 
#' the quality of interpretation of screening lung CT exams. The registry is based on the 
#' ACR Lung Imaging Reporting and Data System (Lung-RADS), which is the product of the 
#' ACR Lung Cancer Screening Committee subgroup on Lung-RADS. This Lung-RADS system is 
#' a quality assurance tool designed to standardize lung cancer screening CT reporting 
#' and management recommendations, reduce confusion in lung cancer screening CT 
#' interpretations and facilitate outcome monitoring. The ACR LCSR will capture Lung-RADS 
#' recommendations and monitor and compare appropriate use of Lung-RADS#' 
#' 
#' @import lgr
#' @importFrom dplyr mutate select filter
#'    
#' @param state Character string or vector of character strings of state postal abbreviations
#' 
#' @param \dots passed to [readr::read_csv()] and useful for limiting
#'   the number of rows read for testing or glimpsing data.
#' 
#' @return A data frame (data.frame) containing data pulled from LCSR.
#'
#' @source - [Data Source](https://www.acr.org/Clinical-Resources/Lung-Cancer-Screening-Resources/LCS-Locator-Tool)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#'
#' lcs = src_acr_lung_cancer_screening_data(n_max=1000)
#'
#' colnames(lcs)
#'
#' dplyr::glimpse(lcs)
#'
#' @export

src_acr_lung_cancer_screening_data = function(state = NULL, ...){
    lg <- get_carddealr_logger()
    
    lg$info('Starting acr_lung_cancer_screening_data')
    
    lcs = read_csv('https://report.acr.org/t/PUBLIC/views/NRDRLCSLocator/ACRLCSDownload.csv',...)
    
    colnames(lcs) = c('Name','Street','City','State','Zip_code','Phone_number', 'Notes', 'inRegistry')
    
    lcs = lcs |> 
        dplyr::filter(if (!is.null(state)) State %in% state else TRUE) |>
        dplyr::mutate(
            Address = paste0(Street, ', ', City, ', ', State, ' ', Zip_code),
            Type = 'Lung Cancer Screening',
            latitude = '',
            longitude = ''
        ) |> 
        dplyr::select(Type, Name, Address, State, Phone_number, Notes, latitude, longitude) 
    
    lg$info('Completing acr_lung_cancer_screening_data')
    
    return(lcs)
}
