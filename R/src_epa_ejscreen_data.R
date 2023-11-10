#' EPA Environmental Justice Screening (EJScreen) Dataset
#'
#' @description
#' `src_epa_ejscreen_data()` returns a dataframe of census tract level environmental
#' data from the EPA's EJScreen dataset.
#' 
#' @details
#' EJScreen is an EPA's environmental justice mapping and screening tool that 
#' provides EPA with a nationally consistent dataset and approach for combining 
#' environmental and demographic socioeconomic indicators. EJScreen users choose 
#' a geographic area; the tool then provides demographic socioeconomic and environmental 
#' information for that area. All of the EJScreen indicators are publicly-available 
#' data. EJScreen simply provides a way to display this information and includes 
#' a method for combining environmental and demographic indicators into EJ indexes.
#' 
#' @import lgr
#' @importFrom dplyr select rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_pad str_replace_all
#' 
#' @return A data frame (data.frame) containing data pulled from the EPA EJScreen dataset.
#'
#' @source - [Data Source](https://www.epa.gov/ejscreen/download-ejscreen-data)
#'
#' @family data accessors
#'
#' @author Todd Burus <tburus@uky.edu>
#'
#' @examples
#' # example code
#' 
#' #function pivots data longwise, so output is 13*n_max rows
#' ejs = src_epa_ejscreen_data(n_max = 500)
#'
#' colnames(ejs)
#'
#' dplyr::glimpse(ejs)
#'
#' @export

src_epa_ejscreen_data = function(...){
    lg <- get_carddealr_logger()
    
    lg$info('Starting epa_ejscreen_data')
    
    url = 'https://gaftp.epa.gov/EJScreen/2023/2.22_September_UseMe/EJSCREEN_2023_Tracts_with_AS_CNMI_GU_VI.csv.zip'
    
    #create tempfile to store zip file in
    temp = tempfile()
    
    #download zip file
    download.file(url, temp)
    
    #read files inside zip file
    files = unzip(temp, list = TRUE)
    
    #locate files inside zip file
    csv = files$Name[str_detect(files$Name, pattern = '.csv')]
    
    #read file to dataframe
    df = read_csv(unz(temp, csv),...)
    
    #unlink the temp file
    unlink(temp)
    
    #manipulate dataframe
    df = df %>% 
        dplyr::select('ID', 'ST_ABBREV', 'PM25', 'OZONE', 'DSLPM', 'CANCER',
                      'RESP', 'RSEI_AIR', 'PTRAF', 'PRE1960PCT', 'PNPL', 'PRMP', 'PTSDF', 'UST', 'PWDIS') %>% 
        dplyr::rename(
            FIPS = ID,
            State = ST_ABBREV,
            Lead_Paint = PRE1960PCT,
            Diesel_PM = DSLPM,
            Air_Toxics_Cancer = CANCER,
            Air_Toxics_Resp = RESP,
            Traffic_Proximity = PTRAF,
            Water_Discharge = PWDIS,
            Superfund_Proximity = PNPL,
            RMP_Proximity = PRMP,
            Hazardous_Waste_Proximity = PTSDF,
            Ozone = OZONE,
            Underground_Storage_Tanks = UST,
            Toxics_Release_to_Air = RSEI_AIR
        ) %>% 
        dplyr::mutate(FIPS = stringr::str_pad(FIPS, side = 'left', pad = '0', width = 11)) %>% 
        tidyr::pivot_longer(cols = PM25:Water_Discharge, names_to = 'measure') %>% 
        dplyr::mutate(measure = stringr::str_replace_all(measure, '_', ' '))
    
    lg$info('Completing epa_ejscreen_data')
    
    return(df)
}
