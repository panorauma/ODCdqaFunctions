#' checklist_to_df
#'
#' Tidy results of checks into df to display in app
#'
#' @param check_list : Results of structure/schema check.
#'
#' @return Dataframe of all check_list information
#' @export
checklist_to_df <- function(check_list){ #MARK: checklist_to_df  

  #create empty df
  table_report <- data.frame(CheckName=names(check_list),
                             Num_blank=NA,
                             Name_blank=NA)

  #fill in info
  for(i in 1:nrow(table_report)){
    CheckName <- table_report$CheckName[i]
    loc <- match(CheckName,names(check_list))
    table_report$Num_blank[i] <- check_list[[loc]][1]
    table_report$Name_blank[i] <- check_list[[loc]][2]
  }

  #add in P/F status (custom funct used to simplify code)
  allPassed <- function(ifTrue,ifFalse){
    ifelse(table_report$Num_blank==0,ifTrue,ifFalse)
  }

  #apply to Status & Name_blank cols
  table_report$Status <- allPassed("Pass","Fail")
  table_report$Name_blank <- allPassed("",table_report$Name_blank)
  
  #tidy Name_blank col (renamed to Fails in final output)
  table_report$Name_blank <- str_replace_all(table_report$Name_blank,
                                             c(","="","c[(]"="","[)]"=""))

  ## reorder the columns to be more intuitive
  table_report<-table_report[,c("CheckName", "Status", "Num_blank", "Name_blank")]

  #add warning
  table_report$Status <- ifelse(table_report$Status=="Fail"&table_report$CheckName=="duplicated_column",
                                "Warning",table_report$Status)
  
  return(table_report)
}

#' empty_data_dict
#'
#' Create empty data dict template (if no data dict uploaded)
#'
#' @param upload_dataset : Dataframe of uploaded dataset csv file. 
#'
#' @return Dataframe of partially filled data dictionary, if no data dictionary uploaded.
#' @export
empty_data_dict <- function(upload_dataset){ #MARK: empty_data_dict
  
  #get colnames from uploaded dataset
  df_data_Names <- unlist(names(upload_dataset))

  #get corresponding class
  df_data_DataType <- lapply(df_data_Names,function(x){
    class(upload_dataset[[x]])
  })

  #create empty df
  autoDataDic <- data.frame(VariableName=unlist(df_data_Names),
                            Title=NA,
                            Unit_of_Measure=NA,
                            Description=NA,
                            DataType=unlist(df_data_DataType),
                            PermittedValues=NA,
                            Comments=NA)

  #add in context for class==Date
  autoDataDic[,c("Title","Description")] <- ifelse(autoDataDic$DataType=="Date",
                                                   "Date of ",NA)

  #add permitted values
  autoDataDic$PermittedValues <- lapply(df_data_Names,function(x){
    varType <- class(upload_dataset[[x]])
    if(varType=="numeric"){
      #collapse into vector of length 1
      autoDataDic$PermittedValues <- str_c(range(upload_dataset[[x]],na.rm=TRUE),collapse=", ")
    }else if(varType=="character"){
      autoDataDic$PermittedValues <- paste(str_replace_na(unique(upload_dataset[[x]])),collapse=", ")
    }else{
      autoDataDic$PermittedValues <- "" #placing NA here turns entire col into class Boolean (don't want that)
    }
  })

  #use PermittedValues to get min & max
  autoDataDic <- separate(autoDataDic,PermittedValues,c("MinimumValue","MaximumValue"),
                          sep=", ",remove=FALSE) %>%
    relocate("Comments",.after="MaximumValue")

  #use side effect of as.numeric to force characters to NA
  autoDataDic[,c("MinimumValue","MaximumValue")] <- lapply(autoDataDic[,c("MinimumValue","MaximumValue")],as.numeric)

  #convert to datatable
  DT::datatable(autoDataDic,editable = TRUE,
                extensions = 'Buttons', options = list(
                  dom = 'Blfrtip',
                  buttons = 'csv'
                ))
}

#' library.dynam.unload
#'
#' used only for package version of app
#'
#' @return Unloads all libraries not included in R startup
#' @export
library.dynam.unload <- function(chname,libpath,verbose=getOption("verbose"),file.ext=.Platform$dynlib.ext){ #MARK: library.dynam.unload
    if(!is.null(dev_meta(chname))){
        try({
            unload_dll(pkg_name(libpath))
        })
        return()
    }
    base::library.dynam.unload(chname,libpath,verbose,file.ext)
}

#' render_check_table
#'
#' Show results of structure_checks in shiny app
#'
#' @param structure_checks : Result of structure checks.
#' @param schema_checks : Result of schema checks.
#'
#' @return Single dataframe of combined structure and schema check results.
#' @export
render_check_table <- function(structure_checks,schema_checks){ #MARK: render_check_table
  
  #df for struct check results
  temp_structure <- checklist_to_df(structure_checks)
  temp_structure$CheckName <- c("Blank header","Duplicated header","Blank row",
                                "Blank column","Duplicated row","Duplicated column",
                                "Required variables (data)")
  temp_structure$type<-"Structure"
  
  #df for schema check results
  temp_schema <- checklist_to_df(schema_checks)
  temp_schema$CheckName <- c("Data dictionary header","Data extra header","Data missing header",
                             "Missing description","Missing title","Required variables (dictionary)",
                             "Invalid special characters (variable name)","Starts with a letter (variable name)",
                             "More than 60 characters (variable name)")
  temp_schema$type <-"Schema"

  table_report<-rbind(temp_structure, temp_schema)

  table_report<-table_report%>%
    relocate(type, .before = CheckName)
  
  # table_report$CheckName <- c("Blank Header","")
  
  table_html <- DT::datatable(table_report,
                              selection = "none",
                              rownames = FALSE,
                              colnames = c('Type of check','Check name', 'Status', 
                                           'Number of Fails', 'Fails'),
                              extensions = c('Buttons', 'RowGroup'),
                              options = list(
                                dom = 'tB',
                                buttons = c('copy', 'csv'),
                                rowGroup = list(dataSrc = 0),
                                pageLength = 16
                              ),
                              editable = FALSE,
                              callback = JS("return table")) %>%
    formatStyle('Status',target='row',
                backgroundColor=styleEqual(c("Pass","Fail","Warning"),
                                           c('#b6d7a8','#e06666',"#ffa500"))
    )
  return(list("table_html"=table_html,"df"=table_report))
}

#' startApp
#'
#' Single functiont to run shiny app (will load required packages by default)
#'
#' @param loadReqPackages : Should the function load required packages on user's behalf? Default is `TRUE`.
#'
#' @return Opens shiny app inside default web browser.
#' @export
startApp <- function(loadReqPackages=TRUE){ #MARK: startApp
  if(loadReqPackages==TRUE){
    library(tidyverse)
    library(shiny)
    library(shinyjs)
    library(DT)
    shiny::runApp(system.file("app.R",package="odcDataChecker"),launch.browser=TRUE)
  }else if(loadReqPackages==FALSE){
    shiny::runApp(system.file("app.R",package="odcDataChecker"),launch.browser=TRUE)
  }else{
    stop("Accepted arguments are `TRUE` or `FALSE`. Please try again using one of
         the two accepted arguments.")
  }
}

#' summary.odcCheck
#'
#' Prints out report of checks in console. Not used in shiny app.
#'
#' @param validate_results : Results from structure and schema checks.
#'
#' @return Prints out report for the checks.
#' @export
summary.odcCheck <- function(validate_results){ #MARK: summary.odcCheck

  #The reports for the checks are a set of list. This function
  #prints out the report for the checks.

  #temp var
  a<-validate_results

  #STRUCTURE CHECK
  cat('-- STRUCTURE CHECK:',paste(rep('-',45),collapse = '') ,'\n')

  #blank header
  if("blank_header"%in%names(a$structure)){
    if (a$structure$blank_header$n_blank_header==0){
      cat('*Blank Header:','no errors','\n\n')
    }else{
      cat('*Blank Header:',a$structure$blank_header$n_blank_header,
          'error/s detected','\n')
      cat('\t','Blank Header number:',
          paste(a$structure$blank_header$which_blank_header, collapse = '; '),'\n\n')
    }
  }

  #duplicated header
  if("duplicated_header"%in%names(a$structure)){
    if (a$structure$duplicated_header$n_dupli_header==0){
      cat('*Duplicated Header:','no errors','\n\n')
    }else{
      cat('*Duplicated Header:',a$structure$duplicated_header$n_dupli_header,
          'error/s detected','\n')
      cat('\t','Duplicated Headers:',
          paste(a$structure$duplicated_header$which_dupli_header,collapse = '; '),'\n\n')
    }
  }

  #blank row
  if("blank_row"%in%names(a$structure)){
    if (a$structure$blank_row$n_blank_row==0){
      cat('*Blank row:','no errors','\n\n')
    }else{
      cat('*Blank row:',a$structure$blank_row$n_blank_row,
          'error/s detected','\n')
      cat('\t','Rows with all blank fields:',
          paste(a$structure$blank_row$which_blank_row,collapse = '; '),'\n\n')
    }
  }

  #blank column
  if("blank_column"%in%names(a$structure)){
    if (a$structure$blank_column$n_blank_column==0){
      cat('*Blank column:','no errors','\n\n')
    }else{
      cat('*Blank column:',a$structure$blank_column$n_blank_column,
          'error/s detected','\n')
      cat('\t','Columns with all blank fields:',
          paste(a$structure$blank_column$which_blank_column, collapse = '; '),
          '\n\n')
    }
  }

  #duplicated rows
  if ("duplicated_row"%in%names(a$structure)){
    if (a$structure$duplicated_row$n_dupli_row==0){
      cat('*Duplicated row:','no errors','\n\n')
    }else{
      cat('*Duplicated row:',a$structure$duplicated_row$n_dupli_row,
          'error/s detected','\n')
      cat('\t','Rows duplicated:',
          paste(a$structure$duplicated_row$which_dupli_row, collapse = '; '),'\n\n')
    }
  }

  #duplicated columns
  if ("duplicated_column"%in%names(a$structure)){
    if (a$structure$duplicated_column$n_dupli_column==0){
      cat('*Duplicated column:','no warnings','\n\n')
    }else{
      cat('*Duplicated column:',a$structure$duplicated_column$n_dupli_column,
          'warning/s detected','\n')
      cat('\t','Columns duplicated:',
          paste(a$structure$duplicated_column$which_dupli_column,collapse = '; '),'\n\n')
    }
  }

  # missing min vars
  if ("minimal_var"%in%names(a$structure)){
    if (a$structure$minimal_var$n_missing_minimal_var==0){
      cat('*Requiered variables:','no errors','\n\n')
    }else{
      cat('*Requiered variables:',a$structure$minimal_var$n_missing_minimal_var,
          'error/s detected','\n')
      cat('\t','Missing requiered variables:',
          paste(a$structure$minimal_var$which_missing_minimal_var,collapse = '; '),'\n\n')
    }
  }

  #SCHEMA CHECK
  cat('\n','-- SCHEMA CHECK:',paste(rep('-',45),collapse = '') ,'\n')

  #data dic header
  if("data_dic_headers"%in%names(a$schema)){

    if (a$schema$data_dic_headers$n_datadic_Noheaders==0){

      if (!is.null(a$schema$data_dic_headers$data_dic_header_in_order) &&
          !a$schema$data_dic_headers$data_dic_header_in_order){
        cat('*Data dictionary header:','wrong order','\n\n')

        cat('\t','Data dictionary headers are in the wrong order, make sure the order is:',
            paste(c('VariableName','Title',	'Unit_of_Measure',	'Description',
                    'Comments',	'PermittedValues',	'DataType',	'MaximumValue',
                    'MinimumValue'),collapse = '; '),'\n\n')
      }else{
        cat('*Data dictionary header:','no errors','\n\n')
      }
    }else{
      cat('*Data dictionary header:',a$schema$data_dic_headers$n_datadic_Noheaders,
          'error/s detected','\n')
      cat('\t','Data dictionary header missing:',
          paste(a$schema$data_dic_headers$which_datadic_Noheaders,collapse = '; '),'\n\n')
    }
  }

  #data extra header
  if("data_dic_headers"%in%names(a$schema) ){

    if (!is.list(a$schema$extra_header)){
      cat(a$schema$extra_header,'\n\n')
    }else if(a$schema$extra_header$n_extra_header==0){
      cat('*Data extra header:','no errors','\n\n')
    }else if(a$schema$extra_header$n_extra_header!=0){
      cat('*Data extra header:',a$schema$extra_header$n_extra_header,
          'error/s detected','\n')
      cat('\t','Data extra header:',
          paste(a$schema$extra_header$which_extra_header, collapse = '; '),'\n\n')
    }
  }

  #data missing header
  if("missing_header"%in%names(a$schema)){
    if (!is.list(a$schema$missing_header)){
      cat(a$schema$missing_header,'\n\n')
    }else if(a$schema$missing_header$n_missing_header==0){
      cat('*Data missing header:','no errors','\n\n')
    }else if(a$schema$missing_header$n_missing_header!=0){
      cat('*Data missing header:',a$schema$missing_header$n_missing_header,
          'error/s detected','\n')
      cat('\t','Data missing header:',
          paste(a$schema$missing_header$which_missing_header, collapse = '; '),'\n\n')
    }
  }

  #missing description
  if("missing_description"%in%names(a$schema)){
    if (!is.list(a$schema$missing_description)){
      cat(a$schema$missing_description,'\n\n')
    }else if(a$schema$missing_description$n_missing_des==0){
      cat('*Missing description:','no errors','\n\n')
    }else if(a$schema$missing_description$n_missing_des!=0){
      cat('*Missing description:',a$schema$missing_description$n_missing_des,
          'error/s detected','\n')
      cat('\t','Variable missing description:',
          paste(a$schema$missing_description$which_missing_des, collapse = '; '),'\n\n')
    }
  }

  #missing title
  if("missing_title"%in%names(a$schema)){
    if (!is.list(a$schema$missing_title)){
      cat(a$schema$missing_title,'\n\n')
    }else if(a$schema$missing_title$n_missing_title==0){
      cat('*Missing title:','no errors','\n\n')
    }else if(a$schema$missing_title$n_missing_title!=0){
      cat('*Missing title:',a$schema$missing_title$n_missing_title,
          'error/s detected','\n')
      cat('\t','Variable missing title:',
          paste(a$schema$missing_title$which_missing_title, collapse = '; '),'\n\n')
    }
  }

  # missing min vars
  if("missing_minimal_var"%in%names(a$schema)){
    if (a$schema$missing_minimal_var$n_missing_minimal_var==0){
      cat('*Required variables:','no errors','\n\n')
    }else{
      cat('*Required variables:',a$schema$missing_minimal_var$n_missing_minimal_var,
          'error/s detected','\n')
      cat('\t','Missing required variables:',
          paste(a$schema$missing_minimal_var$which_missing_minimal_var,collapse = '; '),'\n\n')
    }
  }
}

#' system.file
#'
#' Find whether package is installed. Only used in package version of app
#'
#' @return Path to package (if found)
#' @export
system.file <- function(...,package="base",lib.loc=NULL,mustWork=FALSE){ #MARK: system.file
    if(!(package %in% dev_packages())){
        base::system.file(..., package = package, lib.loc = lib.loc, 
            mustWork = mustWork)
    }
    else{
        pkg_path <- find.package(package)
        files_inst <- file.path(pkg_path, "inst", ...)
        present_inst <- file.exists(files_inst)
        files_top <- file.path(pkg_path, ...)
        present_top <- file.exists(files_top)
        files <- files_top
        files[present_inst] <- files_inst[present_inst]
        files <- files[present_inst | present_top]
        if(length(files)>0){
            normalizePath(files, winslash = "/")
        }
        else{
            if(mustWork){
                stop("No file found", call. = FALSE)
            }
            else{
                ""
            }
        }
    }
}

#' validate_odc
#'
#' Single (wrapper) function to run both structure & schema checks. Only structure checks are conducted if datadic are not present.
#'
#' @param dataset : Dataset dataframe in global environment
#' @param datadic : Data dictionary dataframe in global environment
#' @param dataset_path : Path to dataset csv
#' @param datadic_path : Path to data dictionary csv
#' @param str_checks : Default is "all"
#' @param sch_checks : Default is "all"
#'
#' @return List of structure and schema check results.
#' @export 
validate_odc <- function(dataset=NULL,datadic=NULL,
                       dataset_path=NULL,datadic_path=NULL,str_checks,
                       sch_checks){ #MARK: validate_odc

  minimal_var <- c("Subject_ID", "Species", "Strain", "Animal_origin", "Age", "Weight", "Sex",
                   'Group', 'Laboratory', 'StudyLeader', 'Published', "Exclusion_in_origin_study",
                   'Exclusion_reason', 'Cause_of_Death', 'Injury_type', 'Injury_device','Injury_level',
                   'Injury_details')

  results<-list()

  if(is.null(datadic_path)&is.null(datadic)){
    if(!is.null(dataset_path)){
      results[['structure']]<-validate_structure(dataset_path=dataset_path,
                                                 str_checks = str_checks)
    }else{
      results[['structure']]<-validate_structure(dataset=dataset,
                                                 str_checks = str_checks)
    }
  }else{
    if(!is.null(dataset_path)&!is.null(datadic_path)){
      results[['structure']]<-validate_structure(dataset_path=dataset_path,
                                                 str_checks = str_checks)
      results[['schema']]<-validate_schema(dataset_path=dataset_path,
                                           datadic_path=datadic_path,
                                           sch_checks = sch_checks)
    }else{
      results[['structure']]<-validate_structure(dataset=dataset,
                                                 str_checks = str_checks)
      results[['schema']]<-validate_schema(dataset=dataset,datadic=datadic,
                                           sch_checks = sch_checks)
    }

  }

  #class(results)<-'odcCheck'
  return(results)
}

#schema check
validate_schema <- function(dataset,datadic,sch_checks="all"){

  # Performs the schema check given the dataset_path and the data_dic_path

  minimal_var <- c("Subject_ID", "Species", "Strain", "Animal_origin", "Age", "Weight", "Sex",
                   'Group', 'Laboratory', 'StudyLeader', 'Published', "Exclusion_in_origin_study",
                   'Exclusion_reason', 'Cause_of_Death', 'Injury_type', 'Injury_device','Injury_level',
                   'Injury_details')

  #'sci = all, tbi = else (only difference is tbi doesn't use "min_vars")
  #'min_vars = minimal_var = vars that must be present in dataset/data dic
  if(sch_checks == "all"){
    sch_checks <- c("dic_header", "data_extra_header","data_miss_header",
                    "miss_description","miss_title","min_vars",
                    "other_symbols","pos1_char","over_60char")
  }else{
    #tbi does not have "min_vars"
    sch_checks <- c("dic_header", "data_extra_header","data_miss_header",
                    "miss_description","miss_title",
                    "other_symbols","pos1_char","over_60char")
  }

  checks<-match.arg(sch_checks, c("dic_header", "data_extra_header","data_miss_header",
                                  "miss_description","miss_title", "min_vars",
                                  "other_symbols","pos1_char","over_60char"),several.ok = T)

  results<-list()

  #data dic headers
  datadic_headers<-colnames(datadic)
  datadic_headers_stand<-c('VariableName','Title',	'Unit_of_Measure',	'Description',
                           'Comments',	'PermittedValues',	'DataType',	'MaximumValue',
                           'MinimumValue')

  #rm NA values to avoid errors
  dataset_header<-na.omit(colnames(dataset))
  datadic_vars<-na.omit(datadic$VariableName)

  #Check whether there is a missing header or misspelled header in the data dictionary
  if("dic_header"%in%checks){
    results[['data_dic_headers']]<-list('n_datadic_Noheaders'=
                                          sum(!datadic_headers_stand%in%datadic_headers),
                                        'which_datadic_Noheaders'=
                                          datadic_headers_stand[!datadic_headers_stand%in%datadic_headers])
  }

  #Check whether the headers are in the right order
  #@deprecated. As of March 2021, the odc-sci doesn't require that the data dic has the vars in
  #any specific order
  # results[['data_dic_headers']]['data_dic_header_in_order']<-
  #                                               !is.unsorted(match(datadic_headers_stand,
  #                                                                 datadic_headers))


  #Extra header
  if("data_extra_header"%in%checks){

    if(!'VariableName'%in%datadic_headers){
      results[['extra_header']]<-'Schema checks requires that data dictionary contains VariableName as a header'
    }

    results[['extra_header']]<-list('n_extra_header'=
                                      sum(!dataset_header%in%datadic_vars),
                                    'which_extra_header'=
                                      dataset_header[!dataset_header%in%datadic_vars])
  }

  #Missing header
  if("data_miss_header"%in%checks){

    if(!'VariableName'%in%datadic_headers){
      results[['missing_header']]<-'Schema checks requires that data dictionary contains VariableName as a header'
    }

    results[['missing_header']]<-list('n_missing_header'=
                                        sum(!datadic_vars%in%dataset_header),
                                      'which_missing_header'=
                                        datadic_vars[!datadic_vars%in%dataset_header])
  }

  #Missing description and title
  if("miss_description"%in%checks){
    if(!'Description'%in%datadic_headers){
      results[['missing_description']]<-'Schema checks requires that data dictionary contains Description as a header'
    }else{
      datadic_des<-datadic$Description
      results[['missing_description']]<-list('n_missing_des'=sum(datadic_des==''|is.na(datadic_des)),
                                             'which_missing_des'=
                                               datadic_vars[datadic_des==''|is.na(datadic_des)])
    }
  }

  #Check that all titles are present
  if("miss_title"%in%checks){
    if(!'Title'%in%datadic_headers){
      results[['missing_title']]<-'Schema checks requires that data dictionary contains Title as a header'
    }else{
      datadic_title<-datadic$Title

      results[['missing_title']]<-list('n_missing_title'=sum(datadic_title==''|is.na(datadic_title)),
                                       'which_missing_title'=
                                         datadic_vars[datadic_title==''|is.na(datadic_title)])
    }
  }

  #Check for the presence of minimal variables
  if("min_vars"%in%checks){
    results[['missing_minimal_var']]<-list('n_missing_minimal_var'=
                                             sum(!minimal_var%in%datadic_vars),
                                           'which_missing_minimal_var'=
                                             minimal_var[!minimal_var%in%datadic_vars])
  }
  
  #Check if variable name contains anything other than accepted values (A-z0-9_.)
  if("other_symbols"%in%checks){
    
    #check & store in df
    other_symbols_check <- var_other_symbols(datadic_vars)
    other_symbols_df <- data.frame(Key=datadic_vars,Value=unlist(other_symbols_check))
    
    #'wrangle other_symbols_df
    #'Value = TRUE when other symbols detected in var name
    other_symbols_df <- other_symbols_df %>%
      dplyr::mutate(
        Value = dplyr::case_when(
          Value==TRUE ~ "Fail",
          Value==FALSE ~ "Pass"
        )
      ) %>%
      dplyr::filter(Value=="Fail")
    
    #store results in nested lists
    results[['other_symbols']][["n_other_symbols_headers"]] <- nrow(other_symbols_df)
    results[['other_symbols']][["which_other_symbols_headers"]] <- unlist(other_symbols_df$Key)
    
    #rm temp vars
    rm(other_symbols_check,other_symbols_df)
  }
    
  #Check if 1st position in variable name is a letter
  if("pos1_char"%in%checks){
    
    #check value in first position of string & store in df
    pos1_check <- lapply(datadic_vars,first_char_letter)
    pos1_check_df <- data.frame(Key=datadic_vars,Value=unlist(pos1_check))
    
    #keep datadic_vars that failed check
    pos1_check_df <- pos1_check_df %>%
      dplyr::filter(Value=="Fail")
    
    #store results in nested lists
    results[['pos1_char']][["n_pos1_char_headers"]] <- nrow(pos1_check_df)
    results[['pos1_char']][["which_pos1_char_headers"]] <- unlist(pos1_check_df$Key)
    
    #rm temp vars
    rm(pos1_check,pos1_check_df)
  }
  
  #Check if variable name is over 60 characters
  if("over_60char"%in%checks){
    
    #check each datadic_vars & store in df
    char_length_check <- lapply(datadic_vars,function(x){
      ifelse(nchar(x)>60,"Fail","Pass")
    })
    char_length_df <- data.frame(Key=datadic_vars,Value=unlist(char_length_check))
    
    #keep datadic_vars that failed check
    char_length_df <- char_length_df %>%
      dplyr::filter(Value=="Fail")
    
    #store results in nested lists
    results[['over_60char']][["n_over_60char_headers"]] <- nrow(char_length_df)
    results[['over_60char']][["which_over_60char_headers"]] <- unlist(char_length_df$Key)
    
    #rm temp vars
    rm(char_length_check,char_length_df)
  }  
  
  return(results)
}

#struct check
validate_structure <- function(dataset,str_checks="all"){ #MARK: validate_structure

  #Performs the structural check given the dataset_path

  minimal_var <- c("Subject_ID", "Species", "Strain", "Animal_origin", "Age", "Weight", "Sex",
                   'Group', 'Laboratory', 'StudyLeader', 'Published', "Exclusion_in_origin_study",
                   'Exclusion_reason', 'Cause_of_Death', 'Injury_type', 'Injury_device','Injury_level',
                   'Injury_details')
  
  #'same as schema checks
  if(str_checks == "all"){
    str_checks <- c("blank_header", "dup_header","blank_row", "blank_column",
                    "dup_row", "dup_column", "min_vars")
  }else{
    str_checks <- c("blank_header", "dup_header","blank_row", "blank_column",
                    "dup_row", "dup_column")
  }

  checks<-match.arg(str_checks, c("blank_header", "dup_header","blank_row", "blank_column",
                                  "dup_row", "dup_column", "min_vars"),several.ok = T)

  results<-list()

  if ("blank_header"%in%checks){
    #blank header (at upload)
    name_header<-colnames(dataset)
    results[['blank_header']]<-list('n_blank_header'=sum(name_header==''),
                                    'which_blank_header'=which(name_header==''))
  }

  #duplicated header (at upload)
  if ("dup_header"%in%checks){
    duplicated_header<-table(colnames(dataset))
    results[['duplicated_header']]<-list('n_dupli_header'=sum(duplicated_header>1),
                                         'which_dupli_header'=names(which(duplicated_header>1)))
  }

  #blank row
  if ("blank_row"%in%checks){
    blank_row<-apply(dataset, 1, function(x){
      sum(x == '' | is.na(x) | x == "na",na.rm = T)/length(x)
    })
    results[['blank_row']]<-list('n_blank_row'=sum(blank_row==1),
                                 'which_blank_row'=which(blank_row==1))
  }

  #blank column
  if ("blank_column"%in%checks){
    blank_column<-apply(dataset, 2, function(x){
      sum(x == '' | is.na(x) | x == "na",na.rm = T)/length(x)
    })
    results[['blank_column']]<-list('n_blank_column'=sum(blank_column==1),
                                    'which_blank_column'=names(which(blank_column==1)))
  }

  #duplicated row
  if ("dup_row"%in%checks){
    row_distance<-duplicated(dataset) | duplicated(dataset, fromLast = T)

    results[['duplicated_row']]<-list('n_dupli_row'=sum(row_distance,na.rm = T),
                                      'which_dupli_row'=which(row_distance==T,
                                                              arr.ind = T))
  }
  #duplicated column
  if ("dup_column"%in%checks){
    col_distance <- duplicated(t(dataset)) | duplicated(t(dataset), fromLast = T)

    results[['duplicated_column']]<-list('n_dupli_column'=sum(col_distance,na.rm = T),
                                         'which_dupli_column'=rownames(which(col_distance==T,
                                                                             arr.ind = T)))
  }

  #minimal vars
  if ("min_vars"%in%checks){
    results[['minimal_var']] <- list('n_missing_minimal_var'=
                                       sum(!minimal_var%in%name_header),
                                     'which_missing_minimal_var'=
                                       minimal_var[!minimal_var%in%name_header])
  }

  return(results)
}

#' first_char_letter
#'
#' Test if variable name starts with a letter.
#'
#' @param string : String to test
#'
#' @return String of "Pass" or "Fail"
#' @export
first_char_letter <- function(string){ #MARK: first_char_letter
  
  #find position of 1st letter in string
  temp <- string %>%
    str_replace("[_.]","1") %>% #str_replace used to ensure "_" & "." will be excluded
    str_locate("[A-z]")
  
  #Pass/Fail
  ifelse(temp[1]==1,"Pass","Fail")
}

#' var_other_symbols
#'
#' Test if var name contains non-accepted symbols (NOT A-z0-9_.)
#'
#' @param string : String to test
#'
#' @return String of True if non-accepted symbols present in variable name, Fail if string ONLY contains accepted characters.
#' @export
var_other_symbols <- function(string){
  
  #returns T if non-accepted symbols present in var name, F if string ONLY contains A-z0-9_.
  temp <- lapply(string,function(x){
    str_detect(x,"[^A-z0-9_.]")
  })
  
  return(temp)
}
