list.of.packages <- c("xlsx", "stringr", "plotrix", "gWidgets2", "gWidgets2tcltk")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = T)

options("guiToolkit" = "tcltk")


prime_final_sheet <- function(pm_combinations, final_row_titles, final_col_titles)
{
    final_sheet_titles <- c("Patient #", "# of 20x-fields of view")
    
    # Collapse each combination into a string:  Phenotype/marker1/marker2/...
    collapsed_combinations <- list()
    for (combination in pm_combinations)
    {
        collapsed_combinations <- c(collapsed_combinations, paste(paste(combination[["phenotypes"]], collapse = "/", sep = ""),
                                                                  paste(combination[["markers"]], collapse = "/", sep = ""), collapse = "/", sep = "  "))
    }
    
    # Add phenotype / marker combination names each followed by a standard error column.
    final_sheet_titles <- c(final_sheet_titles, rbind(collapsed_combinations, "SEM"))
    
    # Create data.frame to hold list titles as column titles for easier adding to master sheet.
    temp_frame <- as.data.frame(matrix(ncol = length(final_sheet_titles)))
    names(temp_frame) <- final_sheet_titles
    
    # Add temp_frame to master sheet's top row to title each column.
    add_frame_to_sheet(temp_frame, sheet_final, final_row_titles, final_col_titles, titles = T)
}

prime_master_sheet <- function(pm_combinations, master_col_tumor, master_col_stroma)
{
    #
    # Create titles of phenotype / marker combinations for master data sheet.
    #
    
    # Add tumor and stroma titles to top row
    master_sheet_ts_header <- c("Tumor", rep(NA, master_col_stroma - master_col_tumor - 1), "Stroma")
    
    # Create data.frame to hold list titles as headers for easier adding to master sheet.
    temp_frame <- as.data.frame(matrix(ncol = length(master_sheet_ts_header)))
    names(temp_frame) <- master_sheet_ts_header
    
    # Add temp_frame to master sheet's top row to head each section.
    add_frame_to_sheet(temp_frame, sheet_master, master_row_tumor, master_col_tumor, titles = T)
    
    # Template list:  Will always start with sample and image number.
    master_sheet_titles <- c("Sample", "Image #")
    
    # Add initial, common titles to master sheet.
    temp_frame <- as.data.frame(matrix(ncol = length(master_sheet_titles)))
    names(temp_frame) <- master_sheet_titles
    
    # Add temp_frame to master sheet's top row to title each column.
    add_frame_to_sheet(temp_frame, sheet_master, master_row_titles, master_col_titles, titles = T)
    
    # Collapse each combination into a string:  Phenotype/marker1/marker2/...
    collapsed_combinations <- list()
    for (combination in pm_combinations)
    {
        collapsed_combinations <- c(collapsed_combinations, paste(paste(combination[["phenotypes"]], collapse = "/", sep = ""),
                                                                  paste(combination[["markers"]], collapse = "/", sep = ""), collapse = "/", sep = "  "))
    }
    
    # Add phenotype / marker combination names each followed by a standard error column.
    master_sheet_titles <- c(rbind(collapsed_combinations, "STE"))
    
    # Create data.frame to hold list titles as column titles for easier adding to master sheet.
    temp_frame <- as.data.frame(matrix(ncol = length(master_sheet_titles)))
    names(temp_frame) <- master_sheet_titles
    
    # Add temp_frame to master sheet's top row to title each column.
    add_frame_to_sheet(temp_frame, sheet_master, master_row_titles, master_col_tumor, titles = T)
    add_frame_to_sheet(temp_frame, sheet_master, master_row_titles, master_col_stroma, titles = T)
}

prime_front_sheet <- function(titles)
{
    # Create data.frame to hold list titles as column titles for easier adding to front sheet.
    temp_frame <- as.data.frame(matrix(ncol = length(titles)))
    names(temp_frame) <- titles
    
    # Add temp_frame to front sheet's top row to title each column.
    add_frame_to_sheet(temp_frame, sheet_front, front_row_titles, front_col_titles, titles = T)
}

add_frame_to_sheet <- function(frame, sheet, start_row, start_col, titles = F)
{
    #
    # Add data frame to xlsx sheet for later saving.
    #
    
    addDataFrame(x = frame, 
                 sheet = sheet, 
                 startRow = start_row, 
                 startColumn = start_col,
                 row.names = F, 
                 col.names = titles)
}

get_case_numbers <- function(curr_path)
{
    temp_files <- list.files(curr_path, pattern = "cell.*summary")

    pattern <- "_(\\d+|\\[\\d+,\\d+\\])_[a-zA-Z_\\.]+"
    
    case_nums <- unname(sapply(temp_files, function(x) str_match(x, pattern)[,2]))
    
    case_nums
}

get_thresholds <- function(path, markers)
{
    # Data on positive threshold information
    score_data <- read.csv(file = paste(path,"_score_data.txt",sep=""), head=TRUE, sep='\t', check.names = F)
    
    # Find all threshold values on spreadsheet
    thresholds <- c()
    for (marker in markers)
    {
        if ("Positivity.Threshold" %in% names(score_data))
        {
            marker_threshold <- score_data[grepl("Positivity Threshold", names(score_data))]
        }
        else
        {
            marker_threshold <- score_data[grepl(paste(marker, ".*Threshold", sep = ""), names(score_data))]
        }
        thresholds <- c(thresholds, marker_threshold)
    }
    names(thresholds) <- markers
    
    thresholds
}

get_marker_mean_data <- function(phenotype_rows, thresholds, const_region, var_region, var_markers)
{
    marker_means <- list()
    
    # Collect column of data per threshold
    for (threshold in names(thresholds))
    {
        if (threshold %in% var_markers)
        {
            temp_data <- phenotype_rows[grepl(paste(var_region, ".*", threshold, ".*Mean", sep=''), names(phenotype_rows))]
            
        }
        else
        {
            temp_data <- phenotype_rows[grepl(paste(const_region, ".*", threshold, ".*Mean", sep=''), names(phenotype_rows))]
        }
        
        marker_means[length(marker_means) + 1] <- temp_data
        
    }
    names(marker_means) <- names(thresholds)
    
    for (temp in 1:length(marker_means))
    {
        marker_means[[temp]][marker_means[[temp]] == "#N/A"] <- "0"
        marker_means[[temp]] <- as.numeric(marker_means[[temp]])
    }
    
    marker_means
}

get_positives_count <- function(markers, marker_means, thresholds)
{
    positives <- 0
    for (marker_mean in 1:length(marker_means[[1]]))
    {
        part_pos <- T
        
        for (marker in markers)
        {
            if (marker_means[[marker]][[marker_mean]] < thresholds[[marker]])
            {
                part_pos <- F
                break
            }
        }
        if(part_pos == T)
        {
            positives <- positives + 1
        }
    }
    
    positives
}

get_positives <- function(cell_data, pm_combinations, thresholds)
{
    ret_data <- list()
    for (combination in pm_combinations)
    {
        curr_positives <- 0
        if (length(combination$phenotypes) == 0)
        {
            phenotype_rows <- cell_data[cell_data$Phenotype != "", ]
            
            # Get list of mean data for each marker
            marker_means <- get_marker_mean_data(phenotype_rows, thresholds, const_region, var_region, var_markers)
            
            if (length(marker_means[[1]]) == 0)
            {
                curr_positives <- curr_positives + 0
            }
            else if (length(combination$markers) > 0)
            {
                curr_positives <- get_positives_count(combination$markers, marker_means, thresholds)
            }
            else
            {
                curr_positives <- curr_positives + length(marker_means[[1]])
            }
        }
        else
        {
            for (phenotype in combination[["phenotypes"]])
            {
                # Get all rows that are marked by the current phenotype
                if ("Phenotype" %in% names(cell_data))
                {
                    if (is.null(phenotype))
                    {
                        phenotype_rows <- cell_data[cell_data$Phenotype != "", ]
                    }
                    else
                    {
                        phenotype_rows <- cell_data[tolower(cell_data$Phenotype) == tolower(phenotype), ]
                    }
                }
                else
                {
                    phenotype_rows <- cell_data
                }
                
                # Get list of mean data for each marker
                marker_means <- get_marker_mean_data(phenotype_rows, thresholds, const_region, var_region, var_markers)
                
                if (length(marker_means[[1]]) == 0)
                {
                    curr_positives <- curr_positives + 0
                }
                else if (length(combination$markers) > 0)
                {
                    curr_positives <- get_positives_count(combination$markers, marker_means, thresholds)
                }
                else
                {
                    curr_positives <- curr_positives + length(marker_means[[1]])
                }
            }
        }
        
        ret_pheno <- ifelse(length(combination$phenotypes) > 0, 
                            paste(combination$phenotypes, collapse = " & "),
                            NA)
        ret_marker <- ifelse(length(combination$markers) > 0, 
                             paste(combination$markers, collapse = " & "),
                             NA)
        
        ret_data <- rbind(ret_data, c(ret_pheno, ret_marker, curr_positives))
    }
    
    ret_data
}

get_mm2_data <- function(ret_data, summary_data, is_ts = F)
{
    if (is_ts)
    {
        # Convert to Mpixels.
        img_res <- summary_data$`Tissue Category Area (pixels)`[[1]] / 1000000
    }
    else
    {
        all_row <- summary_data[summary_data$Phenotype == "All" ,]
        img_res <- all_row$Total.Cells / all_row$`Cell Density (per megapixel)`
    }
    
    # all_row <- summary_data[summary_data$Phenotype == "All" ,]
    # img_res <- all_row$Total.Cells
    
    if (img_res == 0)
    {        
        mm2_res <- rep(0, length(ret_data[,3]))
    }
    else
    {
        mm2_res <- c()
        
        for (point in ret_data[,3])
        {
            MM2 <- point / (img_res * 0.246)
            mm2_res <- c(mm2_res, MM2)
        }
    }
    
    mm2_res
}

get_percentage_data <- function(ret_data, summary_data, is_ts = F)
{
    if (is_ts)
    {
        # Convert to Mpixels.
        img_res <- summary_data$`Tissue Category Area (pixels)`[[1]] / 1000000
    }
    else
    {
        all_row <- summary_data[summary_data$Phenotype == "All" ,]
        img_res <- all_row$Total.Cells / all_row$`Cell Density (per megapixel)`
    }
    
    # all_row <- summary_data[summary_data$Phenotype == "All" ,]
    # img_res <- all_row$Total.Cells
    
    if (img_res == 0)
    {
        percent_res <- rep(0, length(ret_data[,3]))
    }
    else
    {
        percent_res <- c()
        
        for (point in ret_data[,3])
        {
            percent <- (point / img_res) * 100
            percent_res <- c(percent_res, percent)
        }
    }
    
    percent_res
}

run_analysis_mode <- function(modes_selected, ret_data, summary_data, is_ts = F)
{modes <- list("MM2", "Percentages", "Total Counts")

    mode <- modes_selected[[1]]  # TODO: Expand for all selected
    switch(mode,
           "MM2" = {
               return(get_mm2_data(ret_data, summary_data, is_ts))
           },
           "Percentage" = {
               return(get_percentage_data(ret_data, summary_data, is_ts))
           }
           )
}

get_marker_regions <- function(case_path)
{
    region_info <- list(Membrane = list(), Nucleus = list())
    
    # Data on positive threshold information
    score_data <- read.csv(file = paste(case_path,"_score_data.txt",sep=""), head=TRUE, sep='\t', check.names = F)

    cell_compartments <- score_data[grepl("Cell Compartment", names(score_data))]
    stain_components <- score_data[grepl("Stain Component", names(score_data))]

    for (i in 1:length(cell_compartments))
    {
        if (tolower(as.character(cell_compartments[[i]])) == "membrane")
        {
            payload <- strsplit(as.character(stain_components[[i]]), split = " ")[[1]][[1]]
            region_info$Membrane <- list(c(unlist(region_info$Membrane), payload))
        }
        else
        {
            payload <- strsplit(as.character(stain_components[[i]]), split = " ")[[1]][[1]]
            region_info$Nucleus <- list(c(unlist(region_info$Nucleus), payload))
        }
    }
    
    var_markers <<- region_info$Nucleus
    
        
    region_info
}

collect_patient_statistics <- function(study, files, is_ts = F)
{
    print(study)
    print(is_ts)
    
    if (is_ts)
    {
        stroma_case <- grepl("stroma", files, ignore.case = T)
        # curr_path <- paste(root_path, study, "CD31" ,files[stroma_case], sep = "/")
        curr_path <- paste(root_path, study ,files[stroma_case], sep = "/")
    }
    else
    {
        curr_path <- paste(root_path, study, sep = "/")
    }
    
    pattern <- "(_)(\\d+|\\[\\d+,\\d+\\])_[a-zA-Z_\\.]+"
    common_subpath <- gsub(pattern, "\\1", list.files(curr_path, pattern = "cell.*summary")[[1]])
    
    case_nums <- get_case_numbers(curr_path)
    
    curr_master_data <- list()
    
    for (case_num in case_nums)
    {
        case_path <- paste(paste(curr_path, common_subpath, sep = "/"), case_num, sep = "")
        thresholds <- get_thresholds(case_path, markers)
        
        # regions <- get_marker_regions(case_path = case_path)
        region_info <<- get_marker_regions(case_path = case_path)
        
        # Data on each cell sample by phenotype and marker
        cell_data <- read.csv(file = paste(case_path, "_cell_seg_data.txt", sep = ""), head = T, sep = '\t', stringsAsFactors = F, check.names = F)
        
        if (is_ts)
        {
            curr_tissue_data <- list()
            for (tissue in sort(unique(cell_data$`Tissue Category`), decreasing = T))
            {    
                tissue_rows <- cell_data[cell_data$`Tissue Category` == tissue, ]
                ret_data <- get_positives(tissue_rows, pm_combinations, thresholds)
                ret_data[,3] <- as.numeric(ret_data[,3])
                
                summary_data <- read.csv(file = paste(case_path, "cell_seg_data_summary.txt", sep = "_"), head = T, sep = '\t', check.names = F)
                summary_data <- summary_data[summary_data$`Tissue Category` == tissue, ]
                mm2_data <- run_analysis_mode(modes_selected, ret_data, summary_data, is_ts)
                # mm2_data <- get_mm2_data(ret_data, summary_data, is_ts = T)
                ret_data <- cbind(ret_data, mm2_data)
                
                study_name <- paste(study, common_subpath, sep = "/")
                study_name <- paste(study_name, case_num, sep = "")
                
                if (length(ret_data[,1]) < 2)
                {
                    ret_data <- cbind(c(study_name, tissue), rbind(ret_data, ""))
                }
                else
                {
                    ret_data <- cbind(c(study_name, tissue, rep("", length(ret_data[,1]) - 2)), ret_data)
                }
                
                temp_frame <- as.data.frame(ret_data)
                temp_frame <- transform(temp_frame, V4 = as.numeric(V4), mm2_data = as.numeric(mm2_data))
                
                add_frame_to_sheet(as.data.frame(ret_data), sheet_front, front_row, front_col)
                assign("front_row", front_row + length(ret_data[,1]), envir = .GlobalEnv)
                
                curr_tissue_data <- c(curr_tissue_data, c(rbind(mm2_data, NA)))
            }
        }
        else
        {
            ret_data <- get_positives(cell_data, pm_combinations, thresholds)
            ret_data[,3] <- as.numeric(ret_data[,3])
            
            summary_data <- read.csv(file = paste(case_path, "cell_seg_data_summary.txt", sep = "_"), head = T, sep = '\t', check.names = F)
            mm2_data <- run_analysis_mode(modes_selected, ret_data, summary_data, is_ts)
            # mm2_data <- get_mm2_data(ret_data, summary_data, is_ts = T)            
            ret_data <- cbind(ret_data, mm2_data)
            
            study_num <- paste(study, common_subpath, sep = "/")
            study_num <- paste(study_num, case_num, sep = "")
            
            ret_data <- cbind(c(study_num, rep(NA, length(ret_data[,1]) - 1)), ret_data)
            
            temp_frame <- as.data.frame(ret_data)
            temp_frame <- transform(temp_frame, V4 = as.numeric(as.character(V4)), mm2_data = as.numeric(as.character(mm2_data)))
            
            add_frame_to_sheet(temp_frame, sheet_front, front_row, front_col)
            assign("front_row", front_row + length(ret_data[,1]), envir = .GlobalEnv)
            
            
            curr_tissue_data <- c(rbind(mm2_data, NA))
        }
        
        if (case_num == case_nums[[1]])
        {
            curr_master_data[[length(curr_master_data) + 1]] <- c(study, case_num, curr_tissue_data)
        }
        else
        {
            curr_master_data[[length(curr_master_data) + 1]] <- c("", case_num, curr_tissue_data)
        }
        
    }
    temp_frame <- as.data.frame(matrix(unlist(curr_master_data), ncol = length(curr_master_data[[1]]), byrow = T))
    
    if (is.numeric(temp_frame[,2][[1]]))
    {
        temp_frame[,2] <- as.double(as.character(temp_frame[,2]))
    }
    for(i in seq(3, length(temp_frame), 2))
    {
        temp_frame[,i] <- as.double(as.character(temp_frame[,i]))
    }
    
    add_frame_to_sheet(temp_frame, sheet_master, master_row, master_col)
    assign("master_row", master_row + length(temp_frame[,1]), envir = .GlobalEnv)
    
    stderr_list <- list("", "")
    for(i in seq(3, length(temp_frame), 2))
    {
        stder <- std.error(temp_frame[,i])
        curr_mean <- mean(temp_frame[,i])
        if (is.na(stder))
            stderr_list <- c(stderr_list, curr_mean, 0)
        else
            stderr_list <- c(stderr_list, curr_mean, stder)
    }
    
    temp_frame <- as.data.frame(matrix(stderr_list, ncol = length(stderr_list)))
    for(i in seq(3, length(temp_frame)))
    {
        temp_frame[,i] <- as.double(temp_frame[,i])
    }
    
    add_frame_to_sheet(temp_frame, sheet_master, master_row, master_col)
    assign("master_row", master_row + length(temp_frame[,1]), envir = .GlobalEnv)
    
    final_row_data <- c(study, length(case_nums), stderr_list[3:length(stderr_list)])
    temp_frame <- as.data.frame(matrix(final_row_data, ncol = length(final_row_data)))
    for(i in 2:length(temp_frame))
    {
        temp_frame[,i] <- as.double(as.character(temp_frame[,i]))
    }
    add_frame_to_sheet(temp_frame, sheet_final, final_row, final_col)
    final_row <<- final_row + 1
    
    ret_data
    
}

stop_quietly <- function()
{
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
}


region_info <- list()

var_markers <- c()
var_region <-   "Nucleus"

const_region <- "Membrane"

# Global Data Initialization before script runs
gui_lock <- T
exit_status <- 0

data_fname <- "analyze.dat"
pm_combinations <- list()
phenotypes <- list()
markers <- list()
tissues <- list("TUMOR", "STROMA")
input_dir <- ""
output_dir <- ""
output_fname <- ""

modes <- list("MM2", "Percentages", "Total Counts")
modes_selected <- list()


parse_input_file <- function()
{
    f = file(paste("./", data_fname, sep = ""), "r")
    
    lineno <- 1
    
    while ( T )
    {
        line = readLines(f, n = 1)
        if (length(line) == 0)
        {
            break
        }
        
        split <- strsplit(line, ":")
        
        if (length(unlist(split)) == 0)
        {
            next
        }
        
        identifier <- trimws(tolower(unlist(split)[[1]]), which = "both")
        payload <- trimws(unlist(split)[[2]], which = "both")
        

        switch(identifier,
               "input dir" = {
                   input_dir <<- payload
               },
               "output dir" = {
                   output_dir <<- payload
               },
               "output file" = {
                   output_fname <<- payload
               },
               "membrane" = {
                   print(paste("MEMBRANE LIST", payload))
               },
               "nucleus" = {
                   print(paste("NUCLEUS LIST", payload))
                   var_markers <<- c(trimws(unlist(strsplit(payload, ",")), which = "both"))
               },
               "phenotypes" = {
                   if(payload != "")
                   {
                       pm_combinations[[length(pm_combinations) + 1]] <<- list(phenotypes = trimws(unlist(strsplit(payload, ",")), which = "both"), markers = list())
                   }
                   else
                   {
                       pm_combinations[[length(pm_combinations) + 1]] <<- list(phenotypes = list(), markers = list())
                   }
               },
               "markers" = {
                   if(payload != "")
                   {
                       pm_combinations[[length(pm_combinations)]]$markers <<- trimws(unlist(strsplit(payload, ",")), which = "both")
                   }
                   else
                   {
                       pm_combinations[[length(pm_combinations)]]$markers <<- list()
                   }
               },
               "modes" = {
                   modes_selected <<- payload
               },
               " " = {
                   next
               },
               {
                   print(paste("Incorrect identifier: ", identifier, " : on line ", lineno))
               })
        
        lineno <- lineno + 1
    }
    close(f)
}

run_main_menu <- function()
{
    window <- gwindow("TransForm Analyzer", visible = F, width = 300, height = 100)
    layout <- glayout(container = window)
    
    layout[1, 1:2] <- glabel("Load analysis information from existing file?", container = layout)
    layout[2, 1] <- gbutton("Yes", container = layout)
    layout[2, 2] <- gbutton("No", container = layout)
    
    addHandlerClicked(layout[2, 1], handler = function(h,...) {
        load_from_file <<- T
        dispose(window)
        gui_lock <<- F
    })
    
    addHandlerClicked(layout[2, 2], handler = function(h,...) {
        load_from_file <<- F
        dispose(window)
        gui_lock <<- F
    })
    
    visible(window) <- T
}

run_analyzer_gui <- function()
{
    data_row_num <- 2
    data_row_delta <- 0
    
    marker_row_num <- 2
    marker_row_delta <- 0
    
    # Main window setup, want (notebook -> tabbed panel) tabbed interface.
    window <- gwindow("Analyzer Setup", visible = F, width = 300, height = 400)
    nb <- gnotebook(container = window)
    
    # Tab groups
    group1 <- ggroup(label = "File Options", container = nb)
    group2 <- ggroup(label = "Analysis Modes", container = nb)
    group3 <- ggroup(label = "Combination Options", container = nb)
    
    # File Options layout setup
    layout1 <- glayout(spacing = 5, container = group1)
    
    layout1[1, 1] <- glabel("Project Directory", container = layout1)
    layout1[2, 1] <- gedit("", initial.msg = "Project Directory", container = layout1)
    layout1[2, 2] <- gbutton("Find", container = layout1)
    layout1[3, 1] <- glabel("Output Directory", container = layout1)
    layout1[4, 1] <- gedit("", initial.msg = "Output Directory", container = layout1)
    layout1[4, 2] <- gbutton("Find", container = layout1)
    layout1[5, 1] <- glabel("Output Filename (.xlsx)", container = layout1)
    layout1[6, 1] <- gedit("", initial.msg = "Output filename (.xlsx)", container = layout1)
    layout1[7, 1] <- gbutton("Analyze", container = layout1)
    
    # Button Handlers #
    # Function to handle data manipulation once the user has entered all necessary
    # information for the script to consume.
    analyze_handler <- function(h,...) {
        input_dir <-    svalue(layout1[2,1])
        output_dir <-   svalue(layout1[4,1])
        output_fname <- svalue(layout1[6,1])
        
        if (dir.exists(input_dir))
        {
            if (dir.exists(output_dir))
            {
                # Remove previous data sample
                if (file.exists(data_fname)) { file.remove(data_fname) }
                
                # Write I/O information to file
                write(file = data_fname, paste("Input Dir :", input_dir), append = T)
                write(file = data_fname, paste("Output Dir :", output_dir), append = T)
                # write(file = data_fname, paste("Output File :", paste(output_fname, ".xlsx", sep = "")), append = T)
                write(file = data_fname, paste("Output File :", output_fname), append = T)
                
                # Write analysis modes to file
                write(file = data_fname, "", append = T)
                write(file = data_fname, paste("Modes: ", paste(svalue(layout2[1, 1], collapse = ", "), sep = "")), append = T)
                
                # Write marker region information to file
                write(file = data_fname, "", append = T)
                membrane_list <- list()
                nucleus_list <- list()
                if (marker_row_delta > 0)
                {
                    for (i in (marker_row_num + 1) : (marker_row_num + marker_row_delta))
                    {
                        switch(svalue(layout2[i, 2]),
                               "Membrane" = { 
                                   membrane_list <- c(membrane_list, svalue(layout2[i, 1])) 
                                   },
                               "Nucleus"  = { 
                                   nucleus_list  <- c(nucleus_list, svalue(layout2[i, 1])) 
                                   } 
                        )
                    }
                }
                write(file = data_fname, paste("Membrane: ", paste(membrane_list, collapse = ", ")), append = T)
                write(file = data_fname, paste("Nucleus: ",  paste(nucleus_list,  collapse = ", ")), append = T)
                
                # Write Phenotype / Marker information to file
                write(file = data_fname, "", append = T)
                if (data_row_delta > 0)
                {
                    for (i in (data_row_num + 1) : (data_row_num + data_row_delta))
                    {
                        write(file = data_fname, paste("Phenotypes :", svalue(layout3[i,1])), append = T)
                        write(file = data_fname, paste("Markers :",    svalue(layout3[i,2])), append = T)
                        write(file = data_fname, "", append = T)
                    }
                }
                gui_lock <<- F
            }
            else
            {
                ewin <- gwindow("Error", height = 10, width = 160)
                glabel("Invalid Output Directory!", container = ewin)
            }
        }
        else
        {
            ewin <- gwindow("Error", height = 10, width = 160)
            glabel("Invalid Input Directory!", container = ewin)
        }
        
        exit_status <<- 0
    }
    
    # Analyze button
    addHandlerClicked(layout1[7, 1], handler = analyze_handler)
    
    # Project directory find button
    addHandlerClicked(layout1[2, 2], handler = function(h,...) {
        dir <- gfile("Select the project directory", type = "selectdir")
        svalue(layout1[2,1]) <- dir
    })
    
    # Output directory find button
    addHandlerClicked(layout1[4, 2], handler = function(h,...) {
        dir <- gfile("Select the output directory", type = "selectdir")
        svalue(layout1[4,1]) <- dir
    })
    
    # Analysis Modes
    layout2 <- glayout(container = group2)
    
    layout2[1, 1] <- gcheckboxgroup(items = unlist(modes), container = layout2)
    
    # Combination Options layout setup
    layout3 <- glayout(spacing = 5, container = group3)
    
    layout3[1, 1] <- glabel("Phenotypes", container = layout3)
    layout3[1, 2] <- glabel("Markers", container = layout3)
    layout3[2, 1:2] <- gbutton("Add", container = layout3)
    
    # Button Handlers #
    # Add button
    addHandlerClicked(layout3[2, 1], handler = function(h,...) {
        data_row_delta <<- data_row_delta + 1
        layout3[data_row_num + data_row_delta, 1] <- gedit("", container = layout3)
        layout3[data_row_num + data_row_delta, 2] <- gedit("", container = layout3)
    })
    
    addHandlerDestroy(window, function(h,...) {
        exit_status <<- -1
        gui_lock <<- F
        dispose(window)
    })
    
    # Default to first tab
    svalue(nb) <- 1
    
    # Window creation complete, ready to show result
    visible(window) <- T
}

run_gui <- function()
{
    gui_lock <<- T
    run_main_menu()
    while(gui_lock) {}
    
    if (load_from_file)
        return()
    
    gui_lock <<- T
    run_analyzer_gui()
    print("Waiting for GUI completion")
    while(gui_lock) {}
}

load_from_file <- T

# Generate analysis file from GUI control.
run_gui()

if (exit_status == -1)
{
    stop_quietly()
}

parse_input_file()

# Remove all completely empty combinations
pm_combinations <- pm_combinations[!sapply(pm_combinations, function(x) (length(x[[1]]) == 0) && (length(x[[1]]) == length(x[[2]])))]

# Collect all unique, non-empty, phenotypes from the input combinations.
phenotypes <- unique(sapply(pm_combinations, `[[`, "phenotypes"))
phenotypes <- phenotypes[sapply(phenotypes, length) > 0]

# Collect all unique, non-empty, markers from the input combinations.
markers <- unique(sapply(pm_combinations, `[[`, "markers"))
markers <- markers[sapply(markers, length) > 0]
markers <- unique(unlist(markers))
markers <- lapply(markers, trimws, which = "both")

# Setup xlsx document.
wb_name <- paste(output_fname, ".xlsx", sep = "")
wb_name <- paste(output_dir, wb_name, sep = "/")

# Remove old version of xlsx workbook.
if (file.exists(wb_name)) file.remove(wb_name)
 
# Create xlsx workbook and datasheets.
wb <- createWorkbook()
sheet_front <- createSheet(wb, paste(output_fname, "Overview", sep = " "))
sheet_master <- createSheet(wb, "MASTER")
sheet_final <- createSheet(wb, "Final")

# # # #
# Initialize pointers to row and column for each sheet to avoid clobering data.
# # # #
front_row <- 2
front_col <- 1

front_row_titles <- 1
front_col_titles <- 1

master_row <- 3
master_col <- 1

master_row_tumor <- 1
master_col_tumor <- 3

master_row_stroma <- 1
master_col_stroma <- master_col_tumor + (2 * length(pm_combinations))

master_row_titles <- 2
master_col_titles <- 1

final_row_titles <- 1
final_col_titles <- 1

final_row <- 2
final_col <- 1
# # # #
#
# # # #

# Create titles for master sheet and add them to the master datasheet.
prime_master_sheet(pm_combinations, master_col_tumor, master_col_stroma)
prime_front_sheet(c("Study", "Phenotype", "Marker(s)", "Count", modes_selected[[1]]))
prime_final_sheet(pm_combinations, final_row_titles, final_col_titles)

# Let's go!
root_path = input_dir

for(study in list.files(root_path))
{
    curr_path <- paste(root_path, study, sep = "/")
    files <- list.files(curr_path)

    if ("stroma" %in% tolower(files)) {
        is_ts <<- T
    } else {
        is_ts <<- F
    }
    
    collect_patient_statistics(study, files, is_ts = is_ts)
}


saveWorkbook(wb, wb_name)

