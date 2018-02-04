list.of.packages <- c("xlsx", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = T)


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




get_case_numbers <- function(rows)
{
    pattern <- ".*_(\\d+)_component_data.tif"
    
    case_nums <- unique(unname(sapply((rows), function(x) str_match(x, pattern)[,2])))
    
    return(as.numeric(case_nums))
}



# Setup xlsx document.
wb_name <- paste("HALO ALL", ".xlsx", sep = "")

# Remove old version of xlsx workbook.
if (file.exists(wb_name)) file.remove(wb_name)

# Create xlsx workbook and datasheets.
wb <- createWorkbook()
sheet_master <- createSheet(wb, "MASTER")
sheet_percent <- createSheet(wb, "PERCENTAGES")

master_row <- 1
master_col <- 1

percent_row <- 1
percent_col <- 1

root_path <- "/data/morgan/kristen_data/HALO3"

dye_pattern <- "Dye\\.\\d+\\.Positive(?!\\.)"

# targets <- list(list(2, "ALL"),
#                 list(2),
#                 list(2,5),
#                 list(2,4),
#                 list(2,4,5),
#                 list(2,3),
#                 list(2,3,5),
#                 list(2,3,4),
#                 list(2,3,4,5)
#                 )
# targets <- list(list(2, 6, "ALL"),
#                 list(2,6),
#                 list(2,5,6),
#                 list(2,4,6),
#                 list(2,4,5,6),
#                 list(2,3,6),
#                 list(2,3,5,6),
#                 list(2,3,4,6),
#                 list(2,3,4,5,6)
# )
# targets <- list(list(4, "ALL"),
#                 list(4),
#                 list(4,2),
#                 list(4,3),
#                 list(4,5),
#                 list(4,2,3),
#                 list(4,2,5),
#                 list(4,2,3,5),
#                 list(4,3,5)
#                 )
targets <- list(list(1))

titles <- c("Patient", "Img", paste(lapply(targets, paste, collapse = "/")))

temp_frame <- as.data.frame(matrix(unlist(titles), ncol = length(titles), byrow = T))
add_frame_to_sheet(temp_frame, sheet_master, master_row, master_col)
add_frame_to_sheet(temp_frame, sheet_percent, percent_row, percent_col)


master_row <<- master_row + 1
master_col <<- master_col + 1

percent_row <<- percent_row + 1
percent_col <<- percent_col + 1


for (i in list.files(root_path))
{
    print(i)
    
    temp_frame <- as.data.frame(matrix(unlist(i), ncol = 1, byrow = T))
    add_frame_to_sheet(temp_frame, sheet_master, master_row, 1)
    add_frame_to_sheet(temp_frame, sheet_percent, percent_row, 1)
    
    curr_path <- paste(root_path, i, sep = "/")
    
    obj_file <- list.files(curr_path)[grepl(".*Object\\.csv", list.files(curr_path))]
    obj_file <- paste(curr_path, obj_file, sep = "/")
    
    data <- read.csv(obj_file, header = T)
    
    case_nums <- get_case_numbers(data[[1]])
    
    for (num in case_nums)
    {
        positive_count_list <- list()
        
        positive_count_list[length(positive_count_list) + 1] <- num
        
        case_num_pattern <- paste(".*_", num, "_component_data\\.tif", sep = "")
        case_num_rows <- data[grepl(case_num_pattern, data[[1]]),]
        dye_columns <- case_num_rows[grepl(dye_pattern, names(data), perl = T)]
        dye_columns <- dye_columns[1:length(dye_columns)]
        
        for (target in targets)
        {
            positive_count <- 0
            
            if ("ALL" %in% target)
            {
                positive_rows <- dye_columns[dye_columns[paste("Dye", target[[1]], "Positive", sep = ".")] == 1, ]
                if (length(target) > 1)
                {
                    for (dye in target[2:length(target)])
                    {
                        if (length(positive_rows[[1]]) == 0 || dye == "ALL")
                        {
                            next()
                        }
                        positive_rows <- positive_rows[positive_rows[paste("Dye", dye, "Positive", sep = ".")] == 1, ]
                        
                    }
                }
                if (length(positive_rows[[1]]) > 0)
                {
                    positive_count <- length(positive_rows[[1]])
                }
                else
                {
                    positive_count <- 0
                }
            }
            else
            {
                positive_rows <- dye_columns[dye_columns[paste("Dye", target[[1]], "Positive", sep = ".")] == 1, ]
                if (length(target) > 1)
                {
                    for (dye in target[2:length(target)])
                    {
                        if (length(positive_rows[[1]]) == 0)
                        {
                            break
                        }
                        positive_rows <- positive_rows[positive_rows[paste("Dye", dye, "Positive", sep = ".")] == 1, ]
                        
                    }
                }
                if (length(positive_rows[[1]]) > 0)
                {
                    positive_count <- sum(rowSums(positive_rows) == length(target))
                }
                else
                {
                    positive_count <- 0
                }
            }
            
            positive_count_list[length(positive_count_list) + 1] <- positive_count
        }
        
        # print(paste("Patient:  ", i))
        # print(paste("Image:  ", num))
        # print(unlist(positive_count_list))
        
        temp_frame <- as.data.frame(matrix(unlist(positive_count_list), ncol = length(positive_count_list), byrow = T))
        add_frame_to_sheet(temp_frame, sheet_master, master_row, master_col)
        master_row <<- master_row + 1
        
        master_percent <- positive_count_list[[2]]
        percent_list <- positive_count_list[1]
        
        for (per in positive_count_list[2:length(positive_count_list)])
        {
            percent_list[length(percent_list) + 1] <- (per / master_percent) * 100
        }
        
        temp_frame <- as.data.frame(matrix(unlist(percent_list), ncol = length(percent_list), byrow = T))
        add_frame_to_sheet(temp_frame, sheet_percent, percent_row, percent_col)
        
        percent_row <<- percent_row + 1
    }
}

saveWorkbook(wb, wb_name)








