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




get_case_number <- function(rows)
{
    pattern <- ".*_(\\d+)_component_data.tif"
    
    case_nums <- unique(unname(sapply((rows), function(x) str_match(x, pattern)[,2])))
    
    return(as.numeric(case_nums))
}



# Setup xlsx document.
wb_name <- paste("HALO CD8 EXTRA", ".xlsx", sep = "")

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

data_file <- file.choose()

data <- read.csv(data_file, header = T)

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
targets <- list(list(2, 6, "ALL"),
                list(2,6),
                list(2,5,6),
                list(2,4,6),
                list(2,4,5,6),
                list(2,3,6),
                list(2,3,5,6),
                list(2,3,4,6),
                list(2,3,4,5,6)
)
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

# titles <- c("Patient", "Img", paste(lapply(targets, paste, collapse = "/")))
titles <- c("Patient", paste(lapply(targets, paste, collapse = "/")))


temp_frame <- as.data.frame(matrix(unlist(titles), ncol = length(titles), byrow = T))
add_frame_to_sheet(temp_frame, sheet_master, master_row, master_col)
add_frame_to_sheet(temp_frame, sheet_percent, percent_row, percent_col)


master_row <<- master_row + 1
master_col <<- master_col + 1

percent_row <<- percent_row + 1
percent_col <<- percent_col + 1


for (i in unique(data$Image.Location))
{
    print(i)
    
    temp_frame <- as.data.frame(matrix(unlist(i), ncol = 1, byrow = T))
    add_frame_to_sheet(temp_frame, sheet_master, master_row, 1)
    add_frame_to_sheet(temp_frame, sheet_percent, percent_row, 1)
    
    curr_data <- data[data$Image.Location == i, ]

    pattern <- ".*_(\\d+)_component_data.tif"
    
    num <- unique(unname(sapply((curr_data), function(x) str_match(x, pattern)[,2])))
    
    num <- as.numeric(num)
    
    positive_count_list <- list()
    
    # positive_count_list[length(positive_count_list) + 1] <- num
    
    dye_columns <- curr_data[grepl(dye_pattern, names(curr_data), perl = T)]
    dye_columns <- dye_columns[2:length(dye_columns)]
    
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


saveWorkbook(wb, wb_name)








