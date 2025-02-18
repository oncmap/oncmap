#' @title Read input file
#' @param infile Input CSV file name
#' @param include_formats Which formats to include in checking
#' @param exclude_formats Which formats to exclude from checking
#' @param formats_def New formats definition
#' @param infile_data_output Include infile data frame in the result
#' @examples
#' # print(read_input("tests/testthat/ecap1.csv"))
#' # print(read_input("tests/testthat/ecap2.csv"))
#' # print(read_input("tests/testthat/simplemed.csv"))
#' # print(read_input("tests/testthat/mems.csv"))
#' # print(read_input("tests/testthat/adheretech.csv"))
#' # print(read_input("tests/testthat/ecap.csv"))
#' # print(read_input("tests/testthat/1077.csv"))
#' # print(read_input("tests/testthat/noformat.csv"))
#' # print(read_input("tests/testthat/nocsv.csv"))
#' @importFrom readr read_csv cols
#' @importFrom readxl read_excel read_xlsx
#' @importFrom tools file_ext file_path_sans_ext
#' @export
read_input <- function(infile, include_formats = NULL, exclude_formats = NULL,
                       formats_def = NULL,
                       infile_data_output = FALSE) {
  # init logging
  log <- data.frame()

  # include/exclude formats based on parameters
  # default filtered_formats to built-in input_formats
  filtered_formats <- input_formats
  if (!is.null(formats_def)) {
    filtered_formats <- formats_def
  }
  # exclude any formats specified in exclude_formats
  if (!is.null(include_formats)) {
    filtered_formats <- filtered_formats[rownames(filtered_formats) %in% include_formats, ]
  }
  # include only formats specified in include_formats (unless NULL)
  if (!is.null(exclude_formats)) {
    filtered_formats <- filtered_formats[!(rownames(filtered_formats) %in% exclude_formats), ]
  }
  # attempt to only match formats in filtered_formats


  # iterate through enabled formats for file specified by infile parameter
  for (i in 1:nrow(filtered_formats)) {
    format <- filtered_formats[i, ]
    format_name <- rownames(format)
    infile_extension <- tolower(file_ext(infile))

    # log <- rbind(log, data.frame(format=format_name,message='Starting'))

    patient_id <- NULL
    device_id <- NULL

    # read in file based on extension (csv -> read_csv, xls -> read_excel, xlsx -> read_xlsx)
    infile_data <- tryCatch(
      {
        if (infile_extension == "csv") {
          # use read_csv in order to read in all data as characters

          # read.csv(infile,skip=format$skip_header_lines)
          read_csv(infile, skip = format$skip_header_lines, col_types = cols(.default = "c"), name_repair = "minimal")
          # name_repair="universal")
          # name_repair=make.names)
        } else if (infile_extension == "xls") {
          read_excel(infile, skip = format$skip_header_lines)
        } else if (infile_extension == "xlsx") {
          read_xlsx(infile, skip = format$skip_header_lines)
        } else {
          stop(paste0("Unknown extension of file ", infile))
        }
      },
      error = function(x) {
        log <<- rbind(log, data.frame(format = format_name, message = x$message))
        return(NA)
      }
    )

    # if no data read in or error -- try next file format

    # NA -- error in reading
    if (length(infile_data) == 1 && is.na(infile_data)) {
      log <- rbind(log, data.frame(format = format_name, message = "No meaningful data"))
      next
    }

    # empty file
    if (nrow(infile_data) == 0) {
      log <- rbind(log, data.frame(format = format_name, message = "No data."))
      next
    }
    # if (class(infile_data) == 'logical' && is.na(infile_data)) next

    # gather the list of headers
    # FIXME: ignore ordering
    infile_headers <- paste(names(infile_data), collapse = ",")

    # if headers don't match the format -- try different format
    if (!is.na(format$headers) && infile_headers != format$headers) {
      # headers defined but mismatch headers
      log <- rbind(log, data.frame(format = format_name, message = paste("headers defined but mismatch headers:", infile_headers)))
      next
    }

    # datetime is expected in format$datetime_header -- validates both format and data
    if (!(format$datetime_header %in% colnames(infile_data))) {
      # datetime header not present
      log <- rbind(log, data.frame(format = format_name, message = "datetime header not present"))
      next
    }

    # if the format has patientid_header specified validate it exists
    if (!is.na(format$patientid_header) &&
      !(
        format$patientid_header %in% colnames(infile_data)
      )) {
      # header for patientid_header but not present
      log <- rbind(log, data.frame(format = format_name, message = "header for patientid not present"))
      next
    }

    data <- infile_data

    # patient id on the first line of the header
    if (!is.na(format$header_line_patientid) && infile_extension == "csv") {
      # if patient_id is in the headerline of the file content -- extract patient_id

      # regexp to extract patient id from the headerline
      patient_id <- readLines(infile, n = 1)
      # "Patient: \\(ID: ([^)]*)\\)"
      patient_id <- gsub(format$header_line_patientid, "\\1", patient_id)
    }
    if (!is.na(format$filter)) {
      # some input files need filtering out/in specific
      # filter examples:
      # key == value
      # key != value

      # apply filter (infill might be needed)
      key <- gsub("([^=]*)([!=]=)([^=]*)", "\\1", format$filter)
      cond <- gsub("([^=]*)([!=]=)([^=]*)", "\\2", format$filter)
      val <- gsub("([^=]*)([!=]=)([^=]*)", "\\3", format$filter)
      # filter_str <- paste0("data <- data[data$`", key, '`=="', val, '",]')
      filter_str <- paste0("data <- data[data$`", key, '`',cond,'"', val, '",]')
      # print(filter_str)
      eval(parse(text = filter_str))
    }

    # an format option to use the input filename as patientid
    if (!is.na(format$patientid_filename)) {
      # read in patient id as the filename without extension
      patient_id <- tools::file_path_sans_ext(basename(infile))
    }

    # if patientid column exists validate that it is consistent (unique) and extract
    if (!is.na(format$patientid_header)) {
      # read in patient id
      # verify consistency
      patientids <- unique(infile_data[, format$patientid_header])
      patientids <- patientids[!is.na(patientids)]
      if (length(patientids) > 1) {
        log <- rbind(log, data.frame(format = format_name, message = "patient id has too many values"))
        next
      } else if (length(patientids) == 1) {
        patient_id <- patientids[1]
      } else {
        patient_id <- ""
      }
    }

    # if deviceid column exists validate that it is consistent (unique) and extract
    if (!is.na(format$deviceid_header)) {
      # read in device id
      # verify consistency
      deviceids <- unique(infile_data[, format$deviceid_header])
      deviceids <- deviceids[!is.na(deviceids)]
      if (length(deviceids) > 1) {
        log <- rbind(log, data.frame(format = format_name, message = "device id has too many values"))
        device_id <- ""
        # next
      } else if (length(deviceids) == 1) {
        device_id <- deviceids[1]
      } else {
        device_id <- ""
      }
    }

    # extract timestamps
    data <- data[, format$datetime_header]
    colnames(data) <- "timestamp_infile"

    # remove NAs from timestamps
    data <- data[!is.na(data$timestamp_infile), ]

    if (is.na(format$datetime_format)) {
      # FIXME: handle some default datetime format
    }

    # FIXME: validate -- data format and consecutive

    tmp_timestamp_infile <- data$timestamp_infile

    # remove ":" from timestamp if exists to allow for strptime (expecting TZ as -0400 instead of -04:00)
    # if (format$tz_colon_fix) {
    #   tmp_timestamp_infile <- sub(":([0-9][0-9])$","\\1",tmp_timestamp_infile)
    # }

    data$timestamp <- as.POSIXct(strptime(tmp_timestamp_infile, format = format$datetime_format, tz="UTC"))
    if (all(is.na(data$timestamp))) {
        log <- rbind(log, data.frame(format = format_name, message = "All timestamps are NA. Skipping this format"))
        next
    }

    # subtract with and without timezone -- to get offset from est
    # est_offset <- unique(as.POSIXct(strptime(data$timestamp_infile, format = gsub("%z","",format$datetime_format)))-data$timestamp)
    #
    # if (length(est_offset) != 1) {
    #   log <- rbind(log, data.frame(format = format_name, message = paste("Unable to determine timezone offset",paste(est_offset,collapse=","))))
    #   next
    # }
    #
    # if (est_offset != 0) {
    #    data$timestamp <- data$timestamp + est_offset*3600
    # }

    log <- rbind(log, data.frame(format = format_name, message = "ok"))

    # validate date format
    # verify consistent patient id
    # verify consistent device id (if specified)
    # filter (if specified)
    # return list of openings, patient id, device id
    if (infile_data_output == FALSE) infile_data <- NULL
    return(list(format = format_name, format_def = format, patient_id = patient_id, device_id = device_id, data = data, log = log, infile_data = infile_data)) # , est_offset = est_offset))
  }
  if (infile_data_output == FALSE) infile_data <- NULL
  return(list(format = NULL, format_def = NULL, patient_id = NULL, device_id = NULL, data = NULL, log = log, infile_data = infile_data))
}
