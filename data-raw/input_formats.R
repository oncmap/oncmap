## code to prepare `input_formats` dataset goes here

input_formats <- rbind(
  "ecap1" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "ECM ID",
    # headers="Index,Patient.ID,ECM.ID,Dose..,Dose.Timestamp,Dose.Timestamp.UTC,Dose.Group,Dose.Label",
    headers = "Index,Patient ID,ECM ID,Dose #,Dose Timestamp,Dose Timestamp UTC,Dose Group,Dose Label",
    patientid_header = "Patient ID",
    datetime_header = "Dose Timestamp",
    datetime_format = "%Y-%m-%dT%H:%M:%S", # "YYYY-MM-DDTHH:MM:SS-TZ", # %z -- no timezone
    tz_colon_fix = TRUE,
    filter = NA
  ),
  "ecap2" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "Package ID",
    # headers="Patient,Project,Package.ID,Regimen.ID,Package.Label,Patient.Dose.Index,Dose.Date,Dose.Timestamp,Dose.Timestamp.UTC,Dose.Group,Dose.Label,Adherent",
    headers = "Patient,Project,Package ID,Regimen ID,Package Label,Patient Dose Index,Dose Date,Dose Timestamp,Dose Timestamp UTC,Dose Group,Dose Label,Adherent",
    patientid_header = "Patient",
    datetime_header = "Dose Timestamp",
    datetime_format = "%Y-%b-%dT%H:%M:%S", # "YYYY-MMM-DDTHH:MM:SS-TZ", # %z -- no timezone
    tz_colon_fix = TRUE,
    filter = NA
  ),
  "ecap3" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "Package ID",
    headers = "Patient,Project,Package ID,Regimen ID,Config Label,Patient Dose Index,Dose Timestamp,Dose Timestamp UTC,Medications,Dose Group,Dose Label,Adherent,Type,eDiary Status,Original Timestamp,reason",
    patientid_header = "Patient",
    datetime_header = "Dose Timestamp",
    datetime_format = "%Y-%m-%dT%H:%M:%S", # "YYYY-MM-DDTHH:MM:SS-TZ", # %z -- no timezone
    tz_colon_fix = TRUE,
    filter = NA
  ),
  "ecap2xls" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "Package ID",
    headers = "Package Index,Study ID,Site ID,Subject ID,Kit ID,Package ID,Dose Regimen,Dose Index,Dose Date,Dose Timestamp,Dose Timestamp UTC,Dose Group,Dose Label,Compliant",
    patientid_header = "Study ID",
    datetime_header = "Dose Timestamp",
    datetime_format = "%Y-%m-%dT%H:%M:%S", # "2020-03-17T16:10:33-04:00",  # %z -- no timezone
    tz_colon_fix = TRUE,
    filter = NA
  ),
  "simplemed" = data.frame(
    skip_header_lines = 1,
    header_line_patientid = "Patient:  \\(ID: ([^)]*)\\).*",
    patientid_filename = NA,
    deviceid_header = "Device SN",
    # headers="X,Event.ID,Device.SN,Event.Type,Details,Create.Time,is.Rpm.event",
    # headers="...1,Event ID,Device SN,Event Type,Details,Create Time,is Rpm event",
    headers = ",Event ID,Device SN,Event Type,Details,Create Time,is Rpm event",
    patientid_header = NA,
    datetime_header = "Create Time",
    datetime_format = "%m/%d/%Y, %I:%M:%S %p", # "m/d/yyyy, hh:mm:ss am",
    tz_colon_fix = FALSE,
    filter = "Event Type==Pill was taken"
  ),
  "mems" = data.frame(
    skip_header_lines = 1,
    header_line_patientid = NA,
    patientid_filename = TRUE,
    deviceid_header = "Identification number",
    # headers="Date,IntakeStatusDisplayResource,Indication...pathology,Identification.number,Label,CavityLabel,IntakeChangeReasons",
    headers = "Date,IntakeStatusDisplayResource,Indication / pathology,Identification number,Label,CavityLabel,IntakeChangeReasons,",
    patientid_header = NA,
    datetime_header = "Date",
    datetime_format = "%m/%d/%Y %I:%M:%S %p", # "m/d/yyyy hh:mm:ss am",
    tz_colon_fix = FALSE,
    filter = NA
  ),
  "mems2" = data.frame(
    skip_header_lines = 1,
    header_line_patientid = NA,
    patientid_filename = TRUE,
    deviceid_header = "Identification number",
    headers = "Date,IntakeStatusDisplayResource,Indication / pathology,Identification number,Label,CavityLabel,Comment,IntakeChangeReasons",
    patientid_header = NA,
    datetime_header = "Date",
    datetime_format = "%m/%d/%Y %H:%M", # "m/d/yyyy hh:mm",
    tz_colon_fix = FALSE,
    filter = "IntakeStatusDisplayResource!=Missing day" # NA,
  ),
  "adheretech" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "Device_UID",
    # headers="Patient_UID,Device_UID,Site,Medication,Reminder_Sent,Status,Deadline_UTC,Dose_Date_UTC,Time_Recorded_UTC,Patient_Timezone,Deadline_Patient_Timezone,Dose_Date_Patient_Timezone,Time_Recorded_Patient_Timezone",
    headers = "Patient_UID,Device_UID,Site,Medication,Reminder_Sent,Status,Deadline_UTC,Dose_Date_UTC,Time_Recorded_UTC,Patient_Timezone,Deadline_Patient_Timezone,Dose_Date_Patient_Timezone,Time_Recorded_Patient_Timezone",
    patientid_header = "Patient_UID",
    datetime_header = "Time_Recorded_Patient_Timezone",
    datetime_format = "%m/%d/%Y %H:%M", # "m/d/yyyy hh:mm",
    tz_colon_fix = FALSE,
    filter = "Status!=MISSED"
  ),
  "adheretechxls" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "Device_UID",
    # headers="Patient_UID,Device_UID,Site,Medication,Reminder_Sent,Status,Deadline_UTC,Dose_Date_UTC,Time_Recorded_UTC,Patient_Timezone,Deadline_Patient_Timezone,Dose_Date_Patient_Timezone,Time_Recorded_Patient_Timezone",
    headers = "Patient_UID,Device_UID,Site,Medication,Reminder_Sent,Status,Deadline_UTC,Dose_Date_UTC,Time_Recorded_UTC,Patient_Timezone,Deadline_Patient_Timezone,Dose_Date_Patient_Timezone,Time_Recorded_Patient_Timezone",
    patientid_header = "Patient_UID",
    datetime_header = "Time_Recorded_Patient_Timezone",
    datetime_format = "%Y-%m-%d %H:%M:%S", # "m/d/yyyy hh:mm",
    tz_colon_fix = FALSE,
    filter = "Status!=MISSED"
  ),
  "ecap_old" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "Package ID",
    headers = NA,
    patientid_header = "Subject ID",
    datetime_header = "Dose Timestamp",
    datetime_format = "%Y-%m-%dT%H:%M:%S", # "YYYY-MM-DDTHH:MM:SS-TZ",
    tz_colon_fix = FALSE,
    filter = NA
  ),
  "clevercap" = data.frame(
    skip_header_lines = 0,
    header_line_patientid = NA,
    patientid_filename = NA,
    deviceid_header = "CleverKey",
    headers = NA,
    patientid_header = "PatientNumber",
    datetime_header = "DateStamp",
    datetime_format = "%m/%d/%Y %H:%M:%S", # "YYYY-MM-DDTHH:MM:SS-TZ",
    tz_colon_fix = FALSE,
    filter = "EventType==Taken"
  )
)

usethis::use_data(input_formats, overwrite = TRUE)
