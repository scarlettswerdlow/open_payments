###############################################################################
#                                                                             #
#  Big Data                                                                   #
#  Project: Load dependencies, and read in and clean data                     #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  May 23, 2015                                                               #
#                                                                             #
###############################################################################

##################
#                #
#  DEPENDENCIES  #
#                #
##################

# Use arules package to create association rules
if (require("arules")) {
  cat("arules is loaded correctly\n")
} else {
  cat("Trying to install arules\n")
  install.packages("arules")
  if (require("arules")) {
    cat("arules successfully installed and loaded\n")
  } else {
    stop("Cound not install arules")
  }
}

# Use data.table package to read csv in as data table (faster)
if (require("data.table")) {
  cat("data.table is loaded correctly\n")
} else {
  cat("Trying to install data.table\n")
  install.packages("data.table")
  if (require("data.table")) {
    cat("data.table successfully installed and loaded\n")
  } else {
    stop("Cound not install data.table")
  }
}

# Use gamlr package to build predictive models
if (require("gamlr")) {
  cat("gamlr is loaded correctly\n")
} else {
  cat("Trying to install gamlr\n")
  install.packages("gamlr")
  if (require("gamlr")) {
    cat("gamlr successfully installed and loaded\n")
  } else {
    stop("Cound not install gamlr")
  }
}

# Use igraph package to perform network analysis
if (require("igraph")) {
  cat("igraph is loaded correctly\n")
} else {
  cat("Trying to install igraph\n")
  install.packages("igraph")
  if (require("igraph")) {
    cat("igraph successfully installed and loaded\n")
  } else {
    stop("Cound not install igraph")
  }
}

##########
#        #
#  DATA  #
#        #
##########

################################################################################
# Function to read in and clean general payments data set                      #
# Args:                                                                        #
#   - wd (str): Working directory path; should be project folder               #
#   - fn (str): File name of general payments dataset; should be csv           #
# Return:                                                                      #
#   - Data frame of cleaned payments data set                                  #
################################################################################

loadData <- function(wd, fn) {
  # Set working directory to project folder
  cat(paste("Setting working directory to ", wd, "\n", sep=""))
  setwd(wd)
  
  # Read in data
  cat(paste("Reading in data set from ", wd, fn, "\n", sep=""))
  rv <- fread(fn)
  rv <- as.data.frame(rv)
  
  # Change column names
  cat("Changing column names\n")
  names(rv) <- c(
    "General_Transaction_ID" = "trans_id",
    "Program_Year" = "program_yr",
    "Payment_Publication_Date" = "pmt_pub_date",
    "Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name" = "manu",
    "Covered_Recipient_Type" = "rtype",
    "Teaching_Hospital_ID" = "hosp_id",
    "Teaching_Hospital_Name" = "hosp_name",
    "Physician_Profile_ID" = "phys_id",
    "Physician_First_Name" = "phys_fname",
    "Physician_Middle_Name" = "phys_mname",
    "Physician_Last_Name" = "phys_lname",
    "Physician_Name_Suffix" = "suff",
    "Recipient_Primary_Business_Street_Address_Line1" = "recipient_addr1",
    "Recipient_Primary_Business_Street_Address_Line2" = "recipient_addr2",
    "Recipient_City" = "recipient_city",
    "Recipient_State" = "rstate",
    "Recipient_Zip_Code" = "rzip",
    "Recipient_Country" = "rcountry",
    "Recipient_Province" = "rprovince",
    "Recipient_Postal_Code" = "rpost",
    "Physician_Primary_Type" = "ptype",
    "Physician_Specialty" = "pspec",
    "Physician_License_State_code1" = "phys_license_state1",
    "Physician_License_State_code2" = "phys_license_state2",
    "Physician_License_State_code3" = "phys_license_state3",
    "Physician_License_State_code4" = "phys_license_state4",
    "Physician_License_State_code5" = "phys_license_state5",
    "Product_Indicator" = "proindic",
    "Name_of_Associated_Covered_Drug_or_Biological1" = "drug_name1",
    "Name_of_Associated_Covered_Drug_or_Biological2" = "drug_name2",
    "Name_of_Associated_Covered_Drug_or_Biological3" = "drug_name3",
    "Name_of_Associated_Covered_Drug_or_Biological4" = "drug_name4",
    "Name_of_Associated_Covered_Drug_or_Biological5" = "drug_name5",
    "NDC_of_Associated_Covered_Drug_or_Biological1" = "drug_ndc1",
    "NDC_of_Associated_Covered_Drug_or_Biological2" = "drug_ndc2",
    "NDC_of_Associated_Covered_Drug_or_Biological3" = "drug_ndc3",
    "NDC_of_Associated_Covered_Drug_or_Biological4" = "drug_ndc4",
    "NDC_of_Associated_Covered_Drug_or_Biological5" = "drug_ndc5",
    "Name_of_Associated_Covered_Device_or_Medical_Supply1" = "supply1",
    "Name_of_Associated_Covered_Device_or_Medical_Supply2" = "supply2",
    "Name_of_Associated_Covered_Device_or_Medical_Supply3" = "supply3",
    "Name_of_Associated_Covered_Device_or_Medical_Supply4" = "supply4",
    "Name_of_Associated_Covered_Device_or_Medical_Supply5" = "supply5",
    "Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name" = "paying_gpo_name",
    "Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_ID" = "paying_gpo_id",
    "Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_State" = "mstate",
    "Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Country" = "mcountry",
    "Dispute_Status_for_Publication" = "dispute",
    "Total_Amount_of_Payment_USDollars" = "payam",
    "Date_of_Payment" = "pmt_date",
    "Number_of_Payments_Included_in_Total_Amount" = "pmt_cnt",
    "Form_of_Payment_or_Transfer_of_Value" = "formpay",
    "Nature_of_Payment_or_Transfer_of_Value" = "natpay",
    "City_of_Travel" = "travel_city",
    "State_of_Travel" = "tstate",
    "Country_of_Travel" = "tcountry",
    "Physician_Ownership_Indicator" = "pown",
    "Third_Party_Payment_Recipient_Indicator" = "thirdparty",
    "Name_of_Third_Party_Entity_Receiving_Payment_or_Transfer_of_Value" = "pmt_3rd_party_name",
    "Charity_Indicator" = "charity",
    "Third_Party_Equals_Covered_Recipient_Indicator" = "thirdpartycover",
    "Contextual_Information" = "context",
    "Delay_in_Publication_of_General_Payment_Indicator" = "pub_delay_ind"
  )
  
  # Correct data types
  cat("Correcting data types\n")
  rv$manu <- factor(rv$manu)
  rv$rtype <- factor(rv$rtype)
  rv$suff <- factor(rv$suff)
  rv$rstate <- factor(rv$rstate)
  rv$rzip <- factor(rv$rzip)
  rv$rcountry <- factor(rv$rcountry)
  rv$rprovince <- factor(rv$rprovince)
  rv$rpost <- factor(rv$rpost)
  rv$ptype <- factor(rv$ptype)
  rv$pspec <- factor(rv$pspec)
  rv$proindic <- factor(rv$proindic)
  rv$mstate <- factor(rv$mstate)
  rv$mcountry <- factor(rv$mcountry)
  rv$dispute <- factor(rv$dispute)
  rv$payam <- as.numeric(rv$payam)
  rv$pmt_date <- strptime(rv$pmt_date, "%m/%d/%Y")
  rv$pmt_cnt <- as.numeric(rv$pmt_cnt)
  rv$formpay <- factor(rv$formpay, labels=c("Cash","Dividend","In-kind","Stock"))
  rv$natpay <- factor(rv$natpay, labels=c("Charity","Other","Non-accredited CE", 
                                          "Accredited CE","Consulting",
                                          "Ownership","Education","Entertainment",
                                          "Food & Beverage","Gift","Grant",
                                          "Honoraria","Royalty","Space","Travel"))
  rv$tstate <- factor(rv$tstate)
  rv$tcountry <- factor(rv$tcountry)
  rv$pown <- factor(rv$pown)
  rv$thirdparty <- factor(rv$thirdparty)
  rv$charity <- factor(rv$charity)
  rv$thirdpartycover <- factor(rv$thirdpartycover)
  
  return(rv)
}

