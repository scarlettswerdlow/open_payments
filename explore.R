###############################################################################
#                                                                             #
#  Big Data                                                                   #
#  Project: Exploratory data analysis                                         #
#  Coded by Scarlett Swerdlow                                                 #
#  scarlettswerdlow@uchicago.edu                                              #
#  May 23, 2015                                                               #
#                                                                             #
###############################################################################

source("~/Google Drive/Grad school/Courses/BUS41201 Big Data/project/Big Data Final Project/data.R")

WD <- "~/Google Drive/Grad school/Courses/BUS41201 Big Data/project/Big Data Final Project/"
FN <- "data/OPPR_ALL_DTL_GNRL_09302014.csv"

general_pmts <- loadData(WD, FN)

# Look at unique value counts for each column
counts <- data.frame()
for (name in names(general_pmts)) { 
  unique_len <- length(unique(general_pmts[name])[[1]])
  counts <- rbind(counts, data.frame(name, unique_len))
}

# Payment amount summary stats
summary(general_pmts$payam)
nrow(general_pmts[general_pmts$payam == 0,]) # 646 payments for $0

# Histogram of payment amount
hist(log(general_pmts$payam+1), xlab="Payment amount (log)",
     main="Histogram of payment amount")
abline(v=log(median(general_pmts$payam)),col="red")
legend("topright", bty="n", lwd=1, col="red", legend="Median, $16")

# Create top-1-percent variable
general_pmts$top_1pct <- ifelse(
  general_pmts$payam >= quantile(general_pmts$payam, .99), 1, 0)

# Median payment amount by payment class
by(general_pmts$payam, general_pmts$top_1pct, median)

# Histogram of payment amount for bottom 99 percent
hist(log(general_pmts$payam[general_pmts$top_1pct == 0]+1), 
     xlab="Payment amount (log)", main="Histogram of payment amount for bottom 99%")
abline(v=log(median(general_pmts$payam[general_pmts$top_1pct == 0])),col="red")
legend("topright", bty="n", lwd=1, col="red", legend="Median, $15.53")

# Histogram of payment amount for top 1 percent
hist(log(general_pmts$payam[general_pmts$top_1pct == 1]+1), 
     xlab="Payment amount (log)", main="Histogram of payment amount for top 1%")
abline(v=log(median(general_pmts$payam[general_pmts$top_1pct == 1])),col="blue")
legend("topright", bty="n", lwd=1, col="blue", legend="Median, $4000")

# Histogram of payment month
plot(as.factor(format(general_pmts$pmt_date,"%m")),
     xlab="Month", main="Histogram of payment month")

# Histogram of payment amount
plot(as.factor(format(general_pmts$pmt_date,"%d")),
     xlab="Day", main="Histogram of payment day")

# Get top 25 specialties by count
specialties <- as.data.frame((table(general_pmts$pspec)))
top_specialties <- tail(specialties[order(specialties$Freq),],25)

# Relabel specialties
top_specialties$Var1 <- c("Nephrology", "General Practice",
                          "Dentist", NA, "Interventional Cardiology",
                          "Neurological Surgery", "Optometrist",
                          "Pediatrics", "Surgery", "Dermatology",
                          "Ophthalmology", "Hematology & Oncology",
                          "Rheumatology", "Orthopaedic Surgery",
                          "Pulmonary Disease", "Urology",
                          "Metabolism", "ObGyn", "Specialist",
                          "Neurology", "Gastroenterology",
                          "Psychiatry", "Cardiovascular Disease",
                          "Internal Medicine", "Family Medicine")

# Graph top specialties by count
par(las=2)
par(mar=c(4,11,2,2))
barplot(top_specialties$Freq, names.arg=top_specialties$Var1, horiz=T, axes=T,
        main="Top 25 specialties by payment count")

# Get average payment amount by specialty
avg_pmt_spec <- aggregate(general_pmts$payam, list(general_pmts$pspec),
                          median, na.rm=T)
avg_pmt_spec <- avg_pmt_spec[order(avg_pmt_spec$x),]

# Get top 25 specialties by average payment amount
top_avg_pmt_spec <- tail(avg_pmt_spec, 25)

# Relabel specialties
top_avg_pmt_spec$Group.1 <- c(
  "Pharmacy Technician", "Occupational Therapist", "Case Management", 
  "Phlebotomy", "Immunology", "Medical Laboratory", "Dental", NA,
  "Pediatric Nephrology", "Geriatrics", "Rehabilitation", "Nutrition Education",
  "Radiologic Technologist", "Research Study", "Multi-Specialty", 
  "Molecular Genetic Pathology", "Acute Care", "Clinical Cytogenetic", 
  "Clinical Pharmacology", "Health Educator", "Nutritionist", "Ophthalmic", 
  "Medical Toxicology", "Orthopedic", "Clinical Nurse Specialist")

# Graph top specialties by average payment amount
par(las=2)
par(mar=c(4,12,2,2))
barplot(top_avg_pmt_spec$x, names.arg=top_avg_pmt_spec$Group.1, horiz=T, axes=T,
        main="Top 25 specialties by avg payment amount")

# Get all covered drugs
covered_drugs <- c(general_pmts$drug_name1, general_pmts$drug_name2, 
                   general_pmts$drug_name3, general_pmts$drug_name4, 
                   general_pmts$drug_name5)
covered_drugs <- covered_drugs[covered_drugs != ""]

# Get top 25 drugs by count
drugs <- as.data.frame(table(toupper(covered_drugs)))
top_drugs <- tail(drugs[order(drugs$Freq),],25)

# Relabel DUEXIS-800MG IBUPROFEN- 26.6MG FAMOTIDINE as DUEXIS
top_drugs$Var1 <- as.character(top_drugs$Var1)
top_drugs$Var1 <- ifelse(
  top_drugs$Var1 == "DUEXIS-800MG IBUPROFEN- 26.6MG FAMOTIDINE",
  "DUEXIS", top_drugs$Var1)

# Graph top drugs by count
par(las=2)
par(mar=c(4,11,2,2))
barplot(top_drugs$Freq, names.arg=top_drugs$Var1, horiz=T, axes=T,
        main="Top 25 drugs by payment count")

# Get average payment amount by drug
avg_pmt_drug <- aggregate(general_pmts$payam, 
                          list(toupper(general_pmts$drug_name1)),
                          median, na.rm=T)
avg_pmt_drug <- avg_pmt_drug[order(avg_pmt_drug$x),]

# Get top 25 specialties by average payment amount
top_avg_pmt_drug <- tail(avg_pmt_drug, 25)

# All covered supplies
covered_supplies <- c(general_pmts$supply1,general_pmts$supply2,
                      general_pmts$supply3, general_pmts$supply4,
                      general_pmts$supply5)
covered_supplies <- covered_supplies[covered_supplies != ""]

# Get top 25 supplies by count
supplies <- as.data.frame(table(toupper(covered_supplies)))
top_supplies <- tail(supplies[order(supplies$Freq),],25)

# Look at number of payments
summary(general_pmts$pmt_cnt)

# Group into one, more than one
general_pmts$mt1_pmt <- ifelse(general_pmts$pmt_cnt > 1, 1, 0)
# Not a significant difference in payment amt by more than 1
by(general_pmts$payam, general_pmts$mt1_pmt, summary)

# Look at payment form and type of payment
table(general_pmts$formpay)
table(general_pmts$natpay)

# Graph payment amount by payment form
par(las=1)
par(mar=c(3,3,3,3))
plot(as.factor(general_pmts$formpay), log(general_pmts$payam+1), xaxt = "n")
axis(side=1, at=1:4, labels=c("Cash", "Other ROI", "In-kind", "Ownership"))
title("Payment amount (log) by form of payment")

# Top 1 percent get way more in cash and less in in-kind
by(general_pmts$formpay, general_pmts$top_1pct,
   function(x) prop.table(table(x)))

# Graph it
form_cnts <- as.data.frame(table(general_pmts$top_1pct, general_pmts$formpay))
form_cnts$prop <- ifelse(form_cnts$Var1 == 0,
                         form_cnts$Freq/sum(
                           form_cnts$Freq[form_cnts$Var1 == 0]),
                         form_cnts$Freq/sum(
                           form_cnts$Freq[form_cnts$Var1 == 1]))

par(las=1)
par(mar=c(5,3,3,3))
barplot(form_cnts$prop*100, main="Payment form breakdown by payment class", 
        xlab="Form of payment", ylab="Percent of payments", xaxt="n", beside=T,
        col=c("red","darkblue"), legend = c("Bottom 99%", "Top 1%"))
axis(side=1, at=c(1.3, 3.7, 6.11, 8.5), 
     labels=c("Cash", "Other ROI", "In-kind", "Ownership"))

# Look at payment type breakdown
type_cnts <- as.data.frame(table(general_pmts$natpay))
type_cnts <- type_cnts[order(type_cnts$Freq),]

# Graph payment types
par(las=2)
par(mar=c(5,8,3,1))
barplot(type_cnts$Freq, names.arg=type_cnts$Var1, horiz=T, axes=T,
        main="Payment types by payment counts")

# Payment types for top 1 percent versus 99 percent
type_class_cnts <- as.data.frame(
  table(general_pmts$top_1pct, general_pmts$natpay))

type_class_cnts$prop <- ifelse(
  type_class_cnts$Var1 == 0,
  type_class_cnts$Freq/sum(type_class_cnts$Freq[type_class_cnts$Var1 == 0]),
  type_class_cnts$Freq/sum(type_class_cnts$Freq[type_class_cnts$Var1 == 1]))

par(las=2)
par(mar=c(5,8,3,3))
barplot(type_class_cnts$prop*100, main="Payment type breakdown by payment class",
        xlab="Type of payment", horiz=T, beside=T,
        col=c("red","darkblue"), legend = c("Bottom 99%", "Top 1%"))
axis(side=2, at=seq(1,36.5,36.5/15), labels=levels(type_class_cnts$Var2))

# Average payment by payment type
avg_pmt_type <- aggregate(general_pmts$payam, list(general_pmts$natpay),
                          median, na.rm=T)

# Graph payment amount by type of payment
pal=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c",
      "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928",
      "#d9d9d9", "#999999", "#000000")

par(las=2)
par(mar=c(8,3,3,3))
plot(general_pmts$natpay, log(general_pmts$payam+1), col=pal,
     main="Payment amount (log) by type of payment")

# Look at locations
from_cntry <- as.data.frame(table(general_pmts$rcountry))
to_cntry <- as.data.frame(table(general_pmts$tcountry))

tail(from_cntry[order(from_cntry$Freq),],10)
tail(to_cntry[order(to_cntry$Freq),],11)

# Ideas for further analysis:

# Network analysis
# Association rules
# Map of travel from-to city
# Text field for "other" gifts
# Add Census data

