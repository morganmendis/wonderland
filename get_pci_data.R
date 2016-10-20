# This script will download the KDD-CUP-98 data to be used in the wonderland project
#Created: October 20, 2016
#Updated: October 20, 2016

# Zip File URL location
pci_data <- "https://kdd.ics.uci.edu/databases/kddcup98/epsilon_mirror/cup98lrn.zip"

#Download File
download.file(pci_data,destfile = "~/Projects/wonderland/data/pci_data.zip")

#Unzip folder and put files in the data directory
unzip("~/Projects/wonderland/data/pci_data.zip",exdir = "data/")

rm(pci_data) #Clean environment