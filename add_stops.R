#Read in and save the full stop data (downloaded from https://mpdc.dc.gov/stopdata)
#If it doesn't error, you could use download.file()
stopfile = "data/Stop Data_09092019.csv" #Should this load with the repo?
allstops = read.csv(stopfile)
#How does the racial breakdown of stops compare to the racial breakdown of the census tract?
