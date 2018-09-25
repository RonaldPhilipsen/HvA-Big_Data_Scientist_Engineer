library(magrittr)

# 1. Print the name of your class
print("BD1")
# 2. Print your full name
print("Ronald Philipsen")
# 3. Print the name of the file involved
print("File_BD1_B")

# 4. Read in the file with a line of code, no wizards allowed
df <- read.csv2("./File_BD1_B.csv")

# 5. Drop all the columns from the dataframe exept list.
df <- select(df, 1:5, 17, 7)

# 6. All the values in the column Charge_Point_ID contain a prefix, "EVB" Remove this
df$Charge_Point_ID <- gsub("EVB-", "", df$Charge_Point_ID)

# 7. Rename all columns in such a way that no Underscores appear
colnames(df) <- gsub("_", "", colnames(df))

#8 Rename volume as kwh and column charge_point_zip as postalcode
colnames(df) <- gsub ("Volume", "kwh", colnames(df))
colnames(df) <- gsub("ChargePointZIP", "postalcode", colnames(df))

#9 Create a new dataframe called dfFilter containing only observations for a postalcode starting with 3051
dfFilter <- filter(df, as.character(df$postalcode) %>% startsWith("3051"))

# 10. summarize the amount of kwh ( = volume) for each chargepointid
summarize(group_by(df, ChargePointID, kwh))

# 11. add an extra column for a SessionType
df <- mutate(df, SessionType = ifelse(as.integer(Duration) > 5000, "Long", "Short"))
