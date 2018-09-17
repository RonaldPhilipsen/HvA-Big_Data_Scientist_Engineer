mpgdata = read.csv("./auto-mpg.csv")

print("Head:")
print(head(mpgdata))

cat("number of rows:", nrow(mpgdata))

cat("number of columns", ncol(mpgdata))

print("Summary: ")
print(summary(mpgdata));

print("First row:")
print(mpgdata[1,])

print("First column")
print(mpgdata[, 1])

print("second row, first column ")
print(mpgdata[2, 1])

print("mpgdata without name:")
mpgdata = within(mpgdata, remove("car.name"))
print(head(mpgdata))

print("Class of origin")
class(mpgdata$origin)
mpgdata$origin = factor(mpgdata$origin)

print("levels")
print(levels(mpgdata$origin))

print("Named levels")
levels(mpgdata$origin) <- c("american", "japanese", "european")
print(levels(mpgdata$origin))

print("summary of mpgdata")
print(summary(mpgdata))

print("Removed Na")
mpgdata[mpgdata == '?'] <- NA
print(summary(mpgdata))

print("DataSet without NA")
mpgdataNoNa <- na.omit(mpgdata)
print(dim(mpgdataNoNa))

install.packages("RMySQL")

