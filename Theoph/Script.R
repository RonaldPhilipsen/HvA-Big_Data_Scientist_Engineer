library(dplyr)

Theoph = read.csv("./Theoph.csv")
df = data.frame(Theoph)

# Get the column names of the dataframe
print(names(df))

#select only the collums starting from subject to dose
selection = select(df, 1:3)

#Only select the Wt and Dose columns now
selection = select(df, 2:3)

# Now use the filter command to return df with dose > 5
print(filter(df, Dose > 5))

# Now use the filter command to return df with Dose > 5 and time greater than the mean time
print(filter(df, Dose > 5, Time > mean(df$Time, na.rm = TRUE)))

# use the Arrange() function to arrange df by weight
arrange(df, desc(Wt))
arrange(df, Wt)
arrange(df, Wt, desc(Time))
df2 = data.frame(Theoph)

# Use the mutate () command to create a new column called trend that is equal to time - mean(time)
df2 <- mutate(df2, Trend = Time - mean(df$Time, na.rm = TRUE))

df2 <- mutate(df2, weight_cat =
       ifelse(Wt > 76.20, "Super-middleWeight",
       ifelse(Wt > 72.57, "Middleweight",
       ifelse(Wt > 69.85, "Light-middleweight",
       "Welterweight"))))

weight_group <- group_by(df2, weight_cat)

summarize(weight_group, Avg_Time = mean(Time), Total_Dose = sum(Dose))