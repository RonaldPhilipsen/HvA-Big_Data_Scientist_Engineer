grades <- c(2.3, 4.5, 6.7, 1.3, 9.5, 5.5, 5.6, 2.7)
result <- function(grade) {

    if (grade >= 5.5) {
        print("Voldoende")
        return(grade)

    }
    else {
        print("Onvoldoende")
        return(0)
    }
}


voldoendes <- c()

for (i in 1:length(grades)) {
    grades[i] <- result(grades[i])

    if (grades[i] != 0) {
        voldoendes = append(voldoendes, grades[i])
    }
}

print("Grades:")
print(grades)

print("Voldoendes")
voldoendes