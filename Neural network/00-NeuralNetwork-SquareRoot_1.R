install.packages('neuralnet')
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net_sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net_sqrt)

#Plot the neural network
plot(net_sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net_results <- compute(net_sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net_results)

#Lets see the results
print(net_results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net_results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)