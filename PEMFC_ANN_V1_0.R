library(neuralnet)
setwd("C:/Users/Juan Jose/Desktop/")
dataset<-read.csv(file="PEMFC_TrainingData_1_V1_1.csv",header = TRUE,sep = ",")



time_interval<-function(hidden_count, clock_speed, CPI){
#This funciton obtains the theorical execution time of an ANN in a given processor
#CPI: scalar
#clock_speed: Clock speed of processor in Hertz
#hidden_count: neurons in the hidden layer
  
  instruction_count = 60*hidden_count+10;
  clock_period = 1/clock_speed;
  return (instruction_count*CPI*clock_period)
}

data_training<-function(dataset,time_scale,sample_size){
# time_scale: Voltage will be predicted within a 0.1*time_scale window.
  if(time_scale>sample_size){
    stop("time_scale must be less than sample_size");
  }
  data_training_v<-dataset[(time_scale+1):sample_size,c("Vfc.v.")];
  data_training_i<-dataset[1:(sample_size-time_scale),c("Ifc.A.","PH2.atm.")];
  return(cbind(data_training_i,data_training_v))
}

data_testing<-function(dataset,time_scale,sample_size){
  data_testing_v<-dataset[(time_scale+sample_size):(nrow(dataset)),c("Vfc.v.")];
  data_testing_i<-dataset[sample_size:(nrow(dataset)-1),c("Ifc.A.","PH2.atm.")];
  return(cbind(data_testing_i,data_testing_v))  
  return(data_testing)
}


errorCalculation <- function(ANN_sample, test_sample){
  # This function calculates the classification probability 
  error_vector<-vector();
  for(j in 1:nrow(test_sample)){
    correct_percentage<-abs(ANN_sample[j,]-test_sample[j,])/(test_sample[j,]);
    error_vector[j]<-correct_percentage;
  }
  return (error_vector)
}

resultPlotting <- function (modeled_voltage, predicted_voltage,training_size ,dataset){
  # This function assumes the training sample is contained within the beginning of the dataset
  t_time<-dataset$Time.s.[training_size+1:training_size+nrow(predicted_voltage)]
  plot_result_real<-cbind(t_time,modeled_voltage)
  plot_result_predicted<-cbind(t_time,predicted_voltage)
  plot(plot_result_real, main = "Modeled Voltage over Time", xlab = "time [s]", ylab = "Modeled Voltage [V]")
  plot(plot_result_predicted, main = "Predicted Voltage over Time", xlab = "time [s]", ylab = "Predicted Voltage [V]")
} 

d_t<-data_training(dataset,1,1000);
d_c<-data_testing(dataset,1,1000);
d_t_x<-subset(d_t,select = c(Ifc.A.,PH2.atm.))
d_t_y<-subset(d_t,select = c(data_training_v))
nn<-nnet(x = d_t_x,y = d_t_y,size = 200,maxit = 5000)

nn_1_result<-predict(nn_1,newdata = d_c_x)
r_probability<-errorCalculation(nn_1_result,d_c_y)
t.test(x = nn_1_result,y = d_c_y,conf.level = 0.95)
