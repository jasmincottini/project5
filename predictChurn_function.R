#with error message when customer does not exist
predictChurn <-function(data, Customer){
  #prediction model
  model<- glm(Exited~CreditScore+Gender+Age+Tenure+Balance+NumOfProducts+HasCrCard+IsActiveMember+EstimatedSalary, family=binomial(link="logit"), data=data)
  #prediction variable added to data
  prediction<-predict.glm(model, data, type="response")
  data[,prediction:=prediction]
  #should give result based on given CustomerId
  result<-data[CustomerId==Customer,prediction]

  #check if customer id provided exists
  if (Customer %in% data$CustomerId){
    return(result)
  } else {
    print("CustomerId not found")
  }

}


use_test()
