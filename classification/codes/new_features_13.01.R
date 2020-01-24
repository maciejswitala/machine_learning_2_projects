
colnames(data)

data$Year = year(as.Date(data$PurchDate_1,"%d.%m.%Y"))
data$Day = day(as.Date(data$PurchDate_1,"%d.%m.%Y"))
data$Weekday = as.numeric(as.factor(weekdays(as.Date(data$PurchDate_1,"%d.%m.%Y"))))

other_cars = data %>% 
  group_by(BYRNO,PurchDate_1) %>% 
  summarize(n(),
            mean(VehicleAge),
            max(VehicleAge)-min(VehicleAge),
            mean(VehBCost))

colnames(other_cars) = c('BYRNO','PurchDate_1','CarsAmount','CarsAgeMean',
                         'CarsAgeMaxMin','CarsBCostMean')

data = left_join(data, other_cars, by=c("BYRNO","PurchDate_1"))
