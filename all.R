#amenities to convert...
cols = colnames(trainx_all_0503)[c(53:260)]
trainx_all_0503[cols] <- lapply(trainx_all_0503[cols], factor)

# convert some fators to dummy variables
bed_type_dum=model.matrix(~bed_type+0,data=trainx_all_0503)
city_name_dum=model.matrix(~city_name+0,data=trainx_all_0503)
host_pro_pic_dum=model.matrix(~host_has_profile_pic+0,data=trainx_all_0503)
host_iden_dum=model.matrix(~host_identity_verified+0,data=trainx_all_0503)
host_sup_dum=model.matrix(~host_is_superhost+0,data=trainx_all_0503)
instant_bookable_dum=model.matrix(~instant_bookable+0,data=trainx_all_0503)
is_business_travel_ready_dum=model.matrix(~is_business_travel_ready+0,data=trainx_all_0503)
is_location_exact_dum=model.matrix(~is_location_exact+0,data=trainx_all_0503)
property_type_dum=model.matrix(~property_type+0,data=trainx_all_0503)
requires_license_dum=model.matrix(~requires_license+0,data=trainx_all_0503)
room_type_dum=model.matrix(~room_type+0,data=trainx_all_0503)
state_dum=model.matrix(~state+0,data=trainx_all_0503)

# combine dummies
train_all_with_dum=data.frame(trainx_all_0503,bed_type_dum,city_name_dum,host_pro_pic_dum,host_iden_dum,host_sup_dum,
                          instant_bookable_dum,instant_bookable_dum,is_business_travel_ready_dum,
                          is_location_exact_dum,property_type_dum,requires_license_dum,room_type_dum,state_dum)

# convert to factors
train_all_with_dum$cancellation_policy=as.factor(train_all_with_dum$cancellation_policy)
train_all_with_dum$country=as.factor(train_all_with_dum$country)
train_all_with_dum$country_code=as.factor(train_all_with_dum$country_code)
train_all_with_dum$host_response_time=as.factor(train_all_with_dum$host_response_time)
train_all_with_dum$instant_bookable=as.factor(train_all_with_dum$instant_bookable)
train_all_with_dum$name=as.factor(train_all_with_dum$name)
train_all_with_dum$require_guest_phone_verification=as.factor(train_all_with_dum$require_guest_phone_verification)
train_all_with_dum$require_guest_profile_picture=as.factor(train_all_with_dum$require_guest_profile_picture)
train_all_with_dum$license=as.factor(train_all_with_dum$license)

# drop columns - dummy column
train_all_with_dum <- subset(train_all_with_dum, select = -c(bed_type,city_name,host_has_profile_pic,host_identity_verified,host_is_superhost,instant_bookable,is_business_travel_ready,
                                                     is_location_exact,property_type,requires_license,room_type,state))


# split into train and test datasets
train_x = subset(train_all_with_dum,train_all_with_dum$label=='train')
test_x = subset(train_all_with_dum,train_all_with_dum$label=='test')

# combind train x and train y
train=cbind(train_x, trainy_clean_0503)
train<- subset(train,select = -c(label))
train$high_booking_rate=as.factor(train$high_booking_rate)

# check na and class
sapply(train,class)
sapply(train,function(train) sum(is.na(train)))

# select samples -total:100000,trian:70000,test:30000
set.seed(11224)
train.total = sample(nrow(train), .5*nrow(train)) 
sampleset = train[train.total,]
train.indicies = sample(nrow(sampleset), .9*nrow(sampleset)) 
train.samples = sampleset[train.indicies,]
test.samples = sampleset[-train.indicies,]

library(randomForest)
sapply(train.samples,class)
sapply(train.samples,function(train.samples) sum(is.na(train.samples)))
fit1 <- randomForest(train.samples$high_booking_rate~., data=train.samples)
rf_preds <- predict(fit1,newdata=test.samples,type="response")
test = table(test.samples$high_booking_rate,rf_preds)
test.accuracy=sum(diag(test))/sum(test)
test.accuracy	
