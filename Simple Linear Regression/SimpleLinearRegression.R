#INSTALLING AND LOADING "readxl" LIBRARY
#install.packages("readxl") 
library("readxl")

#IMPORTING DATASET FROM EXCEL FILE
setwd("~path") #path to the directory where your dataset file is
df <- read_excel("SLRData.xlsx", col_names = TRUE)
df

#ASSIGNING COLUMN NAMES TO VARIABLES FOR EASIER USE
x <- df$x
y <- df$y

#ESTIMATING THE SLOPE B1 = corr_xy*(sd(y)/sd(x))
corr_xy <- cor(x, y,  method = "pearson") #correlation of (x,y)
corr_xy
std_x <- sd(x) #Finding standard deviation
std_y <- sd(y)
B1 <- corr_xy*(std_y/std_x)
B1

#Estimating the Intercept B0 = mean(y) - B1*mean(x)
B0=mean(y)-B1*mean(x)
B0

#MAKING PREDICTIONS
predicted_y = B0+B1*x
predicted_y

#VISUALIZING PREDICTIONS
#install.packages("plotly")
library("plotly")
fig <- plot_ly(df, x=x)
fig <- fig %>% add_trace(y=y,marker=list(size=10,color='purple'),name='y',type='scatter',mode='markers')
fig <- fig %>% add_trace(y=predicted_y,marker=list(size=10,color='darkgreen'),name='predicted_y',type='scatter', mode='lines+markers')
fig

#ESTIMATING THE ERROR
MSE <- mean((y-predicted_y)^2)
RMSE <- sqrt(MSE)
RMSE