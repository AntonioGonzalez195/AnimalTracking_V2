
#Install these pacakges
library(EBImage)
library(shiny)
library(dplyr)
library(ggpubr)
library(av)
library(RColorBrewer)
library(shinythemes)
library(ggplot2)
# # 
# # #If EBImage package download causes issues due to version, do this:
# # install.packages("BiocManager") 
# # BiocManager::install("EBImage")
# # library(EBImage)


##### START #####

#Set working directory to file containing avi videos
VideosWD<-"C:/Users/anton/OneDrive/Desktop/Ross"
setwd(VideosWD)

#Run the rest of the code at once
unlink(paste0(getwd(),"/*.jpg"))
Videos<-Sys.glob("*.avi")
First<-Sys.glob("*.avi")
Convert<-av_video_images(First[1], 
                         destdir = VideosWD,
                         format = "jpg", fps = 0.005)
Images<-Sys.glob("*.jpg")
imgdim <-readImage(Images[1])
dim<-dim(imgdim)

ui <- fluidPage(theme = shinytheme("darkly"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("ChamberNames", "Chamber names", value = "Horizontal/Cherry,Vertical/Honey", width = NULL, placeholder = NULL),
                    sliderInput("Select",
                                label = "Select Image:",
                                min = 1,
                                max = length(Images),
                                value = 1),
                    sliderInput("x",
                                label = "Cropping X Axis:",
                                min = 0,
                                max = dim[1],
                                value = c(140,570)),
                    sliderInput("y",
                                label = "Cropping Y Axis:",
                                min = 0,
                                max = dim[2],
                                value = c(225,405)),
                    sliderInput("Blur",
                                label = "Blurring:",
                                min = 1,
                                max = 10,
                                value = 3),
                    sliderInput("Threshold",
                                label = "Threshold:",
                                min = 0,
                                max = 0.1,
                                value = 0.07),
                    checkboxInput("box_checked", "Apply Threshold", value = FALSE),
                    sliderInput("SamplingRate",
                                label = "Sampling rate (FPS):",
                                min = 0.01,
                                max = 20,
                                value = 0.01),
                    actionButton('Track', 'Save Parameters'),
                  ),
                  mainPanel(mainPanel( tabsetPanel(type = "tabs",
                                                   tabPanel("Animal Segmentation", plotOutput("img1",brush="plot_brush")),
                                                   tabPanel("Heatmap", plotOutput("img2")))))))
server <- function(input, output){
  observe(
    if (input$box_checked==0){
      output$img1 <-   renderPlot({
        plot(gblur((readImage(Images[input$Select]))[input$x[1]:input$x[2],input$y[1]:input$y[2],],sigma=input$Blur))
      },height=700,width=900)}
    else { 
      output$img1<-renderPlot({
        plot(gblur((readImage(Images[input$Select]))[input$x[1]:input$x[2],input$y[1]:input$y[2],],sigma=input$Blur)<input$Threshold)
      },height=700,width=900)})
  
  observeEvent(input$Track,{
    Blurring <<- input$Blur
    CroppingX <<- input$x
    CroppingY <<- input$y
    Threshold <<- input$Threshold
    ChamberNames<<-as.character(unlist(strsplit(input$ChamberNames,",")))
    FPS<<-input$SamplingRate})}
shinyApp(ui = ui, server = server)

    unlink(paste0(getwd(),"/*.jpg"))
    Video<-Sys.glob("*.avi")
    X_Coordinates<-data.frame(rep(NA,times=1))
    Y_Coordinates<-data.frame(rep(NA,times=1))
    
    for (i in 1:length(Video)){
      unlink(paste0(getwd(),"/*.jpg"))
      Image<-av_video_images(Video[i], 
                             destdir = VideosWD,
                             format = "jpg", fps = 0.1)
      Images<-Sys.glob("*.jpg")
      for (j in 1:length(Images)){
        Image1<-readImage(Images[j])
        Image2<- bwlabel((gblur(channel(Image1,"gray"),sigma = Blurring))<Threshold)
        Features<-as.data.frame(computeFeatures.moment(Image2))
        Area1<-c(which((Features[,c("m.cx")]<CroppingX[1]|
                          Features[,c("m.cx")]>CroppingX[2]|
                          Features[,c("m.cy")]<CroppingY[1]|
                          Features[,c("m.cy")]>CroppingY[2])==0))
        Area2<-c(which.max((as.data.frame(computeFeatures.shape(Image2))$s.area)[Area1]))
        X_Coordinates[j,i] <- computeFeatures.moment(Image2)[Area1[Area2], c("m.cx")]
        Y_Coordinates[j,i] <- computeFeatures.moment(Image2)[Area1[Area2], c("m.cy")]}}
    
    unlink(paste0(getwd(),"/*.jpg"))
    X_Coordinates<-na.omit(X_Coordinates)
    Y_Coordinates<-na.omit(Y_Coordinates)
    Data <- list()
    for (i in 1:length(Video)) {
      Data[[i]] <- data.frame("X"=X_Coordinates[,i],
                              "Y"=Y_Coordinates[,i])
      colnames(Data[[i]])<-c("X","Y")
      write.csv(Data[[i]],paste0("Coordinates_",Video[i],".csv"))}
  
ExcelFiles<-c(Sys.glob("*.csv"))
nb.cols <- 15
mycolors <- c("white",colorRampPalette(brewer.pal(8, "YlOrRd"))(nb.cols))
plot_list = list()
for (i in 1:length(ExcelFiles)) {
  Data<-read.csv(ExcelFiles[i])
  p=ggplot(data = Data, aes(x = X, y = Y)) +
  geom_density_2d_filled(alpha = 1,bins=nb.cols,h=100)+
  scale_fill_manual(values=mycolors)+
  geom_point(size=1.5,colour="green")+
  ylab("")+
  xlab("")+
  xlim(c(CroppingX[1],CroppingX[2]))+
  ylim(c((CroppingY[1]-100),(CroppingY[2]+100)))+
  theme_void()+
  theme(legend.position = "none")+
  annotate("text", x=CroppingX[1]+100, y=CroppingY[2], label= ChamberNames[1])+
  annotate("text", x=CroppingX[2]-100, y=CroppingY[2], label= ChamberNames[2])
  plot_list[[i]] = p
}
for (i in 1:length(Video)) {
  file_name = paste0(Video[i],".tiff")
  tiff(file_name,width = 700,height = 1000,units = "px")
  print(plot_list[[i]])
  dev.off()
}
ExcelFiles<-c(Sys.glob("*.csv"))
TimePerChamber<-data.frame("Video"=Video)
for (i in 1:length(ExcelFiles)){
Data<-read.csv(ExcelFiles[i])
TimePerChamber[i,2]<-(sum(Data$X<300)/0.1)/60
TimePerChamber[i,3]<-(sum(Data$X>300)/0.1)/60}
colnames(TimePerChamber)<-c("Video",ChamberNames[1],ChamberNames[2])

write.csv(TimePerChamber,"TimePerChamber.csv")



