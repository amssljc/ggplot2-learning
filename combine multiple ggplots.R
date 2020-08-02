
library(ggplot2)
##### create your favorite color #####
color.pink <- c('#FFB6C1', '#FF69B4') #pink
color.purple <- c('#EE82EE', '#DA70D6') #purple
color.blue <- c('#87CEFA', '#00BFFF') #blue
color.red <- c('#FFA07A', '#FF4500') #red
color.green <- c('#98FB98', '#00FF7F') #green
color.orange <- c('#F4AF60', '#FFA500') #orange
colordf <-
  data.frame(
    color.red = color.red,
    color.blue = color.blue,
    color.green = color.green,
    color.pink = color.pink,
    color.orange = color.orange,
    color.purple = color.purple
  )

##### creat data frame #####
methods.name<-c('method1','method2','method3','method4')
method<-rep(methods.name,each=3*3*30)
method<-as.character(method)

var1<-rep(c('gene1','gene2','gene3'),each=3*30)
var1<-rep(var1,length(methods.name))

var2<-rep(c('group1','group2','group3'),each=30)
var2<-rep(var2,3*length(methods.name))

x<-rep(1:30,3*3*length(methods.name))
x<-as.character(x)

exp.l<-0.1*runif(3*3*30)
exp.l<-c(exp.l,0.1*runif(3*3*30)+0.3)
exp.l<-c(exp.l,0.1*runif(3*3*30)+0.6)
exp.l<-c(exp.l,0.1*runif(3*3*30)+0.9)


df<-data.frame(method=method,var1=var1,var2=var2,x=x,exp.l=exp.l)

df$method<-factor(df$method,levels=methods.name)
df$x<-factor(df$x,levels=as.character(1:30))
df$var1<-factor(df$var1,levels=c('gene1','gene2','gene3'))
df$var2<-factor(df$var2,levels=c('group1','group2','group3'))


##### ggplot #####
p <- ggplot(df, aes(x = as.numeric(x), y = exp.l)) + #main input
  geom_line(size = 1, aes(colour = as.factor(method))) + #plot line
  geom_point(size = 2, aes(colour = as.factor(method))) + # plot dots
  facet_grid(cols = vars(var1),rows = vars(var2))+ #set multiple plots in your plot according to your interest vars
  scale_colour_manual(name = 'experiment methods', values = as.matrix(colordf[1, 1:length(methods.name)])) + #set your legend name and colors
  xlab('samples') + 
  ylab('expression levels') +
  ylim(c(0,1))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = 'right'
  )
p

