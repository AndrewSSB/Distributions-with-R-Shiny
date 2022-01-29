#install.packages("shiny")
# library("shiny")
# setwd("K:\\Important\\Facultate\\An 2\\Sem 1\\PS\\Project")
#runApp("Proj.R")
ui<-fluidPage(
  fluidRow(
    column(2,
      selectInput("dist","Choose the probability distribution: ", choices =
                    list(Continuous = list("Normal",
                                           "Beta",
                                           "Chi-squared",
                                           "Exponential",
                                           "F",
                                           "Cauchy",
                                           "Logistic",
                                           "StudentT",
                                           "Uniform",
                                           "Gamma",
                                           "Lognormal",
                                           "Weibull"),
                         Discrete = list("Binomial",
                                         "Poisson",
                                         "Geometric")))),
    column(2,
           conditionalPanel(
             condition = "input.dist == 'Normal'",
             numericInput("mean", "Mean: ", value = 0),
             numericInput("sd", "Standard deviation: ", value = 1, min=0)
           ),
           conditionalPanel(
             condition = "input.dist == 'Chi-squared'",
             numericInput("df", "Degrees of freedom: ", value = 1, min = 2, step = 1)
           ),
           conditionalPanel(
             condition = "input.dist == 'Beta'",
             numericInput("alfa","Alpha: ", value = 1,min=1),
             numericInput("beta", "Beta: ", value = 1,min=1)
           ),
           conditionalPanel(
             condition = "input.dist == 'Poisson'",
             numericInput("lambda","Lambda: ", value = 1,min = 0)
           ),
           conditionalPanel(
             condition = "input.dist == 'Binomial'",
             numericInput("n","n: ", value = 10,step=1),
             numericInput("p", "p: ", value = 0,step=0.1,max=1,min=0)
           ),
           conditionalPanel(
             condition = "input.dist == 'Exponential'",
             numericInput("nExp","Rate: ",value = 1, min=0)
           ),
           conditionalPanel(
             condition = "input.dist == 'F'",
             numericInput("df1","F1: ", value = 1),
             numericInput("df2","F2: ", value = 1)
           ),
           conditionalPanel(
             condition = "input.dist == 'Logistic'",
             numericInput("log","Logistic: ", value = 1),
             numericInput("scale","Scale: ", value = 1)
           ),
           conditionalPanel(
             condition = "input.dist == 'StudentT'",
             numericInput("v","StudentT: ",value = 1, min=1)
           ),
           conditionalPanel(
             condition = "input.dist == 'Uniform'",
             numericInput("u1","Val1: ", value = 1),
             numericInput("u2","Val2: ", value = 1)
           ),
           conditionalPanel(
             condition = "input.dist == 'Cauchy'",
             numericInput("location", "Location: ", value = 0),
             numericInput("cscale", "Scale: ", value = 1, min=1)
           ),
           conditionalPanel(
             condition = "input.dist == 'Gamma'",
             numericInput("rate", "Rate: ", value = 1, min=1),
             numericInput("gshape", "Shape: ", value = 1, min=0),
           ),
           conditionalPanel(
             condition = "input.dist == 'Lognormal'",
             numericInput("meanlog", "Mean: ", value = 0),
             numericInput("sdlog", "Standard deviation: ", value = 0.05, min=0.05, step=0.05)
           ),
           conditionalPanel(
             condition = "input.dist == 'Weibull'",
             numericInput("wscale", "Scale: ", value = 1, min=0),
             numericInput("wshape", "Shape: ", value = 1, min=0)
           ),
           conditionalPanel(
             condition = "input.dist == 'Geometric'",
             numericInput("prob", "Probability: ", value = 1, min=0, step=0.1, max=1)
           )
    )
  ),
  fluidRow(
    column(6,
           plotOutput("densityPlot")
    ),
    column(6,
           plotOutput("cdfPlot")
    )
  ),
  fluidRow(
    column(2,
         selectInput("probType", "Choose type of probability", 
                     choices = list("P(X<=a)", "P(X>=a)", "P(a<=X<=b)"))
    ),
    column(2,
         conditionalPanel(
           condition = "input.probType == 'P(X<=a)'",
           numericInput("a", "a:", value=0)
         ),
         conditionalPanel(
           condition = "input.probType == 'P(X>=a)'",
           numericInput("b", "a:", value=0)
         ),
         conditionalPanel(
           condition = "input.probType == 'P(a<=X<=b)'",
           numericInput("aa", "a:", value=0),
           numericInput("bb", "b:", value=0)
         )           
    ),
    column(6,
           textOutput("result"),
           plotOutput("resultPlot"))
    )
)
  

server<-function(input, output, session) {
  dist<-reactive(input$dist)
  
  a<-reactive(input$a)
  b<-reactive(input$b)
  aa<-reactive(input$aa)
  bb<-reactive(input$bb)
  probType<-reactive(input$probType)
  
  # Distribution specific vars
  # Normal
  nMean<-reactive(input$mean)
  sd<-reactive(input$sd)
  
  # Poisson
  lambda<-reactive(input$lambda)
  
  # Binomial
  n<-reactive(input$n)
  p<-reactive(input$p)
  
  # Beta
  alfa<-reactive(input$alfa)
  beta<-reactive(input$beta)
  
  # Chi-squared
  df1<-reactive(input$df)
  
  # Exponential
  nExp<-reactive(input$nExp)
  
  #F
  f1<-reactive(input$df1)
  f2<-reactive(input$df2)
  
  #Logistic
  log<-reactive(input$log)
  scale<-reactive(input$scale)
  
  #StudentT
  v<-reactive(input$v)
  
  #Uniform
  u1<-reactive(input$u1)
  u2<-reactive(input$u2)
  
  # Cauchy
  location<-reactive(input$location)
  cscale<-reactive(input$cscale)
  
  #Gamma
  rate<-reactive(input$rate)
  gshape<-reactive(input$gshape)
  
  #Geometric
  prob<-reactive(input$prob)
  
  #Lognormal
  meanlog<-reactive(input$meanlog)
  sdlog<-reactive(input$sdlog)
  
  #Weibull
  wscale<-reactive(input$wscale)
  wshape<-reactive(input$wshape)
  
  # Reactive vars to calculate properties of dist
  inf<-reactive({
    switch(dist(),
           "Normal"=nMean()-3*sd(),
           "Beta"=0,
           "Poisson"=0,
           "Binomial"=0,
           "Chi-squared"=0,
           "Exponential"=0,
           "F"=0,
           "Logistic"=-2*log(),
           "StudentT"=-10,
           "Uniform"=-5*u1(),
           "Cauchy"=(-1)*abs(3*location()) + location(),
           "Gamma"=0,
           "Geometric"=0,
           "Lognormal"=0,
           "Weibull"=0)
  })
  
  sup<-reactive({
    switch(dist(),
           "Normal"=nMean()+3*sd(),
           "Beta"=1,
           "Poisson"=2*lambda(),
           "Binomial"=n(),
           "Chi-squared"=df1()*2,
           "Exponential"= 2**(nExp()),
           "F"=f1()+f2(),
           "Logistic"=10*log(),
           "StudentT"=10,
           "Uniform"=5*u2(),
           "Cauchy"=abs(3*location()) + location(),
           "Gamma"=2*gshape(),
           "Geometric"=20,
           "Lognormal"=20,
           "Weibull"=wscale()*2)
  }) 
  
  points<-reactive({
    switch(dist(),
           "Normal"=seq(from=inf(),to=sup(),length.out=1000),
           "Beta"=seq(from=inf(),to=sup(),length.out=1000),
           "Poisson"=inf():sup(),
           "Binomial"=inf():sup(), 
           "Chi-squared"=seq(from=inf(),to=sup(),length.out=1000),
           "Exponential"=seq(from=inf(),to=sup(),length.out=1000),
           "F"=seq(from=inf(),to=sup(),length.out=1000),
           "Logistic"=seq(from=inf(),to=sup(),length.out=1000),
           "StudentT"=seq(from=inf(),to=sup(),length.out=1000),
           "Uniform"=seq(from=inf(),to=sup(),length.out=1000),
           # location=0 then inf=sup=0 
           "Cauchy"=seq(from=if(inf()==0) -5 else inf(),to=if(sup()==0) 5 else sup(),length.out=1000),
           "Gamma"=seq(from=inf(),to=sup(),length.out=1000),
           "Geometric"=inf():sup(),
           "Lognormal"=seq(from=inf(),to=sup(),length.out=1000),
           "Weibull"=seq(from=inf(),to=sup(),length.out=1000))
  })
  
  density<-reactive({
    switch(dist(),
           "Normal"=dnorm(points(),nMean(),sd()),
           "Beta"=dbeta(points(),alfa(),beta()),
           "Poisson"=dpois(points(),lambda()),
           "Binomial"=dbinom(points(),n(),p()),
           "Chi-squared"=dchisq(points(),df1()),
           "Exponential"=dexp(points(), nExp()),
           "F"= df(points(), f1(),f2()),
           "Logistic"=dlogis(points(), log(), scale()),
           "StudentT"=dt(points(), v()),
           "Uniform"=dunif(points(), u1(), u2()),
           "Cauchy"=dcauchy(points(),location(),cscale()),
           "Gamma"=dgamma(points(),gshape(),rate()),
           "Geometric"=dgeom(points(),prob()),
           "Lognormal"=dlnorm(points(),meanlog(),sdlog()),
           "Weibull"=dweibull(points(),wshape(),wscale()))
  })
  
  cdf<-reactive({
    switch(dist(),
           "Normal"=pnorm(points(),nMean(),sd()),
           "Beta"=pbeta(points(),alfa(),beta()),
           "Poisson"=ppois(points(),lambda()),
           "Binomial"=pbinom(points(),n(),p()),
           "Chi-squared"=pchisq(points(),df1()),
           "Exponential"=pexp(points(),nExp()),
           "F"=pf(points(), f1(),f2()),
           "Logistic"=plogis(points(), log(), scale()),
           "StudentT"=pt(points(), v()),
           "Uniform"=punif(points(), u1(), u2()),
           "Cauchy"=pcauchy(points(),location(),cscale()),
           "Gamma"=pgamma(points(),gshape(),rate()),
           "Geometric"=pgeom(points(),prob()),
           "Lognormal"=plnorm(points(),meanlog(),sdlog()),
           "Weibull"=pweibull(points(),wshape(),wscale()))
  })
  
  mean<-reactive({
    switch(dist(),
           "Normal"=nMean(),
           "Beta"=alfa()/(alfa()+beta()),
           "Poisson"=lambda(),
           "Binomial" = n()*p(),
           "Chi-squared" = df1(),
           "Exponential"= 1/nExp(),
           "F"=f2()/(f2()-2),
           "Logistic"=log(),
           "StudentT"=0,
           "Uniform"=(u1()+u2())/2,
           #"Cauchy"="undefined",
           "Gamma"=gshape()/rate(),
           "Geometric"=1/prob(),
           "Lognormal"=exp(meanlog() + 1/2 * sdlog()^2),
           "Weibull"=wscale() * gamma(1 + 1/wshape()))
  })
  
  cdfHelper<-function(x) {
   
      switch(dist(),
             "Normal"=pnorm(x,nMean(),sd()),
             "Beta"=pbeta(x,alfa(),beta()),
             "Poisson"=ppois(x,lambda()),
             "Binomial"=pbinom(x,n(),p()),
             "Chi-squared"=pchisq(x,df1()),
             "Exponential"= pexp(x, nExp()),
             "F"= pf(x, f1(), f2()),
             "Logistic" = plogis(x, log(), scale()),
             "StudentT"= pt(x, v()),
             "Uniform"= punif(x, u1(), u2()),
             "Cauchy"=pcauchy(x,location(),cscale()),
             "Gamma"=pgamma(x,gshape(),rate()),
             "Geometric"=pgeom(x,prob()),
             "Lognormal"=plnorm(x,meanlog(),sdlog()),
             "Weibull"=pweibull(x,wshape(),wscale()))
    
  } 
  
  pdfHelper<-function(x) {
    
      switch(dist(),
             "Normal"=dnorm(x,nMean(),sd()),
             "Beta"=dbeta(x,alfa(),beta()),
             "Poisson"=dpois(x,lambda()),
             "Binomial"=dbinom(x,n(),p()),
             "Chi-squared"=dchisq(x,df1()),
             "Exponential"=dexp(x, nExp()),
             "F"= df(x, f1(),f2()),
             "Logistic"=dlogis(x, log(), scale()),
             "StudentT"=dt(x, v()),
             "Uniform"=dunif(x, u1(), u2()),
             "Cauchy"=dcauchy(x,location(),cscale()),
             "Gamma"=dgamma(x,gshape(),rate()),
             "Geometric"=dgeom(x,prob()),
             "Lognormal"=dlnorm(x,meanlog(),sdlog()),
             "Weibull"=dweibull(x,wshape(),wscale()))
    
  } 
  
  output$densityPlot<-renderPlot({
    currDensity <- density()
    if (is.infinite(max(currDensity)) || is.nan(max(currDensity))) {
      yl<-c(0,4*5/3)
    } else {
      yl<-c(0,4*max(currDensity)/3)
    }
    
    plot(points(),currDensity,
         type="l",
         col = "cyan3",
         lwd=2.5,
         xlab="Values",
         ylab="PDF",
         main=dist(),
         ylim= yl )
    abline(v = mean(), col="cyan2",lwd = 2, lty = 2)
  })
  
  output$cdfPlot<-renderPlot({
    currCdf <- cdf()
    plot(points(),currCdf,
         type="l",
         col = "cyan3",
         lwd=2.5,
         xlab="Values",
         ylab="CDF",
         main=dist(),
         ylim = c(0,1))
  })
  
  output$result<-renderText({
    switch(probType(),
           'P(X<=a)' = cdfHelper(a()),
           'P(X>=a)' = 1 - cdfHelper(b()),
           'P(a<=X<=b)' = cdfHelper(bb()) - cdfHelper(aa())
           )
  })

  output$resultPlot<-renderPlot({
    currPoints<- points()
    currA<- a()
    currB<- b()
    currPdf<- density()
    plot(currPoints,currPdf,
         type="l",
         col = "cyan3",
         lwd=2.5,
         xlab="Values",
         ylab="PDF",
         main=dist(),
         ylim = c(0,1))
    switch(probType(),
           'P(X<=a)' = {
             polygon(c(currPoints[currPoints<=currA], currA),
                     c(currPdf[currPoints<=currA], currPdf[currPoints==min(currPoints)]),
                     col="cyan",
                     density=10,
                     angle=90)
           },
           'P(X>=a)' = {
             polygon(c(currPoints[currPoints>=currB], max(currPoints), currB),
                     c(currPdf[currPoints>=currB], 0, 0),
                     col="cyan",
                     density=10,
                     angle=90)
           },
           'P(a<=X<=b)' = {
             polygon(c(currPoints[currPoints>=aa() & currPoints<=bb()], bb(), aa()),
                     c(currPdf[currPoints>=aa() & currPoints<=bb()], currPdf[currPoints==min(currPoints)],currPdf[currPoints==min(currPoints)]),
                     col="cyan",
                     density=10,
                     angle=90)
           } 
    )
  })
}

shinyApp(ui, server)
