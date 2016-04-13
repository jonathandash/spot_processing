#***************************************************************
# Calculate vegetation indicies
# J.Dash February 2016
# jonathan.dash@scionresearch.com
#***************************************************************

NDVI<- function (NIR, RED)
{
  (NIR-RED)/(NIR+RED)
}

mNDVI<-function(NIR, RED, BLUE)
{
  (NIR-RED)/((NIR+RED)-(2*BLUE))
}

mEVI<-function(NIR, RED, BLUE)
{
  ((NIR/RED)/((NIR+6*RED)-(7.5*BLUE+10)))*(1+10)
}

SAVI<-function(NIR, RED)
{
  ((1+0.5)*(NIR-RED))/(NIR+RED+0.5)
}

mARVI<-function(NIR, RED, BLUE)
{
  (NIR-(RED-2*(BLUE-RED)))/(NIR+2*(BLUE-RED))
}

RGRI<-function(RED,GREEN)
{
  RED/GREEN
}

mARI1<-function(RED, GREEN)
{
  (GREEN-1)/(RED-1)
}

mARI2<-function(NIR, GREEN, RED)
{
  NIR*((GREEN-1)/(RED-1))
}

TVI<-function(NIR, GREEN, RED)
{
  0.5*(120*(NIR-GREEN) - 200*(RED-GREEN))
}

REEI<-function (RED, NIR)
{
  RED/NIR
}

scale_red<- function (RED, GREEN, BLUE)
{
  
  RED / (RED + GREEN + BLUE)
}

scale_blue<- function (RED, GREEN, BLUE)
{
  
  BLUE / (RED + GREEN + BLUE)
}

scale_green<- function (RED, GREEN, BLUE)
{
  
  GREEN / (RED + GREEN + BLUE)
}


