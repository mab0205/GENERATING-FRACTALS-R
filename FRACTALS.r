# SISTEMAS N�O LINEARES 
#FRACTALS

# construindo uma janela gr�fica
f<-function(x){0*x}
plot(f,-4,4,xlim=c(-4,4),ylim=c(-4,4))

#Cores 
cor1 = c( "cyan","deepskyblue", "deepskyblue3" , "blue" )
cor2 = c("grey51", "grey40","grey19",  "black" )
cor3 = c( "hotpink","red","red3",  "darkred" )
cor4 = c( "palegreen","olivedrab1","seagreen1",  "green" )


# Inserindo 3 pontos (as solu��es do sistema)

p1x1=-2.8168073297120
p1x2=0.7333112369316
points(p1x1,p1x2,col=cor1[4],pch=20,lwd=6)

p2x1=2.7522527011831
p2x2=-0.8811345948677
points(p2x1,p2x2,col=cor2[4],pch=20,lwd=6)

p3x1=3.4780635714249
p3x2=0.6029004829299
points(p3x1,p3x2,col=cor3[2],pch=20,lwd=6)

p4x1=-3.5189014959791
p4x2=-0.677105551584
points(p4x1,p4x2,col=cor4[4],pch=20,lwd=6)

# Algoritmo
for (i in 1:1001){
  for( j in 1:1001){
    #intervalos
    px=-4.008+0.008*i
    py=-4.008+0.008*j
    #sistema nao linear
    sistema <- function(x) {
      y <- numeric(2)
      y[1] <- x[1]^2-x[1]*x[2]-10             
      y[2] <- x[2]+x[1]^2*x[2]^2-5
      y
    }
    
    # Aproxima��o inicial
    numIteracoes = 50
    xstart <- c(px,py)
    
    int<-nleqslv(xstart,sistema,method = c("Newton"),control=list(maxit=numIteracoes))
    #blue
    if(sqrt((int$x[1]-p1x1)^2+(int$x[2]-p1x2)^2)<0.00000001){
      if(sqrt((int$x[1]-p1x1)^2+(int$x[2]-p1x2)^2)<0.00000000006){
        points(px,py,pch=".",col=cor1[4])
      }else if(sqrt((int$x[1]-p1x1)^2+(int$x[2]-p1x2)^2)<0.00000000038){
        points(px,py,pch=".",col=cor1[3])
      } else if(sqrt((int$x[1]-p1x1)^2+(int$x[2]-p1x2)^2)<0.0000000007){
        points(px,py,pch=".",col=cor1[2])
      }else {
        points(px,py,pch=".",col=cor1[1])
      }
    }
    #black
    if(sqrt((int$x[1]-p2x1)^2+(int$x[2]-p2x2)^2)<0.00000001){
      if(sqrt((int$x[1]-p2x1)^2+(int$x[2]-p2x2)^2)<0.00000000006){
        points(px,py,pch=".",col=cor2[4])
      }else if(sqrt((int$x[1]-p2x1)^2+(int$x[2]-p2x2)^2)<0.00000000038){
        points(px,py,pch=".",col=cor2[3])
      }else if(sqrt((int$x[1]-p2x1)^2+(int$x[2]-p2x2)^2)<0.0000000007){
        points(px,py,pch=".",col=cor2[2])
      }else {
        points(px,py,pch=".",col=cor2[1])
      }
    }
    #red0
    if(sqrt((int$x[1]-p3x1)^2+(int$x[2]-p3x2)^2)<0.00000001){
      if(sqrt((int$x[1]-p3x1)^2+(int$x[2]-p3x2)^2)<0.00000000006){
        points(px,py,pch=".",col=cor3[4])
      }else if(sqrt((int$x[1]-p3x1)^2+(int$x[2]-p3x2)^2)<0.00000000038){
        points(px,py,pch=".",col=cor3[3])
      }else if(sqrt((int$x[1]-p3x1)^2+(int$x[2]-p3x2)^2)<0.0000000007){
        points(px,py,pch=".",col=cor3[2])
      }else {
        points(px,py,pch=".",col=cor3[1])
      } 
   }
    #green
    if(sqrt((int$x[1]-p4x1)^2+(int$x[2]-p4x2)^2)<0.00000001){
      if(sqrt((int$x[1]-p4x1)^2+(int$x[2]-p4x2)^2)<0.00000000006){
        points(px,py,pch=".",col=cor4[4])
      }else if(sqrt((int$x[1]-p4x1)^2+(int$x[2]-p4x2)^2)<0.00000000038){
        points(px,py,pch=".",col=cor4[3])
      }else if(sqrt((int$x[1]-p4x1)^2+(int$x[2]-p4x2)^2)<0.0000000007){
        points(px,py,pch=".",col=cor4[2])
      }else {
        points(px,py,pch=".",col=cor4[1])
      }
    }
    
    
  }
}