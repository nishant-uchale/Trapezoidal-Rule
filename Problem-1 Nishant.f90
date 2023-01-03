
program trapezoid
implicit none
real::exact,approx,error,h,term1,term2,sum,y,a,b 
integer::n,i
open (15, file='out.dat')
a=0
b=3.1415
exact=((exp(3.0*b))/3.0)+(b**2.0)-(1.0/3.0) 
write(15,*)"Exact value =",exact
write(15,*)"  No.of sub-int    Trapezoid  Numerical Value        Error"
write(15,*)"--------------------------------------------------------"         
do n=2,60,2
h=(b-a)/n
term1=h*(Fun(a)+Fun(b))/2
sum=0
error=0
i=1
201 sum=sum+(Fun(a+i*h))
i=i+1
y=n-1
if(i.le.y)goto 201
term2=sum*h
approx=term1+term2
error=exact-approx
write(15,*)n,"      ", approx,"      ", error      			  											   
end do
contains													 
function Fun(x)
real::Fun,x
Fun=exp(3*x)+2*x
end function Fun
end program 



 


