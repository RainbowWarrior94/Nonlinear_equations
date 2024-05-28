!Autor: Anastasiya Kruhlik
program nonlinear

    real a, b, dokl, h, c, z, z1
    integer n
    character*1 tn  

    write (*,*) 'Autor: Anastasiya Kruhlik' 
    write (*,*) 'Projekt:Rozwiazanie rownan nieliniowych'
    write (*,*)
   
1   write(*,'(A)', advance="no") 'Podaj granice przedzialu "a": ' 
    read(*,*) a
    write(*,'(A)', advance="no") 'Podaj granice przedzialu "b": ' 
    read(*,*) b
    
   if (a.GE.b) then
    write (*,*) 'Wartosc "a" musi byc mniejsza niz wartosc "b"!'
    goto 1 
   end if
    
    write(*,'(A)', advance="no") 'Podaj wartosc przyrostu "h": ' 
    read(*,*) h
    write(*,'(A)', advance="no") 'Podaj dokladnosc: '
    read(*,*) dokl 
    write (*,*)

open (20, file = 'result.txt', status='unknown') 

     n=0
     z=a+h  !paczatek metody przeszukiwania przedzialu 
     
!przechodzimy przez caly przedzial z krokiem h i szukamy pierwiastkow rownania na kazdym odcinku < a, a+h>   
   do while (z.LE.b)                  
     if (f(a)*f(z).LE.0) then
       z1=z
       write (*,*)
       write (20,*)
       write (*,'(a,F6.2,a,F6.2,a)') 'Przedial w ktorym istnieje co najmniej jeden pierwiastek rownania: <',a,',',z1,'>'
       write (20,'(a,F6.2,a,F6.2,a)') 'Przedial w ktorym istnieje co najmniej jeden pierwiastek rownania: <',a,',',z1,'>'
       
!Jesli na odcinku istnieje pierwiastek, kontynuujemy go zmniejszac, az dlugosc odcinka bedzie mniejzsa  lub rowna niz wpisana dokladnosc  
       do while (abs(a-z1).GT.dokl) !Czyli stosujemy metode polowienia przedzialu
         c=(a+z1)/2                 
         if (abs(f(c)).LE.1e-10) then
           goto 2
         end if  
    
         if (f(a)*f(c).LT.1e-10) then
           z1=c
         else
           a=c
         end if
       end do
   
       c=(a+z1)/2

2      write (*,'(a,F10.6,a,F10.6,a)') 'Ostateczny przedzial: <',a,',',z1,'>'
       write (20,'(a,F10.6,a,F10.6,a)') 'Ostateczny przedzial: <',a,',',z1,'>'
       write (*,'(a,F9.7)') 'Ostateczna dlugosc przedzialu: ', abs(z1-a) 
       write (20,'(a,F9.7)') 'Ostateczna dlugosc przedzialu: ', abs(z1-a)
       write (*,*) 'Wartosc X=', c
       write (20,*) 'Wartosc X=', c
       write (*,*) '----------------------------------------------------------------------------------'
       write (20,*) '----------------------------------------------------------------------------------'
       n=n+1      
     end if
     a=z
     z=a+h     
   end do

   if (n.EQ.0) then
     write(*,*) 'Nie ma pierwiastkow'
     write(20,*) 'Nie ma pierwiastkow'
     write(*,*)
   end if
close (20)    

    write(*,*)
    write(*,*) 'Wyniki zostaly zapisane w pliku result.txt'
    write(*,*)

 	write(*,*)'Chcesz podac inne dane dla rozwiazania rownania (t lub n)?  '
    read(*,*)tn
    if(tn.eq.'t')goto 1   

    write(*,*) 'nacisnij ENTER, aby zamknac program'
    read(*,*) 
    
end

function f(x)
  f=2*x**3-9.06843*x**2-72.3753*x+114.834
end










    
