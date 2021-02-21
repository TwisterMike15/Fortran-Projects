c Group 1 - Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306 - Fortran
c GradeAveragingProgramWithArray, Program2
      program Grade_Average_With_Array
      implicit none
      
C Variable and array declarations
      character*20 name
      integer :: i = 1, j = 1, flag = 1
      integer, parameter :: max = 10
      real, dimension(10) :: grades = 0.0
      real :: sum = 0, average = 0, temp = 1
      
      
C	Fomrat Statements      
5     format(a20)
10    format(/,"Enter grade no. ", i2, ":")
15    format(' ',10(f6.2, 2x))
20 	  format(/,"The sum is ", F6.2/, "The average is ", F6.2)
25    format(/,"An invalid grade below 0 or above 100 was entered")
30    format(/,"The maximum number of grades has been entered",/)
35    format(/,"Your name is ", A20/) 


C	Main section of code


c     # prompt and collect user's name
      print *, "Who's grade average is this?"
      read (*,5) name
      
       
        
c     # prompt and collect user's grades
      do while ((temp .ge. 0.0) .and. (temp .le. 100.0))

c Make sure we're not at max          
        if (i .gt. max) then
          print 30
          temp = 101
           
c if not, then we get the grade
        else
          print 10, i
          read (*,*) temp
          
c Check to make sure that we are not less than 0 or greater than 100
          if((temp .lt. 0) .or. (temp .gt. 100)) then
            print 25
          else
            grades(i) = temp
            i=i+1
            temp = 0
          end if
        end if
        
      end do

c Make sure we got a grade, if we did, we average them
      if(i .gt. 1) then
      	do j=1,i
          sum = sum + grades(j-1)
      	end do
      	average = sum/(i-1)
 
      	
c print everything out     	
      	print 35, name
        print *, "Grades Entered:"
    
        print 15, (grades(j), j=1,i-1) 
        print 20, sum, average
      else
        print *, "No Grades were entered"
        end if 
      stop
      end