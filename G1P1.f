c Group 1- Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306- Fortran
c GradeAveragingProgram, Program1
c234567
       program Grade_Average
       implicit none
c Initiated in order: number of grades entered, name of the user, running grade
c total, current grade, and grade average
       integer gradeCount, I
       character userName*30
       real gradeTotal, gradeN, gradeAverage
       gradeTotal = 0
       gradeN = 0
       gradeAverage = 0
       gradeCount = 0
       I = 0
       
c Create formats for the input and output
7      format(A30)
8      format(I5)
9      format(/A)
11     format(/, 'The name is ', A/,'The number of grades is', 
     + I5/, 'The grade total is',F13.2/,'The grade average is', F11.2)
     
c Main portion of code
       write(*,9) "Enter user's name: "
       read(*,7) userName
       
C Asks the user for another value, if they entered a bad one
       do while (gradeCount .LE. 0)
	       write(*,9) "Enter number of grades to be calculated: " 
	       read(*,8) gradeCount
	       
C Checkes to make sure the grade is greater than 0 before proceeding. If it is not, an error message is printed
	       if (gradeCount .GT. 0) then
		       do I=1, gradeCount
		       	 write(*,9) "Enter a grade: "
		       	 read(*,*) gradeN
		       	 gradeTotal = gradeTotal + gradeN
		       end do
		       
C Calculates the average 
	       	   gradeAverage = gradeTotal/gradeCount
	           write(*,11) userName,gradeCount,gradeTotal,gradeAverage
	           
C Prints error message if gradeCount is 0 or less
		   else		   
		   		write(*,9) 'You cannot enter 0 or less grades'
		   end if
	      end do
      
       stop
       end