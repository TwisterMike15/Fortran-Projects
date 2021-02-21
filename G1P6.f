
c Group 1- Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306- Fortran
c ComplexSolver, Program6

C********************** SIMUL *******************************
       module funcs
       implicit none
       contains
      SUBROUTINE simul ( a, b, ndim, n, error )

      INTEGER, INTENT(IN) :: ndim
      COMPLEX, INTENT(INOUT), DIMENSION(10,10) :: a
      COMPLEX, INTENT(INOUT), DIMENSION(10) :: b
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(OUT) :: error
      REAL, PARAMETER :: EPSILON = 1.0E-6
      real :: factor
      INTEGER :: irow
      INTEGER :: ipeak
      INTEGER :: jrow
      INTEGER :: kcol
      COMPLEX :: temp
      

c       main loop processing n times to get all equations
        do irow=1,n
c         find peak pivot for column irow in rows irow to n
          ipeak = irow
          do jrow=irow+1,n
            if(cabs(a(jrow,irow)).GT.cabs(a(ipeak,irow)))then
              ipeak=jrow
            endif
          enddo
c         check for singular equations
          if(abs(real(a(ipeak,irow))).LT.epsilon)then
            error=1
            RETURN
          endif
c         otherwise, if ipeak /= irow, swap equations irow & ipeak
          if(ipeak.NE.irow)then
            do kcol=1,n
              temp=a(ipeak,kcol)
              a(ipeak,kcol)=a(irow,kcol)
              a(irow,kcol)=temp
            enddo
            temp=b(ipeak)
            b(ipeak)=b(irow)
            b(irow)=temp
          endif
c         multiply equation irow by -a(jrow,irow)/a(irow,irow),
c         and add it to equation jrow (for all eqns except irow itself)
          do jrow=1,n
            if(jrow.NE.irow)then
              factor=-a(jrow,irow)/a(irow,irow)
              do kcol=1,n
                a(jrow,kcol)=a(irow,kcol)*factor+a(jrow,kcol)
              enddo
              b(jrow)=b(irow)*factor+b(jrow)
            endif
          enddo
c       end of main loop
        enddo
c       divide each equation by the coefficient of its on-diagonal term
        do irow=1,n
          b(irow)=b(irow)/a(irow,irow)
          a(irow,irow)=1
        enddo
        error=0
      END SUBROUTINE simul
	  
	  SUBROUTINE printMatrix(a,b,ndim)
      implicit none
      
      INTEGER,INTENT(IN)::ndim
      COMPLEX,INTENT(IN),DIMENSION(10,10)::a
      COMPLEX,INTENT(IN),DIMENSION(10)::b
      INTEGER::i,j,k
30    format('(',f12.4,X, '+', X, f12.4,'i',')',2X, 'X',I2, 2X, '+')      
40    format('(',f12.4,X, '+', X, f12.4,'i',')',2X, 'X',I2, 2X, '=')       
50    format('(',f12.4,X, '+', X, f12.4,'i',')') 
60    format(/)      

      do j = 1, ndim
       do k = 1, ndim
        
        if(k .lt. ndim) then
           write(*,30) a(j,k), k
        else
           write(*,40) a(j,k), k 
        end if
        
      end do
      write(*,50) (b(j))
      write(*,60)
      end do 
      
      END SUBROUTINE printMatrix
	  
      
      SUBROUTINE printSolutions(solutions, size)
         implicit none
         integer, intent(in) :: size
         integer :: i
         complex, intent(in), dimension (10) :: solutions
10       format('X', I2,2X, '=', 2X '(', F12.4, '+', F12.4, 'i', ')')          
         write(*,*) "The solution vector is:"
         do i = 1, size
            write(*,10) i, solutions(i)
         end do


      end subroutine printSolutions 

	  end module funcs
C********************* MAIN PROGRAM ********************
      program ComplexSolverProgram
      use funcs
      implicit none

	  logical :: cont = .true., badChoice = .true., incorrect = .true.
	  integer :: n, i, j, k, m, error
      integer :: shape_vec(2)
      integer, parameter :: max = 10
	  character(len=20) :: choice
      real :: real_part, imaginary_part
      complex, dimension(10,10) :: coefficient_matrix
      complex, dimension(10) :: solution_vector
      
1     format(/)

10    format(2f10.2)
15    format(11('('f12.6,XX,'+',XX,'i',',',XX,f12.6')'))
20    format('Please enter ', i2, ' equations with ', i2, 
     c ' coefficients for each coefficient matrix.', /, 'Enter 1 real'
     c' and 1 imaginary value per entry')
25    format('A(',i2,',',i2,') as real imaginary.')
30    format('Enter the ', i2, ' constant values.')
35    format('Constant(', i2, ') as real imaginary.')
40    format(('(',f12.6,XX,'+',XX,'i',',',XX,f12.6')'))
45    format('Constant(',i2, ')',XX, '(',f12.6,XX,'+',XX,'i',',',XX,f12.
     c6')')
50    format('Constant(', i2, ')')


      
      Do while(cont)
c        reset matrix and solution vectors
         do i=1,10
            do j=1,10
                coefficient_matrix(i,j) = 0
            end do
            solution_vector(i) = 0
         end do
         badChoice = .true.
         incorrect = .true.
         do while (incorrect)
            write(*,*) "Would you like to solve for unknowns in ",
     +      "complex linear equations? Enter yes/y or no/n"
            read(*,*) choice 
            select case(choice)
                case("yes", "y", "YES", "Y")
	 			   
	            incorrect = .false.
                case("no", "NO", "N", "n")
				    incorrect = .false.
				    cont = .false.
			   case default 
			      write(*,*) "You entered an option that does not exist. ",
     +            "Please enter one that does" 
            end select
         end do
         if(cont) then
            
c Prompt user for amount of equations or unknowns
      print *,'Please enter the number of equations and unknowns ',
     +'you wish to solve for.'
      read(*,*) n
      if(n .le. 10 .and. n .gt. 0) then 
      write(*,20) n, n 
      i = n
c	     Ensure the number of equations is 10 or less
		 if(n .le. max)then
c	     prompt for coefficients in coefficient array
			do j = 1, n
				do k = 1, i
c	     	prompt and read real and imaginary part of complex number
				  write(*,25) j,k
				  read(*,*) real_part, imaginary_part
c	     	assign values to complex array
				  coefficient_matrix(j,k) = cmplx(real_part,
     c				imaginary_part)
				end do
			end do
	        
	  
c	     prompt for solution vector
		 write(*,30) n
			do j = 1, n
c	     prompt and read real and imaginary part of complex number
				write(*,35) j
				read(*,*) real_part, imaginary_part
c	     assign values to complex array
				solution_vector(j) = cmplx(real_part,imaginary_part)
			end do
		 endif

c	     debug print to see if the print matches data entered
c		 do j = 1, n
c			write(*,15)(coefficient_matrix(j,k),k=1,i), solution_vector(j)
c		 end do		

c check values with user



      call printMatrix(coefficient_matrix,solution_vector,n)
c             do j = 1, n
c               do k = 1, i
c                 write(*,*) (coefficient_matrix(j,k))
c               end do
c             end do

c            do j = 1, n
c               write(*,*)j
c               write(*, *) (solution_vector(j))
c            end do	  
          
          Do while (badChoice)
             write(*,*)"Are these the correct values? (yes/y or no/n)"
             read(*,*) choice
             select case(choice)
                case("yes", "y", "YES", "Y")
		    call simul(coefficient_matrix, solution_vector, n, n, error )
	 	    write (*,1) 
            if(error .ne. 1) then
            
	           call printSolutions(solution_vector, n)
      
	        else
	  
                write(*,*) "There is no solution to this system ", 
     +          "of linear equations"  	
	        end if 
	        write (*,1) 
	            badChoice = .false.
                case("no", "NO", "N", "n")
				    badChoice = .false.
					write(*,*) "Try again"
			   case default 
			      write(*,*) "You entered an option that does not exist.", 
     +            " Please enter one that does" 
            end select
         end do		 
     
      
         else 
	        write(*,*) "You must enter a number between 1 and 10 ", 
     +      "for your number of equations and unknowns."
         end if 
      else
         write(*,*) "You have decided to quit"
      end if
	  end do

      end program ComplexSolverProgram
