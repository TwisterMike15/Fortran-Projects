c Group 1- Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306- Fortran
c MatrixCalculator, Program 4
c23456
c     # module of matrix operations
      module matrixOperations
        implicit none

        contains
c         # addMatrix, the addition operation and print it
          subroutine addMatrix(ndim,mdim,ndim2,mdim2)
c       	# passing the needed variables to the subroutine
			integer,intent(in)::ndim,mdim,ndim2,mdim2
			integer,dimension(ndim,mdim)::array1
			integer,dimension(ndim2,mdim2)::array2
			integer,dimension(ndim2,mdim2)::sumMat
			integer::i,j

5   	  	format(Xi5)
15   	 	format(a)
	        if(((ndim .le. 10).and.(mdim.le.10).and. (ndim .gt. 0)
     +		.and.(mdim .gt. 0)) .and. ((ndim2 .le. 10).and.(mdim2 .le. 10)
     +      .and. (ndim2 .gt. 0) .and. (mdim2 .gt. 0))) then
			   if(ndim .eq. ndim2 .and. mdim .eq. mdim2) then
			       write(*,*) 'Matrix 1'
				   call getMatrix(ndim,mdim,array1)
				   write(*,*) 'Matrix 2'
				   call getMatrix(ndim2,mdim2,array2)
				   do j=1,mdim
					   do i=1,ndim
						   sumMat(i,j) = array1(i,j) + array2(i,j)
					   end do
				   end do
				   call printAdd(ndim,mdim,array1,array2,sumMat)
			   else
				   print *, 'Addition is not possible, bad matrices.'
			   endif
			else
				print *, 'You entered a matrix with invalid dimensions', 
     +          new_line('a'), 'Enter rows and columns no greater than', 
     +          ' 10 and no less than 0.'
            end if
          end subroutine addMatrix

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c         # subMatrix, the subtraction operation and print it
          subroutine subMatrix(ndim,mdim,ndim2,mdim2)
            integer,intent(in)::ndim,mdim,ndim2,mdim2
			integer,dimension(ndim,mdim)::array1
			integer,dimension(ndim2,mdim2)::array2
			integer, dimension(ndim2,mdim2)::diffMat
			integer::i,j

5   	  	format(Xi5)
15   	 	format(a)
	 		if(((ndim .le. 10).and.(mdim.le.10).and. (ndim .gt. 0)
     +		.and.(mdim .gt. 0)) .and. ((ndim2 .le. 10).and.(mdim2 .le. 10)
     +      .and. (ndim2 .gt. 0) .and. (mdim2 .gt. 0))) then
				if(ndim .eq. ndim2 .and. mdim .eq. mdim2) then
				    write (*,*) 'Matrix 1'
					call getMatrix(ndim,mdim,array1)
					write (*,*) 'Matrix 2'
					call getMatrix(ndim2,mdim2,array2)
					do j=1,mdim
						do i=1,ndim
							diffMat(i,j) = array1(i,j) - array2(i,j)
						end do
					end do
					call printSub(ndim,mdim,array1,array2,diffMat)
				else
					print *, 'Subtraction is not possible, bad matrices.'
				end if
			else
				print *, 'You entered a matrix with invalid dimensions', 
     +          new_line('a'), 'Enter rows and columns no greater than', 
     +          ' 10 and no less than 0.'
			end if
          end subroutine subMatrix

c----------------------------------------------------------------------

c         # multiplyMatrix, the multiplication operation and print it
          subroutine multiMatrix(ndim,mdim,ndim2,mdim2)
            integer,intent(in)::ndim,mdim,ndim2,mdim2
			integer,dimension(ndim,mdim)::array1
			integer,dimension(ndim2,mdim2)::array2
			integer, dimension(ndim2,mdim) :: prodMat
			integer :: i,j, k, dotProduct

5   	  	format(Xi5)
15   	 	format(a)
            if(((ndim .le. 10).and.(mdim.le.10).and. (ndim .gt. 0)
     +		.and.(mdim .gt. 0)) .and. ((ndim2 .le. 10).and.(mdim2 .le. 10)
     +      .and. (ndim2 .gt. 0) .and. (mdim2 .gt. 0))) then
			   if(ndim .eq. mdim2) then
			       write(*,*) "Matrix 1"
				   call getMatrix(ndim,mdim,array1)
				   write(*,*) "Matrix 2"
				   call getMatrix(ndim2,mdim2,array2)
				   do j=1,ndim2
					   do i=1,mdim
						   dotProduct = 0
						   do k = 1, ndim
							   dotProduct = dotProduct + array1(k, i) * array2(j, k)
						   end do 
						   prodMat(j,i) = dotProduct
					   end do
				   end do
				   call printMulti(ndim,mdim,ndim2,mdim2,array1,
     +				   array2,prodMat)
			   else
				   print *, 'Multiplication is not possible',
     +	 		   ', bad matrices.'
			   endif
			else
				print *, 'You entered a matrix with invalid dimensions', 
     +          new_line('a'), 'Enter rows and columns no greater than', 
     +          ' 10 and no less than 0.'
			end if 
          end subroutine multiMatrix

c**********************************************************************

c         # transposeMatrix, the transpose operation and print it
          subroutine transposeMatrix(ndim,mdim)
            integer,intent(in)::ndim,mdim
			integer,dimension(ndim,mdim)::array1
			integer, dimension(mdim,ndim)::transMat
			integer::i,j

5   	  	format(Xi5)
15   	 	format(a)
            if(((ndim .le. 10).and.(mdim.le.10).and. (ndim .gt. 0)
     +		.and.(mdim .gt. 0))) then
			   call getMatrix(ndim,mdim,array1)
			   transMat = transpose(array1)
			   call printTrans(ndim,mdim,array1,transMat)
			else
				print *, 'You entered a matrix with invalid dimensions', 
     +          new_line('a'), 'Enter rows and columns no greater than', 
     +          ' 10 and no less than 0.'
			end if 
          end subroutine transposeMatrix

c======================================================================
c********************** GET INFO SUBROUTINES **************************
c		  # getDimensions, having the user enter the matrices sizes
		  subroutine getDimensions(ndim,mdim)
			integer, intent(out) :: ndim,mdim
			print *,"Enter row size: "
			read (*,*) mdim
			print *,"Enter column size: "
			read (*,*) ndim
		  end subroutine getDimensions

c		  # getMatrix, having the user enter their matrices
		  subroutine getMatrix(ndim,mdim,array)
			integer, intent(in) :: ndim,mdim
			integer, intent(out), dimension(ndim,mdim) :: array
			integer :: i, j
			print *,"Enter matrix values: "
			read (*,*) ((array(j,i),j=1,ndim),i=1,mdim)
		  end subroutine getMatrix

	  
c********************** ALL PRINT SUBROUTINES *************************

c		  # printAdd, printing the addition operation
		  subroutine printAdd(ndim,mdim,array1,array2,answerMat)
5   	  	format(Xi5)
15   	 	format(a)
			integer,intent(in)::ndim,mdim
			integer,intent(in),dimension(:,:)::array1
			integer,intent(in),dimension(:,:)::array2
			integer,intent(in),dimension(:,:)::answerMat
			integer::i,j
			write(*,*) ' MATRIX ONE'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') array1(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' + MATRIX TWO'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') array2(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' = SUMMATION'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') answerMat(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
		  end subroutine printAdd
		  
c		  # printSub, printing the subtraction operation
		  subroutine printSub(ndim,mdim,array1,array2,answerMat)
5   	  	format(Xi5)
15   	 	format(a)
			integer,intent(in)::ndim,mdim
			integer,intent(in),dimension(:,:)::array1
			integer,intent(in),dimension(:,:)::array2
			integer,intent(in),dimension(:,:)::answerMat
			integer::i,j
			write(*,*) ' MATRIX ONE'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') array1(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' - MATRIX TWO'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') array2(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' = DIFFERENCE'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') answerMat(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
		  end subroutine printSub		  
		  
c		  # printMulti, printing the multiplication operation
		  subroutine printMulti(ndim,mdim,ndim2,mdim2,arr1,arr2,ansMat)
5   	  	format(Xi5)
15   	 	format(a)
			integer,intent(in)::ndim,mdim,ndim2,mdim2
			integer,intent(in),dimension(:,:)::arr1
			integer,intent(in),dimension(:,:)::arr2
			integer,intent(in),dimension(:,:)::ansMat
			integer::i,j
			write(*,*) ' MATRIX ONE'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') arr1(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' * MATRIX TWO'
			do j=1,mdim2
				do i=1,ndim2
					write(*,5,advance='no') arr2(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' = PRODUCT'
			do j=1,mdim
				do i=1,ndim2
					write(*,5,advance='no') ansMat(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
		  end subroutine printMulti		  
		  
c		  # printTrans, printing the Transpose operation
		  subroutine printTrans(ndim,mdim,array1,answerMat)
5   	  	format(Xi5)
15   	 	format(a)
			integer,intent(in)::ndim,mdim
			integer,intent(in),dimension(:,:)::array1
			integer,intent(in),dimension(:,:)::answerMat
			integer::i,j
			write(*,*) ' MATRIX'
			do j=1,mdim
				do i=1,ndim
					write(*,5,advance='no') array1(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
			write(*,*) ' MATRIX TRANSPOSED'
			do j=1,ndim
				do i=1,mdim
					write(*,5,advance='no') answerMat(i,j)
				end do
				write(*,15,advance='no') new_line('a')
			end do
		  end subroutine printTrans
		  
      end module matrixOperations

c~~~~~~~~~~~~~~~~~~~~~~~~~~~MAIN PROGRAM~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      program MatrixCalculator
        use matrixOperations
        implicit none

c		# initialize variables
        integer :: ndim = 0, mdim = 0, ndim2 = 0, mdim2 = 0
        character choice * 20
        logical :: quit = .false.

c       # do while user doesnt choose quit
        do while (quit .neqv. .true.)
c         # prompt user for input
          print *, new_line('a'), 'Select an option below. This can be', 
     c    ' done by typing the corresponding number, the operation',
     c     new_line('a'), 'symbol, first letter, or enctire word.'
          print *, "1/+/'a'/'add' - Matrix Addition"
          print *, "2/-/'s'/'subtract' - Matrix Subtraction"
          print *, "3/*/'m'/'multiply' - Matrix Dot Product"
          print *, "4/'t'/'transpose' - Matrix Transposition"
          print *, "5/'q'/'quit' - Quit"

          read (*,*) choice

c         # user options case statement
          select case(choice)
            case ('1','+','a','add')
              write(*,*) 'Matrix 1'
              call getDimensions(ndim,mdim)
              write(*,*) 'Matrix 2'
			  call getDimensions(ndim2,mdim2)
              call addMatrix(ndim,mdim,ndim2,mdim2)

            case ('2','-','s','subtract')
              write(*,*) 'Matrix 1'
              call getDimensions(ndim,mdim)
              write(*,*) 'Matrix 2'
			  call getDimensions(ndim2,mdim2)
              call subMatrix(ndim,mdim,ndim2,mdim2)

            case ('3','*','m','multiply')
              write(*,*) 'Matrix 1'
              call getDimensions(ndim,mdim)
              write(*,*) 'Matrix 2'
			  call getDimensions(ndim2,mdim2)
              call multiMatrix(ndim,mdim,ndim2,mdim2)

            case ('4','t','transpose')
              write(*,*) 'Only one matrix will be used for this',
     +        ' operation'
              call getDimensions(ndim,mdim)
              call transposeMatrix(ndim,mdim)

            case ('5','q','quit')
              quit = .true.

            case default
              print *, 'You did not select a valid option. Try again'

          end select
        end do
      end
