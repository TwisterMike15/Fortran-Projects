c Group 1- Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306- Fortran
c MatrixAddProgram, Program3
      program MatrixAddProgram
      implicit none

      integer, parameter :: NMAX = 10, MMAX = 10, NUMOFMATRICIESMAX = 10
      integer :: status, i, j, k
      integer :: flag = 0
      integer :: num_of_matricies = 0
      integer :: ndim = 0, mdim = 0
      integer, dimension(3) :: settings = 0
      real, dimension(NMAX,MMAX,NUMOFMATRICIESMAX) :: input_matrix = 0
      real, dimension(NMAX,MMAX) :: sum_matrix = 0
      
      integer :: inFileError = 0, outFileError = 0, ios = 0
      character(len = 20) :: inFileName, outFileName, choice
      logical :: inFileExists = .false., outFileExists = .true.
      logical :: quit = .false., goodChoice = .false.
      logical :: correctAmount = .true.
      character(len = 1) :: options

5     format(Xf5.2)
10    format(i2)
15    format(a)
20    format(ai2)
25    format(axi2xa)

30     format(A30)


c Main

C Initial prompts
        write (*,*) 'Enter an input file or type QUIT to exit: '
        read (*,30) inFileName

        
        
C Code to handle input file   
C Loop handles subsequent prompts       
      do while((.not. inFileExists) .and. (.not. quit))
      
c Inquire about file's existence.
        Inquire(FILE = inFileName, EXIST = inFileExists)

        if((inFileName .eq. 'quit') .or. (inFileName .eq. 'QUIT')) 
     C   then
              quit = .true.
              write (*,*) 'You have decided to quit'
           end if
        
        if(inFileExists .and. .not. quit) then
           Open(unit = 1, file = inFileName, status = 'old', 
     C      iostat = inFileError)
           write(*,*) 'The file has been opened.'
        else
           
           
           
           if(.not. quit) then 
              write(*,*) 'The file you have chosen does not exist.'
              write(*,*) 'Please reenter a file name or type "QUIT" to 
     Cquit'     
              read(*, 30) inFileName
           end if
        end if
      end do

      if(inFileError .ne. 0) then
             write(*,*) 'There was an error when opening the input file'
             quit = .true.
      end if
      
      
      
C Code to handle outfile   

       if((inFileError .eq. 0) .and. (.not. quit)) then
           write(*,*) 'Enter an outfile name: '
           read (*,*) outFileName
           if (outFileName .eq. 'quit' .or. outFileName .eq. 'QUIT')
     C then
              quit = .true.
              write(*,*) 'You have decided to quit'
           end if
           do while((outFileExists) .and. ( .not. quit))
           
           goodChoice = .false.
           Inquire(FILE = outFileName, EXIST = outFileExists)
        
           if(.not. outFileExists) then
              Open(unit = 2, file = outFileName, status = 'new', 
     C        iostat = ios)
     
           else
              write(*,*) 'The file exists' 
              do while(.not. goodChoice)
                 write(*,*) 'Please type one of the 3 options:'
                 write(*,*) 'new- Prompts to enter a new filename'
                 write(*,*) 'overwrite- Overwrites the selected file'
                 write(*,*) 'quit- Quits the program' 
                 read (*,*) choice
           
                 if ((choice .eq. 'new') .or. (choice .eq. 'NEW')) then
                      write(*,*) 'enter your new file name'
                      read(*,30) outFileName
                      goodChoice = .true.
                 end if 
                 
                 if(choice .eq."overwrite".or. choice .eq."OVERWRITE")
     C then
                      if(inFileName .ne. outFileName) then
                         Open(unit = 2, file = outFileName, 
     C                   status = 'replace', iostat = outFileError)
                         goodChoice = .true.
                         outFileExists = .false.
                      else
                         write(*,*) "Silly professor, you can't overwrit
     Ce your input file"            
              
                      end if
                 end if
     
                 if((choice .eq. 'quit') .or. (choice .eq. 'QUIT')) then
                      quit = .true.
                      write(*,*) 'You have decided quit'
                      goodChoice = .true.
              
                 end if     
                 if(.not. goodChoice) then 
              
                      write(*,*) 'You did not select a valid option. Try 
     C again'
          
                 end if
        
           
              end do
           end if  
          end do
       end if 
       

       if(outFileError .ne. 0) then
          write(*,*) 'There was an error opening the output file'
          quit = .true.
       end if

        
       
C   We make sure that the infile exists and that the outfile does not
C   Exist, and that they did not decide to quit            

      if(inFileExists .and.(.not. outFileExists).and.(.not. quit)) then
      
      
      
      
      
      
      
c     # read number of matricies in file and dimensions
      read(1,*) settings

      num_of_matricies = settings(1)
      mdim = settings(2)
      ndim = settings(3)

c     # check if the values for the matrix are greater than maxes
      if((num_of_matricies .gt. 10) 
     +  .or. (mdim .gt. 10) .or. ndim .gt. 10 
     + .or. num_of_matricies .le. 0 .or. mdim .le. 0 
     + .or. ndim .le. 0) then
        print *, 'Error: Matrix settings out of bounds.'
      else
      

c     # read the matricies from the input file
      do k=1,num_of_matricies
        do j=1,mdim
            read(1,*, IOSTAT=status) (input_matrix(i,j,k),i=1,ndim)
c     # check for end of file
            if(is_iostat_end(status)) then
              print *, 'End of file reached unexpectedly, try again.'
              correctAmount = .false.
            end if
        end do
      end do
      
      if(correctAmount) then
c     # calculate a summation of the matricies and write inputs
      do k=1,num_of_matricies
        write(2,20) 'MATRIX:', k
        sum_matrix = sum_matrix + input_matrix(:,:,k)
        do j=1,mdim
          do i=1, ndim
            write(2,5,advance='no') input_matrix(i,j,k)
          end do
          write(2,15,advance='no') new_line('a')
        end do
      end do

      write(2,25) 'SUM OF ALL', num_of_matricies, 'MATRICIES:'

c     # write summation to file
      do j=1,mdim
        do i=1,ndim
          write(2,5,advance='no') sum_matrix(i,j)
        end do
        write(2,15,advance='no') new_line('a')
      end do
      write(*,*) "Matrices have been placed in the output file"
      write(*,*) "Execution has finished properly"
      end if
      end if
      end if
c     # close files
      if (inFileExists) then
         close(1)
      end if
      if(outFileExists) then
         close(2)
      end if       

      end program