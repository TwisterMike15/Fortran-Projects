c Group 1- Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306- Fortran
c Graphing Program, Program 5
c23456
      module funcs
        implicit none
        contains
c**************************isHere*******************************

c Search subroutine starts here

        subroutine isHere(XValue, DX, numofpoints, aop, XMIN, YMIN, 
     +  YMAX, linesPrinted)
          real,    intent(in) :: XValue, DX, XMIN, YMIN, YMAX
          integer, intent(in) :: numofpoints, linesPrinted
          real,    intent(in), dimension(100,2) :: aop
          integer, dimension(100) :: plotable_X_Indexes
          integer :: i = 1, numOfPointsFound = 0
          real :: maxSearch, minSearch
         

          maxSearch = XValue + DX/2
          minSearch = XValue - DX/2
c          write(*,*) 'Min Search is:', minSearch
c          write(*,*) 'Max Search is:', maxSearch

          do while (i .le. numofpoints)
c           check to see if point exists
               if((aop(i,1) .gt. minSearch ) .and.
     +          (aop(i,1) .le. maxSearch)) then
                   numOfPointsFound = numOfPointsFound+1
                   plotable_X_Indexes(numOfPointsFound) = i
               end if
               i = i+1
          end do
          call pointFound(numOfPointsFound, XMIN, minSearch, aop, 
     +    YMIN,YMAX, plotable_X_Indexes, linesPrinted, XValue)
          i = 1
          numOfPointsFound = 0
        end subroutine


c**************************pointFound*******************************
c check if a point is in X


      subroutine pointFound(numOfPointsFound, XMIN, minSearch, aop,
     +YMIN, YMAX, plotable_X_Indexes, linesPrinted, XValue)
         integer, intent(in) :: numOfPointsFound, linesPrinted
         real, intent(in) :: XMIN, minSearch, YMIN, YMAX, XValue
         real,    intent(in), dimension(100,2) :: aop
         integer, intent(in), dimension(100):: plotable_X_Indexes
         
         if( XMIN .gt. minSearch) then
            call prepDashes(aop,numOfPointsFound, YMIN, YMAX,
     +       plotable_X_Indexes, XValue)
         else
            call prepLineBuff(aop, numOfPointsFound, YMIN, YMAX,
     +      plotable_X_Indexes, linesPrinted, XValue) 
         end if
            
         
c         write(*,*) "Point was found"

      end subroutine pointFound

      
      
      
      
c**************************prepDashes*******************************
c Prepare the initial line of dashes for printing

      subroutine prepDashes(aop, numOfPointsFound, YMIN, YMAX,
     +plotable_X_Indexes, XValue) 
        integer, parameter :: BUFFER_SIZE = 67
        real,    intent(in), dimension(100,2) :: aop
        character(1), dimension(BUFFER_SIZE):: dashBuff 
        integer, intent(in) :: numOfPointsFound 
        integer :: i 
        real, intent(in) :: YMIN, YMAX, Xvalue
        integer, intent(in), dimension(100):: plotable_X_Indexes
        dashBuff = '' 
        do i = 1, BUFFER_SIZE
           dashBuff(i) = '-'
        end do
        do i = 1, BUFFER_SIZE,11
           dashBuff(i) = '+'
        end do
        
        call determineYPlot(aop, dashBuff, BUFFER_SIZE, 
     +  numOfPointsFound, YMIN, YMAX, plotable_X_Indexes, 5, XValue)
        
      end subroutine
      
c**************************prepLineBuff*******************************  
c prepare non-initial lines for printing
    
      subroutine prepLineBuff(aop, numOfPointsFound, YMIN, YMAX,
     + plotable_X_Indexes, linesPrinted, XValue)
        integer, parameter :: BUFFER_SIZE = 67
        real,    intent(in), dimension(100,2) :: aop
        character(1), dimension(BUFFER_SIZE):: lineBuff 
        integer, intent(in) :: numOfPointsFound, linesPrinted
        integer :: i 
        real, intent(in) :: YMIN, YMAX, XValue
        integer, intent(in), dimension(100):: plotable_X_Indexes
        lineBuff = '' 
        
        if(MOD(linesPrinted,5) .eq. 0) then
c           lineBuff(1) = 12
           lineBuff(1) = '+'
        else
           lineBuff(1) = '|'
        end if
        
        call determineYPlot(aop, lineBuff, BUFFER_SIZE, 
     +  numOfPointsFound, YMIN, YMAX, plotable_X_Indexes, 
     +  linesPrinted ,XValue)
     
      end subroutine

c**************************detrmineYPlot*******************************
C determine where on Y to plot the point and plot it
     
      subroutine determineYPlot(aop, Buffer, BUFFER_SIZE,
     +numOfPointsFound, YMIN, YMAX, plotable_X_Indexes, linesPrinted,
     + XValue)
40    format(F12.2, 67A) 
45    format(12X, 67A) 

         real,    intent(in), dimension(100,2) :: aop
         integer, intent(in) :: BUFFER_SIZE, linesPrinted
         character(1), dimension(BUFFER_SIZE), intent(inout):: Buffer
         integer, intent(in) :: numOfPointsFound
         integer :: i, XCord, YPlotIndex
         real, intent(in) :: YMIN, YMAX, XValue
         real YPoint
         integer, intent(in), dimension(100):: plotable_X_Indexes
         do i=1, numOfPointsFound
            XCord = plotable_X_Indexes(i)
            YPoint = aop(XCord,2) 
            YPlotIndex = INT((YPoint-YMIN)/(YMAX-YMIN)*66)+1
            if(Buffer(YPlotIndex) .eq. '*') then
               Buffer(YPlotIndex) = 'O'
            else
               Buffer(YPlotIndex) = '*'
            end if 
        end do
         
        
        if(mod(linesPrinted, 5) .eq. 0) then
           write(2, 40) XValue, Buffer
        else
           write(2, 45) Buffer 
        end if    
         
         
      end subroutine      
      
c**************************findXMin*******************************
c finds XMin
      real function findXMin(aop, numofpoints)
        real,    intent(in), dimension(100,2) :: aop
        integer, intent(in) :: numofpoints
        real :: xmin
        integer :: i = 1

C     find XMin
        xmin = aop(1,1)

        do while (i .le. numofpoints)
          if(xmin > aop(i,1)) then
            xmin = aop(i,1)
          end if
          i = i + 1
        end do

        findXMin = xmin
        return
      end function

c**************************findXMax*******************************
c finds XMAX
      real function findXMax(aop, numofpoints)
        real,    intent(in), dimension(100,2) :: aop
        integer, intent(in) :: numofpoints
        real :: xmax
        integer :: i = 1

C     find XMax
        xmax = aop(1,1)

        do while (i .le. numofpoints)
          if(xmax < aop(i,1)) then
            xmax = aop(i,1)
          end if
          i = i + 1
        end do

        findXMax = xmax
        return
      end function

c**************************findYMin*******************************
c finds Y min
      real function findYMin(aop, numofpoints)
        real,    intent(in), dimension(100,2) :: aop
        integer, intent(in) :: numofpoints
        real :: ymin
        integer :: i = 1

C     find YMin
        ymin = aop(1,2)

        do while (i .le. numofpoints)
          if(ymin > aop(i,2)) then
            ymin = aop(i,2)
          end if
          i = i + 1
        end do

        findYMin = ymin
        return
      end function

c**************************findYMax*******************************
c finds y max
      real function findYMax(aop, numofpoints)
        real,    intent(in), dimension(100,2) :: aop
        integer, intent(in) :: numofpoints
        real :: ymax
        integer :: i = 1

C     find YMax
        ymax = aop(1,2)

        do while (i .le. numofpoints)
          if(ymax < aop(i,2)) then
            ymax = aop(i,2)
          end if
          i = i + 1
        end do

        findYMax = ymax
        return
      end function

      end module funcs
C***************************Main********************************

C Main starts here
C File handling is first, then stuff for graphing

      program GraphingProgram

      use funcs

      implicit none

      integer :: num_of_points = 0, i = 1, j = 1, status = 0, sort_max
      real :: XMAX, YMAX, XMIN, YMIN, YSTEP, DX = 9999999,
     +difference, XValue = 0, YTick
      integer :: inFileError = 0, outFileError = 0, ios = 0
      integer :: XSTEP = 0, linesPrinted = 0
      character(len = 20) :: inFileName, outFileName, choice
      logical :: inFileExists = .false., outFileExists = .true.
      logical :: quit = .false., goodChoice = .false.
      logical :: correctAmount = .true., more = .true.
      real, dimension(100, 2) :: array_of_points
      real :: temp = 0
      real :: tempx, tempy
      real, dimension(7) :: YTicks

5     format(Xf5.2)
10    format(i2)
15    format(a)
20    format(ai2)
25    format(axi2xa)

30    format(A30)
35    format (F14.2, 6F11.2)

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
      write(2,*) 'Data Points:'
      if(inFileExists .and.(.not. outFileExists).and.(.not. quit)) then
         read(1,*) num_of_points
         if(num_of_points .le. 100 .and. num_of_points .ge. 1)
     +   then
            do i = 1, num_of_points
              read(1,*, iostat = status) (array_of_points(i,j),j=1,2)
              write(2,*) array_of_points(i,1), array_of_points(i,2)
              if(is_iostat_end(status)) then
                 print *, 'End of file reached unexpectedly, try again.'
                 correctAmount = .false.
              end if
           end do

C If we make it past here, we know we have good data

C Sorting starts here if we read in the right amount of data
          if(correctAmount) then
              sort_max = num_of_points - 1
              do while(more)
                 more = .false.
                 do i = 1, sort_max
                    if(array_of_points(i,1) .gt. array_of_points(i+1,1))
     +              then
                       more = .true.
                       temp = array_of_points(i,1)
                       array_of_points(i,1) = array_of_points(i+1,1)
                       array_of_points(i+1,1) = temp

                       temp = array_of_points(i, 2)
                       array_of_points(i,2) = array_of_points(i+1,2)
                       array_of_points(i+1,2) = temp
                    end if
               end do
               sort_max = sort_max - 1
            end do
            write(2,*) 'Sorted Data Points:'
            do i = 1, num_of_points
                write(2,*) (array_of_points(i,j),j=1,2)
            end do

c Gets our Y and X min and max
            XMAX = findXMax(array_of_points,num_of_points)
            YMAX = findYMax(array_of_points,num_of_points)
            XMIN = findXMin(array_of_points,num_of_points)
            YMIN = findYMin(array_of_points,num_of_points)

            YSTEP = (YMAX-YMIN)/6

           
c calculates our ytick, the values that we step for y
            do i = 0, 6
               YTick = YMIN + i * YSTEP
               YTicks(i+1) = YTick
               
            end do
              
c Array stuff here
            do i = 1, num_of_points - 1
c                write(*,*) (array_of_points(i,j),j=1,2)
                difference = ABS(ABS(array_of_points(i,1)) -
     +          ABS(array_of_points(i+1,1)))
                if(difference .lt. DX ) then
                   if(difference .ne. 0) then
                      DX = difference
c                      write(*,*) DX
                   end if
                end if
            end do

            XSTEP = ((XMAX-XMIN)/DX) + 1

            if(XSTEP .lt. 20) then
               XSTEP = 20
               DX=(XMAX-XMIN)/(XSTEP - 1)
            else
               if(XSTEP .gt. 200) then
                  XSTEP = 200
                  DX=(XMAX-XMIN)/(XSTEP - 1)
               end if
            end if
            XValue = XMIN + 0 * DX
            


c prints our values to the file
            write(2,*) "YMIN", YMIN, "YMAX", YMAX
            write(2,*) "XMIN", XMIN, "XMAX", XMAX
            write(2,*) "XSTEP", XSTEP, "YSTEP", YSTEP
            write(2,*) "DX", DX
            write(2,35) YTicks


C XSTEP LOOP
            do i = 0, XSTEP
               XValue = XMIN + i * DX
               call isHere(XValue,DX, num_of_points,array_of_points, 
     +          XMIN, YMIN, YMAX, i)
            end do
             write(*,*) 'The program has processed your input ',
     +       'sucessfully'
           end if
        else
              write(*,*) 'The number provided for amount of points ',
     +        'be below 0 or above 100. Try again'
      end if
      end if

      end program GraphingProgram
