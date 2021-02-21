c Group 1- Ryan Merow, Elizabeth Sterling, James Merenda, Mike Gorse
c CSC 306- Fortran, 11-21-2020
c Linked-List, Program 7
c23456
	  
	  MODULE Nodes
		IMPLICIT NONE
		TYPE :: node
			INTEGER::num
			CHARACTER::person*20
			TYPE(node),POINTER::next,prev
		END TYPE node
	  END MODULE Nodes
	  
	  MODULE LLFuncs
		Use Nodes
		IMPLICIT NONE
		
		CONTAINS
c           subroutine to print doubly linked list
			SUBROUTINE printLL(head,tail)

10    format(A15, X, I3)			
			   
				TYPE(node),POINTER,INTENT(INOUT)::head,tail
				TYPE(node),POINTER :: temp
c				write(*,*) "linked list prints"
				temp=>head
c               cycle through each node starting from head and print 
c               data while temp is associated with a node.
				DO WHILE(associated(temp))
					write(*,*)temp%person,temp%num
					write(2,*)temp%person,temp%num
					temp=>temp%next
				END DO
				
			END SUBROUTINE printLL

c           subroutine to add node to doubly linked list
			SUBROUTINE addNode(head,tail,new,filePerson, fileNum)
			
				TYPE(node),POINTER,INTENT(INOUT)::head,tail,new
				CHARACTER (len= 20),INTENT(IN)::filePerson
				INTEGER,INTENT(IN)::fileNum
				INTEGER::istat
c               create memory space for node pointer
				ALLOCATE(new,STAT=istat)
c               if successful, assign data to node
				if(istat .eq. 0)then
					NULLIFY(new%next,new%prev)
					new%person=filePerson
					new%num=fileNum
				endif
c               if the head pointer is associated with an address, then
c               insert after the tail address and point tail to the new
c               node.
				if(ASSOCIATED(head))then
					tail%next=>new
					new%prev=>tail
					tail=>new
c               otherwise, the new node is the first node and assign the
c               head and tail pointers to the new node.
				else
					head=>new
					tail=>new
				endif
				
			END SUBROUTINE addNode
			
c           subroutine to remove a node from doubly linked list
			SUBROUTINE removeNode(head,tail,p)
				
				TYPE(node),POINTER,INTENT(INOUT)::head, tail, p
				TYPE(node),POINTER :: temp
				ALLOCATE(temp)
				NULLIFY(temp)
				
c               if the head, tail, and p pointers are associated, then a
c               doubly linked list exists
				if(ASSOCIATED(head) .and. ASSOCIATED(tail) 
     +		    .and. ASSOCIATED(p)) then
c                  if the next pointer of the selected node p is
c                  associated with an address, then the node previous to
c                  p needs to point to the node after p.
c                  (assuming this node is the head node).
                   if(ASSOCIATED(p%next)) then
c                     if the previous pointer of the selected node p is
c                     associated with an address, the node p must be in
c                     the middle of the doubly linked list. this means
c                     the node previous and after node p must be linked
c                     before the deletion of node p.
                      if(ASSOCIATED(p%prev)) then
c                         write(*,*) "removed from middle"
                         temp => p
                         p%next%prev => p%prev
                         p%prev%next => p%next
                         p=>p%next
                         DEALLOCATE(temp)
c                     this handles the case if node p is not in the
c                     middle of the doubly linked list and is removed
c                     from the beginning of the doubly linked list.
                      else 
c                         write(*,*) "removed from beginning"
                         temp => head
                         head=>head%next
                         p=>p%next
c                        write(*,*) head%person
c                        write(*,*) p%person
                         DEALLOCATE(temp)
                         NULLIFY(p%prev)
                         NULLIFY(head%prev)
c                         write(*,*) head%person
c                         write(*,*) p%person
                      end if
c                  otherwise, the selected node p is the tail node and
c                  is removed from the end of the doubly linked list
                   else 
                      if(ASSOCIATED(p%prev)) then
c                         write(*,*) "removed from end"
                         temp => tail
                         tail=> tail%prev
                         p=>head
                         DEALLOCATE(temp)
                         NULLIFY(tail%next)
                         NULLIFY(p%prev)
c                     if all else fails, the selected node p has no
c                     association with the doubly linked list and cannot
c                     be removed.
                      else
                         write(*,*) "P is not associated to a ", 
     +                   "previous or a next. Check pointers"
                      end if
                   end if
                end if 
c                write(*,*) head%person
c                write(*,*) p%person
c put removal node routine here
		
			END SUBROUTINE removeNode
			
c*************************MORE Than one node**************************
c checks to see if more than one node is remaining
c true if more than one
c false if there is only one			

			logical FUNCTION MoreThanOneNode(head, tail)
			   TYPE(node),POINTER,INTENT(INOUT)::head,tail
c			   write(*,*) "In more than one node"
			   if(ASSOCIATED(head) .and. ASSOCIATED(tail)) then
			     if(ASSOCIATED(head%next) .and. ASSOCIATED(tail%prev)) then
			         MoreThanOneNode = .true.
			      else 
			         MoreThanOneNode = .false.
			      end if
			   else
                  write(*,*) "We should not get here. Error is with ",
     +			  " head or tail pointer."
			   end if
c			   write(*,*) "End of more than one node"
			   RETURN
			END FUNCTION MoreThanOneNode
			
c***********************Remove Data********************************		
c prints data from the eliminated node, remaining nodes in the list,
c and prints the next count to follow
			SUBROUTINE removeData(head,tail,p)
			    TYPE(node),POINTER,INTENT(INOUT)::head,tail,p
			    integer :: movement = 0, i = 1
			    logical :: more = .true., error = .false.
			    
10              format(A20, " has been eliminated. Their count is ", I3)			    
20              format(A20, " is the only person remaining. They're ",
     +          "happy they lived, but are now very lonley")
30              format(A20, " will start the next count")	
40              format(/)		    
			    p => head
			    
c			    write(*,*)head%person
c			    write(*,*) p%person
c               while more nodes exist and an error has not occured
c               write data to the screen and outfile.
			    do while(more .and. .not. error)
			       if(ASSOCIATED(head) .and. ASSOCIATED(tail) .and. 
     +             ASSOCIATED(p)) then
			           movement = p%num
			           write(*,40)
			           write(2,40)
			           write(*,10) p%person, p%num
			           write(2,10) p%person, p%num
			           
			           
			           call removeNode(head,tail,p)
			           
c			          write(*,*) head%person
c                      write(*,*) p%person
			           more = moreThanOneNode(head,tail)
c                      
c                      if more nodes exist, print relevant data to the
c                      screen and outfile.
			           if(more) then
			             write(*,*) "The following people remain:"
			             write(2,*) "The following people remain:" 
			             call printLL(head, tail)
c			             write(*,*) "Inside of more"
c			             write(*,*)head%person
c			             write(*,*) p%person
c                        handle positive movement
			             if( movement .gt. 0) then
c			               write(*,*) "Positive movement"
			                write(*,30) p%person
			                write(2,30) p%person
c                            move around the linked list determined by
c                            the movement from the selected node
			                 do i = 1, movement-1
c			                    write(*,*) "Inside do loop"
			                    if(ASSOCIATED(p%next)) then
c			                        write(*,*) "Inside p%next"
			                        p=>p%next
			                    else
			                       if(ASSOCIATED(p%prev)) then
c			                       write(*,*) "Inside p%prev"
			                          p=> head
			                       else
			                          error = .true.
                                      write (*,*) "Check subroutines", 
     +                                " and pointers. We shouldn't",
     +                                " be here"
			                       end if
			                    end if
			                 end do
c                          handle negative movement
			               else
c                            move around the linked list, but backwards
			                 if(movement .lt. 0) then
c			                    write(*,*) "Negative movement"
			                    if(ASSOCIATED(p%prev))then
			                       write(*,30) p%prev%person
			                       write(2,30) p%prev%person
			                    else
			                       if(ASSOCIATED(p%next)) then
			                          write(*,30) tail%person
			                          write(2,30) tail%person
			                       end if 
			                    end if
c			                    write(*,*) "Inside of negative movement"
			                    do i = 1, ABS(movement)
			                       if(ASSOCIATED(p%prev)) then
			                           p=>p%prev
			                       else
			                          if(ASSOCIATED(p%next)) then
			                             p=> tail
			                          else
			                             error = .true.
                                         write (*,*) "Check methods", 
     +                                   " and pointers. We shouldn't",
     +                                   " be here"
                                      end if
                                   end if
                                end do
                             else
                                write(*,*) "Somehow a node with a ",
     +                          "movement of 0 got added. Debug the ",
     +                          "program"
                             end if     
			              end if
			           end if
c			           write(*,*) "End of more" 
			       end if 
			    end do
c               if only one node exists, then print the sole survivor
                if(.not. moreThanOneNode(head,tail)) then
                   write(*,20) head%person
                   write(2,20) head%person
                else
                   write(*,*) "Nodes are not properly being removed.", 
     +             " Debug"
                end if
			END SUBROUTINE removeData
			
	  END MODULE LLFuncs
	  
	   MODULE FileOperations
	    USE NODES
	    USE LLFuncs
		IMPLICIT NONE
		
		CONTAINS
		
		
C*************************Gets Files***************************	

	
		SUBROUTINE OpenFiles(inFileExists, outFileExists, quit)
		integer :: inFileError = 0, outFileError = 0, ios = 0
		character(len = 20) :: inFileName, outFileName, choice
        logical, intent(out) :: inFileExists,outFileExists, quit
        logical ::  goodChoice = .false.
        inFileExists = .false.
        outFileExists = .true.
        quit = .false.
		
30      format(A30)
		
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
     C 	then
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
                      if (outFileName .eq. 'quit' .or. outFileName 
     C 	              .eq. 'QUIT') then
                      quit = .true.
                      write(*,*) 'You have decided to quit'
                      end if
                      goodChoice = .true.
                 end if

                 if(choice .eq."overwrite".or. choice .eq."OVERWRITE")
     C 	then
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
		   END SUBROUTINE OpenFiles
		   
c ************************************Read data****************************************		

   
		   SUBROUTINE readData(head, tail, p)
		      TYPE(node),POINTER,INTENT(INOUT)::head,tail,p
		      character (len=20) :: personName
		      integer :: movement = 0, NameError, MovementError,
     +  numberRead = 1
c              write(*,*) "Regular prints"
		      Do While ((NameError .ne. -1) .and. (MovementError .ne. -1) 
     +	.and. (numberRead .le. 25))
		         read(1,*, iostat = NameError) personName
		         read(1,*, iostat = MovementError) movement
c		         write(*,*) NameError
c		         write(*,*) MovementError
		         if(NameError .eq. 0 .and. MovementError .eq. 0) then
		            if(movement .ne. 0) then
c		               write(*,*) personName, ' ', movement
		               call addNode(head,tail,p,personName,movement)
		            else 
		               write(2,*) "People with at count of 0",
     +                 " cannot be added. A record was not added", 
     +                 " because of this."
		               write(*,*) "People with at count of 0",
     +                 " cannot be added. A record was not added", 
     +                 " because of this."
		            end if
		         else
c Add whatever we need to do to handle errors here
                 
                    if(NameError .ne. -1 .and. NameError 
     +               .ne. 5001 .and. NameError .ne. 0) then            
		               write(*,*) "There was an error reading the name.", 
     +	               "A record was not added because of this"
                       write(2,*) "There was an error reading the ", 
     +	               "name. A record was not added because of this"
		            end if
		            if(MovementError .ne. 5001 .and. MovementError 
     +              .ne. -1 .and. MovementError .ne. 0) then
                        write(*,*) "There was an error reading the ",
     +	                "count. A record was not added because of this"
                        write(2,*) "There was an error reading the ",
     +                  "count. A record was not added because of this"
		            end if 
		            
		            if(NameError .eq. 0) then
		               if(MovementError .eq. -1) then
                          write(*,*) "The end of file was reached ",
     +                    "early. The last record was not added ", 
     +                    "because of this"
                          write(2,*) "The end of file was reached ",
     +                    "early. The last record was not added ", 
     +                    "because of this"
                       end if
                    end if
		         end if 
		         numberRead = numberRead+1
		      end do
		      if(numberRead .gt. 25) then
		         write(*,*) "You entered more than 25 records.", 
     +		     "Only the first 25 will be used."
                 write(2,*) "You entered more than 25 records.", 
     +		     "Only the first 25 will be used."
              end if
		   END SUBROUTINE readData
	   END MODULE FileOperations
	  
	  
c~~~~~~~~~~~~~~~~~~MAIN PROGRAM~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	  
	  program HungerGames
c		USE LLFuncs
		USE Nodes
		USE FileOperations
		IMPLICIT NONE
		logical :: inFileExists, outFileExists, quit
		character (len= 20) :: test
		TYPE(node),POINTER ::head,tail,p
		ALLOCATE(head,tail,p)
		NULLIFY(head,tail,p)
		call OpenFiles(inFileExists, outFileExists, quit)
        if(inFileExists .and.(.not. outFileExists).and.(.not. quit))
     +  then
          write(*,*) "The input and output files have",
     +    " been properly opened"
		  call readData(head,tail,p)
		  call removeData(head,tail,p)
		  close(1)
		  close(2)
        end if
	    
	  end program HungerGames