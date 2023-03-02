      
      implicit real (8) (a-h,o-z)
      
      ! 操作系统：windows=0, linux=1
      integer, parameter :: i_computer_system=0
      
      integer, parameter :: iDirMax=999999
      
      character*36, dimension (iDirMax) :: DirName
      
      !  读取文件夹名称
      
      open (unit=20,file='DirList.txt',status='old')
      do i=1,iDirMax
      read (20,"(a36)",end=10) DirName(i)
      enddo ! i
      stop ''
10    continue
      iDirtot=i-1
      close (20)
      
      cycle_dir : do i=1,iDirtot
        
        if (i_computer_system==0) then
          call system ('del .\'//DirName(i)//'\*.txt')
        endif
        
        if (i_computer_system==1) then
          call system ('rm -rf ./'//DirName(i)//'/*.txt')
        endif
        
      enddo  cycle_dir! i
      
      end
      
      