      
      implicit none
      
      integer, parameter :: ikind=selected_real_kind(p=12)
      
      ! 操作系统：windows=0, linux=1
      integer, parameter :: i_computer_system=0
      
      integer, parameter :: iDirMax=999999
      integer, parameter :: imodelmax=2000
      character :: ctemp6*6
      character, dimension (iDirMax) :: DirName*32
      
      integer :: iunit
      integer :: i, k, iDirtot, ipercent, ipercentp, iaoec, iaoemodel
      real (ikind) :: percent
      
      !  读取文件夹名称
      
      open (newunit=iunit,file='DirList.txt',status='old')
      do i=1,iDirMax
      read (iunit,"(a32)",end=10) DirName(i)
      enddo ! i
      stop 'error 1'
10    continue
      iDirtot=i-1
      close (iunit)
      
      if (i_computer_system==0) then ! windows
      call system ('del DirList_unfinished.txt')
      call system ('del DirList_failed.txt')
      call system ('del DirList_toomanymodels.txt')
      call system ('del DirList_working.txt')
      call system ('del DirList_error.txt')
      call system ('del DirList_unstarted.txt')
      call system ('del DirList_finished_osc.txt')
      call system ('del DirList_finished_noosc.txt')
      endif
      
      if (i_computer_system==1) then ! linux
      call system ('rm -rf DirList_unfinished.txt')
      call system ('rm -rf DirList_failed.txt')
      call system ('rm -rf DirList_toomanymodels.txt')
      call system ('rm -rf DirList_working.txt')
      call system ('rm -rf DirList_error.txt')
      call system ('rm -rf DirList_unstarted.txt')
      call system ('rm -rf DirList_finished_osc.txt')
      call system ('rm -rf DirList_finished_noosc.txt')
      endif
      
      open (unit=21,file='DirList_unfinished.txt',status='unknown')
      open (unit=22,file='DirList_failed.txt',status='unknown')
      open (unit=23,file='DirList_toomanymodels.txt',status='unknown')
      open (unit=24,file='DirList_working.txt',status='unknown')
      open (unit=25,file='DirList_error.txt',status='unknown')
      open (unit=26,file='DirList_unstarted.txt',status='unknown')
      open (unit=31,file='DirList_finished_osc.txt',status='unknown')
      open (unit=32,file='DirList_finished_noosc.txt',status='unknown')
      
!  检查每个结果，保存最佳参数
      
      ipercentp=-100
      
      cycle_i : do i=1,iDirtot
        
      !  显示完成度
      
      percent=dble(100*i)/dble(iDirtot)
      ipercent=percent
      
    !  write (*,"(60a,' complete : ',f6.3,'%')",advance='no') (char(8),k=1,60),percent
      
    !  if (ipercent-ipercentp>0) then
    !    write (*,"(60a,' complete : ',i3,'%')",advance='no') (char(8),k=1,60),ipercent
    !    ipercentp=ipercent
    !  endif
      
      if (ipercent-ipercentp>0) then
        write (*,"(60a,' complete : ',a20)",advance='no') (char(8),k=1,60),cfun(ipercent)
        ipercentp=ipercent
      endif
      
      iaoec=-1
      iaoemodel=-1
      open (unit=11,file='./'//DirName(i)//'/LastModel.txt',status='old',iostat=k)
      if (k==0) then ! LastModel.txt文件存在，读取计算状态
        read (11,*) iaoec,iaoemodel
        close (11)
      else ! LastModel.txt文件不存在，未开始计算
        close (11)
        write (21,"(a32)") DirName(i)
        write (26,"(a32)") DirName(i)
        cycle cycle_i
      endif
      
      if (iaoec==99) then ! 模型计算失败
        write (22,"(a32)") DirName(i)
        cycle cycle_i
      endif
      
      if (iaoec==1) then ! 已开始但未完成计算
        write(ctemp6,"(i6.6)") iaoemodel
        if (iaoemodel>=imodelmax) then ! 模型数太大，认为是有问题的模型
          write (23,"(a32)") DirName(i)
          cycle cycle_i
        endif
        k=-1
        open (unit=11,file='./'//DirName(i)//'/mod'//ctemp6//'.txt',status='old',iostat=k)
        close (11)
        if (k==0) then ! 最新保存的mod文件存在，正常计算状态
          write (21,"(a32)") DirName(i)
          write (24,"(a32)") DirName(i)
        else ! 最新保存的mod文件不存在，非正常状态
          write (25,"(a32)") DirName(i)
        endif
        cycle cycle_i
      endif
      
      if (iaoec==2.and.iaoemodel>=imodelmax) then ! 模型数太大，认为是有问题的模型
        write (23,"(a32)") DirName(i)
        cycle cycle_i
      endif
      
      ! 已完成计算的模型
      k=-1
      open (unit=11,file='./'//DirName(i)//'/FreAll.txt',status='old',iostat=k)
      close (11)
      if (k==0) then ! 有振动数据
        write (31,"(a32)") DirName(i)
      else ! 没有振动数据
        write (32,"(a32)") DirName(i)
      endif
      
      enddo cycle_i ! i
      
      close (21)
      close (22)
      close (23)
      close (24)
      close (25)
      close (26)
      close (31)
      close (32)
      
      contains
        
        function cfun(ipercent)
        integer :: ipercent
        character :: cfun*20
        integer :: i2,i1,i
        character :: ctemp*2
        i2=ipercent/10
        i1=ipercent-i2*10
        do i=1,10
          if (i<=i2) then
            cfun(2*i-1:2*i)='●'
          else
            cfun(2*i-1:2*i)='〇'
          endif
        enddo ! i
        if (i2<10) then
          select case (i1)
          case (0)
            ctemp='〇'
          case (1)
            ctemp='①'
          case (2)
            ctemp='②'
          case (3)
            ctemp='③'
          case (4)
            ctemp='④'
          case (5)
            ctemp='⑤'
          case (6)
            ctemp='⑥'
          case (7)
            ctemp='⑦'
          case (8)
            ctemp='⑧'
          case (9)
            ctemp='⑨'
          case default
            stop 'error in cfun ! '
          end select
          cfun(2*i2+1:2*i2+2)=ctemp
        endif
        end function cfun
        
      end
      
!==============================================================================
!==============================================================================
      