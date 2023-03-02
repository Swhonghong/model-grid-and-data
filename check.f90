      
      implicit none
      
      integer, parameter :: ikind=selected_real_kind(p=12)
      
      ! ����ϵͳ��windows=0, linux=1
      integer, parameter :: i_computer_system=0
      
      integer, parameter :: iDirMax=999999
      integer, parameter :: imodelmax=2000
      character :: ctemp6*6
      character, dimension (iDirMax) :: DirName*32
      
      integer :: iunit
      integer :: i, k, iDirtot, ipercent, ipercentp, iaoec, iaoemodel
      real (ikind) :: percent
      
      !  ��ȡ�ļ�������
      
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
      
!  ���ÿ�������������Ѳ���
      
      ipercentp=-100
      
      cycle_i : do i=1,iDirtot
        
      !  ��ʾ��ɶ�
      
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
      if (k==0) then ! LastModel.txt�ļ����ڣ���ȡ����״̬
        read (11,*) iaoec,iaoemodel
        close (11)
      else ! LastModel.txt�ļ������ڣ�δ��ʼ����
        close (11)
        write (21,"(a32)") DirName(i)
        write (26,"(a32)") DirName(i)
        cycle cycle_i
      endif
      
      if (iaoec==99) then ! ģ�ͼ���ʧ��
        write (22,"(a32)") DirName(i)
        cycle cycle_i
      endif
      
      if (iaoec==1) then ! �ѿ�ʼ��δ��ɼ���
        write(ctemp6,"(i6.6)") iaoemodel
        if (iaoemodel>=imodelmax) then ! ģ����̫����Ϊ���������ģ��
          write (23,"(a32)") DirName(i)
          cycle cycle_i
        endif
        k=-1
        open (unit=11,file='./'//DirName(i)//'/mod'//ctemp6//'.txt',status='old',iostat=k)
        close (11)
        if (k==0) then ! ���±����mod�ļ����ڣ���������״̬
          write (21,"(a32)") DirName(i)
          write (24,"(a32)") DirName(i)
        else ! ���±����mod�ļ������ڣ�������״̬
          write (25,"(a32)") DirName(i)
        endif
        cycle cycle_i
      endif
      
      if (iaoec==2.and.iaoemodel>=imodelmax) then ! ģ����̫����Ϊ���������ģ��
        write (23,"(a32)") DirName(i)
        cycle cycle_i
      endif
      
      ! ����ɼ����ģ��
      k=-1
      open (unit=11,file='./'//DirName(i)//'/FreAll.txt',status='old',iostat=k)
      close (11)
      if (k==0) then ! ��������
        write (31,"(a32)") DirName(i)
      else ! û��������
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
            cfun(2*i-1:2*i)='��'
          else
            cfun(2*i-1:2*i)='��'
          endif
        enddo ! i
        if (i2<10) then
          select case (i1)
          case (0)
            ctemp='��'
          case (1)
            ctemp='��'
          case (2)
            ctemp='��'
          case (3)
            ctemp='��'
          case (4)
            ctemp='��'
          case (5)
            ctemp='��'
          case (6)
            ctemp='��'
          case (7)
            ctemp='��'
          case (8)
            ctemp='��'
          case (9)
            ctemp='��'
          case default
            stop 'error in cfun ! '
          end select
          cfun(2*i2+1:2*i2+2)=ctemp
        endif
        end function cfun
        
      end
      
!==============================================================================
!==============================================================================
      