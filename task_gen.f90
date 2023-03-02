!==============================================================================
!==============================================================================
      program gen_task
      
      implicit none
      
!+++++++++++++++++++ �û����ò��� +++++++++++++++++++++++++
      
      ! ����ϵͳ��windows=0, linux=1, linux(qsub)=2
      
      integer, parameter :: i_computer_system=0
      
      ! ÿ�������ִ�����ݵ�����
      
      integer, parameter :: in_task=2
      
      ! �Ŷӷ�ʽ��
      ! 0 - ��������Ŷӵ������е���������
      ! 1 - ȫ����˳���Ŷ�
      ! 2 - ȫ�������Ŷ�
      
      integer, parameter :: ictrl_sequence=1
      
      ! ÿ�������ִ�о������ݣ�ע�⣺c_task �븳ֵ in_task ��Ԫ�أ��ҳ��ȱ�����ͬ
      
      character*1024, parameter, dimension (in_task) :: &
      c_task=(/ &
      'ynev_v1063                    ', &
      '                              ' /)
      
      ! �ύ����ʱ����/s
      
      integer, parameter :: iwaits=5
      
      ! ����߳���
      
      integer :: i_thread_num
      
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      integer, parameter :: kmax_dir=999999
      integer :: iostate, km_p1_dir, km_p2_dir, km_p3_dir
      integer :: itask_per_thread, ithrdmod
      character :: cdirtemp*255
      character, dimension (kmax_dir) :: cdir_data*255, cdir_data_chk*255
      
      integer :: i, j, k, ithread, lenc, it, itaskmax, istatus
      character :: char6*6, char3*3, cdirall*255
      
!  �� DirList_working ��ȡĿ¼��
      
      open (unit=11,file='DirList_working.txt',status='old',iostat=iostate)
      if (iostate==0) then
        write (*,"('�� DirList_working.txt ')")
        do k=1,kmax_dir
          read (11,*,end=1) cdir_data(k)
        enddo ! k
        write (*,*) ' p1 : k > kmax_dir !'
        stop
1       continue
        km_p1_dir=k-1
      else
        km_p1_dir=0
      endif
      close (11)
      
!  �� DirList_unstarted ��ȡĿ¼��
      
      open (unit=11,file='DirList_unstarted.txt',status='old',iostat=iostate)
      if (iostate==0) then
        write (*,"('�� DirList_unstarted.txt ')")
        do k=km_p1_dir+1,kmax_dir
          read (11,*,end=2) cdir_data(k)
        enddo ! k
        write (*,*) ' p2 : k > kmax_dir !'
        stop
2       continue
        km_p2_dir=k-1
      else
        km_p2_dir=0
      endif
      close (11)
      
!  �� DirList_unfinished ��ȡĿ¼��
      
      open (unit=11,file='DirList_unfinished.txt',status='old',iostat=iostate)
      if (iostate==0) then
        write (*,"('�� DirList_unfinished.txt ')")
        do k=1,kmax_dir
          read (11,*,end=3) cdir_data_chk(k)
        enddo ! k
        write (*,*) ' p3 : k > kmax_dir !'
        stop
3       continue
        km_p3_dir=k-1
      else
        km_p3_dir=0
      endif
      close (11)
      
!  ���� DirList_working + DirList_unstarted =?= DirList_unfinished
      
      if (km_p2_dir/=km_p3_dir) then
        write (*,*) '  DirList_working.txt   ������ : ',km_p1_dir
        write (*,*) ' DirList_unstarted.txt  ������ : ',km_p2_dir-km_p1_dir
        write (*,*) ' DirList_unfinished.txt ������ : ',km_p3_dir
        write (*,*) ' ǰ�����ĺ����������һ�� '
        stop
      endif
      
    !  if (km_p2_dir>0) then
    !  do i=1,km_p2_dir
    !    it=0
    !    cycle_j2 : do j=1,km_p3_dir
    !      if (trim(cdir_data(i))==trim(cdir_data_chk(j))) then
    !        it=1
    !        exit cycle_j2
    !      endif
    !    enddo cycle_j2 ! j
    !    if (it==0) then
    !      if (i<=km_p1_dir) then
    !        write (*,*) '  DirList_working.txt ������ DirList_unfinished �в����� �� '
    !      else
    !        write (*,*) '  DirList_unstarted.txt ������ DirList_unfinished �в����� �� '
    !      endif
    !      write (*,*) trim(cdir_data(i))
    !      stop
    !    endif
    !  enddo ! i
    !  endif
      
!  �� DirList ��ȡĿ¼��
      
      if (km_p2_dir==0) then
        open (unit=11,file='DirList.txt',status='old',iostat=iostate)
        if (iostate==0) then
          write (*,"('�� DirList.txt ')")
          do k=1,kmax_dir
            read (11,*,end=4) cdir_data(k)
          enddo ! k
          write (*,*) ' p0 : k > kmax_dir !'
          stop
4         continue
          km_p2_dir=k-1
        endif
        close (11)
      endif
      
!  ����߳���
      
      write (*,"('����������߳�����')",advance='no')
      read (*,*) i_thread_num
      if (i_thread_num<1) stop ' ����߳��� < 1 !'
      
!  ���Ȱ�˳���Ŷ������� km_p1_dir
      
      select case (ictrl_sequence)
      case (0)
        ! DirList_working ���ȣ�DirList_unstarted �����Ŷ�
      case (1)
        km_p1_dir=km_p2_dir ! ȫ����˳���Ŷ�
      case (2)
        km_p1_dir=0 ! ȫ�������Ŷ�
      case default
        write(*,*) ' ���� ictrl_sequence ���� '
        stop
      end select
      
!  �����Ŷ����񣺼���ÿ���̵߳������� itask_per_thread ������ ithrdmod
      
      itask_per_thread=(km_p2_dir-km_p1_dir)/i_thread_num ! ÿ���̵߳�������
      ithrdmod=km_p2_dir-km_p1_dir-itask_per_thread*i_thread_num ! ����������
      
!  д���������ļ�
      
      istatus=getcwd(cdirall)
      lenc=len(trim(cdirall))
      do i=1,i_thread_num
        write (char6,"(i6.6)") i
        open (200+i,file='thread_'//char6//'.bat',status='unknown')
        if (i_computer_system==2) then ! linux(qsub)
          write (200+i,"('#PBS -q inspur')")
          write (200+i,"('#PBS -N YNEV')")
          write (200+i,"('#PBS -V')")
          write (200+i,"('#PBS -S /bin/bash')")
        endif
        if (lenc>0) then
          write (200+i,"('cd ')",advance='no')
          do j=1,lenc
            write (200+i,"(a1)",advance='no') cdirall(j:j)
          enddo ! i
        endif
        write (200+i,"()")
      enddo ! i
      
      ithread=i_thread_num+1 ! �����Ŷ�Ĭ����ʼ�̣߳�Ϊ�˷���������Ӵ�����߳̿�ʼ
      do k=1,km_p2_dir
      
      ! �����������̱߳��ithread
      ! �����������漸�д���ĺ��壬
      ! ��������Ϊ����Ŀ¼ cdir_data ��˳��
      ! 
      ! ----------------------------------------------------------------------
      ! 
      ! �������� km_p1_dir ����
      ! 
      ! �� 1 ����������� i_thread_num ���̣߳����߳����ŵ� 1 λ��
      ! �� 2 ����������� i_thread_num-1 ���̣߳����߳����ŵ� 1 λ��
      ! ...
      ! �� i_thread_num ����������� 1 ���̣߳����߳����ŵ� 1 λ��
      ! �� i_thread_num+1 ����������� i_thread_num ���̣߳����߳����ŵ� 2 λ��
      ! �� i_thread_num+2 ����������� i_thread_num-1 ���̣߳����߳����ŵ� 2 λ��
      ! ...
      ! 
      ! ----------------------------------------------------------------------
      ! 
      ! �����ŶӺ��������� ithrdmod*(itask_per_thread+1) ����
      ! 
      ! �ⲿ����ĵ� i*(itask_per_thread+1)+1 ��
      ! (i+1)*(itask_per_thread+1) ����������� i+1 ���̣߳�
      ! ���������������⣬ÿ���߳����� itask_per_thread+1 ������
      ! �ⲿ��ռ�� ithrdmod ���߳�
      ! 
      ! ----------------------------------------------------------------------
      ! 
      ! �����ŶӲ��������������� (i_thread_num-ithrdmod)*itask_per_thread ����
      ! 
      ! �ⲿ����ĵ� i*itask_per_thread+1 ��
      ! (i+1)*itask_per_thread ����������� ithrdmod+i+1 ���̣߳�
      ! ���������������⣬ÿ���߳����� itask_per_thread ������
      ! �ⲿ��ռ�� i_thread_num-ithrdmod ���߳�
      ! 
      ! ----------------------------------------------------------------------
      
      if (k<=km_p1_dir) then
        
        ithread=ithread-1
        
      else
        
        ! ��������Ŷ�
        if (k-km_p1_dir<=(itask_per_thread+1)*ithrdmod) then
          ! ����������Ĳ���
          ithread=(k-km_p1_dir-1)/(itask_per_thread+1)+1
        else
          ! ������������Ĳ���
          ithread=(k-km_p1_dir-(itask_per_thread+1)*ithrdmod-1)/itask_per_thread+1+ithrdmod
        endif
        
      endif
      
      ! ������һ��
      write (200+ithread,"('')") 
      
      ! ���������������ļ���
      lenc=len(trim(cdir_data(k)))
      if (lenc==0) then
      write (*,*) k,cdir_data(k)
      write (*,*) '�ļ�������Ϊ�գ�'
      stop
      endif
    !  if (i_computer_system==0) then ! windows
    !    write (200+ithread,"('cd .\')",advance='no')
    !  else ! linux / linux(qsub)
    !    write (200+ithread,"('cd ./')",advance='no')
    !  endif
        write (200+ithread,"('cd ')",advance='no')
      do i=1,lenc
        write (200+ithread,"(a1)",advance='no') cdir_data(k)(i:i)
      enddo ! i
      write (200+ithread,"()")
      
      ! ���������д��ִ������
      do i=1,in_task
      lenc=len(trim(c_task(i)))
      if (lenc>0) then
      do j=1,lenc
      write (200+ithread,"(a1)",advance='no') c_task(i)(j:j)
      enddo ! i
      endif
      write (200+ithread,"()")
      enddo ! i
      
      ! �˳����ļ���
      write (200+ithread,"('cd ..')")
      write (200+ithread,"()")
      
      ! �߳����ﵽ������ã�ֻ�԰�˳�����Ĳ���������
      if (ithread==1) ithread=i_thread_num+1
      
      enddo ! km
      
      do i=1,i_thread_num
      write (200+i,"()")
      write (200+i,"('exit')")
      close (200+i)
      enddo ! i
      
!  ����һ���ύ��������Ľű�
      
      if (i_computer_system==0) then
        
        !! windowsϵͳ��
        open (unit=21,file='submit_task.bat',status='unknown')
        do i=1,i_thread_num
        write (21,"('start /min /low thread_',i6.6,'.bat')") i
        if (i<i_thread_num) &
        write (21,"('ping 127.0.0.1 -n ',i1,' > nul')") iwaits-1
        write (21,"('')")
        enddo ! i
        close (21)
        
      else
        
      !  !! linuxϵͳ��
      !  open (unit=21,file='submit_task_'//char3//'.bat',status='unknown' )
      !  do i=1,i_thread_num
      !    write (char6,"(i6.6)") i
      !    call system('chmod +x thread_'//char6//'.bat')
      !    if (i_computer_system==1) then ! linux
      !      write (21,"(a21)") './thread_'//char6//'.bat &'
      !    else ! linux(qsub)
      !      write (21,"(a26)") 'qsub ./thread_'//char6//'.bat &'
      !    endif
      !  enddo ! i
      !  close (21)
      !  call system('chmod +x submit_task_'//char3//'.bat')
      
      endif
      
      end program gen_task
!==============================================================================