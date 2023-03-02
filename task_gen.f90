!==============================================================================
!==============================================================================
      program gen_task
      
      implicit none
      
!+++++++++++++++++++ 用户设置参数 +++++++++++++++++++++++++
      
      ! 操作系统：windows=0, linux=1, linux(qsub)=2
      
      integer, parameter :: i_computer_system=0
      
      ! 每个任务的执行内容的条数
      
      integer, parameter :: in_task=2
      
      ! 排队方式：
      ! 0 - 整体均匀排队但进行中的任务优先
      ! 1 - 全部按顺序排队
      ! 2 - 全部均匀排队
      
      integer, parameter :: ictrl_sequence=1
      
      ! 每个任务的执行具体内容，注意：c_task 须赋值 in_task 个元素，且长度必须相同
      
      character*1024, parameter, dimension (in_task) :: &
      c_task=(/ &
      'ynev_v1063                    ', &
      '                              ' /)
      
      ! 提交任务时间间隔/s
      
      integer, parameter :: iwaits=5
      
      ! 最大线程数
      
      integer :: i_thread_num
      
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      integer, parameter :: kmax_dir=999999
      integer :: iostate, km_p1_dir, km_p2_dir, km_p3_dir
      integer :: itask_per_thread, ithrdmod
      character :: cdirtemp*255
      character, dimension (kmax_dir) :: cdir_data*255, cdir_data_chk*255
      
      integer :: i, j, k, ithread, lenc, it, itaskmax, istatus
      character :: char6*6, char3*3, cdirall*255
      
!  从 DirList_working 读取目录名
      
      open (unit=11,file='DirList_working.txt',status='old',iostat=iostate)
      if (iostate==0) then
        write (*,"('打开 DirList_working.txt ')")
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
      
!  从 DirList_unstarted 读取目录名
      
      open (unit=11,file='DirList_unstarted.txt',status='old',iostat=iostate)
      if (iostate==0) then
        write (*,"('打开 DirList_unstarted.txt ')")
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
      
!  从 DirList_unfinished 读取目录名
      
      open (unit=11,file='DirList_unfinished.txt',status='old',iostat=iostate)
      if (iostate==0) then
        write (*,"('打开 DirList_unfinished.txt ')")
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
      
!  检验 DirList_working + DirList_unstarted =?= DirList_unfinished
      
      if (km_p2_dir/=km_p3_dir) then
        write (*,*) '  DirList_working.txt   任务数 : ',km_p1_dir
        write (*,*) ' DirList_unstarted.txt  任务数 : ',km_p2_dir-km_p1_dir
        write (*,*) ' DirList_unfinished.txt 任务数 : ',km_p3_dir
        write (*,*) ' 前两个的和与第三个不一致 '
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
    !        write (*,*) '  DirList_working.txt 任务在 DirList_unfinished 中不存在 ： '
    !      else
    !        write (*,*) '  DirList_unstarted.txt 任务在 DirList_unfinished 中不存在 ： '
    !      endif
    !      write (*,*) trim(cdir_data(i))
    !      stop
    !    endif
    !  enddo ! i
    !  endif
      
!  从 DirList 读取目录名
      
      if (km_p2_dir==0) then
        open (unit=11,file='DirList.txt',status='old',iostat=iostate)
        if (iostate==0) then
          write (*,"('打开 DirList.txt ')")
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
      
!  最大线程数
      
      write (*,"('请输入最大线程数：')",advance='no')
      read (*,*) i_thread_num
      if (i_thread_num<1) stop ' 最大线程数 < 1 !'
      
!  优先按顺序排队任务数 km_p1_dir
      
      select case (ictrl_sequence)
      case (0)
        ! DirList_working 优先，DirList_unstarted 均匀排队
      case (1)
        km_p1_dir=km_p2_dir ! 全部按顺序排队
      case (2)
        km_p1_dir=0 ! 全部均匀排队
      case default
        write(*,*) ' 参数 ictrl_sequence 错误 '
        stop
      end select
      
!  均匀排队任务：计算每个线程的任务数 itask_per_thread 和余数 ithrdmod
      
      itask_per_thread=(km_p2_dir-km_p1_dir)/i_thread_num ! 每个线程的任务数
      ithrdmod=km_p2_dir-km_p1_dir-itask_per_thread*i_thread_num ! 余数任务数
      
!  写入批处理文件
      
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
      
      ithread=i_thread_num+1 ! 优先排队默认起始线程，为了方便起见，从大序号线程开始
      do k=1,km_p2_dir
      
      ! 该任务排入线程编号ithread
      ! 这里描述下面几行代码的含义，
      ! 从上至下为任务目录 cdir_data 的顺序：
      ! 
      ! ----------------------------------------------------------------------
      ! 
      ! 优先任务共 km_p1_dir 个，
      ! 
      ! 第 1 个任务排入第 i_thread_num 个线程（在线程里排第 1 位）
      ! 第 2 个任务排入第 i_thread_num-1 个线程（在线程里排第 1 位）
      ! ...
      ! 第 i_thread_num 个任务排入第 1 个线程（在线程里排第 1 位）
      ! 第 i_thread_num+1 个任务排入第 i_thread_num 个线程（在线程里排第 2 位）
      ! 第 i_thread_num+2 个任务排入第 i_thread_num-1 个线程（在线程里排第 2 位）
      ! ...
      ! 
      ! ----------------------------------------------------------------------
      ! 
      ! 均匀排队含余数任务共 ithrdmod*(itask_per_thread+1) 个，
      ! 
      ! 这部分里的第 i*(itask_per_thread+1)+1 至
      ! (i+1)*(itask_per_thread+1) 个任务排入第 i+1 个线程，
      ! 即，除优先任务外，每个线程增加 itask_per_thread+1 个任务，
      ! 这部分占用 ithrdmod 个线程
      ! 
      ! ----------------------------------------------------------------------
      ! 
      ! 均匀排队不含余数任务共余数 (i_thread_num-ithrdmod)*itask_per_thread 个，
      ! 
      ! 这部分里的第 i*itask_per_thread+1 至
      ! (i+1)*itask_per_thread 个任务排入第 ithrdmod+i+1 个线程，
      ! 即，除优先任务外，每个线程增加 itask_per_thread 个任务，
      ! 这部分占用 i_thread_num-ithrdmod 个线程
      ! 
      ! ----------------------------------------------------------------------
      
      if (k<=km_p1_dir) then
        
        ithread=ithread-1
        
      else
        
        ! 整体均匀排队
        if (k-km_p1_dir<=(itask_per_thread+1)*ithrdmod) then
          ! 含余数任务的部分
          ithread=(k-km_p1_dir-1)/(itask_per_thread+1)+1
        else
          ! 不含余数任务的部分
          ithread=(k-km_p1_dir-(itask_per_thread+1)*ithrdmod-1)/itask_per_thread+1+ithrdmod
        endif
        
      endif
      
      ! 先留空一行
      write (200+ithread,"('')") 
      
      ! 批处理命令：进入该文件夹
      lenc=len(trim(cdir_data(k)))
      if (lenc==0) then
      write (*,*) k,cdir_data(k)
      write (*,*) '文件夹名称为空？'
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
      
      ! 批处理命令：写入执行内容
      do i=1,in_task
      lenc=len(trim(c_task(i)))
      if (lenc>0) then
      do j=1,lenc
      write (200+ithread,"(a1)",advance='no') c_task(i)(j:j)
      enddo ! i
      endif
      write (200+ithread,"()")
      enddo ! i
      
      ! 退出该文件夹
      write (200+ithread,"('cd ..')")
      write (200+ithread,"()")
      
      ! 线程数达到最大，重置，只对按顺序计算的部分起作用
      if (ithread==1) ithread=i_thread_num+1
      
      enddo ! km
      
      do i=1,i_thread_num
      write (200+i,"()")
      write (200+i,"('exit')")
      close (200+i)
      enddo ! i
      
!  生成一个提交批量任务的脚本
      
      if (i_computer_system==0) then
        
        !! windows系统：
        open (unit=21,file='submit_task.bat',status='unknown')
        do i=1,i_thread_num
        write (21,"('start /min /low thread_',i6.6,'.bat')") i
        if (i<i_thread_num) &
        write (21,"('ping 127.0.0.1 -n ',i1,' > nul')") iwaits-1
        write (21,"('')")
        enddo ! i
        close (21)
        
      else
        
      !  !! linux系统：
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