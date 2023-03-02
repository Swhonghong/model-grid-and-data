!========================================================================
!========================================================================
      
      module AM_data_basic
      
      implicit none
      
      integer, parameter :: ikind=selected_real_kind(p=12)
      
! ------------------------ �������� ------------------------
      
      ! ��С���l
      integer, parameter :: lmin=0, lmax=2
      
      ! ÿ��l���������ģʽ��
      integer, parameter :: ilthsmax=100
      
      ! ���ǲ�������
      integer, parameter :: istarvar=15
      
! ------------------------ ������������ ------------------------
      
      ! �������ͣ�����ģʽ
      type :: type_mod
        real (ikind), dimension (0:istarvar) :: star_var ! ���ǲ���
        integer, dimension (lmin:lmax) :: iFthr ! ����l���������Ƶ����
        real (ikind), dimension (ilthsmax,lmin:lmax) :: Fthr, Finer ! ����Ƶ�ʼ�����
        integer, dimension (ilthsmax,lmin:lmax) :: nm ! ��ģʽ���񶯽ڵ���
        contains
        procedure :: initial => mod_initial
        procedure :: write_data => mod_write_data
      end type type_mod
      
! ------------------------ �������� ------------------------
      
      ! ����ģ���ļ��б�������
      integer, parameter :: i_star_para=4
      
      ! ����ģ���ļ���ÿ���������ֳ���
      integer, parameter :: len_star_para_name=3
      
      ! ����ģ���ļ���ÿ���������ֳ���
      integer, parameter :: len_star_para_value=5
      
      ! ����ģ���ļ���ÿ�������ܳ���
      integer, parameter :: len_star_para=len_star_para_name+len_star_para_value
      
      ! ����ģ���ļ����ַ�����
      integer, parameter :: len_star_dir_max=i_star_para*len_star_para
      
      ! �ݻ�������������ģ����
      integer, parameter :: imodel_max=1000
      
! ------------------------ ������������ ------------------------
      
      ! �������ͣ�һ�����ǵ�����
      type :: type_star
        character :: cdir * 255 ! ·��
        integer :: imodel_tot ! ģ������
        integer, dimension (i_star_para) :: stellar_iparameters ! ���ͺ��ǲ���
        type (type_mod), dimension (imodel_max) :: Fre_smod ! ģ������
        contains
        procedure :: initial => star_initial
        procedure :: read_data => star_read_data
        procedure :: write_data => star_write_data
        procedure :: revise_nm => star_revise_nm
      end type type_star
      
! ------------------------ ����Ȩ�� ------------------------
      
      private
      
      ! ����
      public :: lmin, lmax, ilthsmax
      
      ! ��������
      public :: type_mod
      
      ! ����
      public :: i_star_para, len_star_para_name, len_star_para_value
      public :: len_star_para, len_star_dir_max
      
      ! ��������
      public :: type_star
      
! ----------------------------------------------------------
      
      contains
      
!========================================================================






!========================================================================
      
!   ģ�����ݳ�ʼ��
      
!========================================================================
      
      subroutine mod_initial (object)
      
      implicit none
      class(type_mod), intent(inout) :: object
      
! ��ʼ������������
      
      object%star_var(:)=-1
      object%iFthr(:)=-1
      object%Fthr(:,:)=-1
      object%Finer(:,:)=-1
      object%nm(:,:)=-1
      
      end subroutine mod_initial
      
!========================================================================
      
!   д����
      
!========================================================================
      
      subroutine mod_write_data (object,iunit)
      
      implicit none
      class(type_mod), intent(in) :: object
      integer, intent(in) :: iunit
      
      integer :: i, l
      
      write (iunit,"(4e16.8)") object%star_var( 0: 3)
      write (iunit,"(4e16.8)") object%star_var( 4: 7)
      write (iunit,"(4e16.8)") object%star_var( 8:11)
      write (iunit,"(4e16.8)") object%star_var(12:15)
      do l=lmin,lmax
        write (iunit,"(2i4)") object%nm(1,l),object%iFthr(l)
        do i=1,object%iFthr(l)
          write (iunit,"(2e16.8)") object%Fthr(i,l),object%Finer(i,l)
        enddo ! i
      enddo ! l
      
      end subroutine mod_write_data
      
!========================================================================






!========================================================================
      
!   �������ݣ���ʼ��
      
!========================================================================
      
      subroutine star_initial (object)
      
      implicit none
      
      class(type_star), intent(inout) :: object
      
      integer :: i
      
! ��ʼ������������
      
      object%cdir='NULL'
      object%imodel_tot=-1
      object%stellar_iparameters(:)=-1
      do i=1,imodel_max
        call object%Fre_smod(i)%initial
      enddo ! i
      
      end subroutine star_initial
      
!========================================================================
      
!   �������ݣ���ȡ����
      
!========================================================================
      
      subroutine star_read_data ( object, cstardir, ierr )
      
      implicit none
      
      class(type_star), intent(inout) :: object
      character, intent(in) :: cstardir*32
      integer, intent(out) :: ierr
      
      integer, parameter :: ievol_mode_max=9999
      real(ikind), parameter :: pi=4*atan(1e0_ikind)
      
      character :: context*255
      integer :: itemp, l, i, length, j, iostate
      integer :: ireadunit, nh, nptem, ngtem, nmtem
      integer :: imodel, imodel0, imodeltemp
      integer :: ievol_mode_tot
      real(ikind) :: ftemp, ptemp, errtemp, rInertial, rEpmod, rEgmod
      
      integer, dimension(imodel_max) :: model_number
      real(ikind), dimension(26,ievol_mode_max) :: evol_data
      
! ��ʼ������������
      
      ierr=0
      call star_initial ( object )
      
! ���ļ�������ȡ���ǲ���
      
      write (*,*) '��ȡ���ݣ�',cstardir
      
      object%cdir=trim(adjustl(cstardir))
      
      do i=1,i_star_para
        read (cstardir(i*len_star_para-len_star_para_value+1:i*len_star_para),*) &
        object%stellar_iparameters(i)
      enddo ! i
      
!  ��ȡƵ���ļ�
      
      open (newunit=ireadunit,file=trim(adjustl(cstardir))//'/FreAll.txt',status='old',iostat=itemp)
      
      if (itemp/=0) then
        ierr=1
        close (ireadunit)
        return
      endif
      
      do imodel=1,imodel_max
      do l=lmin,lmax
        
        read(ireadunit,"(25x,i6.6)",iostat=iostate,end=99) itemp
        if (iostate/=0) then
          ierr=2
          close (ireadunit)
          return
        endif
        
      !  write (*,*) itemp,l
        
        if (l==lmin) then
          imodel0=itemp
          model_number(imodel)=imodel0
        else
          if (itemp/=imodel0) then
            close (ireadunit)
            write (*,*) 'Error 1 in read_model !'
            stop
          endif
        endif
        
        ! G * M / R**3
        read(ireadunit,"(25x,e18.8)",iostat=iostate,end=99) errtemp
        if (iostate/=0) then
          ierr=3
          close (ireadunit)
          return
        endif
        
        ! 4 * pi * G * rho_c / 3
        read(ireadunit,"(25x,e18.8)",iostat=iostate,end=99) errtemp
        if (iostate/=0) then
          ierr=3
          close (ireadunit)
          return
        endif
        
        ! df
        read(ireadunit,"(13x,e15.6)",iostat=iostate,end=99) object%Fre_smod(imodel)%star_var(1)
        if (iostate/=0) then
          ierr=3
          close (ireadunit)
          return
        endif
        
        ! dp
        read(ireadunit,"(13x,e15.6)",iostat=iostate,end=99) object%Fre_smod(imodel)%star_var(2)
        if (iostate/=0) then
          ierr=3
          close (ireadunit)
          return
        endif
        
        read(ireadunit,"()",iostat=iostate,end=99)
        if (iostate/=0) then
          ierr=3
          close (ireadunit)
          return
        endif
        
        do i=1,ilthsmax
          
          context=''
          read (ireadunit,"(a255)",iostat=iostate,end=12) context ! ���ַ���ʽ�����������
          if (iostate/=0) then
            ierr=4
            close (ireadunit)
            return
          endif
          length=len(trim(context)) ! �ַ����ݳ���
          
          if (length>0) then
            read (context,*,iostat=iostate) imodeltemp,nh,itemp,nmtem,nptem,ngtem,ftemp,ptemp, &
            errtemp,rInertial,rEpmod,rEgmod
            if (iostate/=0) then
              ierr=5
              close (ireadunit)
              return
            endif
            if (itemp/=l) stop 'Error 2 in read_model !'
            if (imodeltemp/=imodel0) then
              write (*,*) 'Error 3 in read_model !'
              stop
            endif
            object%Fre_smod(imodel)%nm(i,l)=nmtem
            object%Fre_smod(imodel)%Fthr(i,l)=ftemp
            object%Fre_smod(imodel)%Finer(i,l)=rInertial
          else
            goto 12
          endif
          
        enddo ! i
        
        write (*,*) trim(adjustl(cstardir)),' : i>=ilthsmax !'
        stop
        
12      continue
        object%Fre_smod(imodel)%iFthr(l)=i-1 ! ��гָ��Ϊlʱ����ģʽ������
        
      enddo ! l
      enddo ! imodel
      
      write (*,*) trim(adjustl(cstardir)),' : Error 4 in read_model'
      stop
      
99    continue
      close (ireadunit)
      object%imodel_tot=imodel-1
      
      if (object%imodel_tot<=0) return
      
! �������ڲ�����ģָ����Ϊ�����������l=1��nm����Ϊ�����仯����nm<0��nm=nm+1
      
      if (lmin<=1.and.lmax>=1) then
        l=1
        do imodel=1,object%imodel_tot
          do i=1,object%Fre_smod(imodel)%iFthr(l)
            itemp=object%Fre_smod(imodel)%nm(i,l)
            if (itemp<0) object%Fre_smod(imodel)%nm(i,l)=itemp+1
          enddo ! i
        enddo ! imodel
      endif
      
!  ��ȡ�����ݻ�ȫ�ֲ���
      
      open (newunit=ireadunit,file=trim(adjustl(cstardir))//'/Evol.txt',status='old',iostat=itemp)
      if (itemp/=0) then
        ierr=6
        close (ireadunit)
        return
      endif
      
      read (ireadunit,"(/)")
      do i=1,ievol_mode_max
        read (ireadunit,*,iostat=itemp) evol_data(:,i)
        if (itemp/=0) exit
      enddo ! i
      
      ievol_mode_tot=i-1
      close (ireadunit)
      
!  ƥ������ݻ�ȫ�ֲ���
      
      itemp=1
      do i=1,object%imodel_tot
        cycle_j : do j=itemp,ievol_mode_tot
          if (model_number(i)==nint(evol_data(4,j))) then
            object%Fre_smod(i)%star_var( 0) = evol_data(12,j) ! t / Gyr
            object%Fre_smod(i)%star_var( 3) = evol_data(1,j) ! lgT
            object%Fre_smod(i)%star_var( 4) = log10(evol_data(3,j)) ! lgR
            object%Fre_smod(i)%star_var( 5) = evol_data(10,j) ! M / M_sun
            object%Fre_smod(i)%star_var( 6) = log10(max(1e-20_ikind,evol_data(6,j))) ! X_s
            object%Fre_smod(i)%star_var( 7) = log10(max(1e-20_ikind,1-evol_data(6,j)-evol_data(8,j))) ! Z_s
            object%Fre_smod(i)%star_var( 8) = log10(max(1e-20_ikind,evol_data(7,j))) ! X_c
            object%Fre_smod(i)%star_var( 9) = log10(max(1e-20_ikind,1-evol_data(7,j)-evol_data(9,j))) ! Z_c
            object%Fre_smod(i)%star_var(10) = log10(max(1e-20_ikind,evol_data(16,j))) ! M_c / M
            object%Fre_smod(i)%star_var(11) = log10(max(1e-20_ikind,evol_data(19,j))) ! R_ce / R
            object%Fre_smod(i)%star_var(12) = log10(max(1e-20_ikind,evol_data(22,j))) ! M_ce / M
            object%Fre_smod(i)%star_var(13) = evol_data(17,j) ! A(Li)
            object%Fre_smod(i)%star_var(14) = evol_data(13,j) ! lg T_c
            object%Fre_smod(i)%star_var(15) = evol_data(14,j) ! lg Rho_c
            itemp=j
            exit cycle_j
          endif
        enddo cycle_j ! j
      enddo ! i
      
      end subroutine star_read_data
      
!========================================================================
      
!   д����
      
!========================================================================
      
      subroutine star_write_data (object,iunit)
      
      implicit none
      class(type_star), intent(in) :: object
      integer, intent(in) :: iunit
      
      integer :: i
      
      write(iunit,"(i6)") object%imodel_tot
      
      do i=1,object%imodel_tot
        call object%Fre_smod(i)%write_data(iunit)
      enddo ! i
      
      end subroutine star_write_data
      
!========================================================================
      
!   ����ģָ��
      
!========================================================================
      
      subroutine star_revise_nm (object,iunit1,iunit2)
      
      implicit none
      class(type_star), intent(inout) :: object
      integer, intent(in) :: iunit1, iunit2
      
      integer :: i, j, l, m, jfmin, jpmin, itemp
      real(ikind) :: dfre, dper, errfmin, errpmin
      real(ikind), dimension(-1:1) :: errtotf, errtotp
      integer, dimension(lmin:lmax,imodel_max) :: ishift
      
      if (object%imodel_tot<=1) return
      
!  ����ÿ��ģ�͸� l ����һ��ģ�Ͷ�Ӧ l ��Ƶ��ƥ���ϵ��
!  ����ָ�� l ���� i ��ģ�͵� k ��Ƶ����� i+1 ��ģ�͵� k + ishift(l,i+1) ��Ƶ��ƥ��
!  ˳���¼ƥ��ƫ�����ļ� iunit ��
      
      ishift(:,1)=0
      
      do i=1,object%imodel_tot-1
        do l=lmin,lmax
          
          do j=-1,1
            
            errtotf(j)=0
            errtotp(j)=0
            itemp=0
            
            do m=1,object%Fre_smod(i)%iFthr(l)
              if (m+j>=1.and.m+j<=object%Fre_smod(i+1)%iFthr(l)) then
                dfre=object%Fre_smod(i)%Fthr(m,l)-object%Fre_smod(i+1)%Fthr(m+j,l)
                dper=1e6_ikind/object%Fre_smod(i)%Fthr(m,l)-1e6_ikind/object%Fre_smod(i+1)%Fthr(m+j,l)
                errtotf(j)=errtotf(j)+dfre**2
                errtotp(j)=errtotp(j)+dper**2
                itemp=itemp+1
              endif
            enddo ! m
            
            if (itemp>0) then
              errtotf(j)=sqrt(errtotf(j)/itemp)/object%Fre_smod(i)%star_var(1)
              errtotp(j)=sqrt(errtotp(j)/itemp)/object%Fre_smod(i)%star_var(2)
            else
              errtotf(j)=-1
              errtotp(j)=-1
              write (*,*) object%stellar_iparameters(:), i, l
              write (*,*) ' Error 1 in star_revise_nm !'
              stop
            endif
            
          enddo ! j
          
          jfmin=-99
          jpmin=-99
          errfmin=1e20_ikind
          errpmin=1e20_ikind
          do j=-1,1
            if (errfmin>errtotf(j)) then
              errfmin=errtotf(j)
              jfmin=j
            endif
            if (errpmin>errtotp(j)) then
              errpmin=errtotp(j)
              jpmin=j
            endif
          enddo ! j
          
          if (jfmin<-90.or.jpmin<-90) then
            write (*,*) object%stellar_iparameters(:), i, l, jfmin, jpmin
            write (*,*) ' Error 2 in star_revise_nm !'
            stop
          endif
          
        !  write (iunit1,"(6i6,99e18.8)") object%stellar_iparameters(:), &
        !  i, l, errtotf(:), errtotp(:)
          
          write (iunit1,"(6i6,99e18.8)") object%stellar_iparameters(:), &
          i, l, errtotf(jfmin), errtotp(jpmin)
          
          if (jfmin/=jpmin) then
            write (*,*) object%stellar_iparameters(:), i, l, jfmin, jpmin
            write (*,*) ' Error 3 in star_revise_nm !'
            stop
          endif
          
          ishift(l,i+1)=jfmin
          
        enddo ! l
      enddo ! i
      
!  ������ģ����ֱ����ϼ������ģָ����
      
      do i=1,object%imodel_tot
        do l=lmin,lmax
          
          dfre=0
          do m=1,object%Fre_smod(i)%iFthr(l)
            dfre=dfre+object%Fre_smod(i)%nm(m,l)-m
          enddo ! m
          dfre=dfre/object%Fre_smod(i)%iFthr(l)
          
          do m=1,object%Fre_smod(i)%iFthr(l)
            object%Fre_smod(i)%nm(m,l)=nint(dfre)+m
          enddo ! m
          
        enddo ! l
      enddo ! i
      
!  ����һ��ģָ���Ƿ����ǰһ���õ���Ƶ��ƥ���ϵ
      
      do i=1,object%imodel_tot-1
        do l=lmin,lmax
          if (object%Fre_smod(i)%nm(1,l)/=object%Fre_smod(i+1)%nm(1,l)+ishift(l,i+1)) then
            write (iunit2,"(99i6)") object%stellar_iparameters(:), i, l, &
            object%Fre_smod(i  )%nm(1,l), ishift(l,i  ), &
            object%Fre_smod(i+1)%nm(1,l), ishift(l,i+1)
          endif
        enddo ! l
      enddo ! i
      
      end subroutine star_revise_nm
      
!========================================================================






!========================================================================
      
      end module AM_data_basic
      
!========================================================================
!========================================================================






!========================================================================
      
      program data_gen
      
      use AM_data_basic
      implicit none
      
      integer, parameter :: istarmax=1000000
      
      integer :: i, istartot, iopenMP_thread, m
      character :: cnum*4
      
      character, dimension(istarmax) :: cdir * 32 ! ·��
      
      type(type_star), allocatable, dimension(:) :: astar
      integer:: kdex, ierr, k
      
      call system ('mkdir .\zdata\')
      call system ('mkdir .\zdata\data')
      
      open (unit=10,file='DirList.txt',status='old')
      do i=1,istarmax
        read (10,"(a32)",end=1) cdir(i)
      enddo ! i
      write (*,*) ' i = istarmax ! '
      stop
1     continue
      close (10)
      istartot=i-1
      
      write (*,"('  Ŀ¼�б��Ѷ�ȡ��Ŀ¼���� ��',i6)") istartot
      write (*,"('  ���������ݴ�����߳��� ��')",advance='no')
      read (*,*) iopenMP_thread
      
      do m=1,iopenMP_thread
        write (cnum,"(i4.4)") m
        open (unit=100+m,file='check_1_'//cnum//'.txt',status='replace')
        open (unit=200+m,file='check_2_'//cnum//'.txt',status='replace')
        open (unit=300+m,file='data_gen_err_'//cnum//'.txt',status='replace')
        open (unit=400+m,file='data_gen_ok_'//cnum//'.txt',status='replace')
      enddo ! m
      
      allocate(astar(iopenMP_thread))
      
      do i=1,istartot/iopenMP_thread+1
      
! ע�⣺OMP ��֧�� private �ڰ����Զ�����������
!$OMP parallel do num_threads(iopenMP_thread) private(m,kdex,ierr,k)
        do m=1,iopenMP_thread
          
          kdex=(i-1)*iopenMP_thread+m
          
          if (kdex<=istartot) then
            call astar(m)%read_data(cdir(kdex),ierr)
            if (ierr==0) then
              call astar(m)%revise_nm(100+m,200+m)
              open (unit=500+m,file='./zdata/'//trim(adjustl(cdir(kdex)))//'.txt', &
                    status='replace')
              call astar(m)%write_data(500+m)
              close (500+m)
              do k=1,len_star_dir_max
                write (400+m,"(1a)",advance='no') cdir(kdex)(k:k)
              enddo ! k
              write (400+m,"()")
            else
              do k=1,len_star_dir_max
                write (300+m,"(1a)",advance='no') cdir(kdex)(k:k)
              enddo ! k
              write (300+m,"()")
            endif
          endif
          
        enddo ! m
!$OMP end parallel do
      
      enddo ! i
      
      deallocate(astar)
      
      do m=1,iopenMP_thread
        close (100+m)
        close (200+m)
        close (300+m)
        close (400+m)
      enddo ! m
      
      call system('copy /b check_1_*.txt check_1.txt')
      call system('copy /b check_2_*.txt check_2.txt')
      call system('copy /b data_gen_err_*.txt data_gen_err.txt')
      call system('copy /b data_gen_ok_*.txt .\zdata\_DirList.txt')
      call system('del check_1_*.txt')
      call system('del check_2_*.txt')
      call system('del data_gen_err_*.txt')
      call system('del data_gen_ok_*.txt')
      
      end program data_gen
      
!========================================================================
!========================================================================