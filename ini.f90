      implicit real (8) (a-h,o-z)
      
      ! 操作系统：windows=0, linux=1
      integer, parameter :: i_computer_system=0
      
      character :: DirName*32
      
      !FeH0=0.32d0
      ZXS_solar=0.0188d0
      !alfamlt=1.8d0 ! alpha_MLT
      
      u=10**FeH0
      
      ! -lgC * 1e-3
      
      !iCLG_1=4000
      !iCLG_2=4000
      !iCLG_Step=1
      !iCLG_D=(iCLG_2-iCLG_1)/max(1,iCLG_Step-1)
      !if (iCLG_D*(iCLG_Step-1)+iCLG_1 /= iCLG_2) stop 'iCLG Error !'
	  
	  ! ALFAMLT * 1e-3
	  iALF_1=1600
	  iALF_2=1800
	  iALF_STEP=3
	  iALF_D=(iALF_2-iALF_1)/max(1,iALF_Step-1)
      if (iALF_D*(iALF_Step-1)+iALF_1 /= iALF_2) stop 'iALF Error !'
	  
	  
      ! lg theta，1e-3
      
      !iFOV_1=1900
      !iFOV_2=2200
      !iFOV_Step=7
      !iFOV_D=(iFOV_2-iFOV_1)/max(1,iFOV_Step-1)
      !if (iFOV_D*(iFOV_Step-1)+iFOV_1 /= iFOV_2) stop 'iFOV Error !'
      
      ! 恒星质量，1e-3 Msun
      
      iStarMass_1=900
      iStarMass_2=1400
      iStarMass_Step=26
      iStarMass_D=(iStarMass_2-iStarMass_1)/max(1,iStarMass_Step-1)
      if (iStarMass_D*(iStarMass_Step-1)+iStarMass_1 /= iStarMass_2) stop 'iStarMass Error !'
      
      
      ! Y00，1e-3
      
      iY00_1=200
      iY00_2=400
      iY00_Step=11
      iY00_D=(iY00_2-iY00_1)/max(1,iY00_Step-1)
      if (iY00_D*(iY00_Step-1)+iY00_1 /= iY00_2) stop 'iY00 Error !'
	  
	  ! Z00, 1e-4
	  iZ00_1=50
	  iZ00_2=350
	  iZ00_STEP=7
	  iZ00_D=(iZ00_2-iZ00_1)/max(1,iZ00_Step-1)
      if (iZ00_D*(iZ00_Step-1)+iZ00_1 /= iZ00_2) stop 'iZ00 Error !'
      
      ! 批量复制文件夹，修改设置文件，写入目录名称
      
      open (unit=21,file='DirList.txt',status='unknown')
      
      do j1 =1,iALF_Step
      iALF=iALF_1+iALF_D*(j1-1)
     
      
	  
      DO j2 =1,iZ00_STEP
	  iZ00=iZ00_1+iZ00_D*(j2-1)
	  
      do j3 =1,iStarMass_Step
      iStarMass=iStarMass_1+iStarMass_D*(j3-1)
	 
	  
      do j4 =1,iY00_Step
      iY00=iY00_1+iY00_D*(j4-1)
      
      write (DirName,"('ALF',i5.5,'Z00',i5.5,'MAS',i5.5,'Y00',i5.5)") &
      iALF,iZ00,iStarMass,iY00
      
      ALF=iALF*1d-3
      
      starmass=iStarMass*1d-3
      
      Y00=iY00*1d-3
	  
	  Z00=iZ00*1d-4
      
	  X00=1-(Y00+Z00)
      !X00=(1-Y00)/(1+ZXS_solar*u)
      !Z00=ZXS_solar*u*X00
      
      
      
      write (21,"(32a)") DirName
      
      if (i_computer_system==0) &
      call system ('xcopy /e /i ynev_template '//DirName//' /y')
      
      if (i_computer_system==1) &
      call system ('cp -rf ynev_template '//DirName)
      
      open  (unit=12,file='./'//DirName//'/_setup.aoe',status='old')
      read  (12,"(150/)")
      write (12,"('  ! ++++++ Automatically writing ++++++ ')")
      write (12,"(7x)")
      write (12,"(7x,'starmass0 = ',e27.17e3)") starmass
      write (12,"(7x)")
      write (12,"(7x,'x00 = ',e27.17e3)") X00
      write (12,"(7x,'z00 = ',e27.17e3)") Z00
      write (12,"(7x)")
      write (12,"(7x,'alfamlt = ',e27.17e3)") ALF
      write (12,"(7x)")
      !write (12,"(7x,'iDIFOVM_core = 1 ')")
      !write (12,"(7x,'DIFOVM_theta_core = ',e27.17e3)") 1d1**FOV
      !write (12,"(7x,'DIFOVM_C_core = ',e27.17e3)") 1d1**(-CLG)
      write (12,"(7x)")
      write (12,"('  ! ++++++ End writing ++++++ ')")
      write (12,"(7x)")
      do i=1,100
        write (12,"(7x)")
      enddo ! i
      write (12,"('/')")
      write (12,"()")
      close (12)
      
      enddo ! j4
      enddo ! j3
      enddo ! j2
      enddo ! j1
      
      close (21)
      
      if (i_computer_system==0) &
      call system ('copy DirList.txt DirList_all.txt')
      
      if (i_computer_system==1) &
      call system ('cp DirList.txt DirList_all.txt')
      
      
      end
      
!==============================================================================
!==============================================================================
      