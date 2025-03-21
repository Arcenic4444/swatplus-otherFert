 subroutine fert_after_prec (isched,fert_count)

      use plant_data_module
      use mgt_operations_module
      use tillage_data_module
      use basin_module
      use hydrograph_module
      use hru_module, only : hru, ihru, cn2, phubase, ndeat, igrz, grz_days,    &
        yr_skip, sol_sumno3, sol_sumsolp, fertnh3, fertno3, fertorgn,  &
        fertorgp, fertsolp, ipl, sweepeff, yr_skip
      use soil_module
      use plant_module
      use time_module
      use constituent_mass_module
      use organic_mineral_mass_module
      use calibration_data_module
      use reservoir_data_module
      use reservoir_module
      use maximum_data_module
      use aquifer_module
      
      implicit none
      
      integer, intent(inout) :: fert_count
      integer :: k = 0          !         |  
      integer :: j = 0             !none     |counter
      integer :: ifrt = 0          !         |fertilizer type from fert data base
      integer :: isched            !         | 
      integer :: ifertop = 0       !frac     |surface application fraction from chem app data base
      real :: frt_kg = 0.          !kg/ha    |amount of fertilizer applied

      j = ihru


      ipl = 1
      ifrt = mgt%op1                          !fertilizer type from fert data base
      frt_kg = mgt%op3                        !amount applied in kg/ha
      ifertop = mgt%op4                       !surface application fraction from chem app data base
      

      do k = 1, delayed_fert(j)%num_fert
        if (delayed_fert(j)%fert_type(k) == mgt%op_char) then
            frt_kg = frt_kg + delayed_fert(j)%fert_amount(k)
            delayed_fert(j)%fert_type(k) = ""
            delayed_fert(j)%fert_amount(k) = 0
            delayed_fert(j)%fert_method(k) = ""
            fert_count = fert_count + 1
        end if
      end do

      if (delayed_fert(j)%num_fert == fert_count) then 
        deallocate(delayed_fert(j)%fert_type)
        deallocate(delayed_fert(j)%fert_amount)
        deallocate(delayed_fert(j)%fert_method)
        fert_count = 0
      endif
      

      if (wet(j)%flo>0.) then !case for surface application with standing water
          call pl_fert_wet (ifrt, frt_kg) 
          call salt_fert_wet(j,ifrt,frt_kg)
          call cs_fert_wet(j,ifrt,frt_kg)
          if (pco%mgtout == "y") then
          write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, " FERT-WET", &
              phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
              soil1(j)%rsd(1)%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
              fertorgn, fertsolp, fertorgp
          endif
      else
          call pl_fert (ifrt, frt_kg, ifertop)
          call salt_fert(j,ifrt,frt_kg,ifertop) !rtb salt 
          call cs_fert(j,ifrt,frt_kg,ifertop) !rtb cs
          if (pco%mgtout == "y") then
          write (2612,*) j, time%yrc, time%mo, time%day_mo, mgt%op_char, "    FERT ", &
              phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pl_mass(j)%tot(ipl)%m,           &
              soil1(j)%rsd(1)%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
              fertorgn, fertsolp, fertorgp
          endif
      endif

      hru(j)%cur_op = hru(j)%cur_op + 1
      if (hru(j)%cur_op > sched(isched)%num_ops) then
        hru(j)%cur_op = 1
      end if
      
      mgt = sched(isched)%mgt_ops(hru(j)%cur_op)
   
      return

      end subroutine fert_after_prec