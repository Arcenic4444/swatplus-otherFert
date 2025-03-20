      subroutine read_mgtops(isched,nops)
      
      use maximum_data_module
      use plant_data_module
      use mgt_operations_module
      use tillage_data_module
      use fertilizer_data_module
      use pesticide_data_module
      use time_module
      
      implicit none      
      
      integer, intent(in) :: nops
      integer :: iyear = 0   !            |
      integer :: day = 0     !            |
      integer :: mon = 0     !            |
      integer :: iop = 0     !none        |
      integer :: ddd = 0     !none        |
      integer :: jjj = 1     !none        |
      integer :: isched      !            |
      integer :: jdt         !none        |julian date
      integer :: idb = 0     !none        | 
      character (len=40) :: op = ""!           |title of file
      character (len=80) :: titldum = ""!           |title of file
      integer :: add_ops = 0
      integer :: eof = 0


      do iop = 1, nops
          read (107,*,iostat=eof) op, titldum
          if (op == "mfrt") add_ops = add_ops + 729
      end do

      do iop = 1, nops
        backspace(107)
      end do

      sched(isched)%num_ops = nops + add_ops
      allocate(sched(isched)%mgt_ops(sched(isched)%num_ops))

      iyear = 1
      do iop = 1, nops                                              !! operation loop
        read (107,*)   sched(isched)%mgt_ops(jjj)%op,               & !! operation character
                       sched(isched)%mgt_ops(jjj)%mon,              &
                       sched(isched)%mgt_ops(jjj)%day,              &
                       sched(isched)%mgt_ops(jjj)%husc,             &
                       sched(isched)%mgt_ops(jjj)%op_char,          & !! operation type character
                       sched(isched)%mgt_ops(jjj)%op_plant,         & !! plant character 
                       sched(isched)%mgt_ops(jjj)%op3               !! override
        
          day = sched(isched)%mgt_ops(jjj)%day
          mon = sched(isched)%mgt_ops(jjj)%mon
          if (day /= 0) then
            sched(isched)%mgt_ops(jjj)%jday = Jdt(ndays,day,mon)
            sched(isched)%mgt_ops(jjj)%year = iyear
            if (sched(isched)%mgt_ops(jjj)%op == "skip") iyear = iyear + 1
          endif

      !! added by lichen lang, do the modal pdf fertilization
      if (sched(isched)%mgt_ops(jjj)%op == "mfrt") then
       call allo_mpdf_fert(isched,iop)
       jjj = jjj + 730
      else
        select case(sched(isched)%mgt_ops(jjj)%op)
          case ("pcom")
            do idb = 1, db_mx%plantcom
                if (sched(isched)%mgt_ops(jjj)%op_char == pcomdb(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
            end do
            
          case ("plnt")
            do idb = 1, db_mx%plantparm
                if (sched(isched)%mgt_ops(jjj)%op_char == pldb(idb)%plantnm) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb 
                    exit
                endif
            end do
            if (sched(isched)%mgt_ops(jjj)%op1 == 0) then
              write (9001,*) " mgt schedule", isched, &
                  " op numb", jjj, sched(isched)%mgt_ops(jjj)%op_char, " not found in plants.plt database" 
            end if
            !! xwalk with transplant data file
            do idb = 1, db_mx%transplant
                if (sched(isched)%mgt_ops(jjj)%op_plant == transpl(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op4 = idb 
                    exit
                endif
            end do
          
          case ("harv")
            do idb = 1, db_mx%harvop_db
                if (sched(isched)%mgt_ops(jjj)%op_plant == harvop_db(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
            end do

          case ("hvkl")
            do idb = 1, db_mx%harvop_db
                if (sched(isched)%mgt_ops(jjj)%op_plant == harvop_db(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
            end do
            
          case ("till")
            do idb = 1, db_mx%tillparm
                if (sched(isched)%mgt_ops(jjj)%op_char == tilldb(idb)%tillnm) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
            end do
            
          case ("irrm","irrp")
            sched(isched)%irr = 1
            do idb = 1, db_mx%irrop_db
              if (sched(isched)%mgt_ops(jjj)%op_char == irrop_db(idb)%name) then
                sched(isched)%mgt_ops(jjj)%op1 = idb
                exit
              endif
            end do

          case ("fert")
            !xwalk fert name with fertilizer data base
            do idb = 1, db_mx%fertparm
              if (sched(isched)%mgt_ops(jjj)%op_char == fertdb(idb)%fertnm) then
                sched(isched)%mgt_ops(jjj)%op1 = idb
                exit
              endif
            end do
            !xwalk application type with chemical application data base
            do idb = 1, db_mx%chemapp_db
              if (sched(isched)%mgt_ops(jjj)%op_plant == chemapp_db(idb)%name) then
                sched(isched)%mgt_ops(jjj)%op4 = idb
                exit
              endif
            end do

          case ("pest")
            !xwalk fert name with fertilizer data base
            do idb = 1, db_mx%pestparm
              if (sched(isched)%mgt_ops(jjj)%op_char == pestdb(idb)%name) then
                sched(isched)%mgt_ops(jjj)%op1 = idb
                exit
              endif
            end do
            !xwalk application type with chemical application data base
            do idb = 1, db_mx%chemapp_db
              if (sched(isched)%mgt_ops(jjj)%op_plant == chemapp_db(idb)%name) then
                sched(isched)%mgt_ops(jjj)%op4 = idb
                exit
              endif
            end do

            case ("graz")
              do idb = 1, db_mx%grazeop_db
                if (sched(isched)%mgt_ops(jjj)%op_char == grazeop_db(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
              end do
  
            case ("burn")
              do idb = 1, db_mx%fireop_db
                if (sched(isched)%mgt_ops(jjj)%op_char == fire_db(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
              end do
            
            case ("swep")
              do idb = 1, db_mx%sweepop_db
                if (sched(isched)%mgt_ops(jjj)%op_char == sweepop_db(idb)%name) then
                    sched(isched)%mgt_ops(jjj)%op1 = idb
                    exit
                endif
  !!herehere no null
              end do      
        end select    
      jjj = jjj + 1     
      endif
      end do                                  !! operation loop
      jjj = 1
      return
      end