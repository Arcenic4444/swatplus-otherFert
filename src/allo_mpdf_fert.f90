subroutine allo_mpdf_fert(isched, iop)
    
    use conditional_module
    use maximum_data_module
    use mgt_operations_module
    use fertilizer_data_module
    use plant_data_module

    implicit none
    
    integer, intent(in) :: isched, iop

    integer, parameter :: n_days = 365
    integer :: ifer, ddd, idb, index_for_sched
    integer :: day, mon, jday, iyear
    real(8), dimension(n_days) :: fertilizer_per_day_P, fertilizer_per_day_N
    
    ! Loop over fertilizer definitions to match current schedule
    do ifer = 1, db_mx%mpdf_fert
        if (mpdf_fert_parmas_db(ifer)%name == sched(isched)%name) then
            call mgt_mpdf_fert(ifer, fertilizer_per_day_P, fertilizer_per_day_N)

            do ddd = 1, n_days
                call jday_to_month_day(ddd, mon, day)
                ! ---------------------------
                ! First operation: apply N fertilizer
                ! ---------------------------
                index_for_sched = iop + (ddd - 1)*2
                sched(isched)%mgt_ops(index_for_sched)%op = "fert"

                sched(isched)%mgt_ops(index_for_sched)%mon = mon
                sched(isched)%mgt_ops(index_for_sched)%day = day
                sched(isched)%mgt_ops(index_for_sched)%jday = ddd
                sched(isched)%mgt_ops(index_for_sched)%husc = 0
                sched(isched)%mgt_ops(index_for_sched)%year = 1
                sched(isched)%mgt_ops(index_for_sched)%op3 = fertilizer_per_day_N(ddd)
                sched(isched)%mgt_ops(index_for_sched)%op_plant = "broadcast"
                sched(isched)%mgt_ops(index_for_sched)%op4 = 1
    
                do idb = 1, db_mx%fertparm
                    if (mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(1)%fer_num == fertdb(idb)%fertnm) then
                        sched(isched)%mgt_ops(index_for_sched)%op_char = fertdb(idb)%fertnm
                        sched(isched)%mgt_ops(index_for_sched)%op1 = idb
                        exit
                    endif
                end do

                ! ---------------------------
                ! Second operation: apply P fertilizer
                ! ---------------------------
                index_for_sched = iop + (ddd - 1)*2 + 1
                sched(isched)%mgt_ops(index_for_sched)%op = "fert"
                sched(isched)%mgt_ops(index_for_sched)%mon = mon
                sched(isched)%mgt_ops(index_for_sched)%day = day
                sched(isched)%mgt_ops(index_for_sched)%jday = ddd
                sched(isched)%mgt_ops(index_for_sched)%year = iyear
                sched(isched)%mgt_ops(index_for_sched)%op3 = fertilizer_per_day_P(ddd)
                sched(isched)%mgt_ops(index_for_sched)%op_plant = "broadcast"
                sched(isched)%mgt_ops(index_for_sched)%op4 = 1

                do idb = 1, db_mx%fertparm
                    if (mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(2)%fer_num == fertdb(idb)%fertnm) then
                        sched(isched)%mgt_ops(index_for_sched)%op_char = fertdb(idb)%fertnm
                        sched(isched)%mgt_ops(index_for_sched)%op1 = idb
                        exit
                    endif
                end do

            end do

            exit
        end if
    end do

contains

    subroutine jday_to_month_day(jday, mon, day)
        implicit none
        integer, intent(in) :: jday
        integer, intent(out) :: mon, day
        integer, dimension(12) :: month_days
        integer :: i, sum

        month_days = (/31,28,31,30,31,30,31,31,30,31,30,31/)

        sum = 0
        do i = 1, 12
            if (jday <= sum + month_days(i)) then
                mon = i
                day = jday - sum
                return
            else
                sum = sum + month_days(i)
            end if
        end do

        mon = 12
        day = 31
    end subroutine jday_to_month_day

end subroutine allo_mpdf_fert
