subroutine mgt_mpdf_fert(ifer, fertilizer_per_day_P, fertilizer_per_day_N)
    use conditional_module
    
    implicit none

    integer, parameter :: n_days = 365
    integer, intent(in) :: ifer
    real(8), dimension(n_days), intent(out) :: fertilizer_per_day_P, fertilizer_per_day_N

    integer :: i, j, fer_elem
    real(8), dimension(n_days) :: fertilizer_per_day
    real(8) :: pdf_value, sum_pdf_values, fertilizer_sum

    do fer_elem = 1, 2  ! 1=N, 2=P
        fertilizer_per_day = 0.0

        ! Step 1: accumulate weighted PDFs
        do i = 1, n_days
            do j = 1, mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(fer_elem)%num_pdfs
                pdf_value = gaussian_pdf(i, &
                    mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(fer_elem)%centers(j), &
                    mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(fer_elem)%time_windows(j))
                fertilizer_per_day(i) = fertilizer_per_day(i) + &
                    mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(fer_elem)%weights(j) * pdf_value
            end do
        end do

        ! Step 2: normalize and scale
        sum_pdf_values = sum(fertilizer_per_day)
        fertilizer_sum = mpdf_fert_parmas_db(ifer)%mpdf_fert_parmas_rot_db(fer_elem)%tot_fer
        do i = 1, n_days
            fertilizer_per_day(i) = fertilizer_sum * fertilizer_per_day(i) / sum_pdf_values
        end do

        ! Assign to output
        if (fer_elem == 1) then
            fertilizer_per_day_N = fertilizer_per_day
        else
            fertilizer_per_day_P = fertilizer_per_day
        end if
    end do

contains

    real(8) function gaussian_pdf(day, mu, sigma)
        integer, intent(in) :: day
        real(8), intent(in) :: mu, sigma
        real(8) :: exponent

        exponent = -((real(day) - mu)**2) / (2.0 * sigma**2)
        gaussian_pdf = exp(exponent) / (sigma * sqrt(2.0 * 3.14159))
    end function gaussian_pdf

end subroutine mgt_mpdf_fert
