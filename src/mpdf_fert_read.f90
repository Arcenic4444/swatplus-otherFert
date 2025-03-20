      subroutine mpdf_fert_read
      
      use input_file_module
      use conditional_module
      use maximum_data_module
      implicit none       
            
      character (len=80) :: titldum = ""!           |title of file
      character (len=80) :: header = "" !           |header of file
      integer :: eof = 0                !           |end of file
      integer :: im = 0               !none       |determine max number for array (imax) and total number in file
      logical :: i_exist                !none       |check to determine if file exists
      integer :: m_ferts = 0            !           |end of loop
      integer :: rows = 0               !           |end of loop
      integer :: kk = 0                 !           |end of loop
      integer :: imfrt = 0
      character(len=8) :: mpdf_rotname  
      integer :: num_pdfs     
      character(len=8) :: fer_num      
      real(8) :: tot_fer      
      real(8), dimension(10) :: centers, time_windows, weights

      eof = 0
      im = 0


      inquire (file=in_mpdfer_params%mpdf_param, exist=i_exist)
      if (.not. i_exist .or. in_mpdfer_params%mpdf_param == "null") then
          allocate (mpdf_fert_parmas_db(0:0))
          db_mx%mpdf_fert = 0
          return
      else
          open (107,file=in_mpdfer_params%mpdf_param)
          read (107,*,iostat=eof) titldum
          read (107,*,iostat=eof) im
          allocate (mpdf_fert_parmas_db(1:im))
          read (107,*,iostat=eof) header
          rows = im * 2
          do imfrt = 1, rows
            read (107,*,iostat=eof) mpdf_rotname, num_pdfs, fer_num, tot_fer, &
                centers, time_windows, weights
            if (eof < 0) exit
            if (mpdf_rotname /= "0") then
                kk = 1 + kk
                mpdf_fert_parmas_db(kk)%name = mpdf_rotname
                allocate (mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(1:2))
                m_ferts = 1
            else
                m_ferts = 2
            endif
            allocate (mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%centers(1:10))
            allocate (mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%time_windows(1:10))
            allocate (mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%weights(1:10))
            mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%num_pdfs = num_pdfs
            mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%fer_num = fer_num
            mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%tot_fer = tot_fer
            mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%centers = centers
            mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%time_windows = time_windows
            mpdf_fert_parmas_db(kk)%mpdf_fert_parmas_rot_db(m_ferts)%weights = weights
          enddo
          db_mx%mpdf_fert  = im 
          write(*,*) "db_mx%mpdf_fert", db_mx%mpdf_fert

      endif
      close(107)
      return     
      end subroutine mpdf_fert_read