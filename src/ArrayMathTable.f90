module arraymath_table
  implicit none
  private

  public :: table_t, replace_real_matrix, transpose_real_matrix

  type :: table_t
    real, allocatable :: values(:,:)
    character(30), allocatable :: rownames(:)
    character(30), allocatable :: colnames(:)
    logical :: has_row_names = .false.
    logical :: has_col_names = .false.
  end type table_t

contains

  subroutine replace_real_matrix(dest, source)
    real, allocatable, intent(inout) :: dest(:,:)
    real, intent(in) :: source(:,:)
    real, allocatable :: tmp(:,:)

    allocate(tmp, source=source)
    call move_alloc(tmp, dest)
  end subroutine replace_real_matrix

  subroutine transpose_real_matrix(dest)
    real, allocatable, intent(inout) :: dest(:,:)
    real, allocatable :: tmp(:,:)

    allocate(tmp, source=transpose(dest))
    call move_alloc(tmp, dest)
  end subroutine transpose_real_matrix

end module arraymath_table
