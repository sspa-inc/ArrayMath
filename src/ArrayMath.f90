! Created by Michael Ou

program arraymath
  use arraymath_stats
  use arraymath_table, only: replace_real_matrix, transpose_real_matrix
  use f90getopt
  use m_mrgref
  use iso_fortran_env, only: input_unit, error_unit, output_unit, iostat_eor, iostat_end
  use iso_c_binding, only: c_int, c_ptr, c_loc, c_char, c_null_ptr, c_associated
  use, intrinsic :: ieee_arithmetic
  implicit none

  real, parameter :: verysmall = tiny(1.0) * 1e5
  real, parameter :: verylarge = huge(1.0) * 1e-3

  type(option_s), allocatable  :: opts(:)

  character(1024) :: arrfile, multfile, outfile, fomt
  character(len=2)  :: opt

  integer         :: ifile, irow, nskiprow, nskipcol, icol
  real            :: rscale, roffset, power, vmin, vmax
  real, allocatable :: results(:,:), array(:,:), multi(:,:), trans(:,:)
  character(256)  :: stemp(9999)
  character(10)   :: func
  character(30), allocatable   :: rownames(:), colnames(:), tmpnames(:)
  character(len=28) :: spaces
  logical         :: verbose, lfOutput

  integer         :: nrow, ncol, itemp, mdim, hasRowName, hasColName, maxRowNameWidth, ntmp
  integer, allocatable:: rowindex(:)

#if defined(_WIN32) || defined(__MINGW32__)
  interface
    function get_std_handle(which) bind(C, name='GetStdHandle') result(handle)
      import c_int, c_ptr
      integer(c_int), value :: which
      type(c_ptr) :: handle
    end function
    function write_file(handle, buffer, count, written, overlapped) bind(C, name='WriteFile') result(ok)
      import c_int, c_ptr
      type(c_ptr), value :: handle, buffer, overlapped
      integer(c_int), value :: count
      integer(c_int) :: written
      integer(c_int) :: ok
    end function
  end interface
#endif

  spaces = new_line(" ")//repeat(" ", 27)
  allocate(opts, source=(/&
    option_s("dim"      ,  "d", 2, "shape of the array, i.e. nrow ncol; this must be defined as the first argument."), &
    option_s("add"      ,  "a", 3, "adding a new array, follwed by three arguments: arrayfile multiplyfile scale; "//spaces//"&
                                   &if arrayfile is '-', it reads stdin; if multiplyfile is '-', no multiarray is used."), &
    option_s("multi"    ,  "m", 1, "multiply with a number or a new array reading from a file; follwed by one argument. "//spaces//"&
                                   &if the argument starts with '*', it multiplies the number after '*'."), &
    ! option_s("dot"      , "dt", 1, "Calculate dot product of the array and another array."), &
    option_s("help"     ,  "h", 0, "show this message."), &
    option_s("lf"       , "lf", 0, "write output with LF (line feed) line endings instead of the compiler default."), &
    option_s("fmt"      , "fm", 1, "fortran format to write results such as '(10F10.3)'; default is '(*(G0.8,x))'."), &
    option_s("offset"   ,  "o", 1, "adding offset to the final result, default is zero."), &
    option_s("duplicate", "dr", 1, "duplicate the rows for a number of times."), &
    option_s("skiprow"  , "sr", 1, "number of rows to skip when reading the input array file; "//spaces//"&
                                   &must define before reading the file; "//spaces//"&
                                   &default is zero; it can be defined multiple times."), &
    option_s("skipcol"  , "sc", 1, "number of columns to skip when reading the input array file; "//spaces//"&
                                   &must define before reading the file; "//spaces//"&
                                   &default is zero; it can be defined multiple times."), &
    option_s("func"     ,  "x", 1, "apply a Fortran intrinsic mathematical function to the final result"), &
    option_s("groupby"  ,  "g", 2, "split and apply a function: min,max,mean,sum,std,centralize."), &
    option_s("moveavg"  , "ma", 2, "calculate exponentially weighted (EW) moving average;  "//spaces//"&
                                   &followed by size of window, decay factor; 0 <= decay factor <= 1. "//spaces//"&
                                   &weights=[1,(1-df),(1-df)^2,...(1-df)^(nw-1)]/sum([1,(1-df),(1-df)^2,...(1-df)^(nw-1)])"//spaces//"&
                                   &When decay factor is 0, it apply uniform weights (moving average)."//spaces//"&
                                   &When decay factor is 1, it only uses last data."), &
    option_s("stack"    , "st", 0, "stack the 2D array into one column."), &
    option_s("power"    ,  "p", 1, "apply a power function to the final result"), &
    option_s("clip",       "c", 2, "clip the array to the interval between vmin and vmax."//spaces//"&
                                   &if `-c 0 1` is specified, values smaller than 0 become 0, and values larger than 1 become 1."), &
    option_s("subset"   ,  "s", 1, "subset the array."//spaces//"&
                                   &-s r1:10 to subset the first 10 rows;"//spaces//"&
                                   &-s c1,3,5 to subset the columns 1,3 and 5;"//spaces//"&
                                   &-s r2:2 or -s c5:5 to subset one row or column."), &
    option_s("head"     , "hd", 1, "keep the first n rows of the current array."), &
    option_s("tail"     , "tl", 1, "keep the last n rows of the current array."), &
    option_s("unique"   , "un", 0, "keep the first occurrence of each distinct numeric row."), &
    option_s("reverse"  , "rv", 0, "reverse the order of rows."), &
    option_s("filter"   ,  "f", 1, "filter array by column names or column index, e.g."//spaces//"&
                                   &--filter time>=3; --filter layer==1; --filter 2>0"), &
    option_s("verbose"  ,  "v", 0, "print running logs to screen."), &
    option_s("transpose",  "t", 0, "transposes the array."), &
    option_s("rowname"  , "rn", 0, "enable/disable reading row names from the first column after skipping columns. default is off."), &
    option_s("colname"  , "cn", 0, "enable/disable reading column names from the first row after skipping rows. default is off."), &
    option_s("mfbin"    , "mb", 0, "TODO: read modflow binary file"), &
    option_s("mfdbin"   , "md", 0, "TODO: read modflow binary file in double precision")  &
  /))


  ! set up the initial values
  fomt='(*(G0.8,x))'
  nrow=0
  nskiprow=0
  nskipcol=0
  roffset = 0.
  func = ''
  power = 1.0
  verbose = .false.
  lfOutput = .false.
  outfile = '~'
  hasRowName = 0
  hasColName = 0

  do
    opt = getopt(opts)
    select case(trim(opt))

      case(char(0)) ! When all options are processed
        if (len_trim(optarg)>0) outfile = trim(optarg)
        exit

      case("d")
        read(optarg, *) nrow, ncol
        mdim = nrow*ncol
        allocate(results(ncol, nrow))
        allocate(rownames(0:mdim), colnames(0:mdim), tmpnames(0:mdim), rowindex(mdim))
        results = 0.
        rownames = ''
        colnames = ''

      case("rn")
        hasRowName = 1

      case("cn")
        hasColName = 1

      case("s")
        call subset()

      case("hd")
        read(optarg, *) ntmp
        call head_rows(ntmp)

      case("tl")
        read(optarg, *) ntmp
        call tail_rows(ntmp)

      case("un")
        call unique_rows()

      case("rv")
        call reverse_rows()

      case("st")
        call stack()

      case("f")
        call filter()

      case("t")
        call rtranspose()

      case("p")
        read(optarg, *) power
        results = results ** power

      case("dr")
        read(optarg, *) ntmp
        call duplicate_rows(ntmp)

      case("c")
        read(optarg, *) vmin, vmax
        results = merge(results, vmin, results>vmin)
        results = merge(results, vmax, results<vmax)

      case("x")
        call applyfunc()

      case("g")
        call groupby()

      case("ma")
        call moving_average()

      case("m")
        read(optarg, *) multfile
        if (multfile(1:1)=="*") then
          read(multfile(2:), *) rscale
          results = results * rscale
        else
          allocate(multi, source=results)
          call readdata(multfile, multi)
          if (verbose) print*, "Multiplying array from "//trim(multfile)
          results = results * multi
          deallocate(multi)
        end if

      ! case("dt")
      !   read(optarg, *) multfile
      !   allocate(array, source=results)
      !   allocate(multi, source=results)
      !   if (trim(multfile) == '-') then
      !     !TODO:
      !     read(input_unit, *) multi
      !   else
      !     call readdata(multfile, multi)
      !   end if
      !   call SGEMM('N','N',nrow,ncol,ncol,1.0,results,ncol,multi,ncol,0.0,array,ncol)
      !   results = array
      !   deallocate(array)
      !   deallocate(multi)

      case("a")
        read(optarg, *) arrfile, multfile, rscale
        if (verbose) print*, "Adding array from "//trim(arrfile)

        allocate(array, source=results)
        allocate(multi, source=results)
        call readdata(arrfile, array)

        if (trim(multfile) /= '' .and. trim(multfile) /= '-') then
          call readdata(multfile, multi)
          array = array * multi
        end if

        results = results + array * rscale
        deallocate(array)
        deallocate(multi)

      case("sr")
        read(optarg, *) nskiprow

      case("sc")
        read(optarg, *) nskipcol

      case("o")
        read(optarg, *) roffset
        if (abs(roffset) > verysmall) results = results + roffset

      case("fm")
        fomt=adjustl(trim(optarg))

      case("lf")
        lfOutput = .true.

      case("h")
        call showhelp

      case("v")
        verbose = .true.

      case default
        !call perror('Unknown option: '//trim(optarg))
        stop
    end select
    if (nrow==0) exit
  end do

  if (nrow==0) then
    call perror("  Error: Dimension needs to be defined in the first argument")
  end if

  call write_reslts()
  contains

  subroutine write_reslts()

    do irow = 1, nrow
      do icol = 1, ncol
        if (.not. ieee_is_nan(results(icol, irow))) then
          if (abs(results(icol, irow)) < verysmall) results(icol, irow) = 0.
        end if
      end do
    end do

    !where(abs(results) > verylarge)
    !  results = 0.
    !end where
    if (trim(outfile)=='~') then
      ifile = output_unit ! print to IO
      if (lfOutput) flush(output_unit)
    else
      if (lfOutput) then
        open(newunit=ifile, file=outfile, status='replace', access='stream', form='unformatted')
      else
        open(newunit=ifile, file=outfile, status='replace')
      end if
    end if

    maxRowNameWidth = 0
    if (hasRowName==1) then
      do irow=1, nrow
        maxRowNameWidth = max(maxRowNameWidth, len_trim(rownames(irow)))
      end do
    end if
    if (hasColName==1) call write_lf_line(.true., 0)

    do irow=1, nrow
      call write_lf_line(.false., irow)
    end do
    if (trim(outfile)/='~') close(ifile)
    if (verbose) print*, 'Results have been written to "'//trim(outfile)//'" successfully'

  end subroutine write_reslts

  subroutine write_lf_line(header, row)
    logical, intent(in) :: header
    integer, intent(in) :: row
    character(:), allocatable :: line
    integer :: capacity, status
#if defined(_WIN32) || defined(__MINGW32__)
    character(kind=c_char,len=:), allocatable, target :: output
    integer(c_int) :: written, ok
    type(c_ptr) :: stdout_handle
#endif

    if (.not. lfOutput) then
      if (header) then
        write(ifile, '(A,x,*(A,x))') colnames(0)(:maxRowNameWidth), &
          (colnames(icol)(1:max(9,len_trim(colnames(icol)))),icol=1,ncol)
      else if (hasRowName==1) then
        write(ifile, "(A,x,"//trim(fomt(2:))) rownames(row)(:maxRowNameWidth), results(1:ncol, row)
      else
        write(ifile, trim(fomt)) results(1:ncol, row)
      end if
      return
    end if

    capacity = max(256, 32*ncol)
    do
      allocate(character(capacity) :: line)
      if (header) then
        write(line, '(A,x,*(A,x))', iostat=status) colnames(0)(:maxRowNameWidth), &
          (colnames(icol)(1:max(9,len_trim(colnames(icol)))),icol=1,ncol)
      else if (hasRowName==1) then
        write(line, "(A,x,"//trim(fomt(2:)), iostat=status) rownames(row)(:maxRowNameWidth), results(1:ncol, row)
      else
        write(line, trim(fomt), iostat=status) results(1:ncol, row)
      end if
      if (status == 0) exit
      if (status /= iostat_eor) call perror('Could not format output line.')
      deallocate(line)
      capacity = capacity*2
    end do
    if (trim(outfile)=='~') then
#if defined(_WIN32) || defined(__MINGW32__)
      output = trim(line)//achar(10)
      stdout_handle = get_std_handle(-11_c_int)
      if (.not. c_associated(stdout_handle)) call perror('Could not get stdout handle.')
      ok = write_file(stdout_handle, c_loc(output), int(len(output), c_int), written, c_null_ptr)
      if (ok == 0 .or. written /= len(output)) call perror('Could not write stdout.')
#else
      write(output_unit, '(A)') trim(line)
#endif
    else
      write(ifile) trim(line)//achar(10)
    end if
  end subroutine write_lf_line

  subroutine readdata(afile, arr)
  character(1024) :: afile
  real            :: arr(:,:)
  character(:), allocatable :: line
  integer         :: iskip

  if (trim(afile) == '-') then
    ifile = input_unit
  else
    open(newunit=ifile, file=trim(afile), status='old')
  end if

  do irow=1, nskiprow
    read(ifile, *)
  end do

  if (hasColName==1) then
    call read_record(ifile, line)
    call normalize_delimiters(line)
    do iskip=1, nskipcol
      call take_field(line, stemp(iskip))
    end do
    do icol=1-hasRowName, ncol
      call take_field(line, colnames(icol))
    end do
    rownames(0) = colnames(0)
  end if

  if (nskipcol==0) then
    if (hasRowName==1) then
      do irow=1, nrow
        call read_record(ifile, line)
        call normalize_delimiters(line)
        call read_named_row(line, arr(:, irow))
      end do
    else
      read(ifile, *) arr
    end if
  else
    if (hasRowName==1) then
      do irow=1, nrow
        call read_record(ifile, line)
        call normalize_delimiters(line)
        do iskip=1, nskipcol
          call take_field(line, stemp(iskip))
        end do
        call read_named_row(line, arr(:, irow))
      end do
    else
      do irow=1, nrow
        call read_record(ifile, line)
        call normalize_delimiters(line)
        do iskip=1, nskipcol
          call take_field(line, stemp(iskip))
        end do
        read(line, *) arr(:, irow)
      end do
    end if
  end if
  if (ifile /= input_unit) close(ifile)
  end subroutine

  subroutine read_record(unit, line)
    integer, intent(in) :: unit
    character(:), allocatable, intent(out) :: line
    character(4096) :: chunk
    integer :: nread, status

    line = ''
    do
      read(unit, '(A)', advance='no', size=nread, iostat=status) chunk
      if (nread > 0) line = line//chunk(:nread)
      if (status == iostat_eor) return
      if (status == iostat_end) call perror('Unexpected end of input file.')
      if (status /= 0) call perror('Error reading input file.')
    end do
  end subroutine

  subroutine read_named_row(line, values)
    character(*), intent(inout) :: line
    real, intent(out) :: values(:)

    call take_field(line, rownames(irow))
    read(line, *) values
  end subroutine

  subroutine normalize_delimiters(line)
    character(*), intent(inout) :: line
    integer :: i

    do i=1, len_trim(line)
      if (line(i:i) == ',' .or. line(i:i) == achar(9)) line(i:i) = ' '
    end do
  end subroutine

  subroutine take_field(line, field)
    character(*), intent(inout) :: line
    character(*), intent(out) :: field
    integer :: boundary

    line = adjustl(line)
    boundary = index(trim(line), ' ')
    if (boundary == 0) then
      field = trim(line)
      line = ''
    else
      field = line(:boundary-1)
      line = adjustl(line(boundary+1:))
    end if
  end subroutine

  subroutine filter()
    integer              :: idx0,idx1,idx2,idxc
    real                 :: rv
    character(30)        :: cname
    integer, allocatable :: idxs(:)
    logical, allocatable :: mask(:)

    idx0=index(optarg, '<')
    idx1=index(optarg, '>')
    idx2=index(optarg, '=')

    if (idx0==0 .and. idx1==0 .and. idx2==0) call perror('Incorrect filter clause: '//trim(optarg))
    allocate(idxs(nrow))
    do idxc=1, nrow
      idxs(idxc)=idxc
    end do

    if(idx0>0) then
      read(optarg(:idx0-1), *) cname
      idxc = get_idxcol(cname)
      if (idx2==0) then
        read(optarg(idx0+1:), *) rv
        mask = results(idxc,:)<rv
      else
        if (idx2 /= idx0 + 1) call perror('Incorrect filter clause: '//trim(optarg))
        read(optarg(idx0+2:), *) rv
        mask = results(idxc,:)<=rv
      end if
    elseif (idx1>0) then
      idx0 = idx1
      read(optarg(:idx0-1), *) cname
      idxc = get_idxcol(cname)
      if (idx2==0) then
        read(optarg(idx0+1:), *) rv
        mask = results(idxc,:)>rv
      else
        if (idx2 /= idx0 + 1) call perror('Incorrect filter clause: '//trim(optarg))
        read(optarg(idx0+2:), *) rv
        mask = results(idxc,:)>=rv
      end if
    else
      idx0 = idx2
      if (optarg(idx0-1:idx0-1)=='/') then
        read(optarg(:idx0-2), *) cname
        read(optarg(idx0+1:), *) rv
        idxc = get_idxcol(cname)
        mask = results(idxc,:)/=rv
      else if (optarg(idx0+1:idx0+1)=='=') then
        read(optarg(:idx0-1), *) cname
        read(optarg(idx0+2:), *) rv
        idxc = get_idxcol(cname)
        mask = results(idxc,:)==rv
      else
        call perror('Incorrect filter clause: '//trim(optarg))
      end if
    end if
    rownames(1:count(mask)) = pack(rownames(1:nrow),mask)
    call redefine_array(source=results(:,pack(idxs,mask)))
    if (nrow==0) call perror('Filter clause remove all data: '//trim(optarg))
  end subroutine

  integer function get_idxcol(cname)
    character(*)         :: cname
    integer              :: idxc
    logical              :: is_colindex
    get_idxcol = 0
    is_colindex = .true.
    do idxc=1, len(cname)
      if (cname(idxc:idxc)==" ") cycle
      if (cname(idxc:idxc)<"0" .or. cname(idxc:idxc)>"9") then
        is_colindex = .false.
        exit
      end if
    end do
    if (is_colindex) then
      read(cname, *) get_idxcol
    else
      do idxc=0, ncol
        if (trim(colnames(idxc))==trim(cname)) then
          get_idxcol = idxc
          return
        end if
      end do
      call perror('Could not identify column name: '//trim(cname))
    end if
  end function


  subroutine stack()
    character(30), allocatable   :: orig_rownames(:), orig_colnames(:)
    orig_rownames = rownames
    orig_colnames = colnames
    itemp = 0
    do irow=1, nrow
      do icol=1, ncol
        if (hasRowName+hasColName>0) then
          itemp = itemp + 1
          if (hasRowName+hasColName==2) then
            rownames(itemp) = trim(orig_rownames(irow))//"_"//trim(orig_colnames(icol))
          else if (hasRowName==1) then
            rownames(itemp) = trim(orig_rownames(irow))
          else
            rownames(itemp) = trim(orig_colnames(icol))
          end if
        end if
      end do
    end do
    if (hasRowName+hasColName>0) then
      hasRowName = 1
    else
      hasRowName = 0
    end if
    if (hasColName==1) colnames(1) = 'stacked_values'
    call redefine_array(source=reshape(results, [1,nrow*ncol]))
  end subroutine


  subroutine head_rows(count)
    integer, intent(in) :: count

    if (count < 1) call perror('The row count for --head must be greater than zero.')
    if (nrow < 1) call perror('No rows are available for --head.')
    call keep_rows(1, min(count, nrow))
  end subroutine


  subroutine tail_rows(count)
    integer, intent(in) :: count
    integer :: first

    if (count < 1) call perror('The row count for --tail must be greater than zero.')
    if (nrow < 1) call perror('No rows are available for --tail.')
    first = max(1, nrow - count + 1)
    call keep_rows(first, nrow)
  end subroutine


  subroutine keep_rows(first, last)
    integer, intent(in) :: first, last
    integer :: new_nrow

    if (first == 1 .and. last == nrow) return
    new_nrow = last - first + 1
    tmpnames = rownames
    results = results(1:ncol, first:last)
    rownames(1:new_nrow) = tmpnames(first:last)
    nrow = new_nrow
  end subroutine


  subroutine unique_rows()
    integer :: i, j, nkeep
    integer, allocatable :: kept(:)
    logical :: duplicate

    allocate(kept(nrow))
    nkeep = 0
    do i = 1, nrow
      duplicate = .false.
      do j = 1, nkeep
        if (all(results(1:ncol, i) == results(1:ncol, kept(j)))) then
          duplicate = .true.
          exit
        end if
      end do
      if (duplicate) cycle
      nkeep = nkeep + 1
      kept(nkeep) = i
    end do

    if (nkeep == nrow) return
    tmpnames = rownames
    results = results(1:ncol, kept(1:nkeep))
    rownames(1:nkeep) = tmpnames(kept(1:nkeep))
    nrow = nkeep
  end subroutine


  subroutine reverse_rows()
    integer :: i

    if (nrow < 2) return
    tmpnames = rownames
    results = results(1:ncol, nrow:1:-1)
    rownames(1:nrow) = tmpnames(nrow:1:-1)
  end subroutine


  subroutine subset()
    integer              :: idx0,idx1,idx2,nidx
    integer              :: idxs(9999)

    optarg=adjustl(optarg)
    idx0 = index(optarg, ':')
    idx1 = index(optarg, ',')
    if (optarg(1:1)/='c' .and. optarg(1:1)/='r') then
      call perror('The first character of the subsetting augument must be "r" or "c" to subset rows or columns, respectively.')
    end if

    if ((idx0>0 .and. idx1>0)) then
      call perror('Subsetting must be performed by slicing using ":" or by one or a list of rows or colums (seperated by ",").')
    end if

    if (idx0 > 0) then
      read(optarg(2:idx0-1),*) idx1
      read(optarg(idx0+1: ),*) idx2
      if (optarg(1:1)=='c') then
        ! print *, "Subsetting Column ", idx1, " to ", idx2
        if (idx1>ncol) call perror('Column index out of range: '//trim(optarg))
        if (idx1>idx2) call perror('Column index out of range: '//trim(optarg))
        if (idx2>ncol) idx2 = ncol
        if (idx1==1 .and. idx2==ncol) return
        ncol = idx2 - idx1 + 1
        results = results(idx1:idx2, 1:nrow)
        colnames(1:ncol)=colnames(idx1:idx2)
      else
        ! print *, "Subsetting Row ", idx1, " to ", idx2
        if (idx1>nrow) call perror('Row index out of range: '//trim(optarg))
        if (idx1>idx2) call perror('Row index out of range: '//trim(optarg))
        if (idx2>nrow) idx2 = nrow
        if (idx1==1 .and. idx2==nrow) return
        nrow = idx2 - idx1 + 1
        results = results(1:ncol, idx1:idx2)
        rownames(1:nrow)=rownames(idx1:idx2)
      end if
    else
      idx2 = 1
      nidx = 0
      do
        idx1 = idx2+1
        idx2 = index(optarg(idx1:), ",")
        if (idx2==0) exit
        idx2 = idx1+idx2-1
        nidx = nidx + 1
        read(optarg(idx1:idx2-1),*) idxs(nidx)
      end do
      nidx = nidx + 1
      read(optarg(idx1:),*) idxs(nidx)
      if (optarg(1:1)=='c') then
        call redefine_array(source=results(idxs(:nidx),:))
        colnames(1:ncol)=colnames(idxs(:nidx))
      else
        call redefine_array(source=results(:,idxs(:nidx)))
        rownames(1:nrow)=rownames(idxs(:nidx))
      end if
    end if

  end subroutine


  subroutine rtranspose()
    integer :: hasname
    call transpose_real_matrix(results)
    ntmp = ncol
    ncol = nrow
    nrow = ntmp
    tmpnames = rownames
    rownames = colnames
    colnames = tmpnames
    hasname = hasRowName
    hasRowName = hasColName
    hasColName = hasname
  end subroutine


  subroutine duplicate_rows(duplicate)
    integer :: duplicate

    allocate(trans, source=results)
    deallocate(results)
    allocate(results(ncol, nrow*duplicate))
    do irow = 1, duplicate
      results(:, (nrow*(irow-1)+1):nrow*irow) = trans
    end do
    deallocate(trans)
    nrow = nrow * duplicate
  end subroutine


  subroutine groupby()
    integer  :: ngroup, ir1, ig1, igroup(nrow)
    character(30)   :: colname

    read(optarg, *) colname, func
    icol = get_idxcol(colname)
    ngroup = 1
    igroup = 0
    igroup(1) = 1
    if (icol==0) then
      do irow = 2, nrow
        do ir1 = 1, irow
          if (rownames(irow)==rownames(ir1)) then
            igroup(irow) = igroup(ir1)
            exit
          end if
        end do
        if (igroup(irow)==0) then
          ngroup = ngroup + 1
          igroup(irow) = ngroup
        end if
      end do
    else
      do irow = 2, nrow
        do ir1 = 1, irow
          if (results(icol,irow)==results(icol,ir1)) then
            igroup(irow) = igroup(ir1)
            exit
          end if
        end do
        if (igroup(irow)==0) then
          ngroup = ngroup + 1
          igroup(irow) = ngroup
        end if
      end do
    end if

    allocate(trans(ncol, ngroup))
    do ig1 = 1, ngroup
      do icol = 1, ncol
        if (trim(func) == "centralize") trans(icol, ig1) = nanavg(pack(results(icol, :), igroup==ig1))
        if (trim(func) == "sum"       ) trans(icol, ig1) = nansum (pack(results(icol, :), igroup==ig1))
        if (trim(func) == "mean"      ) trans(icol, ig1) = nanavg(pack(results(icol, :), igroup==ig1))
        if (trim(func) == "std"       ) trans(icol, ig1) = nanstd (pack(results(icol, :), igroup==ig1))
        if (trim(func) == "max"       ) trans(icol, ig1) = nanmax (pack(results(icol, :), igroup==ig1))
        if (trim(func) == "min"       ) trans(icol, ig1) = nanmin (pack(results(icol, :), igroup==ig1))
      end do
    end do
    if (trim(func) == "centralize") then
      do irow=1, nrow
        results(:,irow) = results(:,irow) - trans(:, igroup(irow))
      end do
    else
      do ig1 = 2, ngroup
        do irow = 1, nrow
          if (igroup(irow) == ig1) then
            rownames(ig1) = rownames(irow)
            exit
          end if
        end do
      end do
      call redefine_array(trans)
    end if
    deallocate(trans)
  end subroutine

  subroutine moving_average()
    integer               :: ws         ! window size
    real                  :: alpha      ! decay factor
    real, allocatable     :: weights(:)
    character(30)   :: colname

    read(optarg, *) ws, alpha
    if (ws > nrow) call perror('Windows size must not be larger the number of rows.')

    ! calculate the weights
    allocate(weights(ws))
    weights(ws) = 1
    alpha = 1.0 - alpha
    do irow=ws-1, 1, -1
      weights(irow) = weights(irow+1) * alpha
    end do
    weights = weights / sum(weights)
    do irow=nrow, ws, -1
      do icol=1, ncol
        results(icol, irow) = sum(results(icol, irow-ws+1:irow) * weights)
      end do
    end do
    results(:, 1:ws-1) = ieee_value(1.0, ieee_quiet_nan)
  end subroutine


  subroutine applyfunc()
  integer itmp
  read(optarg, *) func
  call to_lower(func)
  if (trim(func)=="sort" .or. trim(func)=="sortrow") func="sortr"
  if (trim(func)=="sortcolumn" ) func="sortc"
  select case(adjustl(trim(func)))
  case ('abs'      ); results = abs     (results)
  case ('exp'      ); results = exp     (results)
  case ('log10'    ); results = log10   (results)
  case ('log'      ); results = log     (results)
  case ('sqrt'     ); results = sqrt    (results)
  case ('sinh'     ); results = sinh    (results)
  case ('cosh'     ); results = cosh    (results)
  case ('tanh'     ); results = tanh    (results)
  case ('sin'      ); results = sin     (results)
  case ('cos'      ); results = cos     (results)
  case ('tan'      ); results = tan     (results)
  case ('asin'     ); results = asin    (results)
  case ('acos'     ); results = acos    (results)
  case ('atan'     ); results = atan    (results)
  case ('int'      ); results = int     (results)
  case ('nint'     ); results = nint    (results)
  case ('floor'    ); results = floor   (results)
  case ('inverse'  ); results = 1.0    / results
  ! case ('matinv'   ); call matinv(results)
  case ('fraction' ); results = fraction(results)
  case ('diff1'    ); results(:,2:nrow) = add2dand1d(results(:,2:nrow), -results(:,1)); results(:,1)=0
  case ('diff'     ); results(:,2:nrow) = results(:,2:nrow)-results(:,1:nrow-1)
  case ('cbrt'     ); results = sign(abs(results)**(1.0/3.0), results)
  case ('max'      ); nrow=1; do icol=1, ncol; results(icol, 1)=nanmax(results(icol,:)); end do; rownames(1)="max"
  case ('min'      ); nrow=1; do icol=1, ncol; results(icol, 1)=nanmin(results(icol,:)); end do; rownames(1)="min"
  case ('sum'      ); nrow=1; do icol=1, ncol; results(icol, 1)=nansum(results(icol,:)); end do; rownames(1)="sum"
  case ('std'      ); nrow=1; do icol=1, ncol; results(icol, 1)=nanstd(results(icol,:)); end do; rownames(1)="std"
  case ('mean'     ); nrow=1; do icol=1, ncol; results(icol, 1)=nanavg(results(icol,:)); end do; rownames(1)="avg"
  case ('cumsum'   ); call cumsum()
  case ('sortr'    ); call mrgref(rownames(1:nrow), rowindex(1:nrow)); rownames(1:nrow)=rownames(rowindex(1:nrow)); results=results(:,rowindex(1:nrow))
  case ('sortc'    ); call mrgref(colnames(1:ncol), rowindex(1:ncol)); colnames(1:ncol)=colnames(rowindex(1:ncol)); results=results(rowindex(1:ncol),:)
  case ('transpose'); call rtranspose()
  case default
    call perror('Unknown function: '//trim(func))
  end select
  end subroutine

  subroutine cumsum()
    integer :: i
    do i = 2, nrow
      results(:,i) = results(:,i-1) + results(:,i)
    end do
  end subroutine

  function add2dand1d(a2d, b1d, dim) result(c)
    real, intent(in) :: a2d(:,:), b1d(:)
    real, allocatable :: c(:,:)
    integer, intent(in), optional :: dim
    integer :: shape_a(2), i, idim
    shape_a = shape(a2d)
    idim = 2
    if (present(dim)) idim = dim
    c = a2d + spread(b1d, idim, shape_a(idim))
  end function add2dand1d

  subroutine redefine_array(source)
    real  :: source(:,:)
    ncol = ubound(source, dim=1)
    nrow = ubound(source, dim=2)
    call replace_real_matrix(results, source)
  end subroutine

  ! subroutine matinv(matA)
  !   INTEGER              :: INFO, LWORK
  !   integer, external    :: ILAENV
  !   INTEGER, allocatable :: IPIV(:)
  !   REAL, allocatable    :: WORK(:)
  !   real, contiguous     :: matA(:,:)
  !   allocate(IPIV(nrow))

  !   call SGETRF( nrow, ncol, matA, ncol, IPIV, INFO )
  !   if (INFO /= 0) then
  !     call perror('Matrix factorization failed.')
  !   end if

  !   LWORK = nrow*ILAENV( 1, 'SGETRI', ' ', nrow, -1, -1, -1 )
  !   allocate(WORK(LWORK))

  !   call SGETRI( nrow, matA, ncol, IPIV, WORK, LWORK, INFO )
  !   if (INFO /= 0) then
  !     call perror('Matrix inverse failed.')
  !   end if
  ! end subroutine

  subroutine perror(msg)
    character(*) :: msg
    write(error_unit, '(A)') msg
    stop
  end subroutine

  subroutine showhelp()
    print "(A)", ''
    print "(A)", ''
    print "(A)", ' ArrayMath -d nrow ncol [options] -a arrayfile multiplyfile scale [options] [output]'
    print "(A)", ''
    print "(A)", '   ArrayMath calculates the sum of an offset and the product of one or more arrays, optional scaling arrays and scalers.'
    print "(A)", '   result = func(sum(array_i * multiarray_i * scale_i)**power + offset), `offset` can be applied after `func`.'
    print "(A)", ' '
    print "(A)", '   Arguments:'
    print "(A)", '      -d   or  --dim       shape of the array, i.e. nrow ncol; this needs to be defined in the first argument.'
    print "(A)", '      -a   or  --add       adding a new array, follwed by three arguments, if multiplyfile == "-", no multiarray is used. it can be defined multiple times'
    print "(A)", ' '
    print "(A)", '   Optional arguments:'
    do itemp=3, size(opts)
      print "(A)", '      -'//opts(itemp)%short//'  or  --'//opts(itemp)%name(:10)//trim(opts(itemp)%description)
    end do
    print "(A)", '      output               output file name; must be the final argument. If output=="~" or omitted, output will print to screen.'
    print "(A)", ' '
    print "(A)", '   Functions for -x or --func:'
    print "(A)", '     abs        computes the absolute values of the array'
    print "(A)", '     exp        computes the base e exponential of the array.'
    print "(A)", '     log10      computes the base 10 logarithm of the array.'
    print "(A)", '     log        computes the base e logarithm of the array.'
    print "(A)", '     sqrt       computes the square root of the array.'
    print "(A)", '     sinh       computes the inverse hyperbolic sine of the array.'
    print "(A)", '     cosh       computes the hyperbolic cosine of the array.'
    print "(A)", '     tanh       computes the hyperbolic tangent of the array.'
    print "(A)", '     sin        computes the sine of the array.'
    print "(A)", '     cos        computes the cosine of the array.'
    print "(A)", '     tan        computes the tangent of the array.'
    print "(A)", '     asin       computes the arcsine of its the array (inverse of SIN(X))'
    print "(A)", '     acos       computes the arccosine of the array (inverse of COS(X))'
    print "(A)", '     atan       computes the arctangent of the array(inverse of TAN(X)).'
    print "(A)", '     int        converts the array to integer type.'
    print "(A)", '     nint       rounds the array to the nearest whole number.'
    print "(A)", '     floor      returns the greatest integer less than or equal to the array.'
    print "(A)", '     inverse    computes the reciprocal of each element in the array.'
    ! print "(A)", '     matinv     computes the matrix inverse of the array, using the LU factorization.'
    print "(A)", '     fraction   returns the fractional part of the model representation of the array.'
    print "(A)", '     diff1      subtract the first row from the array.'
    print "(A)", '     diff       calculates the difference of each row and its previous row  row_i - row_(i-1).'
    print "(A)", '     cbrt       calculates the cubic root of the array. The new results have the same sign as the original array.'
    print "(A)", '     max        calculates the maximum values for each columns.'
    print "(A)", '     min        calculates the minimum values for each columns.'
    print "(A)", '     sum        calculates the sum for each columns.'
    print "(A)", '     std        calculates the standard deviation for each columns.'
    print "(A)", '     cumsum     computes the cumulatice sum along each columns.'
    print "(A)", '     mean       calculates the average for each columns.'
    print "(A)", '     transpose  transposes the array.'
    print "(A)", '     sortr      sorts array by row names.'
    print "(A)", '     sortc      sorts array by column names.'
    stop
    end subroutine
  end program
