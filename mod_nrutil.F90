module mod_nrutil

!==============================================================================
! Modules
!==============================================================================

    !use nrtype
    use mod_nrtype

!==============================================================================
! General Commands
!==============================================================================

    implicit none

!==============================================================================
! Parameters for Crossover from Serial to Parallel Algorithms
!==============================================================================

    ! These are used only within this mod_nrutil module
    
    ! Each "npar2" must be less than or equal the corresponding "npar".
    integer(i4b), parameter :: npar_arth = 16, npar2_arth = 8
    integer(i4b), parameter :: npar_geop = 4, npar2_geop = 2
    
    integer(i4b), parameter :: npar_cumsum = 16
    integer(i4b), parameter :: npar_cumprod = 8
    integer(i4b), parameter :: npar_poly = 8
    integer(i4b), parameter :: npar_polyterm = 8

!==============================================================================
! Generic Interfaces for Routines with Overloaded Versions
!==============================================================================

! Naming conventions for appended codes in the names of overloaded routines
! are as follows:
!   r = real;
!   d = double-precision;
!   i = integer;
!   c = complex;
!   z = double-precision complex;
!   h = character;
!   l = logical.

! Any of r, d, i, c, z, h, l may be followed by v=vector or m=matrix.
! (v, m suffixes are used only when needed to resolve ambiguities)

!==============================================================================
! Routines that Move Data
!==============================================================================

	interface array_copy
		module procedure array_copy_r, array_copy_d, array_copy_i
	end interface
	
	interface swap
		module procedure swap_i,swap_r,swap_dp,swap_rv,swap_c, &
			swap_cv,swap_cm,swap_z,swap_zv,swap_zm, &
			masked_swap_rs,masked_swap_ds,masked_swap_rv,masked_swap_rm
	end interface
	
	interface reallocate
		module procedure reallocate_rv,reallocate_rm,&
			reallocate_iv,reallocate_im,reallocate_hv
	end interface

!==============================================================================
! Routines Returning a Location as an Integer Value
!==============================================================================

    ! "ifirstloc", "iminloc" are not currently overloaded and so do not have a
    ! generic interface here

	interface imaxloc
		module procedure imaxloc_r,imaxloc_i
	end interface

!==============================================================================
! Routines for Argument Checking and Error Handling
!==============================================================================

    ! "nrerror" is not currently overloaded

	interface assert
		module procedure assert1,assert2,assert3,assert4,assert_v
	end interface
	
	interface assert_eq
		module procedure assert_eq2,assert_eq3,assert_eq4,assert_eqn
	end interface

!==============================================================================
! Routines Relating to Polynomials and Recurrences
!==============================================================================
	
	! "cumprod", "zroots_unity" are not currently overloaded
	
	interface arth
		module procedure arth_r, arth_d, arth_i
	end interface
	
	interface geop
		module procedure geop_r, geop_d, geop_i, geop_c, geop_dv
	end interface
	
	interface cumsum
		module procedure cumsum_r, cumsum_d, cumsum_i
	end interface
	
	interface poly
		module procedure poly_rr, poly_rrv, poly_dd, poly_ddv, &
			poly_rc, poly_cc, poly_msk_rrv, poly_msk_ddv
	end interface
	
	interface poly_term
		module procedure poly_term_rr, poly_term_cc
	end interface

!==============================================================================
! Routines for “Outer” Operations on Vectors
!==============================================================================

    ! "outerand", "outersum", "outerdiv" are not currently overloaded

	interface outerprod
		module procedure outerprod_r,outerprod_d
	end interface
	
	interface outerdiff
		module procedure outerdiff_r,outerdiff_d,outerdiff_i
	end interface

!==============================================================================
! Routines for Scatter-with-Combine
!==============================================================================
	
	interface scatter_add
		module procedure scatter_add_r,scatter_add_d
	end interface
	
	interface scatter_max
		module procedure scatter_max_r,scatter_max_d
	end interface

!==============================================================================
! Routines for Skew Operations on Matrices
!==============================================================================
	
	! "unit_matrix", "lower_triangle, "upper_triangle" not currently
	! overloaded
	
	interface diagadd
		module procedure diagadd_rv,diagadd_r
	end interface
	
	interface diagmult
		module procedure diagmult_rv,diagmult_r
	end interface
	
	interface get_diag
		module procedure get_diag_rv, get_diag_dv
	end interface
	
	interface put_diag
		module procedure put_diag_rv, put_diag_r
	end interface

!==============================================================================
! General Commands
!==============================================================================

    contains

!==============================================================================
! Routines that Move Data
!==============================================================================
    
	subroutine array_copy_r(src,dest,n_copied,n_not_copied)
    real(sp), dimension(:), intent(in) :: src
    real(sp), dimension(:), intent(out) :: dest
    integer(i4b), intent(out) :: n_copied, n_not_copied
    n_copied=min(size(src),size(dest))
    n_not_copied=size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
	end subroutine array_copy_r

	subroutine array_copy_d(src,dest,n_copied,n_not_copied)
    real(dp), dimension(:), intent(in) :: src
    real(dp), dimension(:), intent(out) :: dest
    integer(i4b), intent(out) :: n_copied, n_not_copied
    n_copied=min(size(src),size(dest))
    n_not_copied=size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
	end subroutine array_copy_d

	subroutine array_copy_i(src,dest,n_copied,n_not_copied)
    integer(i4b), dimension(:), intent(in) :: src
    integer(i4b), dimension(:), intent(out) :: dest
    integer(i4b), intent(out) :: n_copied, n_not_copied
    n_copied=min(size(src),size(dest))
    n_not_copied=size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
	end subroutine array_copy_i

	subroutine swap_i(a,b)
    integer(i4b), intent(inout) :: a,b
    integer(i4b) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_i

	subroutine swap_r(a,b)
    real(sp), intent(inout) :: a,b
    real(sp) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_r
	
	!------------------------------
	subroutine swap_dp(a,b)
    real(dp), intent(inout) :: a,b
    real(dp) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_dp
  !------------------------------

	subroutine swap_rv(a,b)
    real(sp), dimension(:), intent(inout) :: a,b
    real(sp), dimension(size(a)) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_rv

	subroutine swap_c(a,b)
    complex(spc), intent(inout) :: a,b
    complex(spc) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_c

	subroutine swap_cv(a,b)
    complex(spc), dimension(:), intent(inout) :: a,b
    complex(spc), dimension(size(a)) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_cv

	subroutine swap_cm(a,b)
    complex(spc), dimension(:,:), intent(inout) :: a,b
    complex(spc), dimension(size(a,1),size(a,2)) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_cm

	subroutine swap_z(a,b)
    complex(dpc), intent(inout) :: a,b
    complex(dpc) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_z

	subroutine swap_zv(a,b)
    complex(dpc), dimension(:), intent(inout) :: a,b
    complex(dpc), dimension(size(a)) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_zv

	subroutine swap_zm(a,b)
    complex(dpc), dimension(:,:), intent(inout) :: a,b
    complex(dpc), dimension(size(a,1),size(a,2)) :: dum
    dum=a
    a=b
    b=dum
	end subroutine swap_zm

	subroutine masked_swap_rs(a,b,mask)
    real(sp), intent(inout) :: a,b
    logical(lgt), intent(in) :: mask
    real(sp) :: swp
    if (mask) then
      swp=a
      a=b
      b=swp
    end if
	end subroutine masked_swap_rs
  
  !------------------------------
  subroutine masked_swap_ds(a,b,mask)
    real(dp), intent(inout) :: a,b
    logical(lgt), intent(in) :: mask
    real(dp) :: swp
    if (mask) then
      swp=a
      a=b
      b=swp
    end if
	end subroutine masked_swap_ds
  !------------------------------
  
	subroutine masked_swap_rv(a,b,mask)
    real(sp), dimension(:), intent(inout) :: a,b
    logical(lgt), dimension(:), intent(in) :: mask
    real(sp), dimension(size(a)) :: swp
    where (mask)
      swp=a
      a=b
      b=swp
    end where
	end subroutine masked_swap_rv

	subroutine masked_swap_rm(a,b,mask)
    real(sp), dimension(:,:), intent(inout) :: a,b
    logical(lgt), dimension(:,:), intent(in) :: mask
    real(sp), dimension(size(a,1),size(a,2)) :: swp
    where (mask)
      swp=a
      a=b
      b=swp
    end where
	end subroutine masked_swap_rm

	function reallocate_rv(p,n)
    real(sp), dimension(:), pointer :: p, reallocate_rv
    integer(i4b), intent(in) :: n
    integer(i4b) :: nold,ierr
    allocate(reallocate_rv(n),stat=ierr)
    if (ierr /= 0) call &
      nrerror('reallocate_rv: problem in attempt to allocate memory')
    if (.not. associated(p)) return
    nold=size(p)
    reallocate_rv(1:min(nold,n))=p(1:min(nold,n))
    deallocate(p)
	end function reallocate_rv

	function reallocate_iv(p,n)
    integer(i4b), dimension(:), pointer :: p, reallocate_iv
    integer(i4b), intent(in) :: n
    integer(i4b) :: nold,ierr
    allocate(reallocate_iv(n),stat=ierr)
    if (ierr /= 0) call &
      nrerror('reallocate_iv: problem in attempt to allocate memory')
    if (.not. associated(p)) return
    nold=size(p)
    reallocate_iv(1:min(nold,n))=p(1:min(nold,n))
    deallocate(p)
	end function reallocate_iv

	function reallocate_hv(p,n)
    character(1), dimension(:), pointer :: p, reallocate_hv
    integer(i4b), intent(in) :: n
    integer(i4b) :: nold,ierr
    allocate(reallocate_hv(n),stat=ierr)
    if (ierr /= 0) call &
      nrerror('reallocate_hv: problem in attempt to allocate memory')
    if (.not. associated(p)) return
    nold=size(p)
    reallocate_hv(1:min(nold,n))=p(1:min(nold,n))
    deallocate(p)
	end function reallocate_hv

	function reallocate_rm(p,n,m)
    real(sp), dimension(:,:), pointer :: p, reallocate_rm
    integer(i4b), intent(in) :: n,m
    integer(i4b) :: nold,mold,ierr
    allocate(reallocate_rm(n,m),stat=ierr)
    if (ierr /= 0) call &
      nrerror('reallocate_rm: problem in attempt to allocate memory')
    if (.not. associated(p)) return
    nold=size(p,1)
    mold=size(p,2)
    reallocate_rm(1:min(nold,n),1:min(mold,m))=&
      p(1:min(nold,n),1:min(mold,m))
    deallocate(p)
	end function reallocate_rm

	function reallocate_im(p,n,m)
    integer(i4b), dimension(:,:), pointer :: p, reallocate_im
    integer(i4b), intent(in) :: n,m
    integer(i4b) :: nold,mold,ierr
    allocate(reallocate_im(n,m),stat=ierr)
    if (ierr /= 0) call &
      nrerror('reallocate_im: problem in attempt to allocate memory')
    if (.not. associated(p)) return
    nold=size(p,1)
    mold=size(p,2)
    reallocate_im(1:min(nold,n),1:min(mold,m))=&
      p(1:min(nold,n),1:min(mold,m))
    deallocate(p)
	end function reallocate_im

!==============================================================================
! Routines Returning a Location as an Integer Value
!==============================================================================

	function ifirstloc(mask)
	logical(lgt), dimension(:), intent(in) :: mask
	integer(i4b) :: ifirstloc
	integer(i4b), dimension(1) :: loc
	loc=maxloc(merge(1,0,mask))
	ifirstloc=loc(1)
	if (.not. mask(ifirstloc)) ifirstloc=size(mask)+1
	end function ifirstloc

	function imaxloc_r(arr)
	real(sp), dimension(:), intent(in) :: arr
	integer(i4b) :: imaxloc_r
	integer(i4b), dimension(1) :: imax
	imax=maxloc(arr(:))
	imaxloc_r=imax(1)
	end function imaxloc_r

	function imaxloc_i(iarr)
	integer(i4b), dimension(:), intent(in) :: iarr
	integer(i4b), dimension(1) :: imax
	integer(i4b) :: imaxloc_i
	imax=maxloc(iarr(:))
	imaxloc_i=imax(1)
	end function imaxloc_i

	function iminloc(arr)
	real(sp), dimension(:), intent(in) :: arr
	integer(i4b), dimension(1) :: imin
	integer(i4b) :: iminloc
	imin=minloc(arr(:))
	iminloc=imin(1)
	end function iminloc

!==============================================================================
! Routines for Argument Checking and Error Handling
!==============================================================================

	subroutine assert1(n1,string)
	character(len=*), intent(in) :: string
	logical, intent(in) :: n1
	if (.not. n1) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		stop 'program terminated by assert1'
	end if
	end subroutine assert1

	subroutine assert2(n1,n2,string)
	character(len=*), intent(in) :: string
	logical, intent(in) :: n1,n2
	if (.not. (n1 .and. n2)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		stop 'program terminated by assert2'
	end if
	end subroutine assert2

	subroutine assert3(n1,n2,n3,string)
	character(len=*), intent(in) :: string
	logical, intent(in) :: n1,n2,n3
	if (.not. (n1 .and. n2 .and. n3)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		stop 'program terminated by assert3'
	end if
	end subroutine assert3

	subroutine assert4(n1,n2,n3,n4,string)
	character(len=*), intent(in) :: string
	logical, intent(in) :: n1,n2,n3,n4
	if (.not. (n1 .and. n2 .and. n3 .and. n4)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		stop 'program terminated by assert4'
	end if
	end subroutine assert4

	subroutine assert_v(n,string)
	character(len=*), intent(in) :: string
	logical, dimension(:), intent(in) :: n
	if (.not. all(n)) then
		write (*,*) 'nrerror: an assertion failed with this tag:', &
			string
		stop 'program terminated by assert_v'
	end if
	end subroutine assert_v

	function assert_eq2(n1,n2,string)
	character(len=*), intent(in) :: string
	integer, intent(in) :: n1,n2
	integer :: assert_eq2
	if (n1 == n2) then
		assert_eq2=n1
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		stop 'program terminated by assert_eq2'
	end if
	end function assert_eq2

	function assert_eq3(n1,n2,n3,string)
	character(len=*), intent(in) :: string
	integer, intent(in) :: n1,n2,n3
	integer :: assert_eq3
	if (n1 == n2 .and. n2 == n3) then
		assert_eq3=n1
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		stop 'program terminated by assert_eq3'
	end if
	end function assert_eq3

	function assert_eq4(n1,n2,n3,n4,string)
	character(len=*), intent(in) :: string
	integer, intent(in) :: n1,n2,n3,n4
	integer :: assert_eq4
	if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
		assert_eq4=n1
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		stop 'program terminated by assert_eq4'
	end if
	end function assert_eq4

	function assert_eqn(nn,string)
	character(len=*), intent(in) :: string
	integer, dimension(:), intent(in) :: nn
	integer :: assert_eqn
	if (all(nn(2:) == nn(1))) then
		assert_eqn=nn(1)
	else
		write (*,*) 'nrerror: an assert_eq failed with this tag:', &
			string
		stop 'program terminated by assert_eqn'
	end if
	end function assert_eqn

	subroutine nrerror(string)
	character(len=*), intent(in) :: string
	write (*,*) 'nrerror: ',string
	stop 'program terminated by nrerror'
	end subroutine nrerror

!==============================================================================
! Routines Relating to Polynomials and Recurrences
!==============================================================================

	function arth_r(first,increment,n)
	real(sp), intent(in) :: first,increment
	integer(i4b), intent(in) :: n
	real(sp), dimension(n) :: arth_r
	integer(i4b) :: k,k2
	real(sp) :: temp
	if (n > 0) arth_r(1)=first
	if (n <= npar_arth) then
		do k=2,n
			arth_r(k)=arth_r(k-1)+increment
		end do
	else
		do k=2,npar2_arth
			arth_r(k)=arth_r(k-1)+increment
		end do
		temp=increment*npar2_arth
		k=npar2_arth
		do
			if (k >= n) exit
			k2=k+k
			arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
			temp=temp+temp
			k=k2
		end do
	end if
	end function arth_r

	function arth_d(first,increment,n)
	real(dp), intent(in) :: first,increment
	integer(i4b), intent(in) :: n
	real(dp), dimension(n) :: arth_d
	integer(i4b) :: k,k2
	real(dp) :: temp
	if (n > 0) arth_d(1)=first
	if (n <= npar_arth) then
		do k=2,n
			arth_d(k)=arth_d(k-1)+increment
		end do
	else
		do k=2,npar2_arth
			arth_d(k)=arth_d(k-1)+increment
		end do
		temp=increment*npar2_arth
		k=npar2_arth
		do
			if (k >= n) exit
			k2=k+k
			arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
			temp=temp+temp
			k=k2
		end do
	end if
	end function arth_d

	function arth_i(first,increment,n)
	integer(i4b), intent(in) :: first,increment,n
	integer(i4b), dimension(n) :: arth_i
	integer(i4b) :: k,k2,temp
	if (n > 0) arth_i(1)=first
	if (n <= npar_arth) then
		do k=2,n
			arth_i(k)=arth_i(k-1)+increment
		end do
	else
		do k=2,npar2_arth
			arth_i(k)=arth_i(k-1)+increment
		end do
		temp=increment*npar2_arth
		k=npar2_arth
		do
			if (k >= n) exit
			k2=k+k
			arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
			temp=temp+temp
			k=k2
		end do
	end if
	end function arth_i

	function geop_r(first,factor,n)
	real(sp), intent(in) :: first,factor
	integer(i4b), intent(in) :: n
	real(sp), dimension(n) :: geop_r
	integer(i4b) :: k,k2
	real(sp) :: temp
	if (n > 0) geop_r(1)=first
	if (n <= npar_geop) then
		do k=2,n
			geop_r(k)=geop_r(k-1)*factor
		end do
	else
		do k=2,npar2_geop
			geop_r(k)=geop_r(k-1)*factor
		end do
		temp=factor**npar2_geop
		k=npar2_geop
		do
			if (k >= n) exit
			k2=k+k
			geop_r(k+1:min(k2,n))=temp*geop_r(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	end function geop_r

	function geop_d(first,factor,n)
	real(dp), intent(in) :: first,factor
	integer(i4b), intent(in) :: n
	real(dp), dimension(n) :: geop_d
	integer(i4b) :: k,k2
	real(dp) :: temp
	if (n > 0) geop_d(1)=first
	if (n <= npar_geop) then
		do k=2,n
			geop_d(k)=geop_d(k-1)*factor
		end do
	else
		do k=2,npar2_geop
			geop_d(k)=geop_d(k-1)*factor
		end do
		temp=factor**npar2_geop
		k=npar2_geop
		do
			if (k >= n) exit
			k2=k+k
			geop_d(k+1:min(k2,n))=temp*geop_d(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	end function geop_d

	function geop_i(first,factor,n)
	integer(i4b), intent(in) :: first,factor,n
	integer(i4b), dimension(n) :: geop_i
	integer(i4b) :: k,k2,temp
	if (n > 0) geop_i(1)=first
	if (n <= npar_geop) then
		do k=2,n
			geop_i(k)=geop_i(k-1)*factor
		end do
	else
		do k=2,npar2_geop
			geop_i(k)=geop_i(k-1)*factor
		end do
		temp=factor**npar2_geop
		k=npar2_geop
		do
			if (k >= n) exit
			k2=k+k
			geop_i(k+1:min(k2,n))=temp*geop_i(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	end function geop_i

	function geop_c(first,factor,n)
	COMPLEX(sp), intent(in) :: first,factor
	integer(i4b), intent(in) :: n
	COMPLEX(sp), dimension(n) :: geop_c
	integer(i4b) :: k,k2
	COMPLEX(sp) :: temp
	if (n > 0) geop_c(1)=first
	if (n <= npar_geop) then
		do k=2,n
			geop_c(k)=geop_c(k-1)*factor
		end do
	else
		do k=2,npar2_geop
			geop_c(k)=geop_c(k-1)*factor
		end do
		temp=factor**npar2_geop
		k=npar2_geop
		do
			if (k >= n) exit
			k2=k+k
			geop_c(k+1:min(k2,n))=temp*geop_c(1:min(k,n-k))
			temp=temp*temp
			k=k2
		end do
	end if
	end function geop_c

	function geop_dv(first,factor,n)
	real(dp), dimension(:), intent(in) :: first,factor
	integer(i4b), intent(in) :: n
	real(dp), dimension(size(first),n) :: geop_dv
	integer(i4b) :: k,k2
	real(dp), dimension(size(first)) :: temp
	if (n > 0) geop_dv(:,1)=first(:)
	if (n <= npar_geop) then
		do k=2,n
			geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
		end do
	else
		do k=2,npar2_geop
			geop_dv(:,k)=geop_dv(:,k-1)*factor(:)
		end do
		temp=factor**npar2_geop
		k=npar2_geop
		do
			if (k >= n) exit
			k2=k+k
			geop_dv(:,k+1:min(k2,n))=geop_dv(:,1:min(k,n-k))*&
				spread(temp,2,size(geop_dv(:,1:min(k,n-k)),2))
			temp=temp*temp
			k=k2
		end do
	end if
	end function geop_dv

	recursive function cumsum_r(arr,seed) RESULT(ans)
	real(sp), dimension(:), intent(in) :: arr
	real(sp), optional, intent(in) :: seed
	real(sp), dimension(size(arr)) :: ans
	integer(i4b) :: n,j
	real(sp) :: sd
	n=size(arr)
	if (n == 0_i4b) return
	sd=0.0_sp
	if (present(seed)) sd=seed
	ans(1)=arr(1)+sd
	if (n < npar_cumsum) then
		do j=2,n
			ans(j)=ans(j-1)+arr(j)
		end do
	else
		ans(2:n:2)=cumsum_r(arr(2:n:2)+arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
	end if
	end function cumsum_r

  !------------------------------
	recursive function cumsum_d(arr,seed) RESULT(ans)
	real(dp), dimension(:), intent(in) :: arr
	real(dp), optional, intent(in) :: seed
	real(dp), dimension(size(arr)) :: ans
	integer(i4b) :: n,j
	real(dp) :: sd
	n=size(arr)
	if (n == 0_i4b) return
	sd=0.0_dp
	if (present(seed)) sd=seed
	ans(1)=arr(1)+sd
	if (n < npar_cumsum) then
		do j=2,n
			ans(j)=ans(j-1)+arr(j)
		end do
	else
		ans(2:n:2)=cumsum_d(arr(2:n:2)+arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
	end if
	end function cumsum_d
	!------------------------------

	recursive function cumsum_i(arr,seed) RESULT(ans)
	integer(i4b), dimension(:), intent(in) :: arr
	integer(i4b), optional, intent(in) :: seed
	integer(i4b), dimension(size(arr)) :: ans
	integer(i4b) :: n,j,sd
	n=size(arr)
	if (n == 0_i4b) return
	sd=0_i4b
	if (present(seed)) sd=seed
	ans(1)=arr(1)+sd
	if (n < npar_cumsum) then
		do j=2,n
			ans(j)=ans(j-1)+arr(j)
		end do
	else
		ans(2:n:2)=cumsum_i(arr(2:n:2)+arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
	end if
	end function cumsum_i
	
	recursive function cumprod(arr,seed) RESULT(ans)
	real(sp), dimension(:), intent(in) :: arr
	real(sp), optional, intent(in) :: seed
	real(sp), dimension(size(arr)) :: ans
	integer(i4b) :: n,j
	real(sp) :: sd
	n=size(arr)
	if (n == 0_i4b) return
	sd=1.0_sp
	if (present(seed)) sd=seed
	ans(1)=arr(1)*sd
	if (n < npar_cumprod) then
		do j=2,n
			ans(j)=ans(j-1)*arr(j)
		end do
	else
		ans(2:n:2)=cumprod(arr(2:n:2)*arr(1:n-1:2),sd)
		ans(3:n:2)=ans(2:n-1:2)*arr(3:n:2)
	end if
	end function cumprod

	function poly_rr(x,coeffs)
	real(sp), intent(in) :: x
	real(sp), dimension(:), intent(in) :: coeffs
	real(sp) :: poly_rr
	real(sp) :: pow
	real(sp), dimension(:), allocatable :: vec
	integer(i4b) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_rr=0.0_sp
	else if (n < npar_poly) then
		poly_rr=coeffs(n)
		do i=n-1,1,-1
			poly_rr=x*poly_rr+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_sp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_rr=vec(1)
		deallocate(vec)
	end if
	end function poly_rr

	function poly_dd(x,coeffs)
	real(dp), intent(in) :: x
	real(dp), dimension(:), intent(in) :: coeffs
	real(dp) :: poly_dd
	real(dp) :: pow
	real(dp), dimension(:), allocatable :: vec
	integer(i4b) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_dd=0.0_dp
	else if (n < npar_poly) then
		poly_dd=coeffs(n)
		do i=n-1,1,-1
			poly_dd=x*poly_dd+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_dp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_dd=vec(1)
		deallocate(vec)
	end if
	end function poly_dd

	function poly_rc(x,coeffs)
	complex(spc), intent(in) :: x
	real(sp), dimension(:), intent(in) :: coeffs
	complex(spc) :: poly_rc
	complex(spc) :: pow
	complex(spc), dimension(:), allocatable :: vec
	integer(i4b) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_rc=0.0_sp
	else if (n < npar_poly) then
		poly_rc=coeffs(n)
		do i=n-1,1,-1
			poly_rc=x*poly_rc+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_sp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_rc=vec(1)
		deallocate(vec)
	end if
	end function poly_rc

	function poly_cc(x,coeffs)
	complex(spc), intent(in) :: x
	complex(spc), dimension(:), intent(in) :: coeffs
	complex(spc) :: poly_cc
	complex(spc) :: pow
	complex(spc), dimension(:), allocatable :: vec
	integer(i4b) :: i,n,nn
	n=size(coeffs)
	if (n <= 0) then
		poly_cc=0.0_sp
	else if (n < npar_poly) then
		poly_cc=coeffs(n)
		do i=n-1,1,-1
			poly_cc=x*poly_cc+coeffs(i)
		end do
	else
		allocate(vec(n+1))
		pow=x
		vec(1:n)=coeffs
		do
			vec(n+1)=0.0_sp
			nn=ishft(n+1,-1)
			vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
			if (nn == 1) exit
			pow=pow*pow
			n=nn
		end do
		poly_cc=vec(1)
		deallocate(vec)
	end if
	end function poly_cc

	function poly_rrv(x,coeffs)
	real(sp), dimension(:), intent(in) :: coeffs,x
	real(sp), dimension(size(x)) :: poly_rrv
	integer(i4b) :: i,n,m
	m=size(coeffs)
	n=size(x)
	if (m <= 0) then
		poly_rrv=0.0_sp
	else if (m < n .or. m < npar_poly) then
		poly_rrv=coeffs(m)
		do i=m-1,1,-1
			poly_rrv=x*poly_rrv+coeffs(i)
		end do
	else
		do i=1,n
			poly_rrv(i)=poly_rr(x(i),coeffs)
		end do
	end if
	end function poly_rrv

	function poly_ddv(x,coeffs)
	real(dp), dimension(:), intent(in) :: coeffs,x
	real(dp), dimension(size(x)) :: poly_ddv
	integer(i4b) :: i,n,m
	m=size(coeffs)
	n=size(x)
	if (m <= 0) then
		poly_ddv=0.0_dp
	else if (m < n .or. m < npar_poly) then
		poly_ddv=coeffs(m)
		do i=m-1,1,-1
			poly_ddv=x*poly_ddv+coeffs(i)
		end do
	else
		do i=1,n
			poly_ddv(i)=poly_dd(x(i),coeffs)
		end do
	end if
	end function poly_ddv

	function poly_msk_rrv(x,coeffs,mask)
	real(sp), dimension(:), intent(in) :: coeffs,x
	logical(lgt), dimension(:), intent(in) :: mask
	real(sp), dimension(size(x)) :: poly_msk_rrv
	poly_msk_rrv=unpack(poly_rrv(pack(x,mask),coeffs),mask,0.0_sp)
	end function poly_msk_rrv

	function poly_msk_ddv(x,coeffs,mask)
	real(dp), dimension(:), intent(in) :: coeffs,x
	logical(lgt), dimension(:), intent(in) :: mask
	real(dp), dimension(size(x)) :: poly_msk_ddv
	poly_msk_ddv=unpack(poly_ddv(pack(x,mask),coeffs),mask,0.0_dp)
	end function poly_msk_ddv

	recursive function poly_term_rr(a,b) RESULT(u)
	real(sp), dimension(:), intent(in) :: a
	real(sp), intent(in) :: b
	real(sp), dimension(size(a)) :: u
	integer(i4b) :: n,j
	n=size(a)
	if (n <= 0) return
	u(1)=a(1)
	if (n < npar_polyterm) then
		do j=2,n
			u(j)=a(j)+b*u(j-1)
		end do
	else
		u(2:n:2)=poly_term_rr(a(2:n:2)+a(1:n-1:2)*b,b*b)
		u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
	end if
	end function poly_term_rr

	recursive function poly_term_cc(a,b) RESULT(u)
	complex(spc), dimension(:), intent(in) :: a
	complex(spc), intent(in) :: b
	complex(spc), dimension(size(a)) :: u
	integer(i4b) :: n,j
	n=size(a)
	if (n <= 0) return
	u(1)=a(1)
	if (n < npar_polyterm) then
		do j=2,n
			u(j)=a(j)+b*u(j-1)
		end do
	else
		u(2:n:2)=poly_term_cc(a(2:n:2)+a(1:n-1:2)*b,b*b)
		u(3:n:2)=a(3:n:2)+b*u(2:n-1:2)
	end if
	end function poly_term_cc

	function zroots_unity(n,nn)
	integer(i4b), intent(in) :: n,nn
	complex(spc), dimension(nn) :: zroots_unity
	integer(i4b) :: k
	real(sp) :: theta
	zroots_unity(1)=1.0
	theta=TWOPI/n
	k=1
	do
		if (k >= nn) exit
		zroots_unity(k+1)=cmplx(cos(k*theta),sin(k*theta),SPC)
		zroots_unity(k+2:min(2*k,nn))=zroots_unity(k+1)*&
			zroots_unity(2:min(k,nn-k))
		k=2*k
	end do
	end function zroots_unity

!==============================================================================
! Routines for “Outer” Operations on Vectors
!==============================================================================

    ! The order convention is:
    ! result(i,j) = first operand(i) (op) second operand(j).

	function outerprod_r(a,b)
	real(sp), dimension(:), intent(in) :: a,b
	real(sp), dimension(size(a),size(b)) :: outerprod_r
	outerprod_r = spread(a,dim=2,ncopies=size(b)) * &
		spread(b,dim=1,ncopies=size(a))
	end function outerprod_r

	function outerprod_d(a,b)
	real(dp), dimension(:), intent(in) :: a,b
	real(dp), dimension(size(a),size(b)) :: outerprod_d
	outerprod_d = spread(a,dim=2,ncopies=size(b)) * &
		spread(b,dim=1,ncopies=size(a))
	end function outerprod_d

	function outerdiv(a,b)
	real(sp), dimension(:), intent(in) :: a,b
	real(sp), dimension(size(a),size(b)) :: outerdiv
	outerdiv = spread(a,dim=2,ncopies=size(b)) / &
		spread(b,dim=1,ncopies=size(a))
	end function outerdiv

	function outersum(a,b)
	real(sp), dimension(:), intent(in) :: a,b
	real(sp), dimension(size(a),size(b)) :: outersum
	outersum = spread(a,dim=2,ncopies=size(b)) + &
		spread(b,dim=1,ncopies=size(a))
	end function outersum

	function outerdiff_r(a,b)
	real(sp), dimension(:), intent(in) :: a,b
	real(sp), dimension(size(a),size(b)) :: outerdiff_r
	outerdiff_r = spread(a,dim=2,ncopies=size(b)) - &
		spread(b,dim=1,ncopies=size(a))
	end function outerdiff_r

	function outerdiff_d(a,b)
	real(dp), dimension(:), intent(in) :: a,b
	real(dp), dimension(size(a),size(b)) :: outerdiff_d
	outerdiff_d = spread(a,dim=2,ncopies=size(b)) - &
		spread(b,dim=1,ncopies=size(a))
	end function outerdiff_d

	function outerdiff_i(a,b)
	integer(i4b), dimension(:), intent(in) :: a,b
	integer(i4b), dimension(size(a),size(b)) :: outerdiff_i
	outerdiff_i = spread(a,dim=2,ncopies=size(b)) - &
		spread(b,dim=1,ncopies=size(a))
	end function outerdiff_i

	function outerand(a,b)
	logical(lgt), dimension(:), intent(in) :: a,b
	logical(lgt), dimension(size(a),size(b)) :: outerand
	outerand = spread(a,dim=2,ncopies=size(b)) .and. &
		spread(b,dim=1,ncopies=size(a))
	end function outerand

!==============================================================================
! Routines for Scatter-with-Combine
!==============================================================================

	subroutine scatter_add_r(dest,source,dest_index)
	real(sp), dimension(:), intent(out) :: dest
	real(sp), dimension(:), intent(in) :: source
	integer(i4b), dimension(:), intent(in) :: dest_index
	integer(i4b) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_add_r')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
	end do
	end subroutine scatter_add_r
	
	subroutine scatter_add_d(dest,source,dest_index)
	real(dp), dimension(:), intent(out) :: dest
	real(dp), dimension(:), intent(in) :: source
	integer(i4b), dimension(:), intent(in) :: dest_index
	integer(i4b) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_add_d')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
	end do
	end subroutine scatter_add_d
	
	subroutine scatter_max_r(dest,source,dest_index)
	real(sp), dimension(:), intent(out) :: dest
	real(sp), dimension(:), intent(in) :: source
	integer(i4b), dimension(:), intent(in) :: dest_index
	integer(i4b) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_max_r')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=max(dest(i),source(j))
	end do
	end subroutine scatter_max_r
	
	subroutine scatter_max_d(dest,source,dest_index)
	real(dp), dimension(:), intent(out) :: dest
	real(dp), dimension(:), intent(in) :: source
	integer(i4b), dimension(:), intent(in) :: dest_index
	integer(i4b) :: m,n,j,i
	n=assert_eq2(size(source),size(dest_index),'scatter_max_d')
	m=size(dest)
	do j=1,n
		i=dest_index(j)
		if (i > 0 .and. i <= m) dest(i)=max(dest(i),source(j))
	end do
	end subroutine scatter_max_d

!==============================================================================
! Routines for Skew Operations on Matrices
!==============================================================================

	subroutine diagadd_rv(mat,diag)
	real(sp), dimension(:,:), intent(inout) :: mat
	real(sp), dimension(:), intent(in) :: diag
	integer(i4b) :: j,n
	n = assert_eq2(size(diag),min(size(mat,1),size(mat,2)),'diagadd_rv')
	do j=1,n
		mat(j,j)=mat(j,j)+diag(j)
	end do
	end subroutine diagadd_rv

	subroutine diagadd_r(mat,diag)
	real(sp), dimension(:,:), intent(inout) :: mat
	real(sp), intent(in) :: diag
	integer(i4b) :: j,n
	n = min(size(mat,1),size(mat,2))
	do j=1,n
		mat(j,j)=mat(j,j)+diag
	end do
	end subroutine diagadd_r

	subroutine diagmult_rv(mat,diag)
	real(sp), dimension(:,:), intent(inout) :: mat
	real(sp), dimension(:), intent(in) :: diag
	integer(i4b) :: j,n
	n = assert_eq2(size(diag),min(size(mat,1),size(mat,2)),'diagmult_rv')
	do j=1,n
		mat(j,j)=mat(j,j)*diag(j)
	end do
	end subroutine diagmult_rv

	subroutine diagmult_r(mat,diag)
	real(sp), dimension(:,:), intent(inout) :: mat
	real(sp), intent(in) :: diag
	integer(i4b) :: j,n
	n = min(size(mat,1),size(mat,2))
	do j=1,n
		mat(j,j)=mat(j,j)*diag
	end do
	end subroutine diagmult_r

	function get_diag_rv(mat)
	real(sp), dimension(:,:), intent(in) :: mat
	real(sp), dimension(size(mat,1)) :: get_diag_rv
	integer(i4b) :: j
	j=assert_eq2(size(mat,1),size(mat,2),'get_diag_rv')
	do j=1,size(mat,1)
		get_diag_rv(j)=mat(j,j)
	end do
	end function get_diag_rv

	function get_diag_dv(mat)
	real(dp), dimension(:,:), intent(in) :: mat
	real(dp), dimension(size(mat,1)) :: get_diag_dv
	integer(i4b) :: j
	j=assert_eq2(size(mat,1),size(mat,2),'get_diag_dv')
	do j=1,size(mat,1)
		get_diag_dv(j)=mat(j,j)
	end do
	end function get_diag_dv

	subroutine put_diag_rv(diagv,mat)
	real(sp), dimension(:), intent(in) :: diagv
	real(sp), dimension(:,:), intent(inout) :: mat
	integer(i4b) :: j,n
	n=assert_eq2(size(diagv),min(size(mat,1),size(mat,2)),'put_diag_rv')
	do j=1,n
		mat(j,j)=diagv(j)
	end do
	end subroutine put_diag_rv

	subroutine put_diag_r(scal,mat)
	real(sp), intent(in) :: scal
	real(sp), dimension(:,:), intent(inout) :: mat
	integer(i4b) :: j,n
	n = min(size(mat,1),size(mat,2))
	do j=1,n
		mat(j,j)=scal
	end do
	end subroutine put_diag_r

	subroutine unit_matrix(mat)
	real(sp), dimension(:,:), intent(out) :: mat
	integer(i4b) :: i,n
	n=min(size(mat,1),size(mat,2))
	mat(:,:)=0.0_sp
	do i=1,n
		mat(i,i)=1.0_sp
	end do
	end subroutine unit_matrix

	function upper_triangle(j,k,extra)
	integer(i4b), intent(in) :: j,k
	integer(i4b), optional, intent(in) :: extra
	logical(lgt), dimension(j,k) :: upper_triangle
	integer(i4b) :: n
	n=0
	if (present(extra)) n=extra
	upper_triangle=(outerdiff(arth_i(1,1,j),arth_i(1,1,k)) < n)
	end function upper_triangle

	function lower_triangle(j,k,extra)
	integer(i4b), intent(in) :: j,k
	integer(i4b), optional, intent(in) :: extra
	logical(lgt), dimension(j,k) :: lower_triangle
	integer(i4b) :: n
	n=0
	if (present(extra)) n=extra
	lower_triangle=(outerdiff(arth_i(1,1,j),arth_i(1,1,k)) > -n)
	end function lower_triangle

!==============================================================================
! Other routines
!==============================================================================

	function vabs(v)
	real(sp), dimension(:), intent(in) :: v
	real(sp) :: vabs
	vabs=sqrt(dot_product(v,v))
	end function vabs

end module mod_nrutil