module mod_numerical_recipes

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Local Variables
!==============================================================================

 	INTEGER(I4B) :: ncom
 	
	!REAL(SP), DIMENSION(:), POINTER :: pcom, xicom
	REAL(dp), DIMENSION(:), POINTER :: pcom, xicom

!==============================================================================
! General Commands
!==============================================================================

  contains

!==============================================================================
! Sort Two Arrays by Quicksort (Modified from sort())
!==============================================================================

! Sorts an array arr into ascending order using Quicksort, while making the
! corresponding rearrangement of the same-size array slave. arr and slave are
! replaced on output by its sorted rearrangement.

! Parameters: NN is the size of subarrays sorted by straight insertion and
! NSTACK is the required auxiliary storage.

	subroutine sort2(arr, slave)                            ! Edited
	
    !use nrtype
    use mod_nrtype
    !use nrutil, only: swap, nrerror
    use mod_nrutil, only: swap, nrerror
  
    implicit none
	
    ! Input/Output variables
    !real(sp), dimension(:), intent(inout) :: arr
    real(dp), dimension(:), intent(inout) :: arr
	  real(dp), dimension(:), intent(inout) :: slave        ! Inserted
	
	  ! Parameters
	  integer(i4b), parameter :: NN = 15, NSTACK = 50
	
    ! Local variables
    !real(sp) :: a
    real(dp) :: a
    real(dp) :: b                                         ! Inserted
    integer(i4b) :: n, k, i, j, jstack, l, r
    integer(i4b), dimension(NSTACK) :: istack
    logical(lgt) :: mask                                  ! Inserted
	
	  n = size(arr)
  	jstack=0
	  l=1
	  r=n
	
    do
      
      ! Insertion sort when subarray small enough
      if (r-l < NN) then
      
        do j = l+1, r
        
          a = arr(j)
          b = slave(j)                                    ! Inserted
          
          do i = j-1, l, -1
          
            if (arr(i) <= a) exit
            arr(i+1) = arr(i)
            slave(i+1) = slave(i)                         ! Inserted
          
          end do
          
          arr(i+1) = a
          slave(i+1) = b                                  ! Inserted
        
        end do
        
        if (jstack == 0) return
        
        ! Pop stack and begin a new round of partitioning
        r = istack(jstack)
        l = istack(jstack-1)
        jstack = jstack - 2
      
      ! Choose median of left, center, and right elements as partitioning
      ! element a. Also rearrange so that a(l) ≤ a(l+1) ≤ a(r)
      else
      
        k = (l+r)/2
        
        call swap(arr(k),arr(l+1))
        call swap(slave(k),slave(l+1))                    ! Inserted
        
        mask = (arr(l) > arr(r))                          ! Inserted
        call swap(arr(l), arr(r), mask)                   ! Edited
        call swap(slave(l), slave(r), mask)               ! Inserted
        
        mask = (arr(l+1) > arr(r))                        ! Inserted
        call swap(arr(l+1), arr(r), mask)                 ! Edited
        call swap(slave(l+1), slave(r), mask)             ! Inserted
        
        mask = (arr(l) > arr(l+1))                        ! Inserted
        call swap(arr(l), arr(l+1), mask)                 ! Edited
        call swap(slave(l), slave(l+1), mask)             ! Inserted
        
        ! Initialize pointers for partitioning
        i = l + 1
        j = r
        
        ! Partitioning element
        a = arr(l+1)
        b = slave(l+1)                                    ! Inserted
        
        ! Here is the meat
        do
          
          ! Scan up to find element >= a
          do
          
            i = i + 1
            if (arr(i) >= a) exit
          
          end do
          
          ! Scan down to find element <= a
          do
          
            j = j - 1
            if (arr(j) <= a) exit
          
          end do
          
          ! Pointers crossed. Exit with partitioning complete
          if (j < i) exit
          
          ! Exchange elements   
          call swap(arr(i), arr(j))
          call swap(slave(i), slave(j))                   ! Inserted
        
        end do
        
        ! Insert partitioning element
        arr(l+1) = arr(j)
        slave(l+1) = slave(j)                             ! Inserted
        arr(j) = a
        slave(j) = b                                      ! Inserted
        jstack = jstack + 2
        
        ! Push pointers to larger subarray on stack; process smaller subarray
        ! immediately
        if (jstack > NSTACK) call nrerror('sort: NSTACK too small')
        
        if (r-i+1 >= j-l) then
        
          istack(jstack) = r
          istack(jstack-1) = i
          r = j - 1
        
        else
        
          istack(jstack) = j - 1
          istack(jstack-1) = l
          l = i
        
        end if
      
      end if
      
    end do
	
	end subroutine sort2

!==============================================================================
! Bracket the Minimum of a Function
!==============================================================================

! Given a function func, and given distinct initial points ax and bx, this
! routine searches in the downhill direction (defined by the function as
! evaluated at the initial points) and returns new points ax, bx, cx that
! bracket a minimum of the function. Also returned are the function values at
! the three points, fa, fb, and fc.

! Parameters: gold is the default ratio by which successive intervals are
! magnified; glimit is the maximum magnification allowed for a parabolic-fit
! step.

  subroutine mnbrak(ax, bx, cx, fa, fb, fc, func)
  
    !use nrtype
    use mod_nrtype
    !use nrutil, only : swap
    use mod_nrutil, only: swap

    implicit none

    ! Input/Output variables
    !real(sp), intent(inout) :: ax, bx
    real(dp), intent(inout) :: ax, bx

    ! Output variables
    !real(sp), intent(out) :: cx, fa, fb, fc
    real(dp), intent(out) :: cx, fa, fb, fc
  
    ! Interface of the function
    interface
      function func(x)
        !use nrtype
        use mod_nrtype
        implicit none
        !real(sp), intent(in) :: x
        real(dp), intent(in) :: x
        !real(sp) :: func
        real(dp) :: func
      end function func
    end interface

    ! Parameters
    !real(sp), parameter :: gold = 1.618034_sp
    real(dp), parameter :: gold = 1.618034_dp
    !real(sp), parameter :: glimit = 100.0_sp
    real(dp), parameter :: glimit = 100.0_dp
    !real(sp), parameter :: tiny = 1.0e-20_sp
    real(dp), parameter :: tiny = 1.0e-20_dp
  
    ! Local variables
    !real(sp) :: fu, q, r, u, ulim
    real(dp) :: fu, q, r, u, ulim
    
    fa = func(ax)
    fb = func(bx)
      
    ! Switch roles of a and b so that we can go downhill in the direction
    ! from a to b
    if (fb > fa) then
      call swap(ax, bx)
      call swap(fa, fb)
    end if
      
    ! First guess for c    
    cx = bx + gold*(bx - ax)
    fc = func(cx)
  
    ! Do-while-loop: Keep returning here until we bracket
    do
        
      if (fb < fc) return
      
      ! Compute u by parabolic extrapolation from a, b, c. tiny is used
      ! to prevent any possible division by zero
      r = (bx - ax)*(fb - fc)
      q = (bx - cx)*(fb - fa)
      !u = bx-((bx-cx)*q-(bx-ax)*r)/(2.0_sp*sign(max(abs(q-r),tiny),q-r))
      u = bx-((bx-cx)*q-(bx-ax)*r)/(2.0_dp*sign(max(abs(q-r),tiny),q-r))
      ulim = bx + glimit*(cx - bx)
      
      ! We won’t go farther than this. Test various possibilities:
        
      ! Parabolic u is between b and c
      if ((bx-u)*(u-cx) > 0.0) then
          
        fu = func(u)
        
        ! Got a minimum between b and c
        if (fu < fc) then
            
          ax = bx
          fa = fb
          bx = u
          fb = fu
          return
        
        ! Got a minimum between a and u
        else if (fu > fb) then
            
          cx = u
          fc = fu
          return
            
        end if
          
        ! Parabolic fit was no use. Use default magnification
        u = cx + gold*(cx - bx)
        fu = func(u)
      
      ! Parabolic fit is between c and its allowed limit
      else if ((cx-u)*(u-ulim) > 0.0) then
          
        fu = func(u)
        
        if (fu < fc) then
            
          bx = cx
          cx = u
          u = cx + gold*(cx - bx)
          call shft(fb, fc, fu, func(u))
            
        end if
      
      ! Limit parabolic u to maximum allowed value
      else if ((u-ulim)*(ulim-cx) >= 0.0) then
          
        u = ulim
        fu = func(u)
      
      ! Reject parabolic u, use default magnification    
      else
          
        u = cx + gold*(cx - bx)
        fu = func(u)
          
      end if
        
      ! Eliminate oldest point and continue
      call shft(ax, bx, cx, u)
      call shft(fa, fb, fc, fu)
        
    end do
  
    contains

    subroutine shft(a, b, c, d)
      !real(sp), intent(out) :: a
      real(dp), intent(out) :: a
      !real(sp), intent(inout) :: b, c
      real(dp), intent(inout) :: b, c
      !real(sp), intent(in) :: d
      real(dp), intent(in) :: d
      a = b
      b = c
      c = d
    end subroutine shft
  
  end subroutine mnbrak

!==============================================================================
! Find Minimum of a Function by Brent's Method
!==============================================================================

! Given a function func, and given a bracketing triplet of abscissas ax, bx,
! cx (such that bx is between ax and cx, and func(bx) is less than both
! func(ax) and func(cx)), this routine isolates the minimum to a fractional
! precision of about tol using Brent’s method. The abscissa of the minimum is
! returned as xmin, and the minimum function value is returned as brent, the
! returned function value.

! Parameters: Maximum allowed number of iterations; golden ratio; and a small
! number that protects against trying to achieve fractional accuracy for a
! minimum that happens to be exactly zero.

  function brent(ax, bx, cx, func, tol, xmin)
  
    !use nrtype
    use mod_nrtype
    !use nrutil, only: nrerror
    use mod_nrutil, only: nrerror

    implicit none
    
    ! Input variables
    !real(sp), intent(in) :: ax, bx, cx, tol
    real(dp), intent(in) :: ax, bx, cx, tol
    
    ! Output variables
    !real(sp), intent(out) :: xmin
    real(dp), intent(out) :: xmin
    !real(sp) :: brent
    real(dp) :: brent
  
    ! Interface of the function
    interface
      function func(x)
        !use nrtype
        use mod_nrtype
        implicit none
        !real(sp), intent(in) :: x
        real(dp), intent(in) :: x
        !real(sp) :: func
        real(dp) :: func
      end function func
    end interface

    ! Parameters
    integer(i4b), parameter :: itmax = 1000!100
    !real(sp), parameter :: cgold = 0.3819660_sp
    real(dp), parameter :: cgold = 0.3819660_dp
    !real(sp), parameter :: zeps = 1.0e-3_sp*epsilon(ax)
    real(dp), parameter :: zeps = 1.0e-3_dp*epsilon(ax)
    
    ! Local variables
    integer(i4b) :: iter
    !real(sp) :: a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
    real(dp) :: a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm
    
    ! a and b must be in ascending order, though the input abscissas need
    ! not be    
    a = min(ax, cx)
    b = max(ax, cx)
    
    ! Initializations
    v = bx
    w = v
    x = v
    
    ! This will be the distance moved on the step before last
    e = 0.0
    
    ! Initializations
    fx = func(x)
    fv = fx
    fw = fx

    ! Main program loop
    do iter = 1, itmax
        
      !xm = 0.5_sp*(a + b)
      xm = 0.5_dp*(a + b)
      tol1 = tol*abs(x) + zeps
      !tol2 = 2.0_sp*tol1
      tol2 = 2.0_dp*tol1
        
      ! Test for done here
      !if (abs(x-xm) <= (tol2-0.5_sp*(b-a))) then
      if (abs(x-xm) <= (tol2-0.5_dp*(b-a))) then
          
        ! Arrive here ready to exit with best values
        xmin = x
        brent = fx
        return
          
      end if
        
      ! Construct a trial parabolic fit
      if (abs(e) > tol1) then
          
        r = (x - w)*(fx - fv)
        q = (x - v)*(fx - fw)
        p = (x - v)*q - (x - w)*r
        !q = 2.0_sp*(q - r)
        q = 2.0_dp*(q - r)
        if (q > 0.0) p = -p
        q = abs(q)
        etemp = e
        e = d
          
        ! The below conditions determine the acceptability of the
        ! parabolic fit. Here it is not o.k., so we take the golden
        ! section step into the larger of the two segments
        !if (abs(p) >= abs(0.5_sp*q*etemp) .or. &
        !    p <= q*(a-x) .or. p >= q*(b-x)) then
        if (abs(p) >= abs(0.5_dp*q*etemp) .or. &
          p <= q*(a-x) .or. p >= q*(b-x)) then
          
          e = merge(a-x, b-x, x >= xm )
          d = cgold*e
        
        ! Take the parabolic step    
        else
            
          d = p/q
          u = x + d
          if (u-a < tol2 .or. b-u < tol2) d = sign(tol1, xm-x)
          
        end if
      
      ! Take the golden section step into the larger of the two segments    
      else
      
        e = merge(a-x, b-x, x >= xm )
        d = cgold*e
          
      end if
        
      ! Arrive here with d computed either from parabolic fit, or else
      ! from golden section
      u = merge(x+d, x+sign(tol1, d), abs(d) >= tol1 )
      
      ! This is the one function evaluation per iteration
      fu = func(u)
        
      ! Now we have to decide what to do with our function evaluation
      if (fu <= fx) then
          
        if (u >= x) then
          a = x
        else
          b = x
        end if
          
        call shft(v, w, x, u)
        call shft(fv, fw, fx, fu)
      
      else
          
        if (u < x) then
          a = u
        else
          b = u
        end if
          
        if (fu <= fw .or. w == x) then
          v = w
          fv = fw
          w = u
          fw = fu
        else if (fu <= fv .or. v == x .or. v == w) then
          v = u
          fv = fu
        end if
          
      end if
        
    end do

    call nrerror('brent: exceed maximum iterations')

    contains

    subroutine shft(a, b, c, d)
      !real(sp), intent(out) :: a
      real(dp), intent(out) :: a
      !real(sp), intent(inout) :: b,c
      real(dp), intent(inout) :: b,c
      !real(sp), intent(in) :: d
      real(dp), intent(in) :: d
      a = b
      b = c
      c = d
    end subroutine shft
  
  end function brent

!==============================================================================
! Search an Ordered Table by Bisection
!==============================================================================

! Given an array xx(1:N), and given a value x, returns a value j such that x
! is between xx(j) and xx(j + 1). xx must be monotonic, either increasing or
! decreasing. j = 0 or j = N is returned to indicate that x is out of range.

  function locate(xx, x)

    !use nrtype
    use mod_nrtype
    
    implicit none
    
    ! Input variables
    !real(sp), dimension(:), intent(in) :: xx
    real(dp), dimension(:), intent(in) :: xx
    !real(sp), intent(in) :: x
    real(dp), intent(in) :: x
    
    ! Output variables
    integer(i4b) :: locate
    
    ! Local variables
    integer(i4b) :: n, jl, jm, ju
    logical :: ascnd
    
    ! Dimension of the array
    n = size(xx)
      
    ! True if ascending order of table, false otherwise.
    ascnd = (xx(n) >= xx(1))
    
    ! Initialize lower and upper limits.
    jl = 0
    ju = n + 1
      
    do
        
      ! Repeat until this condition is satisfied
      if (ju-jl <= 1) exit
      
      ! Compute a midpoint
      jm = (ju + jl)/2
        
      if (ascnd .eqv. (x >= xx(jm))) then
          
        ! Replace the lower limit
        jl = jm
          
      else
          
        ! Replace the upper limit
        ju = jm
          
      end if
        
    end do
      
    ! Then set the output, being careful with the endpoints
    if (x == xx(1)) then
        
      locate = 1
        
    else if (x == xx(n)) then
        
      locate = n - 1
        
    else
        
      locate = jl
        
    end if
      
  end function locate

end module mod_numerical_recipes