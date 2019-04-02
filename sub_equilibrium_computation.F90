subroutine sub_equilibrium_computation

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype
  use mod_globals
  use mod_functions

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Local Parameters
!==============================================================================
  
  ! Maximum number of iterations
  integer, parameter :: maxiter = 20
  
  ! Convergence Tolerance
  real(dp), parameter :: tolKK = 0.01_dp    ! Aggregate capital
  real(dp), parameter :: tolLL = 0.01_dp    ! Aggregate labor
  real(dp), parameter :: tolBB = 0.01_dp    ! Bequest transfer
  real(dp), parameter :: tolpE = 0.01_dp    ! EHI premium
  real(dp), parameter :: tolpI = 0.01_dp    ! IHI premium
  real(dp), parameter :: tolchi = 0.01_dp   ! Wage reduction
  real(dp), parameter :: toltauC = 0.01_dp  ! Consumption tax rate
  
  ! Adjustment's Rate
  real(dp), parameter :: adjKK = 1.0_dp   ! Aggregate capital
  real(dp), parameter :: adjLL = 1.0_dp   ! Aggregate labor
  real(dp), parameter :: adjBB = 1.0_dp   ! Bequest transfer
  real(dp), parameter :: adjpE = 1.0_dp   ! EHI premium
  real(dp), parameter :: adjpI = 1.0_dp   ! IHI premium
  real(dp), parameter :: adjchi = 1.0_dp  ! Wage reduction
  real(dp), parameter :: adjtauC = 1.0_dp ! Consumption tax rate
  
!==============================================================================
! Local Variables
!==============================================================================

  ! Number of equilibrium iterations
  integer :: iter
  
  ! Distance between initial and updated equilibrium variables
  real(dp) :: distKK    ! Aggregate capital
  real(dp) :: distLL    ! Aggregate labor
  real(dp) :: distBB    ! Bequest transfer
  real(dp) :: distpE    ! EHI premium
  real(dp) :: distpI    ! IHI premium
  real(dp) :: distchi   ! Wage reduction
  real(dp) :: disttauC  ! Consumption tax rate

!==============================================================================
! Initial Guess for Equilibrium Variables
!==============================================================================

  ! Open file
  open(unit=1, file="./inout/equilibrium_variables.txt")
   
  read(1, *) KK0  ! Aggregate capital
  read(1, *) LL0  ! Aggregate labor
  read(1, *) BB0  ! Bequest transfer
  read(1, *) pE0  ! EHI premium
  
  do j = 1, RR-2
    do ih = 1, Nh
      read(1, *) pI0(ih, j) ! IHI premiums
    end do
  end do
  
  read(1, *) chi0   ! Wage reduction
  read(1, *) tauC0  ! Consumption tax rate
  
  ! Close file
  close(1)

!==============================================================================
! Initialize Updated Equilibrium Variables
!==============================================================================

  KK1 = KK0     ! Aggregate capital
  LL1 = LL0     ! Aggregate labor
  BB1 = BB0     ! Bequest transfer
  pE1 = pE0     ! EHI premium
  pI1 = pI0     ! IHI premiums
  chi1 = chi0   ! Wage reduction
  tauC1 = tauC0 ! Consumption tax rate

!==============================================================================
! Initialize Distances of Equilibrium Variables
!==============================================================================
  
  distKK = tolKK + 1.0_dp     ! Aggregate capital
  distLL = tolLL + 1.0_dp     ! Aggregate labor
  distBB = tolBB + 1.0_dp     ! Bequest transfer
  distpE = tolpE + 1.0_dp     ! EHI premium
  distpI = tolpI + 1.0_dp     ! IHI premiums
  distchi = tolchi + 1.0_dp   ! Wage reduction
  disttauC = toltauC + 1.0_dp ! Consumption tax rate

!==============================================================================
! Begin of Equilibrium Computation
!==============================================================================

  ! Initialize the number of iterations
  iter = 1
  
  !Loop: equilibrium
  do while ( (distKK   >= tolKK   .or. &
              distLL   >= tolLL   .or. &
              distBB   >= tolBB   .or. &
              distpE   >= tolpE   .or. &
              distpI   >= tolpI   .or. &
              distchi  >= tolchi  .or. &
              disttauC >= toltauC)     &
           .and.                       &
             (iter <= maxiter)         &
           )
    
    ! Update equilibrium variables
    KK = (1.0_dp - adjKK)*KK0 + adjKK*KK1           ! Aggregate capital
    LL = (1.0_dp - adjLL)*LL0 + adjLL*LL1           ! Aggregate labor
    BB = (1.0_dp - adjBB)*BB0 + adjBB*BB1           ! Bequest transfer
    pE = (1.0_dp - adjpE)*pE0 + adjpE*pE1           ! EHI premium
    pI = (1.0_dp - adjpI)*pI0 + adjpI*pI1           ! IHI premiums
    chi = (1.0_dp - adjchi)*chi0 + adjchi*chi1      ! Wage reduction
    tauC = (1.0_dp - adjtauC)*tauC0 + adjtauC*tauC1 ! Consumption tax rate
    
    ! Update initial guess of equilibrium variables
    KK0 = KK     ! Aggregate capital
    LL0 = LL     ! Aggregate labor
    BB0 = BB     ! Bequest transfer
    pE0 = pE     ! EHI premium
    pI0 = pI     ! IHI premiums
    chi0 = chi   ! Wage reduction
    tauC0 = tauC ! Consumption tax rate
        
!==============================================================================
! Factor Prices
!==============================================================================
        
    ! Interest rate
    r = alpha*AA*(KK/LL)**(-one_alpha) - delta
    
    ! Gross interest rate
    one_r = 1.0_dp + r

    ! Wage rate
    w = one_alpha*AA*(KK/LL)**alpha

!==============================================================================
! Health Insurance Premiums
!==============================================================================

    ! EHI
    p(iEE, :, :) = one_omega*pE
    
    ! IHI
    p(iII, :, :) = pI(:, :)

!==============================================================================
! Medicaid Income Limits
!==============================================================================
    
    ! Federal Poverty Level
    FPL = FPL_Y*FF(KK, LL)
    
    ! Test: don't run Medicaid Expansion
    if (polME == 0) then
    
      ! Medicaid income threshold
      yMA = yMA_FPL*FPL
    
    ! Test: run Medicaid Expansion
    else
      
      ! Medicaid income threshold
      yMA = zeta*FPL
      
    end if
    
    ! Medically Needy income threshold
    yMN = yMN_FPL*FPL

!==============================================================================
! Variable: 1 + tauC
!==============================================================================

    one_tauC = 1.0_dp + tauC
  
!==============================================================================
! Agent's Problem
!==============================================================================

    call sub_agents_problem

!==============================================================================
! Agent's Distribution
!==============================================================================

    call sub_agents_distribution

!==============================================================================
! Equilibrium Variables
!==============================================================================

    call sub_equilibrium_variables

!==============================================================================
! Updated Distances of Equilibrium Variables
!==============================================================================
        
    distKK = abs(KK0 - KK1)         ! Aggregate capital
    distLL = abs(LL0 - LL1)         ! Aggregate labor
    distBB = abs(BB0 - BB1)         ! Bequest transfer
    distpE = abs(pE0 - pE1)         ! EHI premium
    distpI = maxval(abs(pI0 - pI1)) ! IHI premiums
    distchi = abs(chi0 - chi1)      ! Wage reduction
    disttauC = abs(tauC0 - tauC1)   ! Consumption tax rate
        
!==============================================================================
! End of Equilibrium Computation
!==============================================================================
         
    ! Print iteration number
    print "(a)", repeat("-", 79)
    print "(a, i4)", "Iteration: ", iter
    print *, ""

    ! Print distances and equilibrium variables
    print "(a, f12.3, f14.2, f14.2)", "KK  ", distKK, KK0, KK1
    print "(a, f12.3, f14.2, f14.2)", "LL  ", distLL, LL0, LL1
    print "(a, f12.3, f14.2, f14.2)", "BB  ", distBB, BB0, BB1
    print "(a, f12.3, f14.2, f14.2)", "pE  ", distpE, pE0, pE1
    print "(a, f12.3, f14.2, f14.2)", "pI  ", distpI, pI0(hB, RR-2), pI1(hB, RR-2)
    print "(a, f12.3, f14.2, f14.2)", "chi ", distchi, chi0, chi1
    print "(a, f12.3, f14.4, f14.4)", "tauC", disttauC, tauC0, tauC1
    
    ! Print results to be matched
    print *, ""
    print "(a, f12.2)", "KK/YY", KK1/YY
    print "(a, f12.2)", "YY   ", YY
        
    ! Update iteration
    iter = iter + 1
        
  end do  ! while

end subroutine sub_equilibrium_computation