module mod_functions

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype
  use mod_nrutil
  use mod_numerical_recipes
  use mod_array_vec
  use mod_globals

!==============================================================================
! General Commands
!==============================================================================

  implicit none

  contains

!==============================================================================
! Indicator Function (=)
!==============================================================================

  function Ieq(x, y) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Values to be compared
    integer, intent(in) :: x, y
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: x = y
    if (x == y) then
    
      ! Returns 1
      fres = 1.0_dp
    
    ! Test: x <> y
    else
    
      ! Returns 0
      fres = 0.0_dp
    
    end if
  
  end function Ieq

!==============================================================================
! Indicator Function (>)
!==============================================================================

  function Igt(x, y) result(fres)
    
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Values to be compared
    real(dp), intent(in) :: x, y
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: x > y
    if (x > y) then
    
      ! Returns 1
      fres = 1.0_dp
    
    ! Test: x <= y
    else
    
      ! Returns 0
      fres = 0.0_dp
    
    end if
  
  end function Igt

!==============================================================================
! Utility Function
!==============================================================================

  function u(c, l, h) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Consumption
    real(dp), intent(in) :: c
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Health status
    integer, intent(in) :: h
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
              
    fres = (((c**gamma)*((ell - l - phi1*Igt(l, 0.0_dp) - phi2*Ieq(h, hB))**one_gamma))**one_sigma)/one_sigma
      
  end function u

!==============================================================================
! Bequest Utility Function
!==============================================================================

  function uB(ap) result(fres)
      
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------

    ! Next period's assets
    real(dp), intent(in) :: ap
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Function result
    real(dp) :: fres
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
    ! Bequest utility
    fres = psi1*((psi2 + ap)**(gamma*one_sigma))/one_sigma
      
  end function uB

!==============================================================================
! Wage Rate Function
!==============================================================================

  function wtil(iota) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Current EHI offer status
    integer, intent(in) :: iota
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Wage rate
    fres = w - chi*Ieq(iota, 2)
  
  end function wtil

!==============================================================================
! Labor Income Function
!==============================================================================

  function y(z, iota, l) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Current labor productivity
    real(dp), intent(in) :: z
    
    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Labor time
    real(dp), intent(in) :: l
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Labor income
    fres = wtil(iota)*z*l
  
  end function y

!==============================================================================
! Social Security Benefit Function - Primary Insurance Amount (PIA)
!==============================================================================

  function b(x) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Average lifetime earnings
    real(dp), intent(in) :: x
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
    ! Test: average lifetime earnings is less than or equal the first bend
    !       point
    if (x <= x1) then
        
      ! Social Security benefit
      fres = theta1*x
    
    ! Test: average lifetime earnings is greater than the first bend point
    !       and less than or equal the second bend point
    else if (x <= x2) then
        
      ! Social Security benefit
      fres = theta1*x1 + theta2*(x - x1)
    
    ! Test: average lifetime earnings is greater than the second bend point
    !       and less than or equal the SSWB
    else
        
      ! Social Security benefit
      fres = theta1*x1 + theta2*(x2 - x1) + theta3*(x - x2)
        
    end if
      
  end function b

!==============================================================================
! Eligibility Criteria for Medicaid
!==============================================================================

  function indMA(j, h, a, im, z, iota, i, l) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a

    ! Bin (index) of current medical expenditures
    integer, intent(in) :: im
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Current health insurance
    integer, intent(in) :: i
    
    ! Labor time
    real(dp), intent(in) :: l
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    integer :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: eligible for Medicaid
    if (y(z, iota, l) + r*a <= yMA) then
                    
      ! Eligible for Medicaid
      fres = 1
                        
    ! Test: eligible for Medically Needy
    else if (y(z, iota, l) + r*a - mtil(i, im, h, j) <= yMN .and. &
             a <= aMN) then
                    
      ! Eligible for Medicaid
      fres = 1
                        
    ! Test: not eligible for Medicaid
    else
                    
      ! Not eligible for Medicaid
      fres = 0
                        
    end if
  
  end function indMA

!==============================================================================
! Eligibility Criteria for EHI
!==============================================================================

  function indE(iota, l) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Labor time
    real(dp), intent(in) :: l
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    integer :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: eligible for EHI
    if (iiota == 2 .and. l > 0) then
                        
      ! Eligible for EHI
      fres = 1
    
    ! Test: not eligible for EHI
    else
      
      ! Not eligible for EHI
      fres = 0
      
    end if
  
  end function indE

!==============================================================================
! Set of Available Labor Times
!==============================================================================

  function setLT(i) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Current health insurance
    integer, intent(in) :: i
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
    
    ! Function result
    real(dp), dimension(Nl) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Initialize the function result
    fres = -1.0_dp
  
    ! Test: current health insurance is EHI
    if (i == iEE) then
    
      ! Add part-time as an option
      fres(1) = lP
      
      ! Add full-time as an option
      fres(2) = lF
    
    ! Test: current health insurance is not EHI
    else
    
      ! Add all options
      fres = [0.0_dp, lP, lF]
    
    end if
  
  end function setLT

!==============================================================================
! Set of Next Period's Health Insurances
!==============================================================================

  function setHI(j, h, a, im, z, iota, i, l) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a

    ! Bin (index) of current medical expenditures
    integer, intent(in) :: im
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Current health insurance
    integer, intent(in) :: i
    
    ! Labor time
    real(dp), intent(in) :: l
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
    
    ! Function result
    integer, dimension(Ni-2) :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
    
    ! Number of available health insurances
    integer :: NHI
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
    
    ! Initialize the function result
    fres = 0
    
    ! Initialize the number of available health insurances
    NHI = 0
    
    ! Test: eligible for Medicaid
    if (indMA(j, h, a, im, z, iota, i, l) == 1) then
      
      ! Update the number of available health insurances
      NHI = NHI + 1
      
      ! Add Medicaid as an option
      fres(NHI) = iMA
                      
    ! Test: not eligible for Medicaid
    else
      
      ! Update the number of available health insurances
      NHI = NHI + 1
           
      ! Add no coverage as an option
      fres(NHI) = i0
                      
    end if
                      
    ! Test: Eligible for EHI
    if (indE(iota, l) == 1) then
      
      ! Update the number of available health insurances
      NHI = NHI + 1
      
      ! Add EHI as an option
      fres(NHI) = iEE
                        
    end if
    
    ! Update the number of available health insurances
    NHI = NHI + 1
                
    ! Add IHI as an option
    fres(NHI) = iII
  
  end function setHI

!==============================================================================
! Taxable Income Function
!==============================================================================

  function yT(j, h, a, im, z, iota, i, l, ip) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
    
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a

    ! Bin (index) of current medical expenditures
    integer, intent(in) :: im
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Current health insurance
    integer, intent(in) :: i
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
    
    ! Taxable income
    fres = max( y(z, iota, l)                 &
              + r*a                           &
              - one_omega*pE*Ieq(ip, iEE)     &
              - max( mtil(i, im, h, j)        &
                   - xi*(y(z, iota, l) + r*a) &
                , 0.0_dp)                     &
           , 0.0_dp)
  
  end function yT

!==============================================================================
! Income Tax Function
!==============================================================================

  function tauY(yT) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Taxable income
    real(dp), intent(in) :: yT
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres

  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
    
    ! Test: taxable income is in the 1st bracket
    if (yT <= y1) then
      
      ! Income tax
      fres = tau1*yT
    
    ! Test: taxable income is in the 2nd bracket
    else if (yT <= y2) then
      
      ! Income tax
      fres = tau1*y1        &
           + tau2*(yT - y1)
    
    ! Test: taxable income is in the 3rd bracket
    else if (yT <= y3) then
      
      ! Income tax
      fres = tau1*y1        &
           + tau2*(y2 - y1) &
           + tau3*(yT - y2)
    
    ! Test: taxable income is in the 4th bracket
    else if (yT <= y4) then
      
      ! Income tax
      fres = tau1*y1        &
           + tau2*(y2 - y1) &
           + tau3*(y3 - y2) &
           + tau4*(yT - y3)
    
    ! Test: taxable income is in the 5th bracket
    else if (yT <= y5) then
      
      ! Income tax
      fres = tau1*y1        &
           + tau2*(y2 - y1) &
           + tau3*(y3 - y2) &
           + tau4*(y4 - y3) &
           + tau5*(yT - y4)
    
    ! Test: taxable income is in the 6th bracket
    else
      
      ! Income tax
      fres = tau1*y1        &
           + tau2*(y2 - y1) &
           + tau3*(y3 - y2) &
           + tau4*(y4 - y3) &
           + tau5*(y5 - y4) &
           + tau6*(yT - y5)
      
    end if
  
  end function tauY

!==============================================================================
! Social Security Tax Function
!==============================================================================

  function TSS(z, iota, l, ip) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Current labor productivity
    real(dp), intent(in) :: z
    
    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Social Security tax
    fres = tauSS*min( max( y(z, iota, l)             &
                         - one_omega*pE*Ieq(ip, iEE) &
                      , 0.0_dp)                      &
                 , ySS)
  
  end function TSS

!==============================================================================
! Medicare Tax Function
!==============================================================================

  function TMC(z, iota, l, ip) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Current labor productivity
    real(dp), intent(in) :: z
    
    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Medicare tax
    fres = tauMC*max( y(z, iota, l)             &
                    - one_omega*pE*Ieq(ip, iEE) &
                 , 0.0_dp)
  
  end function TMC

!==============================================================================
! Total Taxes Function
!==============================================================================

  function TT(j, h, a, im, z, iota, i, l, ip) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a

    ! Bin (index) of current medical expenditures
    integer, intent(in) :: im
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Current health insurance
    integer, intent(in) :: i
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Total taxes
    fres = tauY(yT(j, h, a, im, z, iota, i, l, ip)) &
         + TSS(z, iota, l, ip)                      &
         + TMC(z, iota, l, ip)
  
  end function TT

!==============================================================================
! Social Insurance Function
!==============================================================================

  function TSI(j, h, a, im, z, x, iota, i, l, ip, p) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a

    ! Bin (index) of current medical expenditures
    integer, intent(in) :: im
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current average lifetime earnings
    real(dp), intent(in) :: x

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Current health insurance
    integer, intent(in) :: i
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip

    ! Health insurance premium that is deductible
    real(dp), intent(in) :: p
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Social Insurance transfer
    fres = max( one_tauC*cbar                      &
              + TT(j, h, a, im, z, iota, i, l, ip) &
              + mtil(i, im, h, j)                  &
              + p                                  &
              - y(z, iota, l)                      &
              - b(x)                               &
              - one_r*a                            &
              - BB                                 &
           , 0.0_dp)
  
  end function TSI

!==============================================================================
! Production Function
!==============================================================================

  function FF(KK, LL) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Aggregate capital
    real(dp), intent(in) :: KK
    
    ! Aggregate labor
    real(dp), intent(in) :: LL
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
  
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
    
    ! Aggregate production
    fres = AA*(KK**alpha)*(LL**one_alpha)
    
  end function FF

!==============================================================================
! Affordable Health Insurance Criteria
!==============================================================================

  function indA(j, h, a, z, iota, l) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Labor time
    real(dp), intent(in) :: l
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------  
  
    ! Function result
    real(dp) :: fres
    
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: IHI is affordable
    if (indE(iota, l) == 0 .and. &
        pI(h, j) <= dI*(y(z, iota, l) + r*a)) then
    
      ! IHI is affordable
      fres = 1
    
    ! Test: EHI is affordable
    else if (indE(iota, l) == 1 .and. &
             one_omega*pE <= dE*(y(z, iota, l) + r*a)) then
    
      ! EHI is affordable
      fres = 1
    
    ! Test: HI is not affordable
    else
    
      ! HI is not affordable
      fres = 0
    
    end if
  
  end function indA

!==============================================================================
! Premium Tax Credits Function
!==============================================================================

  function TP(j, h, a, z, iota, l, ip) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------  
  
    ! Function result
    real(dp) :: fres
    
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: 
    if (indA(j, h, a, z, iota, l) == 0  .and. &
        ip == iII                       .and. &
        g1*FPL <= (y(z, iota, l) + r*a) .and. &
        g2*FPL >  (y(z, iota, l) + r*a)) then
    
      ! Premium Tax Credits
      fres = pI(h, j) - varepsilon1*(y(z, iota, l) + r*a)
    
    ! Test: 
    else if (indA(j, h, a, z, iota, l) == 0  .and. &
             ip == iII                       .and. &
             g2*FPL <= (y(z, iota, l) + r*a) .and. &
             g3*FPL >  (y(z, iota, l) + r*a)) then
    
      ! Premium Tax Credits
      fres = pI(h, j) - varepsilon2*(y(z, iota, l) + r*a)
    
    ! Test: 
    else if (indA(j, h, a, z, iota, l) == 0  .and. &
             ip == iII                       .and. &
             g3*FPL <= (y(z, iota, l) + r*a) .and. &
             g4*FPL >  (y(z, iota, l) + r*a)) then
    
      ! Premium Tax Credits
      fres = pI(h, j) - varepsilon3*(y(z, iota, l) + r*a)
    
    ! Test: 
    else if (indA(j, h, a, z, iota, l) == 0  .and. &
             ip == iII                       .and. &
             g4*FPL <= (y(z, iota, l) + r*a) .and. &
             g5*FPL >  (y(z, iota, l) + r*a)) then
    
      ! Premium Tax Credits
      fres = pI(h, j) - varepsilon4*(y(z, iota, l) + r*a)
    
    ! Test: 
    else if (indA(j, h, a, z, iota, l) == 0  .and. &
             ip == iII                       .and. &
             g5*FPL <= (y(z, iota, l) + r*a) .and. &
             g6*FPL >  (y(z, iota, l) + r*a)) then
    
      ! Premium Tax Credits
      fres = pI(h, j) - varepsilon5*(y(z, iota, l) + r*a)
    
    ! Test: 
    else if (indA(j, h, a, z, iota, l) == 0  .and. &
             ip == iII                       .and. &
             g6*FPL <= (y(z, iota, l) + r*a) .and. &
             g7*FPL >  (y(z, iota, l) + r*a)) then
    
      ! Premium Tax Credits
      fres = pI(h, j) - varepsilon6*(y(z, iota, l) + r*a)
    
    ! Test: 
    else
    
      ! Premium Tax Credits
      fres = 0.0_dp
    
    end if
  
  end function TP

!==============================================================================
! Individual Mandate Penalty Function
!==============================================================================

  function TM(j, h, a, im, z, iota, i, l, ip) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Age
    integer, intent(in) :: j
    
    ! Current health status
    integer, intent(in) :: h
    
    ! Current assets
    real(dp), intent(in) :: a

    ! Bin (index) of current medical expenditures
    integer, intent(in) :: im
    
    ! Current labor productivity
    real(dp), intent(in) :: z

    ! Current EHI offer status
    integer, intent(in) :: iota
    
    ! Current health insurance
    integer, intent(in) :: i
    
    ! Labor time
    real(dp), intent(in) :: l
    
    ! Next period's health insurance
    integer, intent(in) :: ip
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------  
  
    ! Function result
    real(dp) :: fres
    
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
  
    ! Test: pay the penalty
    if (indA(j, h, a, z, iota, l) == 1 .and. &
        ip == i0) then
    
      ! Penalty
      fres = min(max(upsilon, nu*yT(j, h, a, im, z, iota, i, l, ip)), rho)
    
    ! Test: don't pay the penalty
    else
    
      ! Penalty
      fres = 0.0_dp
    
    end if
  
  end function TM

!==============================================================================
! Worker-to-Worker Objective Function to be Optimized by the Brent's Method
!==============================================================================

  function objfunWW(ap) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
      
    ! Next period's assets
    real(dp), intent(in) :: ap
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
      
    ! Consumption
    real(dp) :: c
    
    ! Index of the lower bound of assets used in the interpolation/
    ! extrapolation
    integer :: ial

    ! Index of the upper bound of assets used in the interpolation/
    ! extrapolation
    integer :: iau
    
    ! Lower bound of assets used in the interpolation/extrapolation
    real(dp) :: al

    ! Upper bound of assets used in the interpolation/extrapolation
    real(dp) :: au
    
    ! Proportion of the value function assigned to the lower bound of assets
    ! in the interpolation/extrapolation
    real(dp) :: propal
    
    ! Proportion of the value function assigned to the upper bound of assets
    ! in the interpolation/extrapolation
    real(dp) :: propau
    
    ! Next period's average lifetime earnings
    real(dp) :: xp    
    
    ! Index of the lower bound of average lifetime earnings used in the
    ! interpolation/extrapolation
    integer :: ixl
    
    ! Index of the upper bound of average lifetime earnings used in the
    ! interpolation/extrapolation
    integer :: ixu
    
    ! Lower bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    real(dp) :: xl
    
    ! Upper bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    real(dp) :: xu
    
    ! Proportion of the value function assigned to the lower bound of average
    ! lifetime earnings in the interpolation/extrapolation
    real(dp) :: propxl
    
    ! Proportion of the value function assigned to the upper bound of average
    ! lifetime earnings in the interpolation/extrapolation
    real(dp) :: propxu
    
    ! Index of the vectorized state associated to:
    !   lower bound of assets
    !   lower bound of average lifetime earnings
    integer :: isll
    
    ! Index of the vectorized state associated to:
    !   lower bound of assets
    !   upper bound of average lifetime earnings
    integer :: islu
    
    ! Index of the vectorized state associated to:
    !   upper bound of assets
    !   lower bound of average lifetime earnings
    integer :: isul
    
    ! Index of the vectorized state associated to:
    !   upper bound of assets
    !   upper bound of average lifetime earnings
    integer :: isuu
    
    ! Next period's approximate value function
    real(dp) :: Vp
    
    ! Expected value function
    real(dp) :: EV
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
    ! Consumption
    c = (netres - ap)/one_tauC
      
    ! Test: negative consumption
    if (c <= 0.0_dp) then
        
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
                
    end if
      
    ! Test: next period's assets is limited by the first and last elements of
    !       the set of assets
    if (ap >= setA(1) .and. ap <= setA(Na)) then
      
      ! Index of the lower bound of assets used in the interpolation
      ial = locate(setA, ap)
      
      ! Index of the upper bound of assets used in the interpolation
      iau = ial + 1
      
    ! Test: next period's assets is greater than the last element of the set
    !       of assets
    else if (ap > setA(Na)) then
      
      ! Index of the lower bound of assets used in the extrapolation
      ial = Na - 1
      
      ! Index of the upper bound of assets used in the extrapolation
      iau = Na
      
    ! Test: next period's assets is less than the first element of the set of
    !       assets
    else
      
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
        
    end if
      
    ! Lower bound of assets used in the interpolation/extrapolation
    al = setA(ial)
        
    ! Upper bound of assets used in the interpolation/extrapolation
    au = setA(iau)
    
    ! Proportion of the value function assigned to the lower bound of assets
    ! in the interpolation/extrapolation
    propal = (au - ap)/(au - al)
    
    ! Proportion of the value function assigned to the upper bound of assets
    ! in the interpolation/extrapolation
    propau = 1.0_dp - propal
    
    ! Next period's average lifetime earnings
    xp = (x*(j-1) + min(y(z, iiota, l), ySS))/j
      
    ! Test: next period's average lifetime earnings is limited by the first
    !       and last elements of the set of average lifetime earnings
    if (xp >= setX(1) .and. xp <= setX(Nx)) then
      
      ! Index of the lower bound of average lifetime earnings used in the
      ! interpolation
      ixl = locate(setX, xp)
      
      ! Index of the upper bound of average lifetime earnings used in the
      ! interpolation
      ixu = ixl + 1
      
    ! Test: next period's average lifetime earnings is greater than the last
    !       element of the set of average lifetime earnings
    else if (xp > setX(Nx)) then
      
      ! Index of the lower bound of average lifetime earnings used in the
      ! extrapolation
      ixl = Nx - 1
      
      ! Index of the upper bound of average lifetime earnings used in the
      ! extrapolation
      ixu = Nx
      
    ! Test: next period's average lifetime earnings is less than the first
    !       element of the set of average lifetime earnings
    else
      
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
        
    end if
      
    ! Lower bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    xl = setX(ixl)
        
    ! Upper bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    xu = setX(ixu)
    
    ! Proportion of the value function assigned to the lower bound of average
    ! lifetime earnings in the interpolation/extrapolation
    propxl = (xu - xp)/(xu - xl)
    
    ! Proportion of the value function assigned to the upper bound of average
    ! lifetime earnings in the interpolation/extrapolation
    propxu = 1.0_dp - propxl       
    
    ! Initialize expected value function
    EV = 0.0_dp
      
    ! Loop: next period's health status
    do ihp = 1, Nh
    
      ! Loop: next period's medical expenditures
      do imp = 1, Nm
    
        ! Loop: next period's labor productivity
        do izp = 1, Nz
        
          ! Loop: next period's EHI offer status
          do iiotap = 1, Niota
        
            ! Index of the vectorized state associated to:
            !   lower bound of assets
            !   lower bound of average lifetime earnings
            isll = ind([j+1, ie, ihp, ial, imp, izp, ixl, iiotap, setIp(iip)], jumpW)
        
            ! Index of the vectorized state associated to:
            !   lower bound of assets
            !   upper bound of average lifetime earnings
            islu = ind([j+1, ie, ihp, ial, imp, izp, ixu, iiotap, setIp(iip)], jumpW)
        
            ! Index of the vectorized state associated to:
            !   upper bound of assets
            !   lower bound of average lifetime earnings
            isul = ind([j+1, ie, ihp, iau, imp, izp, ixl, iiotap, setIp(iip)], jumpW)
        
            ! Index of the vectorized state associated to:
            !   upper bound of assets
            !   upper bound of average lifetime earnings
            isuu = ind([j+1, ie, ihp, iau, imp, izp, ixu, iiotap, setIp(iip)], jumpW)
          
            ! Interpolate/Extrapolate to find the next period's value function
            Vp = propal*propxl*VW(isll) &
               + propal*propxu*VW(islu) &
               + propau*propxl*VW(isul) &
               + propau*propxu*VW(isuu)
          
            ! Expected value function
            EV = EV                                     &
               + PPHI(ihp, ih, ie, j)                   &
                 *PPSI(imp)                             &
                 *GGAMMA(iiotap, izp, iiota, iz, ie, j) &
                 *Vp
          
          end do  ! iiotap
          
        end do  ! izp
      
      end do  ! imp
      
    end do  ! ihp
      
    ! Return the negative of the value function
    fres = -( u(c, l, ih)                                          &
            + beta*(PPI(ih, ie, j)*EV + one_PPI(ih, ie, j)*uB(ap)) &
            )
      
  end function objfunWW

!==============================================================================
! Worker-to-Retired Objective Function to be Optimized by the Brent's Method
!==============================================================================

  function objfunWR(ap) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
      
    ! Next period's assets
    real(dp), intent(in) :: ap
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
  
    ! Consumption
    real(dp) :: c
    
    ! Index of the lower bound of assets used in the interpolation/
    ! extrapolation
    integer :: ial

    ! Index of the upper bound of assets used in the interpolation/
    ! extrapolation
    integer :: iau
    
    ! Lower bound of assets used in the interpolation/extrapolation
    real(dp) :: al

    ! Upper bound of assets used in the interpolation/extrapolation
    real(dp) :: au
    
    ! Proportion of the value function assigned to the lower bound of assets
    ! in the interpolation/extrapolation
    real(dp) :: propal
    
    ! Proportion of the value function assigned to the upper bound of assets
    ! in the interpolation/extrapolation
    real(dp) :: propau
    
    ! Next period's average lifetime earnings
    real(dp) :: xp    
    
    ! Index of the lower bound of average lifetime earnings used in the
    ! interpolation/extrapolation
    integer :: ixl
    
    ! Index of the upper bound of average lifetime earnings used in the
    ! interpolation/extrapolation
    integer :: ixu
    
    ! Lower bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    real(dp) :: xl
    
    ! Upper bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    real(dp) :: xu
    
    ! Proportion of the value function assigned to the lower bound of
    ! average lifetime earnings in the interpolation/extrapolation
    real(dp) :: propxl
    
    ! Proportion of the value function assigned to the upper bound of
    ! average lifetime earnings in the interpolation/extrapolation
    real(dp) :: propxu
    
    ! Index of the vectorized state associated to:
    !   lower bound of assets
    !   lower bound of average lifetime earnings
    integer :: isll
    
    ! Index of the vectorized state associated to:
    !   lower bound of assets
    !   upper bound of average lifetime earnings
    integer :: islu
    
    ! Index of the vectorized state associated to:
    !   upper bound of assets
    !   lower bound of average lifetime earnings
    integer :: isul
    
    ! Index of the vectorized state associated to:
    !   upper bound of assets
    !   upper bound of average lifetime earnings
    integer :: isuu
    
    ! Next period's approximate value function
    real(dp) :: Vp
    
    ! Expected value function
    real(dp) :: EV
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
    ! Consumption
    c = (netres - ap)/one_tauC
    
    ! Test: negative consumption
    if (c <= 0.0_dp) then
        
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
                
    end if
      
    ! Test: next period's assets is limited by the first and last elements of
    !       the set of assets
    if (ap >= setA(1) .and. ap <= setA(Na)) then
      
      ! Index of the lower bound of assets used in the interpolation
      ial = locate(setA, ap)
      
      ! Index of the upper bound of assets used in the interpolation
      iau = ial + 1
      
    ! Test: next period's assets is greater than the last element of the set
    !       of assets
    else if (ap > setA(Na)) then
      
      ! Index of the lower bound of assets used in the extrapolation
      ial = Na - 1
      
      ! Index of the upper bound of assets used in the extrapolation
      iau = Na
      
    ! Test: next period's assets is less than the first element of the set of
    !       assets
    else
      
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
        
    end if
      
    ! Lower bound of assets used in the interpolation/extrapolation
    al = setA(ial)
        
    ! Upper bound of assets used in the interpolation/extrapolation
    au = setA(iau)
    
    ! Proportion of the value function assigned to the lower bound of assets
    ! in the interpolation/extrapolation
    propal = (au - ap)/(au - al)
    
    ! Proportion of the value function assigned to the upper bound of assets
    ! in the interpolation/extrapolation
    propau = 1.0_dp - propal
    
    ! Next period's average lifetime earnings
    xp = (x*(j-1) + min(y(z, iiota, l), ySS))/j
      
    ! Test: next period's average lifetime earnings is limited by the first
    !       and last elements of the set of average lifetime earnings
    if (xp >= setX(1) .and. xp <= setX(Nx)) then
      
      ! Index of the lower bound of average lifetime earnings used in the
      ! interpolation
      ixl = locate(setX, xp)
      
      ! Index of the upper bound of average lifetime earnings used in the
      ! interpolation
      ixu = ixl + 1
      
    ! Test: next period's average lifetime earnings is greater than the last
    !       element of the set of average lifetime earnings
    else if (xp > setX(Nx)) then
      
      ! Index of the lower bound of average lifetime earnings used in the
      ! extrapolation
      ixl = Nx - 1
      
      ! Index of the upper bound of average lifetime earnings used in the
      ! extrapolation
      ixu = Nx
      
    ! Test: next period's average lifetime earnings is less than the first
    !       element of the set of average lifetime earnings
    else
      
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
        
    end if
      
    ! Lower bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    xl = setX(ixl)
        
    ! Upper bound of average lifetime earnings used in the interpolation/
    ! extrapolation
    xu = setX(ixu)
    
    ! Proportion of the value function assigned to the lower bound of average
    ! lifetime earnings in the interpolation/extrapolation
    propxl = (xu - xp)/(xu - xl)
    
    ! Proportion of the value function assigned to the upper bound of average
    ! lifetime earnings in the interpolation/extrapolation
    propxu = 1.0_dp - propxl
    
    ! Initializes expected value function
    EV = 0.0_dp
      
    ! Loop: next period's health status
    do ihp = 1, Nh
    
      ! Loop: next period's medical expenditures
      do imp = 1, Nm
    
        ! Index of the vectorized state associated to:
        !   lower bound of assets
        !   lower bound of average lifetime earnings
        isll = ind([(j-RR+1)+1, ie, ihp, ial, imp, ixl], jumpR)
      
        ! Index of the vectorized state associated to:
        !   lower bound of assets
        !   upper bound of average lifetime earnings
        islu = ind([(j-RR+1)+1, ie, ihp, ial, imp, ixu], jumpR)
      
        ! Index of the vectorized state associated to:
        !   upper bound of assets
        !   lower bound of average lifetime earnings
        isul = ind([(j-RR+1)+1, ie, ihp, iau, imp, ixl], jumpR)
      
        ! Index of the vectorized state associated to:
        !   upper bound of assets
        !   upper bound of average lifetime earnings
        isuu = ind([(j-RR+1)+1, ie, ihp, iau, imp, ixu], jumpR)
      
        ! Interpolate/Extrapolate to find the next period's value
        ! function
        Vp = propal*propxl*VR(isll) &
           + propal*propxu*VR(islu) &
           + propau*propxl*VR(isul) &
           + propau*propxu*VR(isuu)
         
        ! Expected value function
        EV = EV + PPHI(ihp, ih, ie, j)*PPSI(imp)*Vp
      
      end do  ! imp
      
    end do  ! ihp
      
    ! Return the negative of the value function
    fres = -( u(c, l, ih)                                          &
            + beta*(PPI(ih, ie, j)*EV + one_PPI(ih, ie, j)*uB(ap)) &
            )
      
  end function objfunWR

!==============================================================================
! Retired-to-Retired Objective Function to be Optimized by the Brent's Method
!==============================================================================

  function objfunRR(ap) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
      
    ! Next period's assets
    real(dp), intent(in) :: ap
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
  
    ! Consumption
    real(dp) :: c
    
    ! Index of the lower bound of assets used in the interpolation/
    ! extrapolation
    integer :: ial

    ! Index of the upper bound of assets used in the interpolation/
    ! extrapolation
    integer :: iau
    
    ! Lower bound of assets used in the interpolation/extrapolation
    real(dp) :: al

    ! Upper bound of assets used in the interpolation/extrapolation
    real(dp) :: au
    
    ! Proportion of the value function assigned to the lower bound of assets
    ! in the interpolation/extrapolation
    real(dp) :: propal
    
    ! Proportion of the value function assigned to the upper bound of assets
    ! in the interpolation/extrapolation
    real(dp) :: propau
    
    ! Index of the vectorized state associated to:
    !   lower bound of assets
    integer :: isl
    
    ! Index of the vectorized state associated to:
    !   upper bound of assets
    integer :: isu
    
    ! Next period's approximate value function
    real(dp) :: Vp
    
    ! Expected value function
    real(dp) :: EV
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
    ! Consumption
    c = (netres - ap)/one_tauC
      
    ! Test: negative consumption
    if (c <= 0.0_dp) then
        
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
                
    end if
      
    ! Test: next period's assets is limited by the first and last elements of
    !       the set of assets
    if (ap >= setA(1) .and. ap <= setA(Na)) then
      
      ! Index of the lower bound of assets used in the interpolation
      ial = locate(setA, ap)
      
      ! Index of the upper bound of assets used in the interpolation
      iau = ial + 1
      
    ! Test: next period's assets is greater than the last element of the set
    !       of assets
    else if (ap > setA(Na)) then
      
      ! Index of the lower bound of assets used in the extrapolation
      ial = Na - 1
      
      ! Index of the upper bound of assets used in the extrapolation
      iau = Na
      
    ! Test: next period's assets is less than the first element of the set of
    !       assets
    else
      
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
        
    end if
      
    ! Lower bound of assets used in the interpolation/extrapolation
    al = setA(ial)
        
    ! Upper bound of assets used in the interpolation/extrapolation
    au = setA(iau)
    
    ! Proportion of the value function assigned to the lower bound of assets
    ! the interpolation/extrapolation
    propal = (au - ap)/(au - al)
    
    ! Proportion of the value function assigned to the upper bound of assets
    ! in the interpolation/extrapolation
    propau = 1.0_dp - propal        
    
    ! Initializes expected value function
    EV = 0.0_dp
      
    ! Loop: next period's health status
    do ihp = 1, Nh
      
      ! Loop: next period's medical expenditures
      do imp = 1, Nm
      
        ! Index of the vectorized state associated to:
        !   lower bound of assets
        isl = ind([(j-RR+1)+1, ie, ihp, ial, imp, ix], jumpR)
      
        ! Index of the vectorized state associated to:
        !   upper bound of assets
        isu = ind([(j-RR+1)+1, ie, ihp, iau, imp, ix], jumpR)
      
        ! Interpolate/Extrapolate to find the next period's value function
        Vp = propal*VR(isl) &
           + propau*VR(isu)
      
        ! Expected value function
        EV = EV + PPHI(ihp, ih, ie, j)*PPSI(imp)*Vp
      
      end do  ! imp
      
    end do  ! ihp
      
    ! Return the negative of the value function
    fres = -( u(c, 0.0_dp, ih)                                     &
            + beta*(PPI(ih, ie, j)*EV + one_PPI(ih, ie, j)*uB(ap)) &
            )
      
  end function objfunRR

!==============================================================================
! Retired-to-Death Objective Function to be Optimized by the Brent's Method
!==============================================================================

  function objfunRD(ap) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
      
    ! Next period's assets
    real(dp), intent(in) :: ap
      
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
      
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
  
    ! Consumption
    real(dp) :: c
      
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
      
    ! Consumption
    c = (netres - ap)/one_tauC
      
    ! Test: negative consumption
    if (c <= 0.0_dp) then
        
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
                
    end if
      
    ! Test: next period's assets is less than the first element of
    !       the set of assets
    if (ap < setA(1)) then
    
      ! Return the largest possible value
      fres = huge(fres)
                  
      ! Exit the function
      return
    
    end if
      
    ! Return the negative of the value function
    fres = -(u(c, 0.0_dp, ih) + beta*one_PPI(ih, ie, j)*uB(ap))
      
  end function objfunRD

!==============================================================================
! Gini Coefficient
!==============================================================================

  function gini(x, f, tx, tf) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
        
    ! Vector of the distribution to calculate the Gini coefficient
    real(dp), dimension(:), intent(in) :: x
    
    ! Probability distribution over the vector "x"
    real(dp), dimension(:), intent(in) :: f
    
    ! Temporary vector of the distribution to calculate the Gini coefficient
    real(dp), dimension(:), intent(inout) :: tx
    
    ! Temporary probability distribution over the vector "x"
    real(dp), dimension(:), intent(inout) :: tf
    
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
        
    ! Function result
    real(dp) :: fres
    
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
    
    ! Size of the distribution vector
    integer :: n
    
    ! Loop index
    integer :: i
    
    ! Cumulative sum of f(xj)*xj from j=1 to j=i-1
    real(dp) :: Si_1
    
    ! Cumulative sum of f(xj)*xj from j=1 to j=i
    real(dp) :: Si
    
    ! Cumulative sum of f(xj)*(Si_1 + Si) from j=1 to j=n
    real(dp) :: cum
    
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------
    
    ! Populate the temporary vector of the distribution
    tx = x
    
    ! Populate the temporary probability distribution over the vector "x"
    tf = f
        
    ! Sort the distributions in ascending order
    call sort2(tx, tf)

    ! Initialize the cumulative sum of f(xj)*xj from j=1 to j=i-1
    Si_1 = 0.0_dp
    
    ! Initialize the cumulative sum of f(xj)*(Si_1 + Si) from j=1 to j=n
    cum = 0.0_dp
    
    ! Size of the distribution vector
    n = size(x)
    
    ! Loop: dimension of the distribution vector
    do i = 1, n
      
      ! Calculate the cumulative sum of f(xj)*xj from j=1 to j=i
      Si = Si_1 + tf(i)*tx(i)
      
      ! Calculate the cumulative sum of f(xj)*(Si_1 + Si) from j=1 to j=n
      cum = cum + tf(i)*(Si_1 + Si)
      
      ! Update the cumulative sum of f(xj)*xj from j=1 to j=i-1
      Si_1 = Si
    
    end do
    
    ! Final Gini coefficient
    fres = 1.0_dp - cum/Si
    
  end function gini

!==============================================================================
! Concentration of a Distribution
!==============================================================================

  function concentration(x, f, tx, tf, q) result(fres)
  
  !----------------------------------------------------------------------------
  ! Input Variables
  !----------------------------------------------------------------------------
  
    ! Vector of the distribution to calculate the concentration
    real(dp), dimension(:), intent(in) :: x
    
    ! Probability distribution over the vector "x"
    real(dp), dimension(:), intent(in) :: f
    
    ! Temporary vector of the distribution to calculate the concentration
    real(dp), dimension(:), intent(inout) :: tx
    
    ! Temporary probability distribution over the vector "x"
    real(dp), dimension(:), intent(inout) :: tf
    
    ! Quantile of the distribution to calculate the concentration
    real(dp), intent(in) :: q
  
  !----------------------------------------------------------------------------
  ! Output Variables
  !----------------------------------------------------------------------------
        
    ! Function result
    real(dp) :: fres
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
    
    ! Cumulative probability distribution over the vector "x"
    real(dp) :: fcum
     
    ! Size of the distribution vector
    integer :: n
    
    ! Loop index
    integer :: i
    
  !----------------------------------------------------------------------------
  ! Function Commands
  !----------------------------------------------------------------------------

    ! Assign the temporary vector of the distribution to calculate the
    ! concentration
    tx = x

    ! Assign the temporary probability distribution over the vector "x"
    tf = f

    ! Sort the distributions in ascending order
    call sort2(tx, tf)
    
    ! Size of the distribution vector
    n = size(tx)
    
    ! Initialize cumulative probability distribution
    fcum = tf(1)    
    
    ! Loop: dimension of the distribution vector
    do i = 1, n
    
      ! Test: cumulative distribution is greater than or equal the complement
      !       of the quantile
      if (fcum >= (1.0_dp - q)) then
        
        ! Concentration above the quantile      
        fres = sum(tx(i:n)*tf(i:n))/sum(tx*tf)
        
        ! Exit the function
        return
        
      end if
      
      ! Update cumulative probability distribution
      fcum = fcum + tf(i+1)
      
    end do
  
  end function concentration

end module mod_functions