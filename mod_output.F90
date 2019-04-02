module mod_output

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
  
  contains

!==============================================================================
! Main Results
!==============================================================================

  subroutine sub_output_results
  
    ! Open file
    open(unit=1, file="./outputs/results.txt", action="write", &
         status="replace")
    
    !--------------------------------------------------------------------------
    write(1, "(a)") "Health Insurance"
    !--------------------------------------------------------------------------
  
    write(1, "(a, a, f22.15)") "% Uninsured            ", char(9), sum(frac0)
    write(1, "(a, a, f22.15)") "% Medicare             ", char(9), sum(fracMC)
    write(1, "(a, a, f22.15)") "% Medicaid             ", char(9), sum(fracMA)
    write(1, "(a, a, f22.15)") "% Private              ", char(9), sum(fracE) + sum(fracI)
    write(1, "(a, a, f22.15)") "% EHI                  ", char(9), sum(fracE)
    write(1, "(a, a, f22.15)") "% IHI                  ", char(9), sum(fracI)
    write(1, "(a, a, f22.15)") "EHI take-up ratio      ", char(9), fracOfferE/fracOffer
  
    !--------------------------------------------------------------------------
    write(1, "(a)") "Macroeconomic Variables"
    !--------------------------------------------------------------------------
  
    write(1, "(a, a, f22.15)") "Output                 ", char(9), YY
    write(1, "(a, a, f22.15)") "Capital                ", char(9), KK
    write(1, "(a, a, f22.15)") "Consumption            ", char(9), CC
    write(1, "(a, a, f22.15)") "Medical expenditures   ", char(9), MM
    write(1, "(a, a, f22.15)") "Average work hours     ", char(9), LL
  
    !--------------------------------------------------------------------------
    write(1, "(a)") "Fiscal Revenues"
    !--------------------------------------------------------------------------
  
    write(1, "(a, a, f22.15)") "Income taxes           ", char(9), aggTT - aggTSS - aggTMC
    write(1, "(a, a, f22.15)") "Consumption taxes      ", char(9), tauC*CC
    write(1, "(a, a, f22.15)") "Social Security taxes  ", char(9), aggTSS
    write(1, "(a, a, f22.15)") "Medicare taxes         ", char(9), aggTMC
    write(1, "(a, a, f22.15)") "Medicare premium       ", char(9), pMC*fracpMC
  
    !--------------------------------------------------------------------------
    write(1, "(a)") "Fiscal Outlays"
    !--------------------------------------------------------------------------
  
    write(1, "(a, a, f22.15)") "Government expenditures", char(9), GG
    write(1, "(a, a, f22.15)") "Debt service           ", char(9), one_r*DD
    write(1, "(a, a, f22.15)") "Social Security benefit", char(9), aggSS
    write(1, "(a, a, f22.15)") "Medicare coverage      ", char(9), one_varphi*aggMC
    write(1, "(a, a, f22.15)") "Medicaid coverage      ", char(9), aggMA
    write(1, "(a, a, f22.15)") "Social insurance       ", char(9), aggTSI
  
    !--------------------------------------------------------------------------
    write(1, "(a)") "Prices"
    !--------------------------------------------------------------------------
  
    write(1, "(a, a, f22.15)") "Interest rate          ", char(9), r
    write(1, "(a, a, f22.15)") "Wage rate              ", char(9), w*Ez
    write(1, "(a, a, f22.15)") "EHI premium            ", char(9), pE
    write(1, "(a, a, f22.15)") "Average IHI premium    ", char(9), sum(pI)/size(pI)
  
    !--------------------------------------------------------------------------
    write(1, "(a)") "Other"
    !--------------------------------------------------------------------------
  
    write(1, "(a, a, f22.15)") "Wage rate reduction    ", char(9), chi*Ez
    write(1, "(a, a, f22.15)") "Consumption tax rate   ", char(9), tauC
    write(1, "(a, a, f22.15)") "Capital-output ratio   ", char(9), KK/YY
    write(1, "(a, a, f22.15)") "Welfare                ", char(9), aggV1*((10.0_dp)**30)
    
    write(1, *) ""
    write(1, "(a, a, f22.15)") "EHI expenditures       ", char(9), aggE
    write(1, "(a, a, f22.15)") "Demand for EHI         ", char(9), demE
    
    ! Close file
    close(1)
  
  end subroutine sub_output_results

!==============================================================================
! Equilibrium Variables
!==============================================================================

  subroutine sub_output_equilibrium_variables
  
    ! Open file
    open(unit=1, file="./inout/equilibrium_variables.txt", action="write", &
         status="replace")
    
    ! Aggregate capital
    write(1, "(f22.15)") KK
  
    ! Aggregate labor
    write(1, "(f22.15)") LL
  
    ! Bequest transfer
    write(1, "(f22.15)") BB
  
    ! EHI premium
    write(1, "(f22.15)") pE
  
    ! Loop: age
    do j = 1, RR-2
  
      ! Loop: current health status
      do ih = 1, Nh
      
        ! IHI premiums
        write(1, "(f22.15)") pI(ih, j)
      
      end do  ! ih
    
    end do  ! j
  
    ! Wage reduction
    write(1, "(f22.15)") chi
  
    ! Consumption tax rate
    write(1, "(f22.15)") tauC
    
    ! Close file
    close(1)
    
  end subroutine sub_output_equilibrium_variables

!==============================================================================
! Utilities
!==============================================================================

  subroutine sub_output_utilities
  
  !----------------------------------------------------------------------------
  ! Subroutine Commands
  !----------------------------------------------------------------------------
  
    ! Open file
    open(unit=1, file="./outputs/utilities.txt", action="write", &
         status="replace")
    
    !--------------------------------------------------------------------------
    ! Equilibrium Variables
    !--------------------------------------------------------------------------
    
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Equilibrium Variables"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    ! Aggregate capital
    write(1, *) "KK", char(9), KK
    write(1, "(a, a, f15.2)") "KK1", char(9), KK1
    write(1, "(a, a, f15.2)") "KK0", char(9), KK0
  
    write(1, *) ""
  
    ! Aggregate labor
    write(1, *) "LL", char(9), LL
    write(1, "(a, a, f15.2)") "LL1", char(9), LL1
    write(1, "(a, a, f15.2)") "LL0", char(9), LL0
  
    write(1, *) ""
  
    ! Bequest transfer
    write(1, *) "BB", char(9), BB
    write(1, "(a, a, f15.2)") "BB1", char(9), BB1
    write(1, "(a, a, f15.2)") "BB0", char(9), BB0
  
    write(1, *) ""
  
    ! EHI premium
    write(1, *) "pE", char(9), pE
    write(1, "(a, a, f15.2)") "pE1", char(9), pE1
    write(1, "(a, a, f15.2)") "pE0", char(9), pE0
  
    write(1, *) ""
  
    ! Minimum IHI premium
    write(1, *) "min(pI)", char(9), minval(pI)
    write(1, "(a, a, f15.2)") "min(pI1)", char(9), minval(pI1)
    write(1, "(a, a, f15.2)") "min(pI0)", char(9), minval(pI0)
    
    write(1, *) ""
  
    ! Maximum IHI premium
    write(1, *) "max(pI)", char(9), maxval(pI)
    write(1, "(a, a, f15.2)") "max(pI1)", char(9), maxval(pI1)
    write(1, "(a, a, f15.2)") "max(pI0)", char(9), maxval(pI0)
  
    write(1, *) ""
  
    ! Wage rate reduction
    write(1, *) "chi", char(9), chi
    write(1, "(a, a, f15.2)") "chi1", char(9), chi1
    write(1, "(a, a, f15.2)") "chi0", char(9), chi0
  
    write(1, *) ""
  
    ! Consumption tax rate
    write(1, *) "tauC", char(9), tauC
    write(1, "(a, a, f15.4)") "tauC1", char(9), tauC1
    write(1, "(a, a, f15.4)") "tauC0", char(9), tauC0  
  
    !--------------------------------------------------------------------------
    ! Factor Prices
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Factor Prices"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.4)") "Real interest rate", char(9), r
    write(1, "(a, a, f15.2)") "Real wage rate    ", char(9), w
  
    !--------------------------------------------------------------------------
    ! Wage Rate Reduction
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Wage Rate Reduction"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Real wage rate reduction", char(9), chi
  
    !--------------------------------------------------------------------------
    ! Health Insurance Premiums
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Health Insurance Premiums"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "EHI premium        ", char(9), pE
    write(1, "(a, a, f15.2)") "Minimum IHI premium", char(9), minval(pI)
    write(1, "(a, a, f15.2)") "Average IHI premium", char(9), sum(pI)/size(pI)
    write(1, "(a, a, f15.2)") "Maximum IHI premium", char(9), maxval(pI)
  
    !--------------------------------------------------------------------------
    ! Asset and Labor Markets
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Asset and Labor Markets"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Current assets               ", char(9), aggA
    write(1, "(a, a, f15.2)") "Next period's assets         ", char(9), aggAp
    write(1, "(a, a, f15.2)") "Current capital              ", char(9), KK
    write(1, "(a, a, f15.2)") "Next period's capital        ", char(9), KKp
    write(1, "(a, a, f15.2)") "Current government debt      ", char(9), DD
    write(1, "(a, a, f15.2)") "Next period's government debt", char(9), DDp
    write(1, "(a, a, f15.2)") "Labor time                   ", char(9), LL
  
    !--------------------------------------------------------------------------
    ! Consumption Goods Market
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Consumption Goods Market"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Consumption                      ", char(9), CC
    write(1, "(a, a, f15.2)") "Next period's capital            ", char(9), KKp
    write(1, "(a, a, f15.2)") "Total medical expenditures       ", char(9), MM
    write(1, "(a, a, f15.2)") "Government expenditures          ", char(9), GG
    write(1, "(a, a, f15.2)") "Real production                  ", char(9), YY
    write(1, "(a, a, f15.2)") "Current capital less depreciation", char(9), one_delta*KK
  
    !--------------------------------------------------------------------------
    ! Medical Expenditures
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Medical Expenditures"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Total medical expenditures    ", char(9), MM
    write(1, "(a, a, f15.2)") "Agents' medical expenditures  ", char(9), aggMM
    write(1, "(a, a, f15.2)") "Medicare's administrative cost", char(9), varphi*aggMC
    write(1, "(a, a, f15.2)") "EHI's administrative cost     ", char(9), kappa*aggE
    write(1, "(a, a, f15.2)") "IHI's administrative cost     ", char(9), kappa*sum(aggI)
    write(1, "(a, a, f15.2)") "IHI's fixed cost              ", char(9), varkappa*demIp
  
    !--------------------------------------------------------------------------
    ! Government Budget
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Government Budget"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Government expenditures      ", char(9), GG
    write(1, "(a, a, f15.2)") "Gross current government debt", char(9), one_r*DD
    write(1, "(a, a, f15.2)") "Social Security benefit      ", char(9), aggSS
    write(1, "(a, a, f15.2)") "Medicare's coverage expenses ", char(9), one_varphi*aggMC
    write(1, "(a, a, f15.2)") "Medicaid's coverage expenses ", char(9), aggMA
    write(1, "(a, a, f15.2)") "Social insurance transfer    ", char(9), aggTSI
    write(1, "(a, a, f15.2)") "Next period's government debt", char(9), DDp
    write(1, "(a, a, f15.2)") "Consumption taxes            ", char(9), tauC*CC
    write(1, "(a, a, f15.2)") "Taxes less consumption tax   ", char(9), aggTT
    write(1, "(a, a, f15.2)") "Medicare's premium revenue   ", char(9), pMC*fracpMC
  
    !--------------------------------------------------------------------------
    ! Bequest Transfer
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Bequest Transfer"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Bequest transfer", char(9), BB
  
    !--------------------------------------------------------------------------
    ! Health Insurance
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Health Insurance"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "EHI's coverage expenses", char(9), aggE
    write(1, "(a, a, f15.2)") "IHI's coverage expenses", char(9), sum(aggI)
  
    write(1, *) ""
    write(1, "(a)") "Total population"
  
    write(1, "(a, a, f15.4)") "% of uninsured   ", char(9), sum(frac0)
    write(1, "(a, a, f15.4)") "% with Medicare  ", char(9), sum(fracMC)
    write(1, "(a, a, f15.4)") "% with Medicaid  ", char(9), sum(fracMA)
    write(1, "(a, a, f15.4)") "% with EHI       ", char(9), sum(fracE)
    write(1, "(a, a, f15.4)") "% with IHI       ", char(9), sum(fracI)
    write(1, "(a, a, f15.4)") "% with private HI", char(9), sum(fracE) + sum(fracI)
                            
    write(1, *) ""
    write(1, "(a)") "Population younger than 65 "
  
    write(1, "(a, a, f15.4)") "% of uninsured   ", char(9), sum(frac0)/sum(lambdaW)
    write(1, "(a, a, f15.4)") "% with Medicaid  ", char(9), sum(fracMA)/sum(lambdaW)
    write(1, "(a, a, f15.4)") "% with EHI       ", char(9), sum(fracE)/sum(lambdaW)
    write(1, "(a, a, f15.4)") "% with IHI       ", char(9), sum(fracI)/sum(lambdaW)
    write(1, "(a, a, f15.4)") "% with private HI", char(9), (sum(fracE) + sum(fracI))/sum(lambdaW)
  
    write(1, *) ""
  
    write(1, "(a, a, f15.4)") "EHI take-up ratio              ", char(9), fracOfferE/fracOffer
    write(1, "(a, a, f15.2)") "Working hours with an EHI offer", char(9), LLE
  
    !--------------------------------------------------------------------------
    ! Parameters
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Parameters"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.4)") "beta    ", char(9), beta
    write(1, "(a, a, f15.4)") "tauC    ", char(9), tauC
    write(1, "(a, a, f15.4)") "tauSS   ", char(9), tauSS
    write(1, "(a, a, f15.4)") "tauMC   ", char(9), tauMC
    write(1, "(a, a, f15.4)") "xi      ", char(9), xi
    write(1, "(a, a, f15.4)") "alpha   ", char(9), alpha
    write(1, "(a, a, f15.4)") "delta   ", char(9), delta
    write(1, "(a, a, f15.4)") "varphi  ", char(9), varphi
    write(1, "(a, a, f15.2)") "eta     ", char(9), eta
    write(1, "(a, a, f15.2)") "KKp/KK-1", char(9), KKp/KK - 1.0_dp
    write(1, "(a, a, f15.2)") "DDp/DD-1", char(9), DDp/DD - 1.0_dp
    write(1, "(a, a, f15.2)") "yMA     ", char(9), yMA
    write(1, "(a, a, f15.2)") "yMN     ", char(9), yMN

    !--------------------------------------------------------------------------
    ! Welfare Measures
    !--------------------------------------------------------------------------
    
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Welfare Measures"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
    
    write(1, "(a, a, f15.2)") "Average value function of newborns", char(9), aggV1*((10.0_dp)**30)
    
    !--------------------------------------------------------------------------
    ! Choices
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Choices"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Maximum asset choice of workers ", char(9), maxval(aW)
    write(1, "(a, a, f15.2)") "Maximum asset choice of retirees", char(9), maxval(aR)
  
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Maximum labor time choice", char(9), maxval(lW)
    write(1, "(a, a, f15.2)") "Minimum labor time choice", char(9), minval(lW)
  
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Minimum consumption choice of workers", char(9), minval(cW)
    write(1, "(a, a, f15.2)") "Minimum consumption choice of retirees", char(9), minval(cR)
  
  
    !--------------------------------------------------------------------------
    ! Targets
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Targets"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, "(a, a, f15.4)") "% of population older than 65", char(9), sum(lambdaR)
    write(1, "(a, a, f15.4)") "Target                       ", char(9), targetPopOlder65
    write(1, "(a, a, f15.4)") "Error                        ", char(9), abs(sum(lambdaR) - targetPopOlder65)
    
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Medical expenditures per capita", char(9), MM
    write(1, "(a, a, f15.2)") "Target                         ", char(9), targetMM
    write(1, "(a, a, f15.2)") "Error                          ", char(9), abs(MM - targetMM)
    
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Capital-output ratio", char(9), KK/YY
    write(1, "(a, a, f15.2)") "Target              ", char(9), targetKY
    write(1, "(a, a, f15.2)") "Error               ", char(9), abs(KK/YY - targetKY)
    
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Output per capita", char(9), YY
    write(1, "(a, a, f15.2)") "Target           ", char(9), targetYY
    write(1, "(a, a, f15.2)") "Error            ", char(9), abs(YY - targetYY)
  
    write(1, *) ""
    
    write(1, "(a, a, f15.4)") "% of uninsured", char(9), sum(frac0)
    write(1, "(a, a, f15.4)") "Target        ", char(9), targetFracUnin
    write(1, "(a, a, f15.4)") "Error         ", char(9), abs(sum(frac0) - targetFracUnin)
                            
    !--------------------------------------------------------------------------
    ! Checks
    !--------------------------------------------------------------------------
  
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Checks"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
  
    write(1, *) "Sum of agents distribution  ", char(9), sum(lambdaW) + sum(lambdaR)
    write(1, *) "Sum of workers distribution ", char(9), sum(lambdaW)
    write(1, *) "Sum of retirees distribution", char(9), sum(lambdaR)
  
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "Profit", char(9), YY - (r + delta)*KK - w*LL
  
    write(1, *) ""
  
    write(1, "(a, a, f15.2)") "LHS          ", char(9), CC + KKp + MM + GG
    write(1, "(a, a, f15.2)") "RHS          ", char(9), YY + one_delta*KK + EE
    write(1, "(a, a, f15.2)") "Excess Demand", char(9), ex_dem

    ! Close file
    close(1)
  
  end subroutine sub_output_utilities

!==============================================================================
! Inequality Measures
!==============================================================================

  subroutine sub_output_inequality
    
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
    
    ! Concatenated policy function of consumption (Workers + Retirees)
    real(dp), dimension(NsW+NsR) :: cWR

    ! Concatenated policy function of assets (Workers + Retirees)
    real(dp), dimension(NsW+NsR) :: aWR
  
    ! Workers' normalized distribution
    real(dp), dimension(NsW) :: lambdaWnorm
    
    ! Retirees' normalized distribution
    real(dp), dimension(NsR) :: lambdaRnorm
    
    ! Concatenated distribution (Workers + Retirees)
    real(dp), dimension(NsW+NsR) :: lambdaWR
    
  !----------------------------------------------------------------------------
  ! Temporary Variables
  !----------------------------------------------------------------------------
    
    ! Temporary worker's generic policy function
    real(dp), dimension(NsW) :: tW
  
    ! Temporary retirees' generic policy function
    real(dp), dimension(NsR) :: tR
  
    ! Temporary concatenated generic policy function (Workers + Retirees)
    real(dp), dimension(NsW+NsR) :: tWR
    
    ! Temporary workers' distribution
    real(dp), dimension(NsW) :: tlambdaW
  
    ! Temporary retirees' distribution
    real(dp), dimension(NsR) :: tlambdaR
  
    ! Temporary concatenated distribution (Workers + Retirees)
    real(dp), dimension(NsW+NsR) :: tlambdaWR
    
  !----------------------------------------------------------------------------
  ! Subroutine Commands
  !----------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/inequality.txt", action="write", &
         status="replace")
    
    !--------------------------------------------------------------------------
    ! Distributions
    !--------------------------------------------------------------------------
    
    ! Workers' normalized distribution
    lambdaWnorm = lambdaW/sum(lambdaW)
  
    ! Retirees' normalized distribution
    lambdaRnorm = lambdaR/sum(lambdaR)
    
    ! Concatenate workers' distribution
    lambdaWR(1:NsW) = lambdaW
    
    ! Concatenate retirees' distribution
    lambdaWR(NsW+1:NsW+NsR) = lambdaR
    
    !--------------------------------------------------------------------------
    ! Gini of Consumption
    !--------------------------------------------------------------------------
    
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Gini of Consumption"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
    
    ! Concatenate workers' policy function of consumption
    cWR(1:NsW) = cW
    
    ! Concatenate retirees' policy function of consumption
    cWR(NsW+1:NsW+NsR) = cR
    
    write(1, "(a, a, f15.4)") "Workers ",           &
                              char(9),              &
                              gini(cW, lambdaWnorm, tW, tlambdaW)
  
    write(1, "(a, a, f15.4)") "Retirees",           &
                              char(9),              &
                              gini(cR, lambdaRnorm, tR, tlambdaR)
  
    write(1, "(a, a, f15.4)") "Total   ",           &
                              char(9),              &
                              gini(cWR, lambdaWR, tWR, tlambdaWR)
    
    !--------------------------------------------------------------------------
    ! Gini of Assets
    !--------------------------------------------------------------------------
    
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Gini of Assets"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
    
    ! Concatenate workers' policy function of assets
    aWR(1:NsW) = aW
    
    ! Concatenate retirees' policy function of assets
    aWR(NsW+1:NsW+NsR) = aR
  
    write(1, "(a, a, f15.4)") "Workers ",           &
                              char(9),              &
                              gini(aW, lambdaWnorm, tW, tlambdaW)
  
    write(1, "(a, a, f15.4)") "Retirees",           &
                              char(9),              &
                              gini(aR, lambdaRnorm, tR, tlambdaR)
  
    write(1, "(a, a, f15.4)") "Total   ",           &
                              char(9),              &
                              gini(aWR, lambdaWR, tWR, tlambdaWR)
    
    !--------------------------------------------------------------------------
    ! Concentration of Assets
    !--------------------------------------------------------------------------
    
    write(1, *) ""
    write(1, "(a)") repeat("=", 79)
    write(1, "(a)") "Concentration of Assets"
    write(1, "(a)") repeat("=", 79)
    write(1, *) ""
    
    write(1, "(a, a, f15.4)") "Top 1%  ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.01_dp)

    write(1, "(a, a, f15.4)") "Top 5%  ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.05_dp)

    write(1, "(a, a, f15.4)") "Top 10% ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.10_dp)

    write(1, "(a, a, f15.4)") "Top 20% ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.20_dp)

    write(1, "(a, a, f15.4)") "Top 40% ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.40_dp)

    write(1, "(a, a, f15.4)") "Top 60% ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.60_dp)

    write(1, "(a, a, f15.4)") "Top 80% ",                           &
                              char(9),                              &
                              concentration(aWR, lambdaWR, tWR, tlambdaWR, 0.80_dp)

    ! Close file
    close(1)
  
  end subroutine sub_output_inequality

!==============================================================================
! Life Cycle Macroeconomic Variables
!==============================================================================
  
  subroutine sub_output_life_cycle_macro
  
    ! Open file
    open(unit=1, file="./outputs/life_cycle_macro.txt", action="write", &
         status="replace")
    
    ! Loop: age
    do j = 1, JJ
    
      ! Life cycle variables
      write(1, "(i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
        j + 19     , char(9),                    &
        CCj(j)      / sum(mu(:, :, j)), char(9), &
        aggApj(j)   / sum(mu(:, :, j)), char(9), &
        aggMTILj(j) / sum(mu(:, :, j)), char(9), &
        aggPj(j)    / sum(mu(:, :, j)), char(9), &
        aggTTj(j)   / sum(mu(:, :, j)), char(9), &
        aggYYj(j)   / sum(mu(:, :, j)), char(9), &
        aggAj(j)    / sum(mu(:, :, j)), char(9), &
        aggTSIj(j)  / sum(mu(:, :, j)), char(9), &
        BBj(j)      / sum(mu(:, :, j))
    
    end do  ! j
    
    ! Close file
    close(1)
  
  end subroutine sub_output_life_cycle_macro

!==============================================================================
! Distribution Health Insurance Coverage
!==============================================================================

  subroutine sub_output_dist_hi
  
    !--------------------------------------------------------------------------
    ! Education Level
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_educ.txt", &
         action="write", status="replace")
    
    ! Loop: education level
    do ie = 1, Ne
    
      write(1, "(i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
        ie                                       , char(9), &
        sum(frac0(:, ie, :))  / sum(mu(:, ie, :)), char(9), &
        sum(fracMC(:, ie))    / sum(mu(:, ie, :)), char(9), &
        sum(fracMA(:, ie, :)) / sum(mu(:, ie, :)), char(9), &
        sum(fracE(:, ie, :))  / sum(mu(:, ie, :)), char(9), &
        sum(fracI(:, ie, :))  / sum(mu(:, ie, :))

      
    end do  ! ie
    
    ! Close file
    close(1)
    
    !--------------------------------------------------------------------------
    ! Health Status
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_hs.txt", &
         action="write", status="replace")
    
    ! Loop: health status
    do ih = 1, Nh
    
      write(1, "(i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
        ih                                       , char(9), &
        sum(frac0(ih, :, :))  / sum(mu(ih, :, :)), char(9), &
        sum(fracMC(ih, :))    / sum(mu(ih, :, :)), char(9), &
        sum(fracMA(ih, :, :)) / sum(mu(ih, :, :)), char(9), &
        sum(fracE(ih, :, :))  / sum(mu(ih, :, :)), char(9), &
        sum(fracI(ih, :, :))  / sum(mu(ih, :, :))
    
    end do  ! ih
    
    ! Close file
    close(1)
    
    !--------------------------------------------------------------------------
    ! Assets
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_assets.txt", &
         action="write", status="replace")
    
    ! Loop: assets
    do ia = 1, Na
    
      write(1, "(i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
        ia                     , char(9), &
        frac0a(ia)  / margA(ia), char(9), &
        fracMCa(ia) / margA(ia), char(9), &
        fracMAa(ia) / margA(ia), char(9), &
        fracEa(ia)  / margA(ia), char(9), &
        fracIa(ia)  / margA(ia)
    
    end do  ! ia
    
    ! Close file
    close(1)         
  
  end subroutine sub_output_dist_hi

!==============================================================================
! Life Cycle Health Insurance Coverage
!==============================================================================

  subroutine sub_output_life_cycle_hi
  
    !--------------------------------------------------------------------------
    ! Age
    !--------------------------------------------------------------------------
  
    ! Open file
    open(unit=1, file="./outputs/hi_age.txt", &
         action="write", status="replace")
    
    ! Loop: age
    do j = 1, (RR-1)
    
      ! Health insurance allocation by age
      write(1, "(i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
        j + 19                                 , char(9), &
        sum(frac0(:, :, j))  / sum(mu(:, :, j)), char(9), &
        sum(fracMA(:, :, j)) / sum(mu(:, :, j)), char(9), &
        sum(fracE(:, :, j))  / sum(mu(:, :, j)), char(9), &
        sum(fracI(:, :, j))  / sum(mu(:, :, j))
    
    end do  ! j
    
    ! Close file
    close(1)
    
    !--------------------------------------------------------------------------
    ! Age and Education Level
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_age_educ.txt", &
         action="write", status="replace")
    
    ! Loop: education level
    do ie = 1, Ne
      
      ! Loop: age groups
      do j = 1, (RR-1)
    
        ! Age group variables
        write(1, "(i4, a, i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
          ie                                       , char(9), &
          j                                        , char(9), &
          sum(frac0(:, ie, j))  / sum(mu(:, ie, j)), char(9), &
          sum(fracMA(:, ie, j)) / sum(mu(:, ie, j)), char(9), &
          sum(fracE(:, ie, j))  / sum(mu(:, ie, j)), char(9), &
          sum(fracI(:, ie, j))  / sum(mu(:, ie, j))
      
      end do  ! j
    
    end do  ! ie
    
    ! Close file
    close(1)
    
    !--------------------------------------------------------------------------
    ! Age and Health Status
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_age_hs.txt", &
         action="write", status="replace")
    
    ! Loop: health status
    do ih = 1, Nh
      
      ! Loop: age groups
      do j = 1, (RR-1)
    
        ! Age group variables
        write(1, "(i4, a, i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
          ih                                       , char(9), &
          j                                        , char(9), &
          sum(frac0(ih, :, j))  / sum(mu(ih, :, j)), char(9), &
          sum(fracMA(ih, :, j)) / sum(mu(ih, :, j)), char(9), &
          sum(fracE(ih, :, j))  / sum(mu(ih, :, j)), char(9), &
          sum(fracI(ih, :, j))  / sum(mu(ih, :, j))
      
      end do  ! j
    
    end do  ! ih
    
    ! Close file
    close(1)
    
  end subroutine sub_output_life_cycle_hi

!==============================================================================
! Distribution of Health Insurance Allocation
!==============================================================================
  
  subroutine sub_output_age_group_hi
    
  !----------------------------------------------------------------------------
  ! Local Parameters
  !----------------------------------------------------------------------------
  
    ! Number of age groups
    integer, parameter :: n = 9
    
    ! size of each age group
    integer, parameter :: s = 5
  
  !----------------------------------------------------------------------------
  ! Local Variables
  !----------------------------------------------------------------------------
    
    ! Age limits of the age groups
    integer :: j1, j2
  
  !----------------------------------------------------------------------------
  ! Subroutine Commands
  !----------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------
    ! Age Group
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_age_group.txt", &
         action="write", status="replace")
    
    ! Initialize the inferior age of age group
    j1 = 1
    
    ! Initialize the superior age of age group
    j2 = j1 + s - 1
    
    ! Loop: age groups
    do j = 1, n
    
      ! Age group variables
      write(1, "(i4, a, i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
        j1 + 19                                        , char(9), &
        j2 + 19                                        , char(9), &
        sum(frac0(:, :, j1:j2))  / sum(mu(:, :, j1:j2)), char(9), &
        sum(fracMA(:, :, j1:j2)) / sum(mu(:, :, j1:j2)), char(9), &
        sum(fracE(:, :, j1:j2))  / sum(mu(:, :, j1:j2)), char(9), &
        sum(fracI(:, :, j1:j2))  / sum(mu(:, :, j1:j2))
      
      ! Update the inferior age of age group
      j1 = j2 + 1
      
      ! Update the superior age of age group
      j2 = j1 + s - 1
    
    end do  ! j
    
    ! Close file
    close(1)
    
    !--------------------------------------------------------------------------
    ! Age Group and Education Level
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_age_group_educ.txt", &
         action="write", status="replace")
    
    ! Loop: education level
    do ie = 1, Ne
      
      ! Initialize the inferior age of age group
      j1 = 1
    
      ! Initialize the superior age of age group
      j2 = j1 + s - 1
      
      ! Loop: age groups
      do j = 1, n
    
        ! Age group variables
        write(1, "(i4, a, i4, a, i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
          ie                                               , char(9), &
          j1 + 19                                          , char(9), &
          j2 + 19                                          , char(9), &
          sum(frac0(:, ie, j1:j2))  / sum(mu(:, ie, j1:j2)), char(9), &
          sum(fracMA(:, ie, j1:j2)) / sum(mu(:, ie, j1:j2)), char(9), &
          sum(fracE(:, ie, j1:j2))  / sum(mu(:, ie, j1:j2)), char(9), &
          sum(fracI(:, ie, j1:j2))  / sum(mu(:, ie, j1:j2))
      
      ! Update the inferior age of age group
      j1 = j2 + 1
      
      ! Update the superior age of age group
      j2 = j1 + s - 1
    
      end do  ! j
    
    end do  ! ie
    
    ! Close file
    close(1)
    
    !--------------------------------------------------------------------------
    ! Age Group and Health Status
    !--------------------------------------------------------------------------
    
    ! Open file
    open(unit=1, file="./outputs/hi_age_group_hs.txt", &
         action="write", status="replace")
    
    ! Loop: health status
    do ih = 1, Nh
      
      ! Initialize the inferior age of age group
      j1 = 1
    
      ! Initialize the superior age of age group
      j2 = j1 + s - 1
      
      ! Loop: age groups
      do j = 1, n
    
        ! Age group variables
        write(1, "(i4, a, i4, a, i4, a, f22.15, a, f22.15, a, f22.15, a, f22.15)") &
          ih                                               , char(9), &
          j1 + 19                                          , char(9), &
          j2 + 19                                          , char(9), &
          sum(frac0(ih, :, j1:j2))  / sum(mu(ih, :, j1:j2)), char(9), &
          sum(fracMA(ih, :, j1:j2)) / sum(mu(ih, :, j1:j2)), char(9), &
          sum(fracE(ih, :, j1:j2))  / sum(mu(ih, :, j1:j2)), char(9), &
          sum(fracI(ih, :, j1:j2))  / sum(mu(ih, :, j1:j2))
      
      ! Update the inferior age of age group
      j1 = j2 + 1
      
      ! Update the superior age of age group
      j2 = j1 + s - 1
    
      end do  ! j
    
    end do  ! ih
    
    ! Close file
    close(1)
    
  end subroutine sub_output_age_group_hi

!==============================================================================
! Welfare Measures
!==============================================================================

  subroutine sub_output_welfare_measures
  
    ! Open file
    open(unit=1, file="./outputs/welfare.txt", action="write", &
         status="replace")
    
  !----------------------------------------------------------------------------
  ! Newborns
  !----------------------------------------------------------------------------
    
    ! Expected value function of newborns
    write(1, "(a, a, f15.4)") "Newborns", char(9), aggV1*((10.0_dp)**30)

  !----------------------------------------------------------------------------
  ! Health Status
  !----------------------------------------------------------------------------

    ! Loop: current health status
    do ih = 1, Nh
    
      ! Expected value function of newborns by health status
      write(1, "(i4, a, f15.4)") ih, char(9), aggV1h(ih)*((10.0_dp)**30)
    
    end do

  !----------------------------------------------------------------------------
  ! Labor Productivity
  !----------------------------------------------------------------------------

    ! Loop: current labor productivity
    do iz = 1, Nz
    
      ! Expected value function of newborns by labor productivity
      write(1, "(i4, a, f15.4)") iz, char(9), aggV1z(iz)*((10.0_dp)**30)
    
    end do
    
    ! Close file
    close(1)
    
  end subroutine sub_output_welfare_measures

end module mod_output