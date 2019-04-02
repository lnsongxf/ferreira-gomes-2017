subroutine sub_allocation

!==============================================================================
! Modules
!==============================================================================

  use mod_globals

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Parameters
!==============================================================================
    
  ! Probability distribution of education level
  allocate(LLAMBDA(Ne))
  
  ! Transition probabilities of health status
  allocate(PPHI(Nh, Nh, Ne, JJ))
  
  ! Conditional survival probabilities
  allocate(PPI(Nh, Ne, JJ))
  
  ! Parameter: 1 - PPI
  allocate(one_PPI(Nh, Ne, JJ))
  
  ! Share of types (j, e, h)
  allocate(mu(Nh, Ne, JJ))
  
  ! Set of current assets
  allocate(setA(Na))
  
  ! Probability distribution of assets
  allocate(OOMEGA(Na))
  
  ! Set of labor times
  allocate(setL(Nl))
  
  ! Set of medical expenditures
  allocate(setM(Nm, Nh, JJ))
  
  ! Probability distribution of medical expenditures
  allocate(PPSI(Nm))
  
  ! Set of labor productivities
  allocate(setZ(Nz))
  
  ! Set of average lifetime earnings
  allocate(setX(Nx))
  
  ! Coinsurance rates
  allocate(q(Ni, Nm, Nh, JJ))
  
  ! Parameter: 1 - q
  allocate(one_q(Ni, Nm, Nh, JJ))
  
  ! Current medical expenditures actually paid
  allocate(mtil(Ni, Nm, Nh, JJ))
  
  ! Set of next period's health insurances
  allocate(setIp(Ni-2))
  
  ! Premium
  allocate(p(Ni-1, Nh, RR-2))
  
  ! IHI premium
  allocate(pI(Nh, RR-2))
  
  ! Transition probabilities of labor productivity and EHI offer
  allocate(GGAMMA(Niota, Nz, Niota, Nz, Ne, RR-1))
  
  ! Invariant probabilities of labor productivity and EHI offer
  allocate(GGAMMAbar(Niota, Nz, Ne, RR-1))
  
  ! Vector with the size of the dimensions of the worker's state space
  allocate(dimW(NdimW))
  
  ! Vector with the size of the dimensions of the retired's state space
  allocate(dimR(NdimR))
  
  ! Vector with the size of the jumps made in the worker's vectorized state
  ! space
  allocate(jumpW(NdimW))
  
  ! Vector with the size of the jumps made in the retired's vectorized state
  ! space
  allocate(jumpR(NdimR))
    
!==============================================================================
! Worker's Variables
!==============================================================================
    
  ! Worker's value function
  allocate(VW(NsW))
  
  ! Worker's policy function of consumption
  allocate(cW(NsW))
  
  ! Worker's policy function of assets
  allocate(aW(NsW))
  
  ! Worker's policy function of labor time
  allocate(lW(NsW))
  
  ! Worker's policy function of health insurance
  allocate(iW(NsW))
  
!==============================================================================
! Retired's Variables
!==============================================================================
    
  ! Retired's value function
  allocate(VR(NsR))
  
  ! Retired's policy function of consumption
  allocate(cR(NsR))
  
  ! Retired's policy function of assets
  allocate(aR(NsR))
  
!==============================================================================
! Agents' Distributions
!==============================================================================
  
  ! Worker's distribution
  allocate(lambdaW(NsW))
  
  ! Retired's distribution
  allocate(lambdaR(NsR))
  
  ! Marginal distribution of assets
  allocate(margA(Na))
  
!==============================================================================
! Equilibrium Variables
!==============================================================================

  ! Initial guess for IHI premium
  allocate(pI0(Nh, RR-2))
    
  ! Updated IHI premium ($)
  allocate(pI1(Nh, RR-2))

!==============================================================================
! Aggregate Variables
!==============================================================================

  ! Aggregate consumption by age
  allocate(CCj(JJ))
  
  ! Current aggregate assets by age
  allocate(aggAj(JJ))
  
  ! Next period's aggregate assets by age
  allocate(aggApj(JJ))
  
  ! Aggregate income (labor and SS) by age
  allocate(aggYYj(JJ))
  
  ! Aggregate IHI expenditures
  allocate(aggI(Nh, RR-2))
  
  ! Aggregate current medical expenditures actually paid by age
  allocate(aggMTILj(JJ))
  
  ! Aggregate premiums by age
  allocate(aggPj(JJ))
  
  ! Aggregate Social Insurance transfer by age
  allocate(aggTSIj(JJ))
  
  ! Aggregate total taxes by age
  allocate(aggTTj(JJ))
  
  ! Bequest transfer by age
  allocate(BBj(JJ))
    
  ! Demand for IHI in the current period
  allocate(demI(Nh, RR-2))
  
  ! Fraction of agents uninsured in the current period
  allocate(frac0(Nh, Ne, RR-1))
  
  ! Fraction of agents uninsured in the current period by assets
  allocate(frac0a(Na))
  
  ! Fraction of agents with Medicare in the current period
  allocate(fracMC(Nh, Ne))
  
  ! Fraction of agents with Medicare in the current period by assets
  allocate(fracMCa(Na))
  
  ! Fraction of agents with Medicaid in the current period
  allocate(fracMA(Nh, Ne, RR-1))
  
  ! Fraction of agents with Medicaid in the current period by assets
  allocate(fracMAa(Na))
  
  ! Fraction of agents with EHI in the current period
  allocate(fracE(Nh, Ne, RR-1))
  
  ! Fraction of agents with EHI in the current period by assets
  allocate(fracEa(Na))
  
  ! Fraction of agents with IHI in the current period
  allocate(fracI(Nh, Ne, RR-1))
  
  ! Fraction of agents with IHI in the current period by assets
  allocate(fracIa(Na))

!==============================================================================
! Welfare Measures
!==============================================================================

  ! Expected value function of newborns by labor productivity
  allocate(aggV1z(Nz))

  ! Expected value function of newborns by health status
  allocate(aggV1h(Nh))

end subroutine sub_allocation