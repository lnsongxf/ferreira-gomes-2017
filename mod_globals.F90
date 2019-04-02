module mod_globals

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Patient Protection and Affordable Care Act (PPACA)
!==============================================================================
  
  ! Run PPACA policies
  ! 0: No
  ! 1: Yes
  integer, parameter :: polPTC = 0  ! Premium Tax Credits
  integer, parameter :: polIM  = 0  ! Individual Mandate
  integer, parameter :: polME  = 0  ! Medicaid Expansion
  integer, parameter :: polIR  = 0  ! Health Insurance Regulation
  integer, parameter :: polDME = 0  ! Deductible Medical Expenditures

!==============================================================================
! Health Care Cost Reductions
!==============================================================================

  ! Run health care cost reductions
  ! Benchmark    : 0.0
  ! Rand Proposal: 0.0064437741328291
  ! OECD trend   : 0.294500337916366
  real(dp), parameter :: costRed = 0.0_dp

!==============================================================================
! Indexes
!==============================================================================

  integer :: j,      & ! Age
             ie,     & ! Education level
             ih,     & ! Current health status
             ihp,    & ! Next period's health status
             ia,     & ! Current assets
             il,     & ! Labor time
             im,     & ! Current medical expenditures
             imp,    & ! Next period's medical expenditures
             iz,     & ! Current labor productivity
             izp,    & ! Next period's labor productivity
             ix,     & ! Current average lifetime earnings
             iiota,  & ! Current EHI offer status
             iiotap, & ! Next period's EHI offer status
             ii,     & ! Current health insurance
             iip,    & ! Next period's health insurance
             isW,    & ! Worker's vectorized state
             isR       ! Retired's vectorized state

!==============================================================================
! Age Structure
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Maximum age
  integer, parameter :: JJ = 81
  
  ! Mandatory retirement age
  integer, parameter :: RR = 46

  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------

  ! No variables

!==============================================================================
! Education Level
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of education levels
  ! 1 = Low
  ! 2 = High
  integer, parameter :: Ne = 2

  ! Probabilities of education level (%)
  real(dp), parameter :: LLAMBDAL = 0.717219495105404_dp
  real(dp), parameter :: LLAMBDAH = 0.282780504894596_dp
  
  ! Probability distribution of education level
  real(dp), dimension(:), allocatable :: LLAMBDA

  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------

  ! No variables

!==============================================================================
! Health Status
!==============================================================================

  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------

  ! Number of health statuses
  ! 1 = Bad
  ! 2 = Good
  integer, parameter :: Nh = 2
  
  ! Health statuses
  integer, parameter :: hB = 1  ! Bad
  integer, parameter :: hG = 2  ! Good
  
  ! Transition probabilities of health status
  real(dp), dimension(:, :, :, :), allocatable :: PPHI
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------

  ! No variables

!==============================================================================
! Demography
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Population growth rate (%)
  real(dp), parameter :: eta = 0.027469604870001_dp
  
  ! Parameter: 1 + eta
  real(dp), parameter :: one_eta = 1.0_dp + eta
  
  ! Survival probabilities
  real(dp), dimension(:, :, :), allocatable :: PPI
  
  ! Parameter: 1 - PPI
  real(dp), dimension(:, :, :), allocatable :: one_PPI
    
  ! Share of types (j, e, h)
  real(dp), dimension(:, :, :), allocatable :: mu
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------

  ! No variables

!==============================================================================
! Asset Market
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of current assets
  integer, parameter :: Na = 52
  
  ! Set of assets
  real(dp), dimension(:), allocatable :: setA
  
  ! Probability distribution of assets
  real(dp), dimension(:), allocatable :: OOMEGA
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Current assets
  real(dp) :: a
  
  ! Bequest transfer ($)
  real(dp) :: BB
    
!==============================================================================
! Preferences
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Time endowment (hours)
  real(dp), parameter :: ell = 8760.0_dp
  
  ! Number of all possible labor times
  ! 1 = No work
  ! 2 = Part-time
  ! 3 = Full-time
  integer, parameter :: Nl = 3
  
  ! Time of part-time job (hours)
  real(dp), parameter :: lP = 1008.0_dp
  
  ! Time of full-time job (hours)
  real(dp), parameter :: lF = 2016.0_dp    
  
  ! Intertemporal discount factor
  real(dp), parameter :: beta = 0.9855_dp
  
  ! Share of consumption
  real(dp), parameter :: gamma = 0.615_dp
  
  ! Parameter: 1 - gamma
  real(dp), parameter :: one_gamma = 1.0_dp - gamma
  
  ! Risk aversion parameter
  real(dp), parameter :: sigma = 7.69_dp
  
  ! Parameter: 1 - sigma
  real(dp), parameter :: one_sigma = 1.0_dp - sigma
  
  ! Time cost of work
  real(dp), parameter :: phi1 = 240.0_dp
  
  ! Time cost associated with bad health status
  real(dp), parameter :: phi2 = 202.0_dp
  
  ! Weight on the bequest utility
  real(dp), parameter :: psi1 = 0.0_dp!0.037_dp
  
  ! Curvature parameter of the bequest utility
  real(dp), parameter :: psi2 = 400000.0_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Number of available labor times
  integer :: Nlt
  
  ! Set of available labor times
  real(dp), dimension(:), allocatable :: setL
  
  ! Labor time (hours)
  real(dp) :: l
    
!==============================================================================
! Medical Expenditures
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of medical expenditures
  ! 1 = Bin  1% -  60%
  ! 2 = Bin 61% -  90%
  ! 3 = Bin 91% - 100%
  integer, parameter :: Nm = 3
  
  ! Set of medical expenditures ($)
  real(dp), dimension(:, :, :), allocatable :: setM
  
  ! Probabilities of medical expenditures
  real(dp), parameter :: PPSI1 = 0.6_dp
  real(dp), parameter :: PPSI2 = 0.3_dp
  real(dp), parameter :: PPSI3 = 0.1_dp
  
  ! Probability distribution of medical expenditures
  real(dp), dimension(:), allocatable :: PPSI
  
  ! Relative price of medical expenditures
  real(dp), parameter :: pii = 2.0855_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Current medical expenditures ($)
  real(dp) :: m
    
!==============================================================================
! Labor Productivity
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of labor productivities
  ! 1 = Bin  1% -  25%
  ! 2 = Bin 26% -  50%
  ! 3 = Bin 51% -  75%
  ! 4 = Bin 75% -  95%
  ! 5 = Bin 96% - 100%
  integer, parameter :: Nz = 5
  
  ! Labor productivities
  real(dp), parameter :: z1 = 0.245323240756989_dp
  real(dp), parameter :: z2 = 0.619762063026428_dp
  real(dp), parameter :: z3 = 1.030915975570670_dp
  real(dp), parameter :: z4 = 1.734226703643790_dp
  real(dp), parameter :: z5 = 3.582611322402950_dp
  
  ! Set of labor productivities
  real(dp), dimension(:), allocatable :: setZ
  
  ! Expected labor productivity
  real(dp), parameter :: Ez = 1.03124686720311_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Current labor productivity
  real(dp) :: z
    
!==============================================================================
! Social Security
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of average lifetime earnings
  integer, parameter :: Nx = 10

  ! Set of average lifetime earnings ($)
  real(dp), dimension(:), allocatable :: setX
  
  ! Social Security Wage Base (SSWB) ($)
  real(dp), parameter :: ySS = 106800.0_dp
  
  ! Bend points of the benefit function (PIA) ($)
  real(dp), parameter :: x1 = 9132.0_dp
  real(dp), parameter :: x2 = 55032.0_dp
  
  ! Parameters of the benefit function (PIA)
  real(dp), parameter :: theta1 = 0.90_dp
  real(dp), parameter :: theta2 = 0.32_dp
  real(dp), parameter :: theta3 = 0.15_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Current average lifetime earnings ($)
  real(dp) :: x

!==============================================================================
! Health Insurance
!==============================================================================

  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of current health insurances
  ! 1 = No coverage
  ! 2 = Medicaid
  ! 3 = EHI
  ! 4 = IHI
  ! 5 = Medicare
  integer, parameter :: Ni = 5
  
  ! Types of health insurance coverage
  integer, parameter :: i0 = 1  ! No coverage
  integer, parameter :: iMA = 2 ! Medicaid
  integer, parameter :: iEE = 3 ! EHI
  integer, parameter :: iII = 4 ! IHI
  integer, parameter :: iMC = 5 ! Medicare
  
  ! Coinsurance rates (%) (Except for Medicare)
  real(dp), dimension(:, :, :, :), allocatable :: q
  
  ! Parameter: 1 - q
  real(dp), dimension(:, :, :, :), allocatable :: one_q
  
  ! Medicare premium ($)
  real(dp), parameter :: pMC = 1326.0_dp
  
  ! Medicare's administrative cost (%)
  real(dp), parameter :: varphi = 0.0160833333333333_dp
  
  ! Parameter: 1 + varphi
  real(dp), parameter :: one_varphi = 1.0_dp + varphi
  
  ! Federal Poverty Level as a percentage of output (%)
  real(dp), parameter :: FPL_Y = 0.362651217734446_dp
  
  ! Medicaid income threshold ($)
  real(dp) :: yMA
  
  ! Medicaid income threshold as a percentage of the FPL (%)
  real(dp), parameter :: yMA_FPL = 0.856448794307_dp
  
  ! Medically Needy income threshold ($)
  real(dp) :: yMN
  
  ! Medically Needy income threshold as a percentage of the FPL (%)
  real(dp), parameter :: yMN_FPL = 0.531545756743501_dp
  
  ! Medically Needy asset threshold ($)
  real(dp), parameter :: aMN = 3415.88095501174_dp
  
  ! Number of EHI offer statuses
  ! 1 = No offer
  ! 2 = Offer
  integer, parameter :: Niota = 2
  
  ! Fraction of the EHI premium paid by the employer (%)
  real(dp), parameter :: omega = 0.784583333333333_dp
  
  ! Parameter: 1 - omega
  real(dp), parameter :: one_omega = 1.0_dp - omega
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Current medical expenditures actually paid ($)
  real(dp), dimension(:, :, :, :), allocatable :: mtil
  
  ! Number of next period's health insurances
  integer :: Nip
  
  ! Set of next period's health insurances
  integer, dimension(:), allocatable :: setIp
  
  ! Premium
  real(dp), dimension(:, :, :), allocatable :: p
  
  ! EHI premium
  real(dp) :: pE
  
  ! IHI premium
  real(dp), dimension(:, :), allocatable :: pI
  
  ! Federal Poverty Level
  real(dp) :: FPL
    
!==============================================================================
! Stochastic Process for Labor Productivity and EHI Oﬀer
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Transition probabilities of labor productivity and EHI offer
  real(dp), dimension(:, :, :, :, :, :), allocatable :: GGAMMA
  
  ! Invariant probabilities of labor productivity and EHI offer
  real(dp), dimension(:, :, :, :), allocatable :: GGAMMAbar
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! No variables

!==============================================================================
! Taxation
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Fraction of taxable income used as limit to deduct medical expenditures
  real(dp), parameter :: xi = (1.0_dp - polDME)*0.075_dp + polDME*0.1_dp
  
  ! Marginal income tax rates
  real(dp), parameter :: tau1 = 0.10_dp
  real(dp), parameter :: tau2 = 0.15_dp
  real(dp), parameter :: tau3 = 0.25_dp
  real(dp), parameter :: tau4 = 0.28_dp
  real(dp), parameter :: tau5 = 0.33_dp
  real(dp), parameter :: tau6 = 0.35_dp
  
  ! Income brackets
  real(dp), parameter :: y1 = 11362.50_dp
  real(dp), parameter :: y2 = 45387.50_dp
  real(dp), parameter :: y3 = 101500.00_dp
  real(dp), parameter :: y4 = 169068.75_dp
  real(dp), parameter :: y5 = 326943.75_dp
  
  ! Social Security tax rate (%)
  real(dp), parameter :: tauSS = 0.124_dp
  
  ! Medicare tax rate (%)
  real(dp), parameter :: tauMC = 0.029_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Consumption tax rate (%)
  real(dp) :: tauC
  
  ! Variable: 1 + tauC
  real(dp) :: one_tauC

!==============================================================================
! Social Insurance
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Minimum level of consumption ($)
  real(dp), parameter :: cbar = 6784.0_dp

  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! No variables
  
!==============================================================================
! Production Sector
!==============================================================================
  
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Total Factor Productivity (TFP)
  real(dp), parameter :: AA = 5.4475_dp
  
  ! Share of capital in the output (%)
  real(dp), parameter :: alpha = 0.335833333333333_dp
  
  ! Parameter: 1 - alpha
  real(dp), parameter :: one_alpha = 1.0_dp - alpha
  
  ! Depreciation rate (%)
  real(dp), parameter :: delta = 0.0713_dp
  
  ! Parameter: 1 - delta
  real(dp), parameter :: one_delta = 1.0_dp - delta

  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Interest rate (%)
  real(dp) :: r
  
  ! Wage rate
  real(dp) :: w
  
  ! Gross interest rate
  real(dp) :: one_r
  
  ! Current aggregate capital ($)
  real(dp) :: KK
  
  ! Aggregate labor (hours)
  real(dp) :: LL
  
  ! Aggregate production ($)
  real(dp) :: YY
  
  ! Wage reduction ($)
  real(dp) :: chi

!==============================================================================
! Health Insurance Sector
!==============================================================================

  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------

  ! Private insurance administrative cost (%)
  real(dp), parameter :: kappa = 0.12_dp
  
  ! Parameter: 1 + kappa
  real(dp), parameter :: one_kappa = 1.0_dp + kappa
  
  ! IHI fixed cost ($)
  real(dp), parameter :: varkappa = 30.8735896358543_dp

  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------

  ! No variables

!==============================================================================
! Government
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Debt-to-capital ratio (%)
  real(dp), parameter :: DKratio = 0.13621859636123_dp
  
  ! Parameter: 1 + DKratio
  real(dp), parameter :: one_DKratio = 1.0_dp + DKratio
  
  ! Government-output ration (%)
  real(dp), parameter :: GYratio = 0.187679984005711_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Current government debt ($)
  real(dp) :: DD
  
  ! Government expenditure ($)
  real(dp) :: GG

!==============================================================================
! State Space
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Number of states of worker
  integer, parameter :: NsW = (RR-1)*Ne*Nh*Na*Nm*Nz*Nx*Niota*(Ni-1)
  
  ! Number of states of retired
  integer, parameter :: NsR = (JJ-RR+1)*Ne*Nh*Na*Nm*Nx

  ! Number of dimensions of the worker's state space
  integer, parameter :: NdimW = 9
  
  ! Number of dimensions of the retired's state space
  integer, parameter :: NdimR = 6

  ! Vector with the size of the dimensions of the worker's state space
  integer, dimension(:), allocatable :: dimW
  
  ! Vector with the size of the dimensions of the retired's state space
  integer, dimension(:), allocatable :: dimR
  
  ! Vector with the size of the jumps made in the worker's vectorized state
  ! space
  integer, dimension(:), allocatable :: jumpW
  
  ! Vector with the size of the jumps made in the retired's vectorized state
  ! space
  integer, dimension(:), allocatable :: jumpR
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! No variables
    
!==============================================================================
! Agents' Problem
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! No parameters
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Worker's value function
  real(dp), dimension(:), allocatable :: VW
  
  ! Worker's policy function of consumption
  real(dp), dimension(:), allocatable :: cW
  
  ! Worker's policy function of assets
  real(dp), dimension(:), allocatable :: aW
  
  ! Worker's policy function of labor time
  real(dp), dimension(:), allocatable :: lW
  
  ! Worker's policy function of health insurance
  integer, dimension(:), allocatable :: iW
  
  ! Retirees' value function
  real(dp), dimension(:), allocatable :: VR
  
  ! Retirees' policy function of consumption
  real(dp), dimension(:), allocatable :: cR
  
  ! Retirees' policy function of assets
  real(dp), dimension(:), allocatable :: aR
  
  ! Net resources before savings
  real(dp) :: netres
  
!==============================================================================
! Agents' Distribution
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! No parameters
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Workers' distribution
  real(dp), dimension(:), allocatable :: lambdaW
  
  ! Retirees' distribution
  real(dp), dimension(:), allocatable :: lambdaR
  
  ! Marginal distribution of assets
  real(dp), dimension(:), allocatable :: margA

!==============================================================================
! Equilibrium Variables
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! No parameters
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Initial guess for aggregate capital
  real(dp) :: KK0
  
  ! Updated aggregate capital
  real(dp) :: KK1
  
  ! Initial guess for aggregate labor
  real(dp) :: LL0
  
  ! Updated aggregate labor
  real(dp) :: LL1
  
  ! Initial guess for bequest transfer
  real(dp) :: BB0
  
  ! Updated bequest transfer
  real(dp) :: BB1
  
  ! Initial guess for EHI premium ($)
  real(dp) :: pE0
  
  ! Updated EHI premium ($)
  real(dp) :: pE1
  
  ! Initial guess for IHI premium ($)
  real(dp), dimension(:, :), allocatable :: pI0
  
  ! Updated IHI premium ($)
  real(dp), dimension(:, :), allocatable :: pI1
  
  ! Initial guess for wage reduction ($)
  real(dp) :: chi0
  
  ! Updated wage reduction ($)
  real(dp) :: chi1
  
  ! Initial guess for consumption tax rate (%)
  real(dp) :: tauC0
  
  ! Updated consumption tax rate (%)
  real(dp) :: tauC1

!==============================================================================
! Aggregate Variables
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! No parameters
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Aggregate consumption ($)
  real(dp) :: CC
  
  ! Aggregate consumption by age ($)
  real(dp), dimension(:), allocatable :: CCj
  
  ! Current aggregate assets ($)
  real(dp) :: aggA
  
  ! Current aggregate assets by age ($)
  real(dp), dimension(:), allocatable :: aggAj
  
  ! Next period's aggregate assets ($)
  real(dp) :: aggAp
  
  ! Next period's aggregate assets by age ($)
  real(dp), dimension(:), allocatable :: aggApj
  
  ! Aggregate income (labor and SS) by age ($)
  real(dp), dimension(:), allocatable :: aggYYj
  
  ! Aggregate Social Security benefit ($)
  real(dp) :: aggSS
  
  ! Aggregate endowment ($)
  real(dp) :: EE
  
  ! Aggregate Medicare expenditures ($)
  real(dp) :: aggMC
  
  ! Aggregate Medicaid expenditures ($)
  real(dp) :: aggMA
  
  ! Aggregate EHI expenditures ($)
  real(dp) :: aggE
  
  ! Aggregate IHI expenditures ($)
  real(dp), dimension(:, :), allocatable :: aggI
  
  ! Aggregate current medical expenditures ($)
  real(dp) :: aggMM
  
  ! Aggregate current medical expenditures actually paid by age ($)
  real(dp), dimension(:), allocatable :: aggMTILj
  
  ! Aggregate total medical expenditures ($)
  real(dp) :: MM
  
  ! Aggregate premiums by age ($)
  real(dp), dimension(:), allocatable :: aggPj
  
  ! Aggregate Social Security taxes ($)
  real(dp) :: aggTSS
  
  ! Aggregate Medicare taxes ($)
  real(dp) :: aggTMC
  
  ! Aggregate Social Insurance transfer ($)
  real(dp) :: aggTSI
  
  ! Aggregate Social Insurance transfer by age ($)
  real(dp), dimension(:), allocatable :: aggTSIj
  
  ! Aggregate total taxes ($)
  real(dp) :: aggTT
  
  ! Aggregate total taxes by age ($)
  real(dp), dimension(:), allocatable :: aggTTj
  
  ! Bequest transfer by age ($)
  real(dp), dimension(:), allocatable :: BBj
  
  ! Demand for EHI in the current period (%)
  real(dp) :: demE
  
  ! Demand for EHI in the next period (%)
  real(dp) :: demEp
  
  ! Demand for IHI in the current period (%)
  real(dp), dimension(:, :), allocatable :: demI
  
  ! Demand for IHI in the next period (%)
  real(dp) :: demIp
    
  ! Fraction of agents uninsured in the current period (%)
  real(dp), dimension(:, :, :), allocatable :: frac0
  
  ! Fraction of agents uninsured in the current period by assets (%)
  real(dp), dimension(:), allocatable :: frac0a
  
  ! Fraction of agents with Medicare in the current period (%)
  real(dp), dimension(:, :), allocatable :: fracMC
  
  ! Fraction of agents with Medicare in the current period by assets (%)
  real(dp), dimension(:), allocatable :: fracMCa
  
  ! Fraction of agents that pay the Medicare premium (%)
  real(dp) :: fracpMC
  
  ! Fraction of agents with Medicaid in the current period (%)
  real(dp), dimension(:, :, :), allocatable :: fracMA
  
  ! Fraction of agents with Medicaid in the current period by assets (%)
  real(dp), dimension(:), allocatable :: fracMAa
  
  ! Fraction of agents with EHI in the current period (%)
  real(dp), dimension(:, :, :), allocatable :: fracE
  
  ! Fraction of agents with EHI in the current period by assets (%)
  real(dp), dimension(:), allocatable :: fracEa
  
  ! Fraction of agents with IHI in the current period (%)
  real(dp), dimension(:, :, :), allocatable :: fracI
  
  ! Fraction of agents with IHI in the current period by assets (%)
  real(dp), dimension(:), allocatable :: fracIa
  
  ! Fraction of agents with an EHI offer in the current period (%)
  real(dp) :: fracOffer
  
  ! Fraction of agents with an EHI offer in the current period that choose EHI (%)
  real(dp) :: fracOfferE
  
  ! Aggregate Labor that received an EHI offer
  real(dp) :: LLE
  
  ! Next period's aggregate capital ($)
  real(dp) :: KKp
  
  ! Next period's government debt ($)
  real(dp) :: DDp
  
  ! Excess demand ($)
  real(dp) :: ex_dem

!==============================================================================
! Welfare Measures
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! No parameters
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Expected value function of newborns
  real(dp) :: aggV1
  
  ! Expected value function of newborns by labor productivity
  real(dp), dimension(:), allocatable :: aggV1z
  
  ! Expected value function of newborns by health status
  real(dp), dimension(:), allocatable :: aggV1h

!==============================================================================
! Health Care Reform
!==============================================================================

  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Fraction of income that determines whether IHI is affordable (%)
  real(dp), parameter :: dI = 0.08_dp
  
  ! Fraction of income that determines whether EHI is affordable (%)
  real(dp), parameter :: dE = 0.095_dp
  
  ! Fractions of income spent on insurance used to calculate the Premium Tax
  ! Credit (%)
  real(dp), parameter :: varepsilon1 = 0.02000_dp
  real(dp), parameter :: varepsilon2 = 0.03500_dp
  real(dp), parameter :: varepsilon3 = 0.05150_dp
  real(dp), parameter :: varepsilon4 = 0.07175_dp
  real(dp), parameter :: varepsilon5 = 0.08775_dp
  real(dp), parameter :: varepsilon6 = 0.09500_dp
  
  ! Fractions of the FPL that determine the income categories used to calculate
  ! the Premium Tax Credit (%)
  real(dp), parameter :: g1 = 1.00_dp
  real(dp), parameter :: g2 = 1.33_dp
  real(dp), parameter :: g3 = 1.50_dp
  real(dp), parameter :: g4 = 2.00_dp
  real(dp), parameter :: g5 = 2.50_dp
  real(dp), parameter :: g6 = 3.00_dp
  real(dp), parameter :: g7 = 4.00_dp
  
  ! Minimum value of the Individual Mandate penalty ($)
  real(dp), parameter :: upsilon = 695.0_dp
  
  ! Fraction of taxable income used to calculate the Individual Mandate
  ! penalty (%)
  real(dp), parameter :: nu = 0.025_dp
  
  ! Maximum value of the Individual Mandate penalty ($)
  real(dp), parameter :: rho = 2085.0_dp
  
  ! Fraction of the FPL used in the Medicaid expansion (%)
  real(dp), parameter :: zeta = 1.38_dp

  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! Aggregate Premium Tax Credits ($)
  real(dp) :: aggTP
  
  ! Aggregate Individual Mandate penalty ($)
  real(dp) :: aggTM

!==============================================================================
! Targets
!==============================================================================
    
  !----------------------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------------------
  
  ! Fraction of the population older than 65 years
  real(dp), parameter :: targetPopOlder65 = 0.1255_dp
  
  ! Capital-output ratio
  real(dp), parameter :: targetKY = 3.02_dp
  
  ! Average GDP per capita in 2010 USD
  real(dp), parameter :: targetYY = 44854.87_dp
  
  ! Average medical expenditures per capita in 2010 USD
  real(dp), parameter :: targetMM = 7799.95_dp
  
  ! Fraction of population uninsured
  real(dp), parameter :: targetFracUnin = 0.1234_dp
  
  !----------------------------------------------------------------------------
  ! Variables
  !----------------------------------------------------------------------------
  
  ! No variables
    
end module mod_globals