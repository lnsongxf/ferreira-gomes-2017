subroutine sub_calibration

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype
  use mod_array_vec
  use mod_globals

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Probability Distribution of Education Level
!==============================================================================

  LLAMBDA = [LLAMBDAL, LLAMBDAH]

!==============================================================================
! Transition Probabilities of Health Status
!==============================================================================

  open(unit=1, file="./inputs/transition_probs_health_status.txt")
  
  do j = 1, JJ
    do ie = 1, Ne
      do ih = 1, Nh
        do ihp = 1, Nh
          read(1, *) PPHI(ihp, ih, ie, j)
        end do  ! ihp
      end do  ! ih
    end do  ! ie
  end do  ! j
  
  close(1)

!==============================================================================
! Survival Probabilities
!==============================================================================
    
  open(unit=1, file="./inputs/survival_probs.txt")
  
  do j = 1, JJ
    do ie = 1, Ne
      do ih = 1, Nh
        read(1, *) PPI(ih, ie, j)
      end do  ! ih
    end do  ! ie
  end do  ! j
  
  close(1)

!==============================================================================
! Parameter: 1 - PPI
!==============================================================================

  one_PPI = 1.0_dp - PPI

!==============================================================================
! Share of types (j, e, h)
!==============================================================================

  open(unit=1, file="./inputs/share_types.txt")
  
  do j = 1, JJ
    do ie = 1, Ne
      do ih = 1, Nh
        read(1, *) mu(ih, ie, j)
      end do  ! ih
    end do  ! ie
  end do  ! j
  
  close(1)

!==============================================================================
! Set of Current Assets
!==============================================================================

  open(unit=1, file="./inputs/set_assets.txt")
  
  do ia = 1, Na
    read(1, *) setA(ia)
  end do
  
  close(1)

!==============================================================================
! Probability Distribution of Assets
!==============================================================================

  open(unit=1, file="./inputs/probs_assets.txt")
  
  do ia = 1, Na
    read(1, *) OOMEGA(ia)
  end do
  
  close(1)

!==============================================================================
! Set of Medical Expenditures
!==============================================================================

  open(unit=1, file="./inputs/set_medical_expenditures.txt")
  
  do j = 1, JJ
    do ih = 1, Nh
      do im = 1, Nm
        read(1, *) setM(im, ih, j)
      end do  ! im
    end do  ! ih
  end do  ! j
  
  close(1)
  
  setM = (1.0_dp - costRed)*pii*setM
  
!==============================================================================
! Probability Distribution of Medical Expenditures
!==============================================================================

  PPSI = [PPSI1, PPSI2, PPSI3]

!==============================================================================
! Set of Labor Productivities
!==============================================================================

  setZ = [z1, z2, z3, z4, z5]

!==============================================================================
! Set of Average Lifetime Earnings
!==============================================================================

  open(unit=1, file="./inputs/set_avg_lifetime_earnings.txt")
  
  do ix = 1, Nx
    read(1, *) setX(ix)
  end do
  
  close(1)
  
!==============================================================================
! Coinsurance Rates
!==============================================================================

  open(unit=1, file="./inputs/coinsurance_rates.txt")
  
  do j = 1, JJ
    do ih = 1, Nh
      do im = 1, Nm
        do ii = 1, Ni
          read(1, *) q(ii, im, ih, j)
        end do  ! ii
      end do  ! im
    end do  ! ih
  end do  ! j
  
  close(1)

!==============================================================================
! Parameter: 1 - q
!==============================================================================

  one_q = 1.0_dp - q

!==============================================================================
! Current Medical Expenditures Actually Paid
!==============================================================================

  do j = 1, JJ
    do ih = 1, Nh
      do im = 1, Nm
        m = setM(im, ih, j)
        do ii = 1, Ni
          mtil(ii, im, ih, j) = one_q(ii, im, ih, j)*m
        end do  ! ii
      end do  ! im
    end do  ! ih
  end do  ! j

!==============================================================================
! No Coverage and Medicaid Premiums
!==============================================================================

  ! No coverage
  p(i0, :, :) = 0.0_dp
      
  ! Medicaid
  p(iMA, :, :) = 0.0_dp

!==============================================================================
! Transition Probabilities of Labor Productivity and EHI Offer
!==============================================================================

  open(unit=1, file="./inputs/transition_probs_productivity_ehi_offer.txt")
  
  do j = 1, (RR-1)
    do ie = 1, Ne
      do iz = 1, Nz
        do iiota = 1, Niota
          do izp = 1, Nz
            do iiotap = 1, Niota
              read(1, *) GGAMMA(iiotap, izp, iiota, iz, ie, j)
            end do  ! iiotap
          end do  ! izp
        end do  ! iiota
      end do  ! iz
    end do  ! ie
  end do  ! j
  
  close(1)
    
!==============================================================================
! Invariant Probabilities of Labor Productivity and EHI Offer
!==============================================================================

  open(unit=1, file="./inputs/invariant_probs_productivity_ehi_offer.txt")

  do j = 1, (RR-1)
    do ie = 1, Ne
      do iz = 1, Nz
        do iiota = 1, Niota
          read(1, *) GGAMMAbar(iiota, iz, ie, j)
        end do  ! iiota
      end do  ! iz
    end do  ! ie
  end do  ! j
  
  close(1)

!==============================================================================
! Size of the Dimensions of the Worker's State Space
!==============================================================================

  ! 1st: Age
  ! 2nd: Education level
  ! 3rd: Health status
  ! 4th: Assets
  ! 5th: Medical expenditures
  ! 6th: Labor productivity
  ! 7th: Average lifetime earnings
  ! 8th: EHI offer status
  ! 9th: Health insurance
  dimW = [RR-1, Ne, Nh, Na, Nm, Nz, Nx, Niota, Ni-1]

!==============================================================================
! Size of the Dimensions of the Retired's State Space
!==============================================================================

  ! 1st: Age
  ! 2nd: Education level
  ! 3rd: Health status
  ! 4th: Assets
  ! 5th: Medical expenditures
  ! 6th: Average lifetime earnings
  dimR = [JJ-RR+1, Ne, Nh, Na, Nm, Nx]

!==============================================================================
! Size of the Jumps Made in the Worker's Vectorized State Space
!==============================================================================

  call sub_calc_jump(dimW, jumpW)
    
!==============================================================================
! Size of the Jumps Made in the Retired's Vectorized State Space
!==============================================================================

  call sub_calc_jump(dimR, jumpR)

end subroutine sub_calibration