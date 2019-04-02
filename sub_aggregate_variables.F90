subroutine sub_aggregate_variables

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype
  use mod_array_vec
  use mod_globals
  use mod_functions

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Initialize Aggregate Variables
!==============================================================================
  
  ! Aggregate consumption by age
  CCj = 0.0_dp
  
  ! Current aggregate assets by age
  aggAj = 0.0_dp
  
  ! Next period's aggregate assets by age
  aggApj = 0.0_dp
  
  ! Aggregate income (labor and SS) by age
  aggYYj = 0.0_dp
  
  ! Aggregate endowment
  EE = 0.0_dp
  
  ! Aggregate current medical expenditures
  aggMM = 0.0_dp
  
  ! Aggregate current medical expenditures actually paid by age
  aggMTILj = 0.0_dp
  
  ! Aggregate premiums by age
  aggPj = 0.0_dp
  
  ! Aggregate Social Security taxes
  aggTSS = 0.0_dp
  
  ! Aggregate Medicare taxes
  aggTMC = 0.0_dp
  
  ! Aggregate Social Insurance transfer by age
  aggTSIj = 0.0_dp
  
  ! Aggregate total taxes by age
  aggTTj = 0.0_dp
  
  ! Bequest transfer by age
  BBj = 0.0_dp
  
  ! Demand for IHI in the next period
  demIp = 0.0_dp
  
  ! Fraction of agents uninsured in the current period
  frac0 = 0.0_dp
  
  ! Fraction of agents uninsured in the current period by assets
  frac0a = 0.0_dp
  
  ! Fraction of agents with Medicare in the current period
  fracMC = 0.0_dp
  
  ! Fraction of agents with Medicare in the current period by assets
  fracMCa = 0.0_dp
    
  ! Fraction of agents with Medicaid in the current period
  fracMA = 0.0_dp
  
  ! Fraction of agents with Medicaid in the current period by assets
  fracMAa = 0.0_dp
  
  ! Fraction of agents with EHI in the current period
  fracE = 0.0_dp
  
  ! Fraction of agents with EHI in the current period by assets
  fracEa = 0.0_dp
  
  ! Fraction of agents with IHI in the current period
  fracI = 0.0_dp
  
  ! Fraction of agents with IHI in the current period by assets
  fracIa = 0.0_dp
  
  ! Fraction of agents with an EHI offer in the current period
  fracOffer = 0.0_dp
  
  ! Fraction of agents with an EHI offer in the current period that choose EHI
  fracOfferE = 0.0_dp
  
  ! Expected value function of newborns
  aggV1 = 0.0_dp
  
  ! Expected value function of newborns by labor productivity
  aggV1z = 0.0_dp
  
  ! Expected value function of newborns by health status
  aggV1h = 0.0_dp
  
  ! Marginal distribution of assets
  margA = 0.0_dp
  
!==============================================================================
! Aggregate Variables: j = 1
!==============================================================================

  ! First age
  j = 1
    
  ! Loop: education level
  do ie = 1, Ne
    
    ! Loop: current health status
    do ih = 1, Nh
    
      ! Loop: current assets
      do ia = 1, Na
        
        ! Current assets
        a = setA(ia)
        
        ! Loop: current medical expenditures
        do im = 1, Nm
          
          ! Current medical expenditures
          m = setM(im, ih, j)
          
          ! Loop: current labor productivity
          do iz = 1, Nz
      
            ! Current labor productivity
            z = setZ(iz)
      
            ! Loop: current average lifetime earnings
            do ix = 1, Nx
              
              ! Loop: current EHI offer status
              do iiota = 1, Niota
                
                ! Loop: current health insurance
                do ii = 1, Ni-1
                
                  ! Index of the vectorized state
                  isW = ind([j, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
                  
                  ! Aggregate consumption by age
                  CCj(j) = CCj(j) + cW(isW)*lambdaW(isW)
                  
                  ! Current aggregate assets by age
                  aggAj(j) = aggAj(j) + a*lambdaW(isW)
  
                  ! Next period's aggregate assets by age
                  aggApj(j) = aggApj(j) + aW(isW)*lambdaW(isW)
                  
                  ! Aggregate income (labor and SS) by age
                  aggYYj(j) = aggYYj(j) + y(z, iiota, lW(isW))*lambdaW(isW)
                  
                  ! Aggregate endowment
                  EE = EE + one_r*a*lambdaW(isW)
                  
                  ! Aggregate current medical expenditures
                  aggMM = aggMM + m*lambdaW(isW)
                  
                  ! Aggregate current medical expenditures actually paid by age
                  aggMTILj(j) = aggMTILj(j) + mtil(ii, im, ih, j)*lambdaW(isW)
                  
                  ! Aggregate premiums by age
                  aggPj(j) = aggPj(j) + p(iW(isW), ih, j)*lambdaW(isW)
                  
                  ! Aggregate Social Security taxes
                  aggTSS = aggTSS                                       &
                         + TSS(z, iiota, lW(isW), iW(isW))*lambdaW(isW)
              
                  ! Aggregate Medicare taxes
                  aggTMC = aggTMC                                       &
                         + TMC(z, iiota, lW(isW), iW(isW))*lambdaW(isW)
                  
                  ! Aggregate Social Insurance transfer by age
                  aggTSIj(j) = aggTSIj(j)                                                                     &
                             + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iW(isW), 0.0_dp)*lambdaW(isW)
                  
                  ! Aggregate total taxes by age
                  aggTTj(j) = aggTTj(j)                                                     &
                            + TT(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))*lambdaW(isW)
                  
                  ! Bequest transfer by age
                  BBj(j) = BBj(j) + BB*lambdaW(isW)
                  
                  ! Demand for IHI in the next period
                  demIp = demIp + Ieq(iW(isW), iII)*lambdaW(isW)
                  
                  ! Fraction of agents uninsured in the current period
                  frac0(ih, ie, j) = frac0(ih, ie, j) + Ieq(ii, i0)*lambdaW(isW)
                  
                  ! Fraction of agents uninsured in the current period by assets
                  frac0a(ia) = frac0a(ia) + Ieq(ii, i0)*lambdaW(isW)
                  
                  ! Fraction of agents with an EHI offer in the current period
                  fracOffer = fracOffer + Ieq(iiota, 2)*lambdaW(isW)
                  
                  ! Fraction of agents with an EHI offer in the current period that choose EHI
                  fracOfferE = fracOfferE + Ieq(iiota, 2)*Ieq(iW(isW), iEE)*lambdaW(isW)
                  
                  ! Expected value function of newborns
                  aggV1 = aggV1 + VW(isW)*lambdaW(isW)
                  
                  ! Expected value function of newborns by labor productivity
                  aggV1z(iz) = aggV1z(iz) + VW(isW)*lambdaW(isW)
                  
                  ! Expected value function of newborns by health status
                  aggV1h(ih) = aggV1h(ih) + VW(isW)*lambdaW(isW)
                  
                  ! Marginal distribution of assets
                  margA(ia) = margA(ia) + lambdaW(isW)
                  
                end do  ! ii
              
              end do  ! iiota
              
            end do  ! ix
            
          end do  ! iz
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie

!==============================================================================
! Aggregate Variables: j in {2,...,RR-2}
!==============================================================================

  ! Loop: age
  do j = 2, (RR-2)
    
    ! Loop: education level
    do ie = 1, Ne
      
      ! Loop: current health status
      do ih = 1, Nh
      
        ! Loop: current assets
        do ia = 1, Na
          
          ! Current assets
          a = setA(ia)
          
          ! Loop: current medical expenditures
          do im = 1, Nm
            
            ! Current medical expenditures
            m = setM(im, ih, j)
            
            ! Loop: current labor productivity
            do iz = 1, Nz
        
              ! Current labor productivity
              z = setZ(iz)
        
              ! Loop: current average lifetime earnings
              do ix = 1, Nx
                
                ! Loop: current EHI offer status
                do iiota = 1, Niota
                  
                  ! Loop: current health insurance
                  do ii = 1, Ni-1
                  
                    ! Index of the vectorized state
                    isW = ind([j, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
                    
                    ! Aggregate consumption by age
                    CCj(j) = CCj(j) + cW(isW)*lambdaW(isW)
                    
                    ! Current aggregate assets by age
                    aggAj(j) = aggAj(j) + a*lambdaW(isW)
  
                    ! Next period's aggregate assets by age
                    aggApj(j) = aggApj(j) + aW(isW)*lambdaW(isW)
                    
                    ! Aggregate income (labor and SS) by age
                    aggYYj(j) = aggYYj(j) + y(z, iiota, lW(isW))*lambdaW(isW)
                    
                    ! Aggregate current medical expenditures
                    aggMM = aggMM + m*lambdaW(isW)
                    
                    ! Aggregate current medical expenditures actually paid by age
                    aggMTILj(j) = aggMTILj(j) + mtil(ii, im, ih, j)*lambdaW(isW)
                    
                    ! Aggregate premiums by age
                    aggPj(j) = aggPj(j) + p(iW(isW), ih, j)*lambdaW(isW)
                    
                    ! Aggregate Social Security taxes
                    aggTSS = aggTSS                                       &
                           + TSS(z, iiota, lW(isW), iW(isW))*lambdaW(isW)
                
                    ! Aggregate Medicare taxes
                    aggTMC = aggTMC                                       &
                           + TMC(z, iiota, lW(isW), iW(isW))*lambdaW(isW)
                    
                    ! Aggregate Social Insurance transfer by age
                    aggTSIj(j) = aggTSIj(j)                                                                     &
                               + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iW(isW), 0.0_dp)*lambdaW(isW)
                    
                    ! Aggregate total taxes by age
                    aggTTj(j) = aggTTj(j)                                                     &
                              + TT(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))*lambdaW(isW)
                    
                    ! Bequest transfer by age
                    BBj(j) = BBj(j) + BB*lambdaW(isW)
                    
                    ! Demand for IHI in the next period
                    demIp = demIp + Ieq(iW(isW), iII)*lambdaW(isW)
                                       
                    ! Fraction of agents uninsured in the current period
                    frac0(ih, ie, j) = frac0(ih, ie, j) + Ieq(ii, i0)*lambdaW(isW)
                    
                    ! Fraction of agents uninsured in the current period by assets
                    frac0a(ia) = frac0a(ia) + Ieq(ii, i0)*lambdaW(isW)
                    
                    ! Fraction of agents with Medicaid in the current period
                    fracMA(ih, ie, j) = fracMA(ih, ie, j) + Ieq(ii, iMA)*lambdaW(isW)
                    
                    ! Fraction of agents with Medicaid in the current period by assets
                    fracMAa(ia) = fracMAa(ia) + Ieq(ii, iMA)*lambdaW(isW)
                    
                   ! Fraction of agents with EHI in the current period
                    fracE(ih, ie, j) = fracE(ih, ie, j) + Ieq(ii, iEE)*lambdaW(isW)
                    
                    ! Fraction of agents with EHI in the current period by assets
                    fracEa(ia) = fracEa(ia) + Ieq(ii, iEE)*lambdaW(isW)
                      
                    ! Fraction of agents with IHI in the current period
                    fracI(ih, ie, j) = fracI(ih, ie, j) + Ieq(ii, iII)*lambdaW(isW)
                    
                    ! Fraction of agents with IHI in the current period by assets
                    fracIa(ia) = fracIa(ia) + Ieq(ii, iII)*lambdaW(isW)
                    
                    ! Fraction of agents with an EHI offer in the current period
                    fracOffer = fracOffer + Ieq(iiota, 2)*lambdaW(isW)
                    
                    ! Fraction of agents with an EHI offer in the current period that choose EHI
                    fracOfferE = fracOfferE + Ieq(iiota, 2)*Ieq(iW(isW), iEE)*lambdaW(isW)
                    
                    ! Marginal distribution of assets
                    margA(ia) = margA(ia) + lambdaW(isW)
                    
                  end do  ! ii
                
                end do  ! iiota
                
              end do  ! ix
              
            end do  ! iz
          
          end do  ! im
          
        end do  ! ia
      
      end do  ! ih
      
    end do  ! ie

  end do  ! j

!==============================================================================
! Aggregate Variables: j = RR-1
!==============================================================================

  ! Age before retirement
  j = RR-1
  
  ! Loop: education level
  do ie = 1, Ne
  
    ! Loop: current health status
    do ih = 1, Nh
    
      ! Loop: current assets
      do ia = 1, Na
      
        ! Current assets
        a = setA(ia)
        
        ! Loop: current medical expenditures
        do im = 1, Nm
        
          ! Current medical expenditures
          m = setM(im, ih, j)
          
          ! Loop: current labor productivity
          do iz = 1, Nz
          
            ! Current labor productivity
            z = setZ(iz)
      
            ! Loop: current average lifetime earnings
            do ix = 1, Nx
              
              ! Loop: current EHI offer status
              do iiota = 1, Niota
                
                ! Loop: current health insurance
                do ii = 1, Ni-1
                
                  ! Index of the vectorized state
                  isW = ind([j, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
                  
                  ! Aggregate consumption by age
                  CCj(j) = CCj(j) + cW(isW)*lambdaW(isW)
                  
                  ! Current aggregate assets by age
                  aggAj(j) = aggAj(j) + a*lambdaW(isW)
  
                  ! Next period's aggregate assets by age
                  aggApj(j) = aggApj(j) + aW(isW)*lambdaW(isW)
                  
                  ! Aggregate income (labor and SS) by age
                  aggYYj(j) = aggYYj(j) + y(z, iiota, lW(isW))*lambdaW(isW)
                  
                  ! Aggregate current medical expenditures
                  aggMM = aggMM + m*lambdaW(isW)
                  
                  ! Aggregate current medical expenditures actually paid by age
                  aggMTILj(j) = aggMTILj(j) + mtil(ii, im, ih, j)*lambdaW(isW)
                  
                  ! Aggregate premiums by age
                  aggPj(j) = aggPj(j) + pMC*lambdaW(isW)
                
                  ! Aggregate Social Security taxes
                  aggTSS = aggTSS + TSS(z, iiota, lW(isW), iMC)*lambdaW(isW)
                
                  ! Aggregate Medicare taxes
                  aggTMC = aggTMC + TMC(z, iiota, lW(isW), iMC)*lambdaW(isW)
                  
                  ! Aggregate Social Insurance transfer by age
                  aggTSIj(j) = aggTSIj(j)                                                              &
                             + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iMC, pMC)*lambdaW(isW)
                  
                  ! Aggregate total taxes by age
                  aggTTj(j) = aggTTj(j)                                                 &
                            + TT(j, ih, a, im, z, iiota, ii, lW(isW), iMC)*lambdaW(isW)
                  
                  ! Bequest transfer by age
                  BBj(j) = BBj(j) + BB*lambdaW(isW)
                  
                  ! Fraction of agents uninsured in the current period
                  frac0(ih, ie, j) = frac0(ih, ie, j) + Ieq(ii, i0)*lambdaW(isW)
                  
                  ! Fraction of agents uninsured in the current period by assets
                  frac0a(ia) = frac0a(ia) + Ieq(ii, i0)*lambdaW(isW)
                  
                  ! Fraction of agents with Medicaid in the current period
                  fracMA(ih, ie, j) = fracMA(ih, ie, j) + Ieq(ii, iMA)*lambdaW(isW)
                  
                  ! Fraction of agents with Medicaid in the current period by assets
                  fracMAa(ia) = fracMAa(ia) + Ieq(ii, iMA)*lambdaW(isW)
                  
                  ! Fraction of agents with EHI in the current period
                  fracE(ih, ie, j) = fracE(ih, ie, j) + Ieq(ii, iEE)*lambdaW(isW)
                  
                  ! Fraction of agents with EHI in the current period by assets
                  fracEa(ia) = fracEa(ia) + Ieq(ii, iEE)*lambdaW(isW)
                  
                  ! Fraction of agents with IHI in the current period
                  fracI(ih, ie, j) = fracI(ih, ie, j) + Ieq(ii, iII)*lambdaW(isW)
                  
                  ! Fraction of agents with IHI in the current period by assets
                  fracIa(ia) = fracIa(ia) + Ieq(ii, iII)*lambdaW(isW)
                  
                  ! Fraction of agents with an EHI offer in the current period
                  fracOffer = fracOffer + Ieq(iiota, 2)*lambdaW(isW)
                  
                  ! Marginal distribution of assets
                  margA(ia) = margA(ia) + lambdaW(isW)
                  
                end do  ! ii
              
              end do  ! iiota
              
            end do  ! ix
          
          end do  ! iz
        
        end do  ! im
      
      end do  ! ia
    
    end do  ! ih
  
  end do  ! ie

!==============================================================================
! Aggregate Variables: j in {RR,...,JJ-1}
!==============================================================================

  ! Loop: age
  do j = RR, (JJ-1)
    
    ! Loop: education level
    do ie = 1, Ne
      
      ! Loop: current health status
      do ih = 1, Nh
      
        ! Loop: current assets
        do ia = 1, Na
      
          ! Current assets
          a = setA(ia)
          
          ! Loop: current medical expenditures
          do im = 1, Nm
            
            ! Current medical expenditures
            m = setM(im, ih, j)
            
            ! Loop: current average lifetime earnings
            do ix = 1, Nx
        
              ! Current average lifetime earnings
              x = setX(ix)
        
              ! Index of the vectorized state space
              isR = ind([j-RR+1, ie, ih, ia, im, ix], jumpR)
              
              ! Aggregate consumption by age
              CCj(j) = CCj(j) + cR(isR)*lambdaR(isR)
              
              ! Current aggregate assets by age
              aggAj(j) = aggAj(j) + a*lambdaR(isR)
  
              ! Next period's aggregate assets by age
              aggApj(j) = aggApj(j) + aR(isR)*lambdaR(isR)
              
              ! Aggregate income (labor and SS) by age
              aggYYj(j) = aggYYj(j) + b(x)*lambdaR(isR)
              
              ! Aggregate current medical expenditures
              aggMM = aggMM + m*lambdaR(isR)
              
              ! Aggregate current medical expenditures actually paid by age
              aggMTILj(j) = aggMTILj(j) + mtil(iMC, im, ih, j)*lambdaR(isR)
              
              ! Aggregate premiums by age
              aggPj(j) = aggPj(j) + pMC*lambdaR(isR)
              
              ! Aggregate Social Insurance transfer by age
              aggTSIj(j) = aggTSIj(j)                                                          &
                         + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, iMC, pMC)*lambdaR(isR)
              
              ! Aggregate total taxes by age
              aggTTj(j) = aggTTj(j)                                                  &
                        + TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, iMC)*lambdaR(isR)
              
              ! Bequest transfer by age
              BBj(j) = BBj(j) + BB*lambdaR(isR)
              
              ! Fraction of agents with Medicare in the current period
              fracMC(ih, ie) = fracMC(ih, ie) + lambdaR(isR)
              
              ! Fraction of agents with Medicare in the current period by assets
              fracMCa(ia) = fracMCa(ia) + lambdaR(isR)
              
              ! Marginal distribution of assets
              margA(ia) = margA(ia) + lambdaR(isR)
              
            end do  ! ix
          
          end do  ! im
          
        end do  ! ia
      
      end do  ! ih
      
    end do  ! ie

  end do  ! j

!==============================================================================
! Aggregate Variables: j = JJ
!==============================================================================

  ! Last age
  j = JJ

  ! Loop: education level
  do ie = 1, Ne
  
    ! Loop: current health status
    do ih = 1, Nh
    
      ! Loop: current assets
      do ia = 1, Na
      
        ! Current assets
        a = setA(ia)
        
        ! Loop: current medical expenditures
        do im = 1, Nm
        
          ! Current medical expenditures
          m = setM(im, ih, j)
          
          ! Loop: current average lifetime earnings
          do ix = 1, Nx
            
            ! Current average lifetime earnings
            x = setX(ix)
            
            ! Index of the vectorized state space
            isR = ind([j-RR+1, ie, ih, ia, im, ix], jumpR)
            
            ! Aggregate consumption by age
            CCj(j) = CCj(j) + cR(isR)*lambdaR(isR)
            
            ! Current aggregate assets by age
            aggAj(j) = aggAj(j) + a*lambdaR(isR)
  
            ! Next period's aggregate assets by age
            aggApj(j) = aggApj(j) + aR(isR)*lambdaR(isR)
            
            ! Aggregate income (labor and SS) by age
            aggYYj(j) = aggYYj(j) + b(x)*lambdaR(isR)
            
            ! Aggregate current medical expenditures
            aggMM = aggMM + m*lambdaR(isR)
            
            ! Aggregate current medical expenditures actually paid by age
            aggMTILj(j) = aggMTILj(j) + mtil(iMC, im, ih, j)*lambdaR(isR)
            
            ! Aggregate Social Insurance transfer by age
            aggTSIj(j) = aggTSIj(j)                                                            &
                       + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, i0, 0.0_dp)*lambdaR(isR)
            
            ! Aggregate total taxes by age
            aggTTj(j) = aggTTj(j)                                                 &
                      + TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, i0)*lambdaR(isR)
            
            ! Bequest transfer by age
            BBj(j) = BBj(j) + BB*lambdaR(isR)
            
            ! Fraction of agents with Medicare in the current period
            fracMC(ih, ie) = fracMC(ih, ie) + lambdaR(isR)
            
            ! Fraction of agents with Medicare in the current period by assets
            fracMCa(ia) = fracMCa(ia) + lambdaR(isR)
            
            ! Marginal distribution of assets
            margA(ia) = margA(ia) + lambdaR(isR)
            
          end do  ! ix
        
        end do  ! im
      
      end do  ! ia
    
    end do  ! ih
  
  end do  ! ie

!==============================================================================
! Aggregate Final Medical Expenditures
!==============================================================================

  MM = aggMM + varphi*aggMC + kappa*aggE + kappa*sum(aggI) + varkappa*demIp

!==============================================================================
! Excess Demand
!==============================================================================

  ex_dem = CC + KKp + MM + GG - YY - one_delta*KK - EE

end subroutine sub_aggregate_variables