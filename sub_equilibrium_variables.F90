subroutine sub_equilibrium_variables

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
! Initialize Equilibrium Variables
!==============================================================================
  
  ! Updated aggregate labor
  LL1 = 0.0_dp
  
  ! Updated bequest transfer
  BB1 = 0.0_dp

!==============================================================================
! Initialize Aggregate Variables
!==============================================================================

  ! Aggregate consumption
  CC = 0.0_dp
  
  ! Current aggregate assets
  aggA = 0.0_dp
  
  ! Next period's aggregate assets
  aggAp = 0.0_dp
  
  ! Aggregate Social Security benefit
  aggSS = 0.0_dp
  
  ! Aggregate Medicare expenditures
  aggMC = 0.0_dp
  
  ! Aggregate Medicaid expenditures
  aggMA = 0.0_dp
  
  ! Aggregate EHI expenditures
  aggE = 0.0_dp
  
  ! Aggregate IHI expenditures
  aggI = 0.0_dp
  
  ! Aggregate Social Insurance transfer
  aggTSI = 0.0_dp
  
  ! Aggregate total taxes
  aggTT = 0.0_dp
  
  ! Demand for EHI in the current period
  demE = 0.0_dp
  
  ! Demand for EHI in the next period
  demEp = 0.0_dp
  
  ! Demand for IHI in the current period
  demI = 0.0_dp
  
  ! Fraction of agents that pay the Medicare premium
  fracpMC = 0.0_dp
  
  ! Aggregate Labor that received an EHI offer
  LLE = 0.0_dp
  
  ! Aggregate Premium Tax Credits
  aggTP = 0.0_dp
  
  ! Aggregate Individual Mandate penalty
  aggTM = 0.0_dp

!==============================================================================
! Equilibrium Variables: j = 1
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
                  
                  ! Updated aggregate labor
                  LL1 = LL1 + z*lW(isW)*lambdaW(isW)
                  
                  ! Aggregate consumption
                  CC = CC + cW(isW)*lambdaW(isW)
                  
                  ! Next period's aggregate assets
                  aggAp = aggAp &
                        + (aW(isW) + pE*Ieq(iW(isW), iEE) + (pI(ih, j) - varkappa)*Ieq(iW(isW), iII))*lambdaW(isW)
                  
                  ! Aggregate Social Insurance transfer
                  aggTSI = aggTSI                                                            &
                         + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iW(isW), 0.0_dp) &
                           *lambdaW(isW)
                  
                  ! Aggregate total taxes
                  aggTT = aggTT                                            &
                        + TT(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW)) &
                          *lambdaW(isW)
                  
                  ! Demand for EHI in the next period
                  demEp = demEp + Ieq(iW(isW), iEE)*lambdaW(isW)
                  
                  ! Aggregate Labor that received an EHI offer
                  LLE = LLE + z*lW(isW)*Ieq(iiota, 2)*lambdaW(isW)
                  
                  ! Aggregate Premium Tax Credits
                  aggTP = aggTP + TP(j, ih, a, z, iiota, lW(isW), iW(isW))*lambdaW(isW)
                  
                  ! Aggregate Individual Mandate penalty
                  aggTM = aggTM + TM(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))*lambdaW(isW)
                  
                end do  ! ii
              
              end do  ! iiota
              
            end do  ! ix
      
          end do  ! iz
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie
    
!==============================================================================
! Equilibrium Variables: j in {2,...,RR-2}
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
                    
                    ! Updated aggregate labor
                    LL1 = LL1 + z*lW(isW)*lambdaW(isW)
              
                    ! Updated bequest transfer
                    BB1 = BB1                                     &
                        + (one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                          *(one_r*a - tauY(r*a))                  &
                          *lambdaW(isW)
                    
                    ! Aggregate consumption
                    CC = CC + cW(isW)*lambdaW(isW)
                    
                    ! Aggregate assets
                    aggA = aggA &
                         + ((a + pE*Ieq(ii, iEE) + (pI(ih, j-1) - varkappa)*Ieq(ii, iII))/PPI(ih, ie, j-1))*lambdaW(isW)
                    
                    ! Next period's aggregate assets
                    aggAp = aggAp &
                          + (aW(isW) + pE*Ieq(iW(isW), iEE) + (pI(ih, j) - varkappa)*Ieq(iW(isW), iII))*lambdaW(isW)
                    
                    ! Aggregate Medicaid expenditures
                    aggMA = aggMA + m*q(ii, im, ih, j)*Ieq(ii, iMA)*lambdaW(isW)
                    
                    ! Aggregate EHI expenditures
                    aggE = aggE + m*q(ii, im, ih, j)*Ieq(ii, iEE)*lambdaW(isW)
                    
                    ! Aggregate IHI expenditures
                    aggI(ih, j-1) = aggI(ih, j-1) + m*q(ii, im, ih, j)*Ieq(ii, iII)*lambdaW(isW)
                    
                    ! Aggregate Social Insurance transfer
                    aggTSI = aggTSI                                                            &
                           + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iW(isW), 0.0_dp) &
                             *lambdaW(isW)
                    
                    ! Aggregate total taxes
                    aggTT = aggTT                                               &
                          + ( TT(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))  &
                            + tauY(r*a)*(one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                            )*lambdaW(isW)
                    
                    ! Demand for EHI in the current period
                    demE = demE + (Ieq(ii, iEE)/PPI(ih, ie, j-1))*lambdaW(isW)
                    
                    ! Demand for EHI in the next period
                    demEp = demEp + Ieq(iW(isW), iEE)*lambdaW(isW)
                    
                    ! Demand for IHI in the current period
                    demI(ih, j-1) = demI(ih, j-1) + (Ieq(ii, iII)/PPI(ih, ie, j-1))*lambdaW(isW)
                    
                    ! Aggregate Labor that received an EHI offer
                    LLE = LLE + z*lW(isW)*Ieq(iiota, 2)*lambdaW(isW)
                    
                    ! Aggregate Premium Tax Credits
                    aggTP = aggTP + TP(j, ih, a, z, iiota, lW(isW), iW(isW))*lambdaW(isW)
                    
                    ! Aggregate Individual Mandate penalty
                    aggTM = aggTM + TM(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))*lambdaW(isW)
                    
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
! Equilibrium Variables: j = RR-1
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
                  
                  ! Updated aggregate labor
                  LL1 = LL1 + z*lW(isW)*lambdaW(isW)
            
                  ! Updated bequest transfer
                  BB1 = BB1                                     &
                      + (one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                        *(one_r*a - tauY(r*a))                  &
                        *lambdaW(isW)
                  
                  ! Aggregate consumption
                  CC = CC + cW(isW)*lambdaW(isW)
                  
                  ! Aggregate assets
                  aggA = aggA &
                       + ((a + pE*Ieq(ii, iEE) + (pI(ih, j-1) - varkappa)*Ieq(ii, iII))/PPI(ih, ie, j-1))*lambdaW(isW)
                  
                  ! Next period's aggregate assets
                  aggAp = aggAp + aW(isW)*lambdaW(isW)
                  
                  ! Aggregate Medicaid expenditures
                  aggMA = aggMA + m*q(ii, im, ih, j)*Ieq(ii, iMA)*lambdaW(isW)
                  
                  ! Aggregate EHI expenditures
                  aggE = aggE + m*q(ii, im, ih, j)*Ieq(ii, iEE)*lambdaW(isW)
                  
                  ! Aggregate IHI expenditures
                  aggI(ih, j-1) = aggI(ih, j-1) + m*q(ii, im, ih, j)*Ieq(ii, iII)*lambdaW(isW)
                  
                  ! Aggregate Social Insurance transfer
                  aggTSI = aggTSI                                                     &
                         + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iMC, pMC) &
                           *lambdaW(isW)
                  
                  ! Aggregate total taxes
                  aggTT = aggTT                                               &
                        + ( TT(j, ih, a, im, z, iiota, ii, lW(isW), iMC)      &
                          + tauY(r*a)*(one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                          )*lambdaW(isW)
                  
                  ! Demand for EHI in the current period
                  demE = demE + (Ieq(ii, iEE)/PPI(ih, ie, j-1))*lambdaW(isW)
                  
                  ! Demand for IHI in the current period
                  demI(ih, j-1) = demI(ih, j-1) + (Ieq(ii, iII)/PPI(ih, ie, j-1))*lambdaW(isW)
                  
                  ! Fraction of agents that pay the Medicare premium
                  fracpMC = fracpMC + lambdaW(isW)
                  
                  ! Aggregate Labor that received an EHI offer
                  LLE = LLE + z*lW(isW)*Ieq(iiota, 2)*lambdaW(isW)
                  
                end do  ! ii
              
              end do  ! iiota
              
            end do  ! ix
      
          end do  ! iz
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie

!==============================================================================
! Equilibrium Variables: j in {RR,...,JJ-1}
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
              
              ! Index of the vectorized state
              isR = ind([j-RR+1, ie, ih, ia, im, ix], jumpR)
              
              ! Updated bequest transfer
              BB1 = BB1                                     &
                  + (one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                    *(one_r*a - tauY(r*a))                  &
                    *lambdaR(isR)
              
              ! Aggregate consumption
              CC = CC + cR(isR)*lambdaR(isR)
              
              ! Aggregate assets
              aggA = aggA + (a/PPI(ih, ie, j-1))*lambdaR(isR)
              
              ! Next period's aggregate assets
              aggAp = aggAp + aR(isR)*lambdaR(isR)
              
              ! Aggregate Social Security benefit
              aggSS = aggSS + b(x)*lambdaR(isR)
              
              ! Aggregate Medicare expenditures
              aggMC = aggMC + m*q(iMC, im, ih, j)*lambdaR(isR)
              
              ! Aggregate Social Insurance transfer
              aggTSI = aggTSI                                                 &
                     + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, iMC, pMC) &
                       *lambdaR(isR)
              
              ! Aggregate total taxes
              aggTT = aggTT                                               &
                    + ( TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, iMC)     &
                      + tauY(r*a)*(one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                      )*lambdaR(isR)
              
              ! Fraction of agents that pay the Medicare premium
              fracpMC = fracpMC + lambdaR(isR)
              
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
            
            ! Index of the vectorized state
            isR = ind([j-RR+1, ie, ih, ia, im, ix], jumpR)
            
            ! Updated bequest transfer
            BB1 = BB1                                     &
                + (one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                  *(one_r*a - tauY(r*a))                  &
                  *lambdaR(isR)
            
            ! Aggregate consumption
            CC = CC + cR(isR)*lambdaR(isR)
            
            ! Aggregate assets
            aggA = aggA + (a/PPI(ih, ie, j-1))*lambdaR(isR)
            
            ! Next period's aggregate assets
            aggAp = aggAp + aR(isR)*lambdaR(isR)
            
            ! Aggregate Social Security benefit
            aggSS = aggSS + b(x)*lambdaR(isR)
            
            ! Aggregate Medicare expenditures
            aggMC = aggMC + m*q(iMC, im, ih, j)*lambdaR(isR)
            
            ! Aggregate Social Insurance transfer
            aggTSI = aggTSI                                                   &
                   + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, i0, 0.0_dp) &
                     *lambdaR(isR)
            
            ! Aggregate total taxes
            aggTT = aggTT                                               &
                  + ( TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, i0)      &
                    + tauY(r*a)*(one_PPI(ih, ie, j-1)/PPI(ih, ie, j-1)) &
                    )*lambdaR(isR)
            
          end do  ! ix
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie

!==============================================================================
! Aggregate Capital
!==============================================================================
  
  ! Updated current aggregate capital
  KK1 = aggA/one_DKratio

  ! Next period's aggregate capital
  KKp = aggAp/one_DKratio

!==============================================================================
! EHI Premium
!==============================================================================
  
  ! Test: positive demand for EHI in the current period
  if (demE > 0.0_dp) then
  
    ! Updated EHI premium
    pE1 = (one_kappa*aggE)/(one_r*demE)
    
  ! Test: zero demand for EHI
  else
    
    ! Print error message
    print *, ""
    print *, "Zero demand for the EHI contract!"
    print *, ""
    
    ! Stop the program
    stop
  
  end if
  
!==============================================================================
! IHI Premiums
!==============================================================================
  
  ! Test: positive demand for all IHIs in the current period
  if (minval(demI) > 0.0_dp) then
    
    ! Test: do not run Insurance Regulation
    if (polIR == 0) then
    
      ! Updated IHI premiums
      pI1 = (one_kappa*aggI)/(one_r*demI) + varkappa
      
    ! Test: run Insurance Regulation
    else if (polIR == 1) then
      
      ! Updated IHI premiums for bad health
      pI1(hB, :) = (one_kappa*(aggI(hB, :) + aggI(hG, :))) &
                   /(one_r*(demI(hB, :) + demI(hG, :)))    &
                 + varkappa
      
      ! Updated IHI premiums for good health
      pI1(hG, :) = pI1(hB, :)
    
    end if
    
  ! Test: zero demand for some IHI contract in the current period
  else
    
    ! Print error message
    print *, ""
    print *, "Zero demand for some IHI contract!"
    print *, ""
    
    ! Stop the program
    stop
  
  end if
    
!==============================================================================
! Wage Reduction
!==============================================================================

  chi1 = (omega*pE*demEp)/LLE

!==============================================================================
! Aggregate Production
!==============================================================================
    
  YY = FF(KK1, LL1)
  
!==============================================================================
! Government Consumption
!==============================================================================

  GG = GYratio*YY

!==============================================================================
! Government Debt
!==============================================================================

  ! Current government debt
  DD = DKratio*KK1
  
  ! Next period's government debt
  DDp = DKratio*KKp

!==============================================================================
! Consumption Tax Rate
!==============================================================================

  tauC1 = (GG               &
        +  one_r*DD         &
        +  aggSS            &
        +  one_varphi*aggMC &
        +  aggMA            &
        +  aggTSI           &
        +  polPTC*aggTP     &
        -  DDp              &
        -  aggTT            &
        -  pMC*fracpMC      &
        -  polIM*aggTM      &
          )/CC

end subroutine sub_equilibrium_variables