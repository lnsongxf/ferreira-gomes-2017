subroutine sub_agents_problem

!==============================================================================
! Modules
!==============================================================================

  use mod_nrtype
  use mod_numerical_recipes
  use mod_array_vec
  use mod_globals
  use mod_functions

!==============================================================================
! General Commands
!==============================================================================

  implicit none

!==============================================================================
! Local Parameters
!==============================================================================

  ! Fractional precision of the minimum used by Brent’s method
  real(dp), parameter :: tolBrent = 1.0e-6_dp

!==============================================================================
! Local Variables
!==============================================================================
    
  ! Optimal value function
  real(dp) :: Vmax
  
  ! Temporary optimal value function
  real(dp) :: Vtemp
  
  ! Optimal next period's assets
  real(dp) :: apmax
  
  ! Temporary optimal next period's assets
  real(dp) :: aptemp
  
  ! Optimal labor time
  real(dp) :: lmax
  
  ! Optimal next period's health insurance
  integer :: ipmax
  
  ! Variables used in the bracketing process and in the Brent's method
  real(dp) :: ax, bx, cx, fa, fb, fc

!==============================================================================
! Agents' Problem: j = JJ
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
            
            ! Net resources before savings
            netres = b(x)                                                     &
                   + one_r*a                                                  &
                   + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, i0, 0.0_dp) &
                   + BB                                                       &
                   - mtil(iMC, im, ih, j)                                     &
                   - TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, i0)
            
            ! Initial lower bound for bracketing the optimal next period's
            ! assets
            ax = 0.0_dp
      
            ! Initial upper bound for bracketing the optimal next period's
            ! assets
            bx = netres
            
            ! Bracket the optimal next period's assets
            call mnbrak(ax, bx, cx, fa, fb, fc, objfunRD)
            
            ! Value function and policy function of assets
            VR(isR) = -brent(ax, bx, cx, objfunRD, tolBrent, aR(isR))
            
            ! Policy function of consumption
            cR(isR) = ( b(x)                                                     &
                      + one_r*a                                                  &
                      + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, i0, 0.0_dp) &
                      + BB                                                       &
                      - mtil(iMC, im, ih, j)                                     &
                      - TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, i0)             &
                      - aR(isR)                                                  &
                      )/one_tauC
      
          end do  ! ix
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie

!==============================================================================
! Agents' Problem: j in {RR,...,JJ-1}
!==============================================================================

! Begin of state loops

  ! Loop: age
  do j = (JJ-1), RR, -1

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
              
              ! Net resources before savings
              netres = b(x)                                                   &
                     + one_r*a                                                &
                     + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, iMC, pMC) &
                     + BB                                                     &
                     - mtil(iMC, im, ih, j)                                   &
                     - pMC                                                    &
                     - TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, iMC)
              
              ! Initial lower bound for bracketing the optimal next period's
              ! assets
              ax = 0.0_dp
      
              ! Initial upper bound for bracketing the optimal next period's
              ! assets
              bx = netres
        
              ! Bracket the optimal next period's assets
              call mnbrak(ax, bx, cx, fa, fb, fc, objfunRR)
        
              ! Value function and policy function of assets
              VR(isR) = -brent(ax, bx, cx, objfunRR, tolBrent, aR(isR))
        
              ! Policy function of consumption
              cR(isR) = ( b(x)                                                   &
                        + one_r*a                                                &
                        + TSI(j, ih, a, im, 0.0_dp, x, 0, iMC, 0.0_dp, iMC, pMC) &
                        + BB                                                     &
                        - mtil(iMC, im, ih, j)                                   &
                        - pMC                                                    &
                        - TT(j, ih, a, im, 0.0_dp, 0, iMC, 0.0_dp, iMC)          &
                        - aR(isR)                                                &
                        )/one_tauC
        
            end do  ! ix
          
          end do  ! im
          
        end do  ! ia
      
      end do  ! ih
      
    end do  ! ie
  
  end do  ! j

! End of state loops

!==============================================================================
! Agents' Problem: j = RR-1
!==============================================================================

  ! Age before retirement
  j = RR-1

! Begin of state loops
    
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
      
              ! Current average lifetime earnings
              x = setX(ix)
              
              ! Loop: current EHI offer status
              do iiota = 1, Niota
                
                ! Loop: current health insurance
                do ii = 1, Ni-1
                
                  ! Index of the vectorized state
                  isW = ind([j, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
        
                  ! Initialize the optimal value function
                  Vmax = -huge(Vmax)
        
! Begin of control loops
                  
                  ! Set of available labor times
                  setL = setLT(ii)
                    
                  ! Number of available labor times
                  Nlt = count(setL > -1.0_dp)
                  
                  ! Loop: labor time
                  do il = 1, Nlt
      
                    ! Labor time
                    l = setL(il)
        
                    ! Net resources before savings
                    netres = y(z, iiota, l)                                       &
                           + one_r*a                                              &
                           + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, l, iMC, pMC) &
                           + BB                                                   &
                           - mtil(ii, im, ih, j)                                  &
                           - pMC                                                  &
                           - TT(j, ih, a, im, z, iiota, ii, l, iMC)
        
                    ! Initial lower bound for bracketing the optimal next
                    ! period's assets
                    ax = 0.0_dp
        
                    ! Initial upper bound for bracketing the optimal next
                    ! period's assets
                    bx = netres
        
                    ! Bracket the optimal next period's assets
                    call mnbrak(ax, bx, cx, fa, fb, fc, objfunWR)
        
                    ! Temporary optimal value function and next period's
                    ! assets
                    Vtemp = -brent(ax, bx, cx, objfunWR, tolBrent, aptemp)
        
                    ! Test: Compare value functions
                    if (Vtemp > Vmax) then
        
                      ! Update the optimal value function
                      Vmax = Vtemp
          
                      ! Update the optimal next period's assets
                      apmax = aptemp
        
                      ! Update the optimal labor time
                      lmax = l
        
                    end if
        
                  end do  ! il
      
! End of control loops

                  ! Value function
                  VW(isW) = Vmax
      
                  ! Policy function of assets
                  aW(isW) = apmax
      
                  ! Policy function of labor time
                  lW(isW) = lmax
      
                  ! Policy function of consumption
                  cW(isW) = ( y(z, iiota, lW(isW))                                       &
                            + one_r*a                                                    &
                            + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iMC, pMC) &
                            + BB                                                         &
                            - mtil(ii, im, ih, j)                                        &
                            - pMC                                                        &
                            - TT(j, ih, a, im, z, iiota, ii, lW(isW), iMC)               &
                            - aW(isW)                                                    &
                            )/one_tauC

                end do  ! ii
              
              end do  ! iiota
              
            end do  ! ix
              
          end do  ! iz
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie

! End of state loops

!==============================================================================
! Agents' Problem: j in {1,...,RR-2}
!==============================================================================

! Begin of state loops

  ! Loop: age
  do j = (RR-2), 1, -1

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
          
                ! Current average lifetime earnings
                x = setX(ix)
                
                ! Loop: current EHI offer status
                do iiota = 1, Niota
                  
                  ! Loop: current health insurance
                  do ii = 1, Ni-1
                  
                    ! Index of the vectorized state
                    isW = ind([j, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
          
                    ! Initialize the optimal value function
                    Vmax = -huge(Vmax)
          
! Begin of control loops
                    
                    ! Set of available labor times
                    setL = setLT(ii)
                    
                    ! Number of available labor times
                    Nlt = count(setL > -1.0_dp)
                    
                    ! Loop: labor time
                    do il = 1, Nlt
        
                      ! Labor time
                      l = setL(il)
          
                      ! Set of next period's health insurances
                      setIp = setHI(j, ih, a, im, z, iiota, ii, l)
                      
                      ! Number of next period's health insurances
                      Nip = count(setIp > 0)
                      
                      ! Loop: next period's health insurance
                      do iip = 1, Nip

                        ! Net resources before savings
                        netres = y(z, iiota, l)                                                 &
                               + one_r*a                                                        &
                               + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, l, setIp(iip), 0.0_dp) &
                               + BB                                                             &
                               + polPTC*TP(j, ih, a, z, iiota, l, setIp(iip))                   &
                               - mtil(ii, im, ih, j)                                            &
                               - p(setIp(iip), ih, j)                                           &
                               - TT(j, ih, a, im, z, iiota, ii, l, setIp(iip))                  &
                               - polIM*TM(j, ih, a, im, z, iiota, ii, l, setIp(iip))
                        
                        ! Test: Next period's health insurance is affordable
                        if (netres >= 0) then
                        
                          ! Initial lower bound for bracketing the optimal next
                          ! period's assets
                          ax = 0.0_dp
          
                          ! Initial upper bound for bracketing the optimal next
                          ! period's assets
                          bx = netres

                          ! Bracket the optimal next period's assets
                          call mnbrak(ax, bx, cx, fa, fb, fc, objfunWW)

                          ! Temporary optimal value function and next period's
                          ! assets
                          Vtemp = -brent(ax, bx, cx, objfunWW, tolBrent, aptemp)
                        
                        ! Test: Next period's health insurance is not affordable
                        else  
                          
                          ! Lowest possible value to the value function
                          Vtemp = -huge(Vtemp)
                          
                        end if
                        
                        ! Test: Compare value functions
                        if (Vtemp > Vmax) then
        
                          ! Update the optimal value function
                          Vmax = Vtemp
          
                          ! Update the optimal next period's assets
                          apmax = aptemp
        
                          ! Update the optimal labor time
                          lmax = l
                      
                          ! Update the optimal next period's health insurance
                          ipmax = setIp(iip)
        
                        end if
                      
                      end do  ! iip
                      
                    end do  ! il

! End of control loops
        
                    ! Value function
                    VW(isW) = Vmax
      
                    ! Policy function of assets
                    aW(isW) = apmax
      
                    ! Policy function of labor time
                    lW(isW) = lmax
                  
                    ! Policy function of health insurance
                    iW(isW) = ipmax
      
                    ! Policy function of consumption
                    cW(isW) = ( y(z, iiota, lW(isW))                                              &
                              + one_r*a                                                           &
                              + TSI(j, ih, a, im, z, 0.0_dp, iiota, ii, lW(isW), iW(isW), 0.0_dp) &
                              + BB                                                                &
                              + polPTC*TP(j, ih, a, z, iiota, lW(isW), iW(isW))                   &
                              - mtil(ii, im, ih, j)                                               &
                              - p(iW(isW), ih, j)                                                 &
                              - TT(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))                  &
                              - polIM*TM(j, ih, a, im, z, iiota, ii, lW(isW), iW(isW))            &
                              - aW(isW)                                                           &
                              )/one_tauC
                
                  end do  ! ii
                
                end do  ! iiota
                
              end do  ! ix
        
            end do  ! iz
          
          end do  ! im
          
        end do  ! ia
      
      end do  ! ih
      
    end do  ! ie
  
  end do  ! j

! End of state loops

end subroutine sub_agents_problem