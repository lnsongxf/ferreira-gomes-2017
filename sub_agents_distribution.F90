subroutine sub_agents_distribution

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
! Local Variables
!==============================================================================

  ! Index of the lower bound of assets used to distribute the mass of agents
  integer :: ial
  
  ! Index of the upper bound of assets used to distribute the mass of agents
  integer :: iau
  
  ! Lower bound of assets used to distribute the mass of agents
  real(dp) :: al
  
  ! Upper bound of assets used to distribute the mass of agents
  real(dp) :: au
  
  ! Proportion of the mass of agents assigned to the lower bound of assets
  real(dp) :: propal
  
  ! Proportion of the mass of agents assigned to the upper bound of assets
  real(dp) :: propau
  
  ! Next period's average lifetime earnings
  real(dp) :: xp
  
  ! Index of the lower bound of average lifetime earnings used to distribute
  ! the mass of agents
  integer :: ixl
  
  ! Index of the upper bound of average lifetime earnings used to distribute
  ! the mass of agents
  integer :: ixu
  
  ! Lower bound of average lifetime earnings used to distribute the mass of
  ! agents
  real(dp) :: xl
  
  ! Upper bound of average lifetime earnings used to distribute the mass of
  ! agents
  real(dp) :: xu
  
  ! Proportion of the mass of agents assigned to the lower bound of average
  ! lifetime earnings
  real(dp) :: propxl
  
  ! Proportion of the mass of agents assigned to the upper bound of average
  ! lifetime earnings
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
  
  ! Index of the vectorized state associated to:
  !   lower bound of assets
  integer :: isl
      
  ! Index of the vectorized state associated to:
  !   upper bound of assets
  integer :: isu
  
  ! Mass of agents to be distributed
  real(dp) :: mass

!==============================================================================
! Initialize Agent's Distribution
!==============================================================================

  ! Worker
  lambdaW(:) = 0.0_dp
  
  ! Retired
  lambdaR(:) = 0.0_dp

!==============================================================================
! Agent's Distribution: j = 1
!==============================================================================

  ! First age
  j = 1
  
  ! Index of the zero current average lifetime earnings
  ix = 1
  
  ! Index of the no coverage current health insurance
  ii = i0
  
  ! Loop: education level
  do ie = 1, Ne
    
    ! Loop: current health status
    do ih = 1, Nh
    
      ! Loop: current assets
      do ia = 1, Na
      
        ! Loop: current medical expenditures
        do im = 1, Nm
      
          ! Loop: current labor productivity
          do iz = 1, Nz
          
            ! Loop: current EHI offer status
            do iiota = 1, Niota
          
              ! Index of the vectorized state
              isW = ind([j, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
      
              ! Agent's distribution
              lambdaW(isW) = mu(ih, ie, j)*OOMEGA(ia)*PPSI(im)*GGAMMAbar(iiota, iz, ie, j)
          
            end do  ! iiota
          
          end do  ! iz
      
        end do  ! im
      
      end do  ! ia
      
    end do  ! ih
    
  end do  ! ie

!==============================================================================
! Agent's Distribution: j in {2,...,RR-1}
!==============================================================================

  ! Loop: age
  do j = 2, (RR-1)
    
    ! Loop: education level
    do ie = 1, Ne
      
      ! Loop: current health status
      do ih = 1, Nh
      
        ! Loop: current assets
        do ia = 1, Na
          
          ! Loop: current medical expenditures
          do im = 1, Nm
          
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
                    isW = ind([j-1, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
          
                    ! Test: optimal next period's assets is limited by the
                    !       first and last elements of the set of assets
                    if (aW(isW) >= setA(1) .and. aW(isW) <= setA(Na)) then
              
                      ! Index of the lower bound of assets used to distribute
                      ! the mass of agents
                      ial = locate(setA, aW(isW))
          
                      ! Index of the upper bound of assets used to distribute
                      ! the mass of agents
                      iau = ial + 1
          
                      ! Lower bound of assets used to distribute the mass of
                      ! agents
                      al = setA(ial)
          
                      ! Upper bound of assets used to distribute the mass of
                      ! agents
                      au = setA(iau)
          
                      ! Proportion of the mass of agents assigned to the lower
                      ! bound of assets
                      propal = (au - aW(isW))/(au - al)
        
                      ! Proportion of the mass of agents assigned to the upper
                      ! bound of assets
                      propau = 1.0_dp - propal
        
                    ! Test: optimal next period's assets is greater than the
                    !       last element of the set of assets
                    else if (aW(isW) > setA(Na)) then
              
                      ! Index of the lower bound of assets used to distribute
                      ! the mass of agents
                      ial = Na
          
                      ! Index of the upper bound of assets used to distribute
                      ! the mass of agents
                      iau = Na
          
                      ! Lower bound of assets used to distribute the mass of
                      ! agents
                      al = setA(ial)
          
                      ! Upper bound of assets used to distribute the mass of
                      ! agents
                      au = setA(iau)
          
                      ! Proportion of the mass of agents assigned to the lower
                      ! bound of assets
                      propal = 1.0_dp
          
                      ! Proportion of the mass of agents assigned to the upper
                      ! bound of assets
                      propau = 1.0_dp - propal
        
                    ! Test: next period's assets is less than the first
                    !       element of the set of assets
                    else
          
                      ! Print error message
                      print *, ""
                      print *, "Error: Negative optimal next period's assets!"
                      print *, ""
                  
                      ! Terminate the program
                      stop
              
                    end if
          
                    ! Optimal next period's average lifetime earnings
                    xp = (x*((j-1)-1) + min(y(z, iiota, lW(isW)), ySS))/(j-1)
          
                    ! Test: optimal next period's average lifetime earnings is
                    !       limited by the first and last elements of the set
                    !       of average lifetime earnings
                    if (xp >= setX(1) .and. xp <= setX(Nx)) then
              
                      ! Index of the lower bound of average lifetime earnings
                      ! used to distribute the mass of agents
                      ixl = locate(setX, xp)
          
                      ! Index of the upper bound of average lifetime earnings
                      ! used to distribute the mass of agents
                      ixu = ixl + 1
          
                      ! Lower bound of average lifetime earnings used to
                      ! distribute the mass of agents
                      xl = setX(ixl)
          
                      ! Upper bound of average lifetime earnings used to
                      ! distribute the mass of agents
                      xu = setX(ixu)
          
                      ! Proportion of the mass of agents assigned to the lower
                      ! bound of average lifetime earnings
                      propxl = (xu - xp)/(xu - xl)
        
                      ! Proportion of the mass of agents assigned to the upper
                      ! bound of average lifetime earnings
                      propxu = 1.0_dp - propxl
        
                    ! Test: optimal next period's average lifetime earnings is
                    !       greater than the last element of the set of
                    !       average lifetime earnings
                    else if (xp > setX(Nx)) then
              
                      ! Index of the lower bound of average lifetime earnings
                      ! used to distribute the mass of agents
                      ixl = Nx
          
                      ! Index of the upper bound of average lifetime earnings
                      ! used to distribute the mass of agents
                      ixu = Nx
          
                      ! Lower bound of average lifetime earnings used to
                      ! distribute the mass of agents
                      xl = setX(ixl)
          
                      ! Upper bound of average lifetime earnings used to
                      ! distribute the mass of agents
                      xu = setX(ixu)
          
                      ! Proportion of the mass of agents assigned to the lower
                      ! bound of average lifetime earnings
                      propxl = 1.0_dp
          
                      ! Proportion of the mass of agents assigned to the upper
                      ! bound of average lifetime earnings
                      propxu = 1.0_dp - propxl
        
                    ! Test: next period's average lifetime earnings is less
                    !       than the first element of the set of average
                    !       lifetime earnings
                    else
          
                      ! Print error message
                      print *, ""
                      print *, "Error: Negative optimal next period's assets!"
                      print *, ""
                  
                      ! Terminate the program
                      stop
              
                    end if
                  
                    ! Optimal next period's health insurance
                    iip = iW(isW)
                  
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
                            isll = ind([j, ie, ihp, ial, imp, izp, ixl, iiotap, iip], jumpW)
          
                            ! Index of the vectorized state associated to:
                            !   lower bound of assets
                            !   upper bound of average lifetime earnings
                            islu = ind([j, ie, ihp, ial, imp, izp, ixu, iiotap, iip], jumpW)
            
                            ! Index of the vectorized state associated to:
                            !   upper bound of assets
                            !   lower bound of average lifetime earnings
                            isul = ind([j, ie, ihp, iau, imp, izp, ixl, iiotap, iip], jumpW)
            
                            ! Index of the vectorized state associated to:
                            !   upper bound of assets
                            !   upper bound of average lifetime earnings
                            isuu = ind([j, ie, ihp, iau, imp, izp, ixu, iiotap, iip], jumpW)
            
                            ! Mass of agents to be distributed
                            mass = (PPI(ih, ie, j-1)/one_eta)                &
                                   *PPHI(ihp, ih, ie, j-1)                   &
                                   *PPSI(imp)                                &
                                   *GGAMMA(iiotap, izp, iiota, iz, ie, j-1)  &
                                   *lambdaW(isW)
            
                            ! Agent's distribution assigned to:
                            !   lower bound of assets
                            !   lower bound of average lifetime earnings
                            lambdaW(isll) = lambdaW(isll) + propal*propxl*mass
          
                            ! Agent's distribution assigned to:
                            !   lower bound of assets
                            !   upper bound of average lifetime earnings
                            lambdaW(islu) = lambdaW(islu) + propal*propxu*mass
          
                            ! Agent's distribution assigned to:
                            !   upper bound of assets
                            !   lower bound of average lifetime earnings
                            lambdaW(isul) = lambdaW(isul) + propau*propxl*mass
            
                            ! Agent's distribution assigned to:
                            !   upper bound of assets
                            !   upper bound of average lifetime earnings
                            lambdaW(isuu) = lambdaW(isuu) + propau*propxu*mass
                          
                          end do  ! iiotap
                          
                        end do  ! izp
                  
                      end do  ! imp
                  
                    end do  ! ihp
                
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
! Agent's Distribution: j = RR
!==============================================================================

  ! Retirement's age
  j = RR
  
  ! Loop: education level
  do ie = 1, Ne
    
    ! Loop: current health status
    do ih = 1, Nh
    
      ! Loop: current assets
      do ia = 1, Na
        
        ! Loop: current medical expenditures
        do im = 1, Nm
        
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
                  isW = ind([j-1, ie, ih, ia, im, iz, ix, iiota, ii], jumpW)
        
                  ! Test: optimal next period's assets is limited by the first
                  !       and last elements of the set of assets
                  if (aW(isW) >= setA(1) .and. aW(isW) <= setA(Na)) then
            
                    ! Index of the lower bound of assets used to distribute
                    ! the mass of agents
                    ial = locate(setA, aW(isW))
        
                    ! Index of the upper bound of assets used to distribute
                    ! the mass of agents
                    iau = ial + 1
        
                    ! Lower bound of assets used to distribute the mass of
                    ! agents
                    al = setA(ial)
        
                    ! Upper bound of assets used to distribute the mass of
                    ! agents
                    au = setA(iau)
        
                    ! Proportion of the mass of agents assigned to the lower
                    ! bound of assets
                    propal = (au - aW(isW))/(au - al)
      
                    ! Proportion of the mass of agents assigned to the upper
                    ! bound of assets
                    propau = 1.0_dp - propal
      
                  ! Test: optimal next period's assets is greater than the
                  !       last element of the set of assets
                  else if (aW(isW) > setA(Na)) then
            
                    ! Index of the lower bound of assets used to distribute
                    ! the mass of agents
                    ial = Na
        
                    ! Index of the upper bound of assets used to distribute
                    ! the mass of agents
                    iau = Na
        
                    ! Lower bound of assets used to distribute the mass of
                    ! agents
                    al = setA(ial)
        
                    ! Upper bound of assets used to distribute the mass of
                    ! agents
                    au = setA(iau)
        
                    ! Proportion of the mass of agents assigned to the lower
                    ! bound of assets
                    propal = 1.0_dp
        
                    ! Proportion of the mass of agents assigned to the upper
                    ! bound of assets
                    propau = 1.0_dp - propal
      
                  ! Test: next period's assets is less than the first element
                  !       of the set of assets
                  else
        
                    ! Print error message
                    print *, ""
                    print *, "Error: Negative optimal next period's assets!"
                    print *, ""
                
                    ! Terminate the program
                    stop
            
                  end if
        
                  ! Optimal next period's average lifetime earnings
                  xp = (x*((j-1)-1) + min(y(z, iiota, lW(isW)), ySS))/(j-1)
        
                  ! Test: optimal next period's average lifetime earnings is
                  !       limited by the first and last elements of the set of
                  !       average lifetime earnings
                  if (xp >= setX(1) .and. xp <= setX(Nx)) then
            
                    ! Index of the lower bound of average lifetime earnings
                    ! used to distribute the mass of agents
                    ixl = locate(setX, xp)
        
                    ! Index of the upper bound of average lifetime earnings
                    ! used to distribute the mass of agents
                    ixu = ixl + 1
        
                    ! Lower bound of average lifetime earnings used to
                    ! distribute the mass of agents
                    xl = setX(ixl)
        
                    ! Upper bound of average lifetime earnings used to
                    ! distribute the mass of agents
                    xu = setX(ixu)
        
                    ! Proportion of the mass of agents assigned to the lower
                    ! bound of average lifetime earnings
                    propxl = (xu - xp)/(xu - xl)
      
                    ! Proportion of the mass of agents assigned to the upper
                    ! bound of average lifetime earnings
                    propxu = 1.0_dp - propxl
      
                  ! Test: optimal next period's average lifetime earnings is
                  !       greater than the last element of the set of average
                  !       lifetime earnings
                  else if (xp > setX(Nx)) then
            
                    ! Index of the lower bound of average lifetime earnings
                    ! used to distribute the mass of agents
                    ixl = Nx
        
                    ! Index of the upper bound of average lifetime earnings
                    ! used to distribute the mass of agents
                    ixu = Nx
        
                    ! Lower bound of average lifetime earnings used to
                    ! distribute the mass of agents
                    xl = setX(ixl)
        
                    ! Upper bound of average lifetime earnings used to
                    ! distribute the mass of agents
                    xu = setX(ixu)
        
                    ! Proportion of the mass of agents assigned to the lower
                    ! bound of average lifetime earnings
                    propxl = 1.0_dp
        
                    ! Proportion of the mass of agents assigned to the upper
                    ! bound of average lifetime earnings
                    propxu = 1.0_dp - propxl
      
                  ! Test: next period's average lifetime earnings is less than
                  !       the first element of the set of average lifetime
                  !       earnings
                  else
        
                    ! Print error message
                    print *, ""
                    print *, "Error: Negative optimal next period's assets!"
                    print *, ""
                
                    ! Terminate the program
                    stop
            
                  end if
            
                  ! Loop: next period's health status
                  do ihp = 1, Nh
                
                    ! Loop: next period's medical expenditures
                    do imp = 1, Nm
                
                      ! Index of the vectorized state associated to:
                      !   lower bound of assets
                      !   lower bound of average lifetime earnings
                      isll = ind([j-RR+1, ie, ihp, ial, imp, ixl], jumpR)
              
                      ! Index of the vectorized state associated to:
                      !   lower bound of assets
                      !   upper bound of average lifetime earnings
                      islu = ind([j-RR+1, ie, ihp, ial, imp, ixu], jumpR)
              
                      ! Index of the vectorized state associated to:
                      !   upper bound of assets
                      !   lower bound of average lifetime earnings
                      isul = ind([j-RR+1, ie, ihp, iau, imp, ixl], jumpR)
            
                      ! Index of the vectorized state associated to:
                      !   upper bound of assets
                      !   upper bound of average lifetime earnings
                      isuu = ind([j-RR+1, ie, ihp, iau, imp, ixu], jumpR)
              
                      ! Mass of agents to be distributed
                      mass = (PPI(ih, ie, j-1)/one_eta) &
                             *PPHI(ihp, ih, ie, j-1)    &
                             *PPSI(imp)                 &
                             *lambdaW(isW)
                        
                      ! Agent's distribution assigned to:
                      !   lower bound of assets
                      !   lower bound of average lifetime earnings
                      lambdaR(isll) = lambdaR(isll) + propal*propxl*mass
              
                      ! Agent's distribution assigned to:
                      !   lower bound of assets
                      !   upper bound of average lifetime earnings
                      lambdaR(islu) = lambdaR(islu) + propal*propxu*mass
              
                      ! Agent's distribution assigned to:
                      !   upper bound of assets
                      !   lower bound of average lifetime earnings
                      lambdaR(isul) = lambdaR(isul) + propau*propxl*mass
        
                      ! Agent's distribution assigned to:
                      !   upper bound of assets
                      !   upper bound of average lifetime earnings
                      lambdaR(isuu) = lambdaR(isuu) + propau*propxu*mass
                
                    end do  ! imp
                
                  end do  ! ihp
              
                end do  ! ii
              
              end do  ! iiota
              
            end do  ! ix
      
          end do  ! iz
        
        end do  ! im
        
      end do  ! ia
    
    end do  ! ih
    
  end do  ! ie

!==============================================================================
! Agent's Distribution: j in {RR+1,...,J}
!==============================================================================

  ! Loop: age
  do j = (RR+1), JJ
    
    ! Loop: education level
    do ie = 1, Ne
      
      ! Loop: current health status
      do ih = 1, Nh
      
        ! Loop: current assets
        do ia = 1, Na
          
          ! Loop: current medical expenditures
          do im = 1, Nm
          
            ! Loop: current average lifetime earnings
            do ix = 1, Nx
      
              ! Index of the vectorized state
              isR = ind([(j-RR+1)-1, ie, ih, ia, im, ix], jumpR)
        
              ! Test: optimal next period's assets is limited by the first
              !       and last elements of the set of assets
              if (aR(isR) >= setA(1) .and. aR(isR) <= setA(Na)) then
              
                ! Index of the lower bound of assets used to distribute the
                ! mass of agents
                ial = locate(setA, aR(isR))
          
                ! Index of the upper bound of assets used to distribute the
                ! mass of agents
                iau = ial + 1
        
                ! Lower bound of assets used to distribute the mass of
                ! agents
                al = setA(ial)
        
                ! Upper bound of assets used to distribute the mass of
                ! agents
                au = setA(iau)
        
                ! Proportion of the mass of agents assigned to the lower
                ! bound of assets
                propal = (au - aR(isR))/(au - al)
        
                ! Proportion of the mass of agents assigned to the upper
                ! bound of assets
                propau = 1.0_dp - propal
        
              ! Test: optimal next period's assets is greater than the last
              !       element of the set of assets
              else if (aR(isR) > setA(Na)) then
              
                ! Index of the lower bound of assets used to distribute the
                ! mass of agents
                ial = Na
          
                ! Index of the upper bound of assets used to distribute the
                ! mass of agents
                iau = Na
        
                ! Lower bound of assets used to distribute the mass of
                ! agents
                al = setA(ial)
        
                ! Upper bound of assets used to distribute the mass of
                ! agents
                au = setA(iau)
        
                ! Proportion of the mass of agents assigned to the lower
                ! bound of assets
                propal = 1.0_dp
        
                ! Proportion of the mass of agents assigned to the upper
                ! bound of assets
                propau = 1.0_dp - propal
        
              ! Test: next period's assets is less than the first element of
              !       the set of assets
              else
              
                ! Print error message
                print *, ""
                print *, "Error: Negative optimal next period's assets!"
                print *, ""
                  
                ! Terminate the program
                stop
              
              end if
            
              ! Loop: next period's health status
              do ihp = 1, Nh
                
                ! Loop: next period's medical expenditures
                do imp = 1, Nm
                
                  ! Index of the vectorized state associated to:
                  !   lower bound of assets
                  isl = ind([j-RR+1, ie, ihp, ial, imp, ix], jumpR)
              
                  ! Index of the vectorized state associated to:
                  !   upper bound of assets
                  isu = ind([j-RR+1, ie, ihp, iau, imp, ix], jumpR)
              
                  ! Mass of agents to be distributed
                  mass = (PPI(ih, ie, j-1)/one_eta) &
                         *PPHI(ihp, ih, ie, j-1)    &
                         *PPSI(imp)                 &
                         *lambdaR(isR)
                     
                  ! Agent's distribution assigned to:
                  !   lower bound of assets
                  lambdaR(isl) = lambdaR(isl) + propal*mass
      
                  ! Agent's distribution assigned to:
                  !   upper bound of assets
                  lambdaR(isu) = lambdaR(isu) + propau*mass             
                
                end do  ! imp
                
              end do  ! ihp
            
            end do  ! ix
          
          end do  ! im
          
        end do  ! ia
      
      end do  ! ih
      
    end do  ! ie
      
  end do  ! j

end subroutine sub_agents_distribution