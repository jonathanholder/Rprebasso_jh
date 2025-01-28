


! set fine and coarse woody litter to 0 (turnover still included by stand_all(28/29))
! S_wood = 0.
! S_branch = 0.

! Assigning variables for assortment/harvest residue calculations
!   stand variables h and d are not calculated for the removed/thinned trees
!   In the case of non-random thinnings (below/above), Prebas output uses the pre-thinning mean values
!   --> recalculate from remaining and pre-harvest means

! DBH of removed trees from removed ba and n
d_harvested = sqrt((stand_tot(13)-BA)/(stand_tot(17)-N)/pi)*2*100

! H of removed trees from remaining A (single-tree sapw area), W_c (sapw biomass in crown)
A_rem = (stand_tot(16)*stand_tot(17)-A*N)/(Nold-N)
Hc_rem = (stand_tot(48)-W_c)/(par_rhow * A_rem * (Nold-N))!sapwood stem below Crown
Lc_rem = (stand_tot(49)-W_s)/(par_rhow * A_rem * (Nold-N) * par_betas)! sapwood of stem within crown
h_harvested = Hc_rem + Lc_rem

! additional variables
species = int(max(1.,stand(4)))
harvRatio = pharv(1,species)
energyRatio = pharv(2,species)
stumpRatio = pharv(3,species)
v_harvested = stand_tot(30)-V
n_harvested = Nold-N
felled_branch = stand_all(24,ij)- W_branch + stand_all(51,ij)-Wdb !NOTE: biomass!
felled_croot = stand_all(32,ij) - W_croot !NOTE: biomass!
turnover_fw = stand_all(28,ij)  !NOTE: biomass!
turnover_cw = stand_all(29,ij)  !NOTE: biomass!

!### NEW ASSORTMENT ROUTINE ###
! # including harvest residue allocation
! # stem partitioning based on Laasasenaho taper functions
! # quality reduction of potential sawnwood based on Mehtätalo (2002)
! # + stump removal
! # NOTE: qred currently fixed to 0.3
if(assortType>1) then ! between generic (1) / advanced assortments (2)

! ###### HARVESTED ASSORTMENTs ROUTINE ###
! ###### THINNINGS ###
! # including harvest residue allocation
! # stem partitioning based on Laasasenaho taper functions
! # quality reduction of potential sawnwood based on Mehtätalo (2002)
! # NOTE: qred currently fixed to 0.3

! assort subroutine (NOTE: qred fixed to 0.3 there)
  call assort(INT(species), d_harvested, h_harvested, v_harvested, pharv, stem_assort, INT(mkta), &
              INT(siteType), INT(peat), lat, lon, alt, ets, age)


  !SUBROUTINE assort(spec, d, h, v, pharv, stem_assort, mkta, siteType, peat, lat, lon, alt, ets, age)!; mkta, sitetype, peat, lat, lon, alt, ets, age , coef !qredfact and vols_raw just for checking

! ENERGYWOOD & LITTER

   if(energyCut==1.) then
! LITTER
      S_branch = S_branch + max(0.,(turnover_fw +&                          ! fwoody litter
        felled_branch * (1-energyRatio) +& ! living + dead branches
        felled_croot*0.83))                                      ! coarse roots / fine part; NOTE: stumps not collected in thinnings
      S_wood = S_wood+&
        turnover_cw +&                                                      ! cwoody litter
        felled_croot*0.17 +&                                     ! coarse roots / coarse part
        stem_assort(2)*par_rhow+ &              !abg stump (not colletced in thinnings)                                ! abg stump
        stem_assort(5)*par_rhow*(1-energyRatio)                                  ! potential energywood from stemwood
  ! ENERGY WOOD COLLECTION
      energyWood(year,ij,6) = (stem_assort(5) + &! energywood total: stem+branches
        felled_branch/par_rhow)*energyRatio
      energyWood(year,ij,5) = stem_assort(6)*energyRatio  ! energywood from roundwood (to be used to meet harvest demand)

   else ! NO ENERGYCUT
! LITTER
      S_branch = S_branch + max(0.,(turnover_fw +&                          ! fwoody litter
        felled_branch +& ! living + dead branches
        felled_croot*0.83))                                      ! coarse roots / fine part; NOTE: stumps not collected in thinnings
      S_wood = S_wood+&
        turnover_cw +&                                                      ! cwoody litter
        felled_croot*0.17 +&                                     ! coarse roots / coarse part
        stem_assort(2)*par_rhow+ &                                               ! abg stump
        stem_assort(5)*par_rhow
  ! ENERGY WOOD COLLECTION = 0
        energyWood(year,ij,6) = 0. ! total energywood
        energyWood(year,ij,5) = 0. ! energywood from roundwood (used to meet harvest demand)
   endif !energycut

!# Filling rest of energyWood array ######
    energyWood(year,ij,2) = stem_assort(1)              ! total roundwood
    energyWood(year,ij,3) = stem_assort(3)              ! sawnwood (quality reduced)
    energyWood(year,ij,4) = stem_assort(4)              ! pulpwood (including qred sawn, -harvestRatio)
    !energyWood(year,ij,6) = stem_assort(5)*energyRatio ! energywood total / done above
    !energyWood(year,ij,5) = stem_assort(6)*energyRatio ! energywood from roundwood (used to meet harvest demand) / done above
    energyWood(year,ij,8) = stem_assort(2)              ! stump (100% abg, partially included in energywood if collected)
    energyWood(year,ij,13) = stem_assort(7)*n_harvested  ! total stemwood according to Laasasenaho taper function
    energyWood(year,ij,7) = 0                          ! energywood from stumps (not applicable in thinnings)
    energyWood(year,ij,1) = v_harvested
    energyWood(year,ij,9) = n_harvested
    energyWood(year,ij,10) = d_harvested
    energyWood(year,ij,11) = h_harvested
    energyWood(year,ij,12) = stem_assort(8)              ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
    energyWood(year,ij,14) = thinningType                   ! dummy for variable of interest

    stand_all(37,ij) = stem_assort(3) + stem_assort(4)


else if(assortType==INT(1)) then ! between generic (1) / advanced assortments (2)
! original energywood collection / litter calculations
! NOTE: harvRatio will mean two different things for the two assortment complexities!!

   if(energyCut==1.) then
! energywood collection
      energyWood(year,ij,6) = energyWood(year,ij,6) + ((felled_branch + &
      felled_croot * 0.3)/par_rhow + &
      v_harvested * (1-harvRatio)) * energyRatio
      energyWood(year,ij,5) = v_harvested * (1-harvRatio) * energyRatio
! litter
      S_branch = max(0.,(S_branch + felled_branch * (1-energyRatio) +  &
      (0.3 * (1-energyRatio)+0.7) * felled_croot *0.83))
      S_wood = S_wood + turnover_cw + &
      (0.3 * (1-energyRatio)+0.7) * felled_croot * 0.17 +&
      v_harvested*par_rhow* (1-harvRatio)* (1-energyRatio)

   else ! no energywood collection
      energyWood(year,ij,5) = 0.
      energyWood(year,ij,6) = 0.
    !litter
      S_branch = max(0.,(S_branch + turnover_fw + felled_branch  +  &
       felled_croot *0.83))
      S_wood = S_wood + turnover_cw + felled_croot * 0.17 + &
       v_harvested*par_rhow*(1-harvRatio)!(1-harvRatio) takes into account of the stem
   endif !energywood collection T/F

!# Filling rest of energyWood array ######
  energyWood(year,ij,2) = v_harvested*harvRatio          ! total roundwood
  energyWood(year,ij,3) = v_harvested*harvRatio*0.5          ! NOTE: dummy, so the Vharvested allocation works
  energyWood(year,ij,4) = v_harvested*harvRatio*0.5

  !energyWood(year,ij,6) = energyWood(year,ij,1)! energywood total
  !energyWood(year,ij,5) = v_harvested * (1-harvRatio) * energyRatio ! energywood from STEMwood (used to meet harvest demand
  energyWood(year,ij,8) = 0.              ! stump (100% abg, partially included in energywood if collected)
  energyWood(year,ij,13) = 0.  ! total stemwood according to Laasasenaho taper function
  energyWood(year,ij,7) = 0.                          ! energywood from stumps (not applicable in thinnings)
  energyWood(year,ij,1) = v_harvested
  energyWood(year,ij,9) = n_harvested
  energyWood(year,ij,10) = d_harvested
  energyWood(year,ij,11) = h_harvested
  energyWood(year,ij,12) = 0.           ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
  energyWood(year,ij,14) = thinningType                 ! dummy for variable of interest

  stand_all(37,ij) = v_harvested*harvRatio


endif !simple assortments
