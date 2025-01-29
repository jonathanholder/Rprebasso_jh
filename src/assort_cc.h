


! set fine and coarse woody litter to 0 (turnover still included by stand_all(28/29))
! S_wood = 0.
! S_branch = 0.


! Assigning variables for assortment/harvest residue calculations
species = int(max(1.,stand_all(4,ij)))
harvRatio = pharv(1,species)
energyRatio = pharv(2,species)
stumpRatio = pharv(3,species)
d_harvested = stand_all(12,ij)
h_harvested = stand_all(11,ij)
v_harvested = stand_all(30,ij)
n_harvested = stand_all(17,ij)
felled_branch = stand_all(24,ij) + stand_all(51,ij) !NOTE: biomass!
felled_croot = stand_all(32,ij) !NOTE: biomass!
turnover_fw = stand_all(28,ij)  !NOTE: biomass!
turnover_cw = stand_all(29,ij)  !NOTE: biomass!

if(assortType>1) then ! between generic (1) / advanced assortments (2)

! ###### HARVESTED ASSORTMENTs ROUTINE ###
! ###### CLEARCUTS ###
! # including harvest residue allocation
! # stem partitioning based on Laasasenaho taper functions
! # quality reduction of potential sawnwood based on Mehtätalo (2002)
! # + stump removal

call assort(INT(species), d_harvested, h_harvested, v_harvested, pharv, stem_assort, INT(mkta), &
            INT(siteType), INT(peat), lat, lon, alt, ets, age)

! ENERGYWOOD & LITTER
  if(energyCut==1.) then
  ! LITTER
      S_branch = S_branch + max(0.,(turnover_fw +&                          ! fwoody litter
      felled_branch * (1-energyRatio)))                                      ! coarse roots / fine part; NOTE: stumps not collected in thinnings
      S_wood = S_wood+&
        turnover_cw +&                                                      ! cwoody litter
        stem_assort(5)*par_rhow*(1-energyRatio)                                  ! potential energywood from stemwood
  ! ENERGY WOOD COLLECTION
      energyWood(year,ij,6) = (stem_assort(5) + &! energywood total: stem+branches
        felled_branch/par_rhow)*energyRatio
      energyWood(year,ij,5) = stem_assort(6)*energyRatio    ! energywood from roundwood (to be used to meet harvest demand)

  else !NO ENERGYCUT; stemwood w/o stump + coarse roots; done below
! LITTER
  S_branch = S_branch + max(0.,(turnover_fw +&                          ! fwoody litter
    felled_branch)) ! living + dead branches                                      ! coarse roots / fine part; NOTE: stumps not collected in thinnings
  S_wood = S_wood+&
    turnover_cw +&                                                      ! cwoody litter
    stem_assort(5)*par_rhow
    ! ENERGY WOOD COLLECTION = 0
    energyWood(year,ij,6) = 0 ! energywood total // ADD STUMP, BRANCHES TO THIS !!
    energyWood(year,ij,5) = 0 ! energywood from roundwood (used to meet harvest demand)
  endif !energycut

  !###### STUMP COLLECTION ####################
  !###### and allocation to litter (including coarse roots) ############
    if(energyCut==1. .AND. pharv(3, species)>0. .AND. siteType<5.) then ! stump removal: allocate to energywood and/or litter
      call stumpsample(pharv(3,species), stumpsampled) ! sampling with stumpratio as probability
      if(stumpsampled==1) then !if stumps are collected
        energyWood(year,ij,7) = (stem_assort(2) + felled_croot*0.3/par_rhow)*stumprecoveryrate !note: volume
        energyWood(year,ij,6) = energyWood(year,ij,6) + energyWood(year,ij,7) ! add stumps to total energywood

        S_wood = S_wood + (stem_assort(2)*par_rhow*(1-stumprecoveryrate) + felled_croot*((0.7+0.3*(1-stumprecoveryrate))*0.17)) !note: biomass
        S_branch = S_branch + felled_croot*((0.7+0.3*(1-stumprecoveryrate))*0.83) !note: biomass
      else !if stump removal possible, but not sampled: to litter
        energyWood(year,ij,7) = 0.
        S_wood = S_wood + stem_assort(2)*par_rhow + felled_croot*0.17 !note: biomass
        S_branch = S_branch + felled_croot*0.83 !note: biomass
      endif !stump sampling
    else !no energycut / stump removal not possible: to litter
      energyWood(year,ij,7) = 0.
      S_wood = S_wood + stem_assort(2)*par_rhow + felled_croot*0.17 !note: biomass
      S_branch = S_branch + felled_croot*0.83 !note: biomass
    endif !stumps

   !###### Fill rest of energyWood array ######
   energyWood(year,ij,2) = stem_assort(1) ! total roundwood
   energyWood(year,ij,3) = stem_assort(3) ! sawnwood (quality reduced)
   energyWood(year,ij,4) = stem_assort(4) ! pulpwood (including qred sawn, -harvestRatio)
!   energyWood(year,ij,6) = stem_assort(5)*energyRatio ! energywood total // ADD STUMP, BRANCHES TO THIS !!
!   energyWood(year,ij,5) = stem_assort(6)*energyRatio ! energywood from roundwood (used to meet harvest demand)
   energyWood(year,ij,8) = stem_assort(2) ! stump (100% abg, included in energywood if collected)
   energyWood(year,ij,13) = stem_assort(7)*n_harvested ! total stemwood according to Laasasenaho taper function
!   energyWood(year,ij,7) = 99. !energywood from stumps (if applicable; from stemwood (,,8) + coarse roots, *stumprecoveryrate
   energyWood(year,ij,1) = v_harvested
   energyWood(year,ij,9) = n_harvested
   energyWood(year,ij,10) = d_harvested
   energyWood(year,ij,11) = h_harvested
   energyWood(year,ij,12) = stem_assort(8) ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
   energyWood(year,ij,14) = 4.

   stand_all(37,ij) = stem_assort(3) + stem_assort(4)

! GENERIC ASSORTMENTS, aV1
else if(assortType==INT(1)) then

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
energyWood(year,ij,4) = v_harvested*harvRatio*0.5          !
energyWood(year,ij,8) = 0              ! stump (100% abg, partially included in energywood if collected)
energyWood(year,ij,13) = 0  ! total stemwood according to Laasasenaho taper function
energyWood(year,ij,7) = 0                          ! energywood from stumps (not applicable in thinnings)
energyWood(year,ij,1) = v_harvested
energyWood(year,ij,9) = n_harvested
energyWood(year,ij,10) = d_harvested
energyWood(year,ij,11) = h_harvested
energyWood(year,ij,12) = 88.             ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
energyWood(year,ij,14) = 4.                    ! dummy for variable of interest

 !fill Vharvested with non-energy roundwood removals (to meet harvest demand)
 stand_all(37,ij) = v_harvested*harvRatio

endif !simple assortments
