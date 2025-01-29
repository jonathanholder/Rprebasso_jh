!  energyWood(year,ij,2)
species = STAND(4)
par_rhow = pCrobas(2,species)
age = STAND(7)
ets = STAND(5)
harvRatio = pharv(1,species)
energyRatio = pharv(2,species)
stumpRatio = pharv(3,species)
d_harvested = STAND(12)
h_harvested = STAND(11)
v_harvested = STAND(30)
n_harvested = STAND(17)
felled_branch = STAND(24) + STAND(51) !NOTE: biomass!
felled_croot = STAND(32) !NOTE: biomass!
turnover_fw = STAND(28)  !NOTE: biomass!
turnover_cw = STAND(29)  !NOTE: biomass!



! assort subroutine
call assort(INT(species), d_harvested, h_harvested, v_harvested, pharv, stem_assort, INT(mkta), &
            INT(siteType), INT(peat), lat, lon, alt, ets, age)


  ! ENERGY WOOD COLLECTION
      energyWood(year,ij,6) = (stem_assort(5) + &! energywood total: stem+branches
        felled_branch/par_rhow)*energyRatio
      energyWood(year,ij,5) = stem_assort(6)*energyRatio    ! energywood from roundwood (to be used to meet harvest demand)


  !###### STUMP COLLECTION ####################
  ! simplified version, calculate potential stumps for all species, but oinly on somewhat fertile stands
  ! NOTE: keep stumps separate from potential total energywood, as this might distort interpretation at fist glance...
    if(siteType<5.) then ! stump removal: allocate to energywood and/or litter
  !     call stumpsample(pharv(3,species), stumpsampled) ! sampling with stumpratio as probability
  !     if(stumpsampled==1) then !if stumps are collected
        energyWood(year,ij,7) = (stem_assort(2) + felled_croot*0.3/par_rhow)*stumprecoveryrate !note: volume
        !energyWood(year,ij,6) = energyWood(year,ij,6) + energyWood(year,ij,7) ! DO NOT add stumps to total energywood

    endif !stumps


   !###### Fill rest of energyWood array ######
   energyWood(year,ij,2) = stem_assort(1) ! total roundwood
   energyWood(year,ij,3) = stem_assort(3) ! sawnwood (quality reduced)
   energyWood(year,ij,4) = stem_assort(4) ! pulpwood (including qred sawn, -harvestRatio)
!   energyWood(year,ij,6) = stem_assort(5)*energyRatio ! energywood total // HERE: excluding stumps!!
!   energyWood(year,ij,5) = stem_assort(6)*energyRatio ! energywood from roundwood (used to meet harvest demand)
   energyWood(year,ij,8) = stem_assort(2) ! stump (100% abg, included in energywood if collected)
   energyWood(year,ij,12) = stem_assort(7)*n_harvested ! total stemwood according to Laasasenaho taper function
!   energyWood(year,ij,7) = 99. !energywood from stumps (if applicable; from stemwood (,,8) + coarse roots, *stumprecoveryrate
   energyWood(year,ij,1) = v_harvested
   energyWood(year,ij,9) = n_harvested
   energyWood(year,ij,10) = d_harvested
   energyWood(year,ij,11) = h_harvested
   energyWood(year,ij,12) = stem_assort(8) ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
   energyWood(year,ij,14) = 99.
