!externalised:
! COMPENSATION THINNINGS
! harvesting, litter, assortment processing

! input for assortments (prior to assortType, needed to fill multiWood)
species = int(max(1., multiOut(siteX,ij,4,ijj,1)))
siteType = multiOut(siteX,ij,3,ijj,1)
age = multiOut(siteX,ij,7,ijj,1)
harvRatio = pharv(1,species)
energyRatio = pharv(2,species)
stumpRatio = pharv(3,species)
d_harvested = multiOut(siteX,ij,12,ijj,1)
h_harvested = multiOut(siteX,ij,11,ijj,1)
v_harvested = multiOut(siteX,ij,30,ijj,1)*thinFact
n_harvested = multiOut(siteX,ij,17,ijj,1)*thinFact
felled_branch = multiOut(siteX,ij,24,ijj,1)*thinFact + multiOut(siteX,ij,51,ijj,1)*thinFact !NOTE: biomass!
felled_croot = multiOut(siteX,ij,32,ijj,1)*thinFact !NOTE: biomass!
turnover_fw = multiOut(siteX,ij,28,ijj,1)
turnover_cw = multiOut(siteX,ij,29,ijj,1)
par_rhow = pCrobas(2,species)
ets = multiOut(siteX,ij,5,ijj,1)
!note on reg vs comp mgmt: mw's ij is ew's year, while mw's ijj and ew's ij are layers...

! NEW ASSORTMENTS: aV2
! # stem partitioning based on Laasasenaho taper functions
! # quality reduction of potential sawnwood based on Mehtätalo (2002)
! # + stump removal
if(assortType>1) then ! between generic (1) / advanced assortments (2) / potentials for every year (3; uses 2 for actual harvests)
  if(assortType==3 .and. multiWood(siteX,ij,ijj,14) == 99.) Then
    multiWood(siteX,ij,ijj,1:13) = 0.
    endif
! above: if potential assortments are calculated, set those to zero (otherwise the comp thinning is added to the potential (standing) assortments))
! if there's a regular thinning in the same year, this isn't necessary (potential overwritten by reg thin, to which the comp thin is added)

call assort(INT(species), d_harvested, h_harvested, v_harvested, pharv, stem_assort, INT(mkta), &
            INT(siteType), INT(peat), lat, lon, alt, ets, age)

!stem_assort! assort() output list of stemwood volumes: roundwood, stumps, sawn, pulp, energy (tops+too short sawn/pulp), energy from roundwood

! ENERGYWOOD & LITTER
if(energyCutX==1.) then
!LITTER
  turnover_fw = turnover_fw +&                          ! fwoody litter
    felled_branch * (1-energyRatio) + felled_croot*0.83    ! coarse roots / fine part; NOTE: stumps not collected in thinnings, all to litter
  turnover_cw = turnover_cw +&                                                      ! cwoody litter
    stem_assort(5)*par_rhow*(1-energyRatio) + &  ! potential energywood from stemwood
    felled_croot*0.17+  &   ! coarse roots/coarse part: no stump harvests in thinnings, all to litter
    stem_assort(2)*par_rhow    ! abg stump not collected, to litter
!ENERGY WOOD COLLECTION
  multiWood(siteX,ij,ijj,6) = (stem_assort(5) + &! energywood total: stem+branches
        felled_branch/par_rhow)*energyRatio
  multiWood(siteX,ij,ijj,5) = stem_assort(6)*energyRatio ! energywood from roundwood (to be used to meet harvest demand)

else !NO ENERGYCUT; stemwood, branches, coarse roots
! LITTER
  turnover_fw = turnover_fw +&       ! fwoody litter
    felled_branch + & ! living + dead branches
    felled_croot*0.83     ! coarse roots / fine part; NOTE: stumps not collected in thinnings
  turnover_cw = turnover_cw + &   ! cwoody litter
    stem_assort(5)*par_rhow + & ! potential energywood from stem
    felled_croot*0.17+ &
    stem_assort(2)*par_rhow ! abg stump (stemwood)
! ENERGY WOOD COLLECTION = 0
  multiWood(siteX,ij,ijj,6) = 0 ! energywood total !!
  multiWood(siteX,ij,ijj,5) = 0 ! energywood from roundwood (used to meet harvest demand)
  endif !energycut

   !###### Fill rest of energyWood array ######
multiWood(siteX,ij,ijj,2) = multiWood(siteX,ij,ijj,2) + stem_assort(1) ! total roundwood
multiWood(siteX,ij,ijj,3) = multiWood(siteX,ij,ijj,3) + stem_assort(3) ! sawnwood (quality reduced)
multiWood(siteX,ij,ijj,4) = multiWood(siteX,ij,ijj,4) + stem_assort(4) ! pulpwood (including qred sawn, -harvestRatio)
!   multiWood(siteX,ij,ijj,6) = stem_assort(5)*energyRatio ! energywood total // ADD STUMP, BRANCHES TO THIS !!
!   multiWood(siteX,ij,ijj,5) = stem_assort(6)*energyRatio ! energywood from roundwood (used to meet harvest demand)
multiWood(siteX,ij,ijj,8) = multiWood(siteX,ij,ijj,8) + stem_assort(2) ! stump (100% abg, included in energywood if collected)
multiWood(siteX,ij,ijj,13) = multiWood(siteX,ij,ijj,13) + stem_assort(7)*n_harvested ! total stemwood according to Laasasenaho taper function
!   multiWood(siteX,ij,ijj,7) = 99. !energywood from stumps (if applicable; from stemwood (,,8) + coarse roots, *stumprecoveryrate
multiWood(siteX,ij,ijj,1) = multiWood(siteX,ij,ijj,1) + v_harvested
multiWood(siteX,ij,ijj,9) = multiWood(siteX,ij,ijj,9) + n_harvested
multiWood(siteX,ij,ijj,10) = d_harvested !NOTE: overwrites double thinnings, but this happens extremely rarely
multiWood(siteX,ij,ijj,12) = stem_assort(8) ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
multiWood(siteX,ij,ijj,14) = multiWood(siteX,ij,ijj,14) + 20. !indicate type of mgmt: 1:3 = thinning types, 4=cc; comp_cc: +0.1, comp_thin: + 0.2
multiWood(siteX,ij,ijj,15) = turnover_cw

multiOut(siteX,ij,37,ijj,1) = multiOut(siteX,ij,37,ijj,1) + stem_assort(3) + stem_assort(4)
multiOut(siteX,ij,38,ijj,1) = multiOut(siteX,ij,38,ijj,1) + (stem_assort(3) + stem_assort(4)) * par_rhow
roundWood = roundWood + (stem_assort(3) + stem_assort(4))*areas(siteX) ! collecting variable used to meet roundwoood demand

! SIMPLE ASSORTMENTS (aV1)
else if(assortType==INT(1)) then ! switch between generic (1) / advanced assortments (2)
!NOTE: harvRatio will mean two different things for the two assortment versions
! old structure, should I convert this to the new one for consistency??

if(energyCutX == 1.) then
! energywood collection
  multiWood(siteX,ij,ijj,6) = multiWood(siteX,ij,ijj,6) + (multiOut(siteX,ij,24,ijj,1) + &
    multiOut(siteX,ij,32,ijj,1)*0.3 + multiOut(siteX,ij,31,ijj,1)* (1-harvRatio)) * energyRatio * thinFact
  multiWood(siteX,ij,ijj,5) = multiWood(siteX,ij,ijj,6) ! no distinction between energywood from roundwood and harvest residues here
! litter
  ! multiOut(siteX,ij,28,ijj,1) = max(0.,((multiOut(siteX,ij,24,ijj,1)*(1-energyRatio) * thinFact +   &
  !   multiOut(siteX,ij,51,ijj,1)* thinFact + multiOut(siteX,ij,28,ijj,1)) + &
  !   multiOut(siteX,ij,31,ijj,1)* (1-harvRatio) * (1-energyRatio) * thinFact + &
  !   (0.3 * (1-energyRatio)+0.7) * multiOut(siteX,ij,32,ijj,1) *0.83 * thinFact))
  ! multiOut(siteX,ij,29,ijj,1) = (0.3 * (1-energyRatio)+0.7) * multiOut(siteX,ij,32,ijj,1) *0.17 * thinFact+ &
  ! multiOut(siteX,ij,29,ijj,1)

  turnover_fw = max(0.,(turnover_fw + felled_branch * (1-energyRatio) +  &
    (0.3 * (1-energyRatio)+0.7) * felled_croot *0.83))
  turnover_cw = turnover_cw + &
    ((0.3 * (1-energyRatio) + 0.7) * felled_croot * 0.17 + &
    v_harvested*par_rhow * (1-harvRatio))* (1-energyRatio)

else ! no energywood collection
  !litter
    turnover_fw = max(0.,(turnover_fw + felled_branch  +  &
       felled_croot *0.83))
    turnover_cw = turnover_cw + felled_croot * 0.17 + &
       v_harvested*par_rhow*(1-harvRatio)!(1-harvRatio) takes into account of the stem
endif! simple assortments: energyCut

!# Filling rest of energyWood array ######
multiWood(siteX,ij,ijj,2) = multiWood(siteX,ij,ijj,2) + v_harvested*harvRatio          ! total roundwood
multiWood(siteX,ij,ijj,3) = multiWood(siteX,ij,ijj,3) + v_harvested*harvRatio*0.5          ! NOTE: dummy, so the Vharvested allocation works
multiWood(siteX,ij,ijj,4) = multiWood(siteX,ij,ijj,4) + v_harvested*harvRatio*0.5          !

multiWood(siteX,ij,ijj,8) = 99.              ! stump (100% abg, partially included in energywood if collected)
multiWood(siteX,ij,ijj,13) = 99.  ! total stemwood according to Laasasenaho taper function
multiWood(siteX,ij,ijj,7) = 99.                         ! energywood from stumps (not applicable in thinnings)
multiWood(siteX,ij,ijj,1) = multiWood(siteX,ij,ijj,1) + v_harvested
multiWood(siteX,ij,ijj,9) = multiWood(siteX,ij,ijj,9) + n_harvested
multiWood(siteX,ij,ijj,10) = d_harvested !NOTE: overwrites double thinnings, but this happens extremely rarely
multiWood(siteX,ij,ijj,14) = h_harvested !NOTE: overwrites double thinnings, but this happens extremely rarely
multiWood(siteX,ij,ijj,12) = 99.
multiWood(siteX,ij,ijj,14) = multiWood(siteX,ij,ijj,14) + 20. !indicate type of mgmt: 1:3 = thinning types, 4=cc; comp_cc: +0.1, comp_thin: + 0.2

multiOut(siteX,ij,37,ijj,1) = multiOut(siteX,ij,37,ijj,1) + & !update harvested volume
 v_harvested*harvRatio
multiOut(siteX,ij,38,ijj,1) = multiOut(siteX,ij,38,ijj,1) + & !update harvested biomass
 multiOut(siteX,ij,31,ijj,1)*harvRatio*thinFact

 roundWood = roundWood + v_harvested*harvRatio*areas(siteX) !collecting variable used to meet roundwoood demand (within assorttype to prevent double accounting fo reg+comp mgmt)
 endif !simple assortments


! collecting variables to meet harvest limits
energyWood(1) = energyWood(1) + multiWood(siteX,ij,ijj,5)* areas(siteX) !jh changed from 1 to 6 (energywood from roundwood) ... and back again to troubleshoot  !!energCuts !!!we are looking at volumes
energyWood(2) = energyWood(2) + multiWood(siteX,ij,ijj,6)* areas(siteX) !jh changed from 1 to 6 (energywood from roundwood) ... and back again to troubleshoot  !!energCuts !!!we are looking at volumes

! REMOVE
! ! Litter: feed back into multiOut
! multiOut(siteX,ij,28,ijj,1) = turnover_fw
! multiOut(siteX,ij,29,ijj,1) = turnover_cw

!jh [relocated] updating harvested dimension

!
! multiOut(siteX,ij,9:10,ijj,2) = multiOut(siteX,ij,9:10,ijj,1) * thinFact
! multiOut(siteX,ij,11:12,ijj,2) = multiOut(siteX,ij,11:12,ijj,1)
! multiOut(siteX,ij,13,ijj,2) = multiOut(siteX,ij,13,ijj,1) * thinFact
! multiOut(siteX,ij,14:16,ijj,2) = multiOut(siteX,ij,14:16,ijj,1)
! multiOut(siteX,ij,17:23,ijj,2) = multiOut(siteX,ij,17:23,ijj,1) * thinFact
! multiOut(siteX,ij,26:29,ijj,2) = multiOut(siteX,ij,26:29,ijj,1) * thinFact
!
! !update biomasses and Volumes
! multiOut(siteX,ij,24:25,ijj,2) = multiOut(siteX,ij,24:25,ijj,1) * thinFact + &
! 		multiOut(siteX,ij,24:25,ijj,2)
! multiOut(siteX,ij,30:34,ijj,2) = multiOut(siteX,ij,30:34,ijj,1) * thinFact + &
! 		multiOut(siteX,ij,30:34,ijj,2)
!
! multiOut(siteX,ij,35,ijj,2) = multiOut(siteX,ij,35,ijj,1)
! multiOut(siteX,ij,44,ijj,2) = multiOut(siteX,ij,44,ijj,1) * thinFact

!/REMOVE




!UPDATE LITTER
! Litter foliage and fine roots
multiOut(siteX,ij,26,ijj,1) = multiOut(siteX,ij,33,ijj,1) * thinFact + multiOut(siteX,ij,26,ijj,1)
multiOut(siteX,ij,27,ijj,1) = multiOut(siteX,ij,25,ijj,1) * thinFact + multiOut(siteX,ij,27,ijj,1)
! Litter: fine & coarse woody
multiOut(siteX,ij,28,ijj,1) = turnover_fw
multiOut(siteX,ij,29,ijj,1) = turnover_cw

!UPDATE additional variables
multiOut(siteX,ij,9:10,ijj,1) = multiOut(siteX,ij,9:10,ijj,1)*(1-thinFact)
multiOut(siteX,ij,13,ijj,1) = multiOut(siteX,ij,13,ijj,1)*(1-thinFact)
multiOut(siteX,ij,17:25,ijj,1) = multiOut(siteX,ij,17:25,ijj,1)*(1-thinFact)
multiOut(siteX,ij,30:33,ijj,1) = multiOut(siteX,ij,30:33,ijj,1)*(1-thinFact)
multiOut(siteX,ij,47:51,ijj,1) = multiOut(siteX,ij,47:51,ijj,1)*(1-thinFact)
multiOut(siteX,ij,53:nVar,ijj,1) = multiOut(siteX,ij,53:nVar,ijj,1)*(1-thinFact)

initVar(siteX,1,ijj) = multiOut(siteX,ij,4,ijj,1)
initVar(siteX,2,ijj) = multiOut(siteX,ij,7,ijj,1)
initVar(siteX,3:6,ijj) = multiOut(siteX,ij,11:14,ijj,1)
initVar(siteX,7,ijj) = multiOut(siteX,ij,16,ijj,1)
