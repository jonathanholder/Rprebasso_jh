! externalised:
! COMPENSATION CLEAR CUTS
! harvesting, litter, assortment processing


! legacy, can be removed (no need for ...,2)
multiOut(siteX,ij,6:23,ijj,2) = multiOut(siteX,ij,6:23,ijj,1)

   !    multiOut(siteX,ij,26,ijj,1) = multiOut(siteX,ij,33,ijj,1) + multiOut(siteX,ij,26,ijj,1)
   !    multiOut(siteX,ij,27,ijj,1) = multiOut(siteX,ij,25,ijj,1) + multiOut(siteX,ij,27,ijj,1)
   ! !   multiOut(siteX,ij,28:29,ijj,2) = multiOut(siteX,ij,28:29,ijj,1) litter/turnover fwoody and cwoody, are changed in assortment procedure
  !   multiOut(siteX,ij,28:29,ijj,2) = multiOut(siteX,ij,28:29,ijj,1)!
     ! multiOut(siteX,ij,35:nVar,ijj,2) = multiOut(siteX,ij,35:nVar,ijj,1)!!jh ATTENTION: might hamper assortment proc; now happens in

     !update biomasses and Volumes
     multiOut(siteX,ij,24:25,ijj,2) = multiOut(siteX,ij,24:25,ijj,1) + &
           multiOut(siteX,ij,24:25,ijj,2)
     multiOut(siteX,ij,30:34,ijj,2) = multiOut(siteX,ij,30:34,ijj,1) + &
           multiOut(siteX,ij,30:34,ijj,2)



! ASSIGNING INPUTS
! input for assortments (prior to assortType, needed to fill multiWood)
species = int(max(1., multiOut(siteX,ij,4,ijj,1)))
siteType = multiOut(siteX,ij,3,ijj,1)
age = multiOut(siteX,ij,7,ijj,1)
harvRatio = pharv(1,species)
energyRatio = pharv(2,species)
stumpRatio = pharv(3,species)
d_harvested = multiOut(siteX,ij,12,ijj,1) !use ,1, as ,2 covers both regular and comp cuts
h_harvested = multiOut(siteX,ij,11,ijj,1)
v_harvested = multiOut(siteX,ij,30,ijj,1)
n_harvested = multiOut(siteX,ij,17,ijj,1)
felled_branch = multiOut(siteX,ij,24,ijj,1) + multiOut(siteX,ij,51,ijj,1) !NOTE: biomass!
felled_croot = multiOut(siteX,ij,32,ijj,1) !NOTE: biomass!
turnover_fw = multiOut(siteX,ij,28,ijj,1)
turnover_cw = multiOut(siteX,ij,29,ijj,1)
par_rhow = pCrobas(2,species)
ets = multiOut(siteX,ij,5,ijj,1)


! NEW ASSORTMENTS: aV2
! # stem partitioning based on Laasasenaho taper functions
! # quality reduction of potential sawnwood based on Mehtätalo (2002)
! # + stump removal
if (assortType > 1) then ! between generic (1) / advanced assortments (2) / potentials for every year (3; uses 2 for actual harvests)
  if(assortType==3 .and. multiWood(siteX,ij,ijj,14) == 99.) then !potentials=T and no regular mgmt
    multiWood(siteX,ij,ijj,1:13) = 0. ! set potential to 0, then the comp cc can be added without double-accounting.
  endif
  ! above: if potential assortments are calculated, set those to zero (otherwise the comp thinning is added to the potential (standing) assortments))
  ! if there's a regular thinning in the same year, this isn't necessary (potential overwritten by reg thin, to which the comp thin is added)

!REMOVE BELOW
!################
! siteType = multiOut(siteX,ij,3,ijj,1)
! age = multiOut(siteX,ij,7,ijj,1)
! species = INT(multiOut(siteX,ij,4,ijj,1))
! d_harvested = multiOut(siteX,ij,12,ijj,1)
! h_harvested = multiOut(siteX,ij,11,ijj,1)
! v_harvested = multiOut(siteX,ij,30,ijj,1)
! n_harvested = multiOut(siteX,ij,17,ijj,1)
! felled_branch = multiOut(siteX,ij,24,ijj,1) + multiOut(siteX,ij,51,ijj,1) !NOTE: biomass!
! felled_croot = multiOut(siteX,ij,32,ijj,1) !NOTE: biomass!
! turnover_fw = multiOut(siteX,ij,28,ijj,1)
! turnover_cw = multiOut(siteX,ij,29,ijj,1)
!
! par_rhow = pCrobas(2,species)
! ets = multiOut(siteX,ij,5,ijj,1)

! if assortment potentials are calculated (assorttype=3),
! comp ccs without regular mgmt in the same year are added to the potentials
! (i.e. double what is actually in the forests).
! if(assortType==3 .and. multiWood(siteX,ij,ijj,1) == v_harvested) then
!   multiWood(siteX,ij,ijj,1:14) = 0. ! set potential to 0, then the comp cc can be added without problems.
!   !NOTE: waitaminute, what about comp thinnings without prior mgmt?? same problem??
! endif
!ATTENTION: comp cc conducted AFTER current year's growth, potential BEFORE that, so they don't match (and the above switch doesn't work)
! if(assortType==3 .and. multiOut(siteX,ij,1,1,2) < 1) then !potentials=T and no regular mgmt
!   multiWood(siteX,ij,iij,1:14) = 0. ! set potential to 0, then the comp cc can be added without problems.
!   !NOTE: waitaminute, what about comp thinnings without prior mgmt?? same problem??
! endif

! set fine and coarse woody litter to 0 (turnover still included by stand_all(28/29))
!disabled in comp version
!S_wood = 0.
!S_branch = 0.

! adjustments for comp cuts:
! - replace S_wood and S_branch with turnover_cw and turnover_fw, respectively
! - replace core prebas' 'energyWood' with regionprebas' 'multiwood'
!   - attention: one more dimension for site (siteX)
!   - attention:

!         multiWood(siteX,ij,ijj,2)
!         energyWood(year,ij,3)

! mw's ij is ew's year, while mw's ijj and ew's ij are layers...
!###################
!REM ABOVE

call assort(INT(species), d_harvested, h_harvested, v_harvested, pharv, stem_assort, INT(mkta), &
            INT(siteType), INT(peat), lat, lon, alt, ets, age)
!stem_assort! assort() output list of stemwood volumes: roundwood, stumps, sawn, pulp, energy (tops+too short sawn/pulp), energy from roundwood (! not considering the short sawn(<1.3m-stump)/pulp sections(<~2m))

! ENERGYWOOD & LITTER //note: coarse roots and abg stem allocated in stump harvesting routine below
if(energyCutX==1.) then
! LITTER
  turnover_fw = turnover_fw +&   ! fwoody litter
    felled_branch * (1-energyRatio)   ! coarse roots / fine part; NOTE: stumps not collected in thinnings
  turnover_cw = turnover_cw +&     ! cwoody litter
    stem_assort(5)*par_rhow*(1-energyRatio)    ! potential energywood from stemwood
! ENERGY WOOD COLLECTION
  multiWood(siteX,ij,ijj,6) = (stem_assort(5) + &! energywood total: stem+branches
    felled_branch/par_rhow)*energyRatio
  multiWood(siteX,ij,ijj,5) = stem_assort(6)*energyRatio    ! energywood from roundwood (to be used to meet harvest demand)

  else !NO ENERGYCUT; stemwood w/o stump + coarse roots; done below
! LITTER
  turnover_fw = turnover_fw +&  ! fwoody litter
    felled_branch ! living + dead branches
  turnover_cw = turnover_cw +&     ! cwoody litter
    stem_assort(5)*par_rhow
! ENERGY WOOD COLLECTION = 0
  multiWood(siteX,ij,ijj,6) = 0 ! energywood total // ADD STUMP, BRANCHES TO THIS !!
  multiWood(siteX,ij,ijj,5) = 0 ! energywood from roundwood (used to meet harvest demand)
  endif !energycut

!###### STUMP COLLECTION ####################
!###### and allocation to litter (including coarse roots) ############
  if(energyCutX==1. .AND. pharv(3, species)>0. .AND. siteType<5) then ! stump removal: allocate to energywood and/or litter
    call stumpsample(pharv(3,species), stumpsampled) ! sampling with stumpratio as probability
    if(stumpsampled==1) then !if stumps are collected
      multiWood(siteX,ij,ijj,7) = (stem_assort(2) + felled_croot*0.3/par_rhow)*stumprecoveryrate !note: volume
      multiWood(siteX,ij,ijj,6) = multiWood(siteX,ij,ijj,6) + multiWood(siteX,ij,ijj,7) ! add stumps to total energywood

      turnover_cw = turnover_cw + (stem_assort(2)*par_rhow*(1-stumprecoveryrate) + &
        felled_croot*((0.7+0.3*(1-stumprecoveryrate))*0.17)) !note: biomass
      turnover_fw = turnover_fw + felled_croot*((0.7+0.3*(1-stumprecoveryrate))*0.83) !note: biomass
    else !if stump removal possible, but not sampled: to litter
      multiWood(siteX,ij,ijj,7) = 0.
      turnover_cw = turnover_cw + stem_assort(2)*par_rhow + felled_croot*0.17 !note: biomass
      turnover_fw = turnover_fw + felled_croot*0.83 !note: biomass
    endif !stump sampling
  else !stump removal not possible: to litter
    multiWood(siteX,ij,ijj,7) = 0.
    turnover_cw = turnover_cw + stem_assort(2)*par_rhow + felled_croot*0.17 !note: biomass
    turnover_fw = turnover_fw + felled_croot*0.83 !note: biomass
  endif !stump collection

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
multiWood(siteX,ij,ijj,10) = d_harvested
multiWood(siteX,ij,ijj,11) = h_harvested
multiWood(siteX,ij,ijj,12) = stem_assort(8) ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
multiWood(siteX,ij,ijj,14) = multiWood(siteX,ij,ijj,14) + 10. !indicate type of mgmt: 1:3 = thinning types, 4=cc; comp_cc: +0.1, comp_thin: + 0.2
multiWood(siteX,ij,ijj,15) = energyCutX

multiOut(siteX,ij,37,ijj,1) = multiOut(siteX,ij,37,ijj,1) + stem_assort(3) + stem_assort(4)
multiOut(siteX,ij,38,ijj,1) = multiOut(siteX,ij,38,ijj,1) + (stem_assort(3) + stem_assort(4)) * par_rhow
roundWood = roundWood + (stem_assort(3) +stem_assort(4)) *areas(siteX) ! collecting variable used to meet roundwoood demand
    !NOTE: within assorttype, as this shouldn't be an aggregate of potential regular thinnings and comp ccs (double accounting of reg thin)

! GENERIC ASSORTMENTS, aV1
else if (assortType==1) then

  !REMOVE BELOW
  !additional clearcuts: simple 'assortments'
  !   roundWood = roundWood + multiOut(siteX,ij,30,ijj,1)*harvRatio*areas(siteX) !!energCuts
     !multiOut(siteX,ij,37,:,1) =  multiOut(siteX,ij,37,:,1) +  multiOut(siteX,ij,30,1:jj,1) !exclude 'regular' ,37 and see if this works (at least for the comp ccs themselves)
      ! multiOut(siteX,ij,37,ijj,1) = multiOut(siteX,ij,37,ijj,1) + & !potential pre-add harvest vharvested from 'regular' thinning
      !  multiOut(siteX,ij,30,ijj,1)*harvRatio !new, additional harvests
      ! multiOut(siteX,ij,38,ijj,1) = multiOut(siteX,ij,38,ijj,1) + &
      !   multiOut(siteX,ij,31,ijj,1)*harvRatio

            ! multiOut(siteX,ij,6:23,ijj,2) = multiOut(siteX,ij,6:23,ijj,1)!
            ! multiOut(siteX,ij,26,ijj,1) = multiOut(siteX,ij,33,ijj,1) + multiOut(siteX,ij,26,ijj,1)!
            ! multiOut(siteX,ij,27,ijj,1) = multiOut(siteX,ij,25,ijj,1) + multiOut(siteX,ij,27,ijj,1)!
            ! multiOut(siteX,ij,28:29,ijj,2) = multiOut(siteX,ij,28:29,ijj,1)!
            multiOut(siteX,ij,35:nVar,ijj,2) = multiOut(siteX,ij,35:nVar,ijj,1) ! remove, no need for ...,2
          !update biomasses and Volumes
          ! multiOut(siteX,ij,24:25,ijj,2) = multiOut(siteX,ij,24:25,ijj,1) + &
          !       multiOut(siteX,ij,24:25,ijj,2)
          ! multiOut(siteX,ij,30:34,ijj,2) = multiOut(siteX,ij,30:34,ijj,1) + &
          !       multiOut(siteX,ij,30:34,ijj,2)

          !multiOut(siteX,ij,37,ijj,1) = multiOut(siteX,ij,37,ijj,1) + multiOut(siteX,ij,30,ijj,2)*harvRatio


!             if(energyCutX == 1.) then !energywood harvests
!            multiWood(siteX,ij,ijj,2) = multiWood(siteX,ij,ijj,2) + (multiOut(siteX,ij,24,ijj,1) + &
!             multiOut(siteX,ij,32,ijj,1)*0.3 + multiOut(siteX,ij,31,ijj,1)* (1-harvRatio)) * energyRatio
!            species = int(multiOut(siteX,ij,4,ijj,1))
!            multiWood(siteX,ij,ijj,1) = multiWood(siteX,ij,ijj,2) / pCrobas(2,species)
!            energyWood = energyWood + multiWood(siteX,ij,ijj,1) * areas(siteX)   !!energCuts !!!we are looking at volumes
!            multiOut(siteX,ij,28,ijj,1) = max(0.,((multiOut(siteX,ij,24,ijj,1)*(1-energyRatio) +   &
!             multiOut(siteX,ij,51,ijj,1) + multiOut(siteX,ij,28,ijj,1)) + &
!            multiOut(siteX,ij,31,ijj,1)* (1-harvRatio) * (1-energyRatio) + &
!            (0.3 * (1-energyRatio)+0.7) * multiOut(siteX,ij,32,ijj,1) *0.83 ))
!              multiOut(siteX,ij,29,ijj,1) = (0.3 * (1-energyRatio)+0.7) * multiOut(siteX,ij,32,ijj,1) *0.17 + &
!             multiOut(siteX,ij,29,ijj,1)
!
!           else !no energywood harvest
!            multiOut(siteX,ij,28,ijj,1) = max(0.,(multiOut(siteX,ij,24,ijj,1) + multiOut(siteX,ij,28,ijj,1) + &
!           multiOut(siteX,ij,51,ijj,1) + multiOut(siteX,ij,32,ijj,1)*0.83 + &
!           multiOut(siteX,ij,31,ijj,1)* (1-harvRatio)))
!              multiOut(siteX,ij,29,ijj,1)=multiOut(siteX,ij,32,ijj,1)*0.17+multiOut(siteX,ij,29,ijj,1) !0.1 takes into account of the stem residuals after clearcuts
!           endif ! energycutx
!
!           ! NOTE: harvRatio will mean two different things for the two assortment complexities!!
!/END REMOVE
if(energyCutX==1.) then
! energywood collection
  multiWood(siteX,ij,ijj,6) = multiWood(siteX,ij,ijj,6) + (((felled_branch + &
    felled_croot * 0.3)/par_rhow + &
    v_harvested * (1-harvRatio)) * energyRatio)
  multiWood(siteX,ij,ijj,5) = multiWood(siteX,ij,ijj,6)
! litter
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
endif !energywood collection T/F

!# Filling rest of energyWood array ######
multiWood(siteX,ij,ijj,2) = multiWood(siteX,ij,ijj,2) + v_harvested*harvRatio          ! total roundwood
multiWood(siteX,ij,ijj,3) = multiWood(siteX,ij,ijj,3) + v_harvested*harvRatio*0.5          ! NOTE: dummy, so the Vharvested allocation works
multiWood(siteX,ij,ijj,4) = multiWood(siteX,ij,ijj,4) + v_harvested*harvRatio*0.5          !
! multiWood(siteX,ij,ijj,6) = multiWood(siteX,ij,ijj,6) + v_harvested * (1-harvRatio) * energyRatio! energywood total
! multiWood(siteX,ij,ijj,5) = multiWood(siteX,ij,ijj,6) + v_harvested * (1-harvRatio) * energyRatio ! energywood from STEMwood (used to meet harvest demand
multiWood(siteX,ij,ijj,8) = 0              ! stump (100% abg, partially included in energywood if collected)
multiWood(siteX,ij,ijj,13) = 0  ! total stemwood according to Laasasenaho taper function
multiWood(siteX,ij,ijj,7) = 0                          ! energywood from stumps (not applicable in thinnings)
multiWood(siteX,ij,ijj,1) = multiWood(siteX,ij,ijj,1) + v_harvested
multiWood(siteX,ij,ijj,9) = multiWood(siteX,ij,ijj,9) + n_harvested
multiWood(siteX,ij,ijj,10) = d_harvested ! clear-cut only, possible prior thinning ignored
multiWood(siteX,ij,ijj,11) = h_harvested ! clear-cut only, possible prior thinning ignored
multiWood(siteX,ij,ijj,12) = 0    ! quality reduction factor (share of potential sawnwood unfit for sawnwood processing)
multiWood(siteX,ij,ijj,14) = multiWood(siteX,ij,ijj,14) + 10. !indicate type of mgmt: 1:3 = thinning types, 4=cc; comp_cc: +0.1, comp_thin: + 0.2
multiWood(siteX,ij,ijj,15) = turnover_cw                      ! dummy for variable of interest

multiOut(siteX,ij,37,ijj,1) = multiOut(siteX,ij,37,ijj,1) + v_harvested*harvRatio
multiOut(siteX,ij,38,ijj,1) = multiOut(siteX,ij,38,ijj,1) + & !update harvested biomass
  v_harvested * harvRatio * par_rhow

roundWood = roundWood + v_harvested*harvRatio *areas(siteX) ! collecting variable used to meet roundwoood demand (within assorttype to prevent double accounting fo reg+comp mgmt)
endif !simple assortments

! ENERGYWOOD COLLECTING VARIABLES (to match demand)
energyWood(1) = energyWood(1) + multiWood(siteX,ij,ijj,5)* areas(siteX) ! from roundw
energyWood(2) = energyWood(2) + multiWood(siteX,ij,ijj,6)* areas(siteX) ! total

! UPDATE LITTER
! foliage and fine roots
multiOut(siteX,ij,26,ijj,1) = multiOut(siteX,ij,33,ijj,1) + multiOut(siteX,ij,26,ijj,1) ! foliage
multiOut(siteX,ij,27,ijj,1) = multiOut(siteX,ij,25,ijj,1) + multiOut(siteX,ij,27,ijj,1) ! fine roots
! fine & coarse woody
multiOut(siteX,ij,28,ijj,1) = turnover_fw ! fine woody
multiOut(siteX,ij,29,ijj,1) = turnover_cw ! coarse woodx

! after clearcut: set vars to 0
!---- multiOut(siteX,ij,8,ijj,1) = 0.
multiOut(siteX,ij,10:17,ijj,1) = 0.
multiOut(siteX,ij,19:21,ijj,1) = 0.
multiOut(siteX,ij,2,ijj,1) = 0. !!newX
!  multiOut(siteX,ij,23:36,ijj,1) = 0. !#!# !jh no, litter should remain there!!!
multiOut(siteX,ij,23:25,ijj,1) = 0.
multiOut(siteX,ij,30:36,ijj,1) = 0.
multiOut(siteX,ij,43,ijj,1) = 0.
multiOut(siteX,ij,47:nVar,ijj,1) = 0.
