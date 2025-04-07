  MODULE asetdown
!=======================================================================
!  Initialize OUTPUT GRID  arrays
!=======================================================================
   REAL,    ALLOCATABLE :: DOWNT(:,:),DOWNDEW(:,:),DOWNU(:,:), DOWNV(:,:),DOWNQ(:,:), DOWNP(:,:)
   REAL,    ALLOCATABLE :: WGUST(:,:),SKY(:,:),BLR(:,:)
   REAL,    ALLOCATABLE :: DIRTRANS(:,:),MGTRANS(:,:),LAL(:,:),MIXHGT(:,:)
! For 3D-RTMA
   REAL,    ALLOCATABLE :: WSPD(:,:),WDIR(:,:)
  END MODULE asetdown


