module moduleMesh
  !---------------------------------------  
  use, intrinsic :: iso_c_binding
  !---------------------------------------
  use omp_lib
  
  implicit none ; private
  
  type, public :: meshF
    private
    !-------
    integer               :: nlog
    !-------
    character(256)        :: mesh_name
    integer               :: geo
    integer               :: prec
    integer               :: nvert
    real(8), pointer      :: vertx(:,:)   ! on tente avec un allocatable
    integer, pointer      :: mark(:)      ! on tente avec un allocatable
    integer               :: nH6
    integer, pointer      :: hexas(:,:)
    integer               :: nT4
    integer, pointer      :: tetra(:,:)   ! on tente avec un allocatable
    integer               :: nQ4
    integer, pointer      :: quadr(:,:)
    integer               :: nT3
    integer, pointer      :: trian(:,:)
    integer               :: nL2
    integer, pointer      :: edges(:,:)
    real(8), pointer      :: norms(:,:)
    real(8)               :: xmin,xmax
    real(8)               :: ymin,ymax
    real(8)               :: zmin,zmax
    real(8)               :: baryCenter(3)
    !-------
    logical               :: solution
    character(256)        :: solu_name
    integer               :: nsol,nfld
    real(8), pointer      :: solu(:,:)
    integer               :: kind(20)
    integer               :: ker
    !-------
  end type meshF
  
  type, public, bind(c) :: meshC
    private
    !-------
    integer(c_int)      :: nlog
    !-------
    character(len=1,kind=c_char)   :: mesh_name(256)
    integer(c_int)                 :: geo
    integer(c_int)                 :: prec
    integer(c_int)                 :: nVert
    type   (c_ptr)                 :: vertx
    type   (c_ptr)                 :: mark
    integer(c_int)                 :: nH6
    type   (c_ptr)                 :: hexas
    integer(c_int)                 :: nT4
    type   (c_ptr)                 :: tetra
    integer(c_int)                 :: nQ4
    type   (c_ptr)                 :: quadr
    integer(c_int)                 :: nT3
    type   (c_ptr)                 :: trian
    integer(c_int)                 :: nL2
    type   (c_ptr)                 :: edges
    type   (c_ptr)                 :: norms
    real(c_double)                 :: xmin,xmax
    real(c_double)                 :: ymin,ymax
    real(c_double)                 :: zmin,zmax
    real(c_double)                 :: baryCenter(3)
    !-------
    logical(c_bool)                :: solution
    character(len=1,kind=c_char)   :: solu_name(256)
    integer(c_int)                 :: nsol,nfld
    type(c_ptr)                    :: solu
    integer(c_int)                 :: kind(20)
    integer(c_int)                 :: ker
    !-------
  end type meshC

  interface read_mesh         ; module procedure meshF_read_mesh        ; end interface
  public :: read_mesh
  
contains
  
  subroutine meshC2F(obC,obF)
    !-----------------------------------
    type(meshC) :: obC
    type(meshF) :: obF
    !-----------------------------------
    integer     :: iChar
    integer     :: iField
    integer     :: nCmp
    integer     :: nCell
    !-----------------------------------
    print '(">>> meshC2F")'
    !-----------------------------------
    obF%nlog=obC%nlog
    !-------

    iChar=1
    do while( obC%mesh_name(iChar)/=c_null_char .and. obC%mesh_name(iChar)/='.' )
      iChar=iChar+1
      !print '("iChar=",i0)',ichar
    enddo
    if( .not.iChar==1 )write(obF%mesh_name,*)obC%mesh_name(1:iChar-1)
    !print *,"obC%mesh_name(iChar)=",obC%mesh_name(1:iChar-1)
    
    obF%geo=obC%geo
    obF%prec=obC%prec
    
    obF%nVert=obC%nVert
    if( c_associated(c_ptr_1=obC%vertx) )then
      call c_f_pointer(cptr=obC%vertx, fptr=obF%vertx, shape=[obF%geo,obF%nVert])
    else
      obF%vertx=>null()
    endif
    
    if( c_associated(c_ptr_1=obC%mark) )then
      call c_f_pointer(cptr=obC%mark , fptr=obF%mark , shape=[        obF%nVert])
    else
      obF%mark=>null()
    endif
    
    obF%nH6=obC%nH6
    if( c_associated(c_ptr_1=obC%hexas) )then
      call c_f_pointer(cptr=obC%hexas, fptr=obF%hexas, shape=[9,obF%nH6])
    else
      obF%hexas=>null()
    endif
    
    obF%nT4=obC%nT4
    if( c_associated(c_ptr_1=obC%tetra) )then
      call c_f_pointer(cptr=obC%tetra, fptr=obF%tetra, shape=[5,obF%nT4])
    else
      obF%tetra=>null()
    endif
    
    obF%nQ4=obC%nQ4
    if( c_associated(c_ptr_1=obC%quadr) )then
    call c_f_pointer(cptr=obC%quadr, fptr=obF%quadr, shape=[5,obF%nQ4])
    else
      obF%quadr=>null()
    endif
    
    obF%nT3=obC%nT3
    if( c_associated(c_ptr_1=obC%trian) )then
      call c_f_pointer(cptr=obC%trian, fptr=obF%trian, shape=[4,obF%nT3])
    else
      obF%trian=>null()
    endif
    
    obF%nL2=obC%nL2
    if( c_associated(c_ptr_1=obC%edges) )then
      call c_f_pointer(cptr=obC%edges, fptr=obF%edges, shape=[3,obF%nL2])
    else
      obF%edges=>null()
    endif
    
    obF%xmin=obC%xmin ; obF%xmax=obC%xmax
    obF%xmin=obC%ymin ; obF%xmax=obC%ymax
    obF%xmin=obC%zmin ; obF%xmax=obC%zmax
    obF%baryCenter(1:3)=obC%baryCenter(1:3)
    !-------
    obF%solution=obC%solution
    
    iChar=1
    do while( obC%solu_name(iChar)/=c_null_char .and. obC%solu_name(iChar)/='.' )
      iChar=iChar+1
    enddo
    
    if( iChar/=1 )write(obF%solu_name,'(a)')obC%solu_name(1:iChar-1)
    
    obF%nsol=obC%nsol
    obF%nfld=obC%nfld
    obF%kind(1:20)=obC%kind(1:20)
    obF%ker=obC%ker
    
    nCmp=0
    do iField=1,obF%nfld
      select case(obF%kind(iField))
      case(1) ; nCmp=nCmp+1
      case(2) ; nCmp=nCmp+obF%ker
      case default
        print '("Unknown kind of field obF%kind(",i2,")=",i10)',iField,obF%kind(iField)
        print '("stop @ meshC2F")'
        stop
      end select
    enddo
    
    if( c_associated(c_ptr_1=obC%solu) )then
      call c_f_pointer(obC%solu, obF%solu, [nCmp,obF%nSol])
    else
      obF%solu=>null()
    endif
    !-----------------------------------
    print '("<<< meshC2F")'
    !-----------------------------------
    
    return
  end subroutine meshC2F
  
  subroutine meshF2C(obF,obC)
    !-----------------------------------
    type(meshF) :: obF
    type(meshC) :: obC
    !-----------------------------------
    print '(">>> meshC2F")'
    !-----------------------------------
    print '(/"@meshC2F: tetra(:,1)=",5(i2,1x)/)',obF%tetra(:,1)
    !-------
    obC%nlog=obF%nlog
    !------- 
    obC%mesh_name=trim(obF%mesh_name)//c_null_char
    !
    obC%geo=obF%geo
    obC%prec=obF%prec
    !
    obC%nVert=obF%nVert
    if( associated(obF%vertx) )obC%vertx=c_loc(obF%vertx(1,1))
    if( associated(obF%mark ) )obC%mark =c_loc(obF%mark (1  ))

    obC%nH6=obF%nH6 ; if( associated(obF%hexas) )obC%hexas=c_loc(obF%hexas(1,1))
    obC%nQ4=obF%nQ4 ; if( associated(obF%quadr) )obC%quadr=c_loc(obF%quadr(1,1))
    obC%nT4=obF%nT4 ; if( associated(obF%tetra) )obC%tetra=c_loc(obF%tetra(1,1))
    obC%nT3=obF%nT3 ; if( associated(obF%trian) )obC%trian=c_loc(obF%trian(1,1))
    obC%nL2=obF%nL2
    if( associated(obF%edges) )then
      obC%edges=c_loc(obF%edges(1,1))
    else
      print '("obF%edges n''est pas associe")'
      obC%edges=c_loc(obF%edges(1,1))
      print '("et pourtant obC%edges=c_loc(obF%edges(1,1)) ne plante pas")'
    endif

    if( associated(obF%norms) )obC%norms=c_loc(obF%norms(1,1))
    
    obC%xmin=obF%xmin ; obC%xmax=obF%xmax
    obC%xmin=obF%ymin ; obC%xmax=obF%ymax
    obC%xmin=obF%zmin ; obC%xmax=obF%zmax
    obC%baryCenter(1:3)=obF%baryCenter(1:3)
    
    obC%solution=obF%solution
    obC%solu_name=trim(obF%solu_name)//c_null_char
    
    obC%nsol=obF%nsol
    obC%nfld=obF%nfld
    obC%ker=obF%ker    
    obC%kind(1:20)=obF%kind(1:20)
    if( associated(obF%solu) )obC%solu=c_loc(obF%solu(1,1))
    !-----------------------------------
    print '(">>> meshC2F")'
    !-----------------------------------
    return
  end subroutine meshF2C
  
  subroutine meshC_read_mesh(obC)  bind(C, name="read_mesh")
    !-----------------------------------
    type(meshC), intent(in) :: obC
    type(meshF)             :: obF
    !-----------------------------------
    print '(">>> meshC_read_mesh")'
    !-----------------------------------    
    call meshC2F(obC=obC,obF=obF)
    call meshF_read_mesh(ob=obF)
    call meshF2C(obF=obF,obC=obC)
    !-----------------------------------
    print '("<<< meshC_read_mesh")'
    !-----------------------------------    
    return
  end subroutine meshC_read_mesh
  
  subroutine meshF_read_mesh(ob)
    !-----------------------------------
    type(meshF)           :: ob
    !-----------------------------------
    print '(">>> meshF_read_mesh")'
    !-----------------------------------
    ob%geo=3
    
    ob%nVert=4
    allocate(ob%vertx(3,ob%nVert),ob%mark(ob%nVert))
    ob%vertx(1:3,1)=[0d0,0d0,0d0] ; ob%mark(1)=0
    ob%vertx(1:3,2)=[1d0,0d0,0d0] ; ob%mark(2)=0
    ob%vertx(1:3,3)=[0d0,1d0,0d0] ; ob%mark(3)=0
    ob%vertx(1:3,4)=[0d0,0d0,1d0] ; ob%mark(4)=0
    
    ob%nT4=1
    allocate(ob%tetra(5,ob%nT4))
    ob%tetra(1:5,1)=[1,2,3,4,0]

    ob%nT3=4
    allocate(ob%trian(4,ob%nT3))
    ob%trian(1:4,1)=[2,3,4,5]
    ob%trian(1:4,2)=[1,4,3,5]
    ob%trian(1:4,3)=[1,2,4,5]
    ob%trian(1:4,4)=[1,3,2,5]

    print '(/"@meshF_read_mesh: tetra(:,1)=",5(i2,1x)/)',ob%tetra(:,1)

    !-----------------------------------
    print '("<<< meshF_read_mesh")'
    !-----------------------------------
    return
  end subroutine meshF_read_mesh



end module moduleMesh
