@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZADCOSET_ITRSCO'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Rep. obj. of Tr. Request for Code Search'

define view ZADCOSET_I_TransportSrcCodObj
  as select from e071  as TransportObject

    inner join   tadir as Object
      on  TransportObject.pgmid    = Object.pgmid
      and TransportObject.object   = Object.object
      and TransportObject.obj_name = Object.obj_name

{
  key TransportObject.trkorr   as Request,
  key TransportObject.pgmid    as ProgramId,
  key TransportObject.object   as ObjectType,
  key TransportObject.obj_name as ObjectName,

      Object.devclass          as DevelopmentPackage,
      Object.author            as Owner,
      Object.created_on        as CreatedDate
}

where TransportObject.obj_name not like '______________________________VC'
  and  Object.pgmid             = 'R3TR'
  and  Object.delflag           = ''
  and( Object.object            = 'CLAS'
    or Object.object            = 'INTF'
    or Object.object            = 'PROG'
    or Object.object            = 'FUGR'
    or Object.object            = 'TYPE'
    or Object.object            = 'DDLS'
    or Object.object            = 'DCLS'
    or Object.object            = 'DDLX'
    or Object.object            = 'BDEF'
    or Object.object            = 'XSLT'
    or Object.object            = 'TABL' )

union all

  select from e071 as TransportObject

{
  key TransportObject.trkorr   as Request,
  key TransportObject.pgmid    as ProgramId,
  key TransportObject.object   as ObjectType,
  key TransportObject.obj_name as ObjectName,

      ''                       as DevelopmentPackage,
      ''                       as Owner,
      cast('' as abap.dats)    as CreatedDate
}

where ( TransportObject.pgmid    = 'LIMU'
    and( TransportObject.object   = 'FUNC'
      or TransportObject.object   = 'METH'
      or TransportObject.object   = 'REPS'
      or TransportObject.object   = 'CLSD'
      or TransportObject.object   = 'CPUB'
      or TransportObject.object   = 'CPRO'
      or TransportObject.object   = 'CPRI'
      or TransportObject.object   = 'CINC' ) )
  and    TransportObject.obj_name not like '______________________________VC'
