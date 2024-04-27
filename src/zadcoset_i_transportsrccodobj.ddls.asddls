@AbapCatalog.sqlViewName: 'ZADCOSET_ITRSCO'
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rep. obj. of Tr. Request for Code Search'

define view ZADCOSET_I_TransportSrcCodObj
  as select from e071  as transportObject
    inner join   tadir as object on  transportObject.pgmid    = object.pgmid
                                 and transportObject.object   = object.object
                                 and transportObject.obj_name = object.obj_name
{
  key transportObject.trkorr   as Request,
  key transportObject.pgmid    as ProgramId,
  key transportObject.object   as ObjectType,
  key transportObject.obj_name as ObjectName,
      object.devclass          as DevelopmentPackage,
      object.author            as Owner,
      object.created_on        as CreatedDate
}
where
       transportObject.obj_name not like '______________________________VC'
  and  transportObject.pgmid    = 'R3TR'
  and  object.delflag           = ''
  and(
       object.object            = 'CLAS'
    or object.object            = 'INTF'
    or object.object            = 'PROG'
    or object.object            = 'FUGR'
    or object.object            = 'TYPE'
    or object.object            = 'DDLS'
    or object.object            = 'DCLS'
    or object.object            = 'DDLX'
    or object.object            = 'BDEF'
    or object.object            = 'XSLT'
  )

union

select from  e071                        as transportObject
  inner join tadir                       as object on  transportObject.pgmid    = object.pgmid
                                                   and transportObject.object   = object.object
                                                   and transportObject.obj_name = object.obj_name
  inner join ZADCOSET_I_SearchableTables as tabl   on object.obj_name = tabl.ObjectName
{
  key transportObject.trkorr   as Request,
  key transportObject.pgmid    as ProgramId,
  key ObjectType,
  key transportObject.obj_name as ObjectName,
      devclass                 as DevelopmentPackage,
      author                   as Owner,
      created_on               as CreatedDate

}
where
      transportObject.pgmid  = 'R3TR'
  and delflag                = ''
  and transportObject.object = 'TABL'

union

select from e071
{
  key trkorr                  as Request,
  key pgmid                   as ProgramId,
  key object                  as ObjectType,
  key obj_name                as ObjectName,
      ''                      as DevelopmentPackage,
      ''                      as Owner,
      cast( '' as abap.dats ) as CreatedDate
}
where
  (
         pgmid    = 'LIMU'
    and(
         object   = 'FUNC'
      or object   = 'METH'
      or object   = 'REPS'
      or object   = 'CLSD'
      or object   = 'CPUB'
      or object   = 'CPRO'
      or object   = 'CPRI'
      or object   = 'CINC'
    )
  )
  and    obj_name not like '______________________________VC'
