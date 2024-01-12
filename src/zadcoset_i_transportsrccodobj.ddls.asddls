@AbapCatalog.sqlViewName: 'ZADCOSET_ITRSCO'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rep. obj. of Tr. Request for Code Search'

define view ZADCOSET_I_TransportSrcCodObj
  as select from e071                        as TransportObject
    inner join   ZADCOSET_I_SourceCodeObject as SourceCodeObject on  TransportObject.pgmid    = SourceCodeObject.ProgramId
                                                                 and TransportObject.object   = SourceCodeObject.ObjectType
                                                                 and TransportObject.obj_name = SourceCodeObject.ObjectName
{
  key trkorr   as Request,
      pgmid    as ProgramId,
      object   as ObjectType,
      obj_name as ObjectName,
      DevelopmentPackage,
      Owner,
      CreatedDate
}
where
  obj_name not like '______________________________VC'

union

select from e071
{
  key trkorr                  as Request,
      pgmid                   as ProgramId,
      object                  as ObjectType,
      obj_name                as ObjectName,
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
