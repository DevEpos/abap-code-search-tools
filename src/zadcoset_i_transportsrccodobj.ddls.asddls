@AbapCatalog.sqlViewName: 'ZADCOSET_ITRSCO'
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rep. obj. of Tr. Request for Code Search'

define view ZADCOSET_I_TransportSrcCodObj
  as select from e071                        as TransportObject
    inner join   ZADCOSET_I_SourceCodeObject as SourceCodeObject on  TransportObject.pgmid    = SourceCodeObject.ProgramId
                                                                 and TransportObject.object   = SourceCodeObject.ObjectType
                                                                 and TransportObject.obj_name = SourceCodeObject.ObjectName
{
  key TransportObject.trkorr   as Request,
  key TransportObject.pgmid    as ProgramId,
  key TransportObject.object   as ObjectType,
  key TransportObject.obj_name as ObjectName,
      SourceCodeObject.DevelopmentPackage,
      SourceCodeObject.Owner,
      SourceCodeObject.CreatedDate
}
where
  TransportObject.obj_name not like '______________________________VC'

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
