@AbapCatalog.sqlViewName: 'ZADCOSET_TRSCO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rep. obj. of Tr. Request for Code Search'

define view ZADCOSET_TransportSourceCodObj
  as select from e071                      as TransportObject
    inner join   ZADCOSET_SourceCodeObject as SourceCodeObject on  TransportObject.pgmid    = SourceCodeObject.ProgramId
                                                               and TransportObject.object   = SourceCodeObject.ObjectType
                                                               and TransportObject.obj_name = SourceCodeObject.ObjectName
{
  key trkorr   as Request,
      pgmid    as ProgramId,
      object   as ObjectType,
      obj_name as ObjectName
}
where
  obj_name not like '______________________________VC'

union

select from e071
{
  key trkorr   as Request,
      pgmid    as ProgramId,
      object   as ObjectType,
      obj_name as ObjectName
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
