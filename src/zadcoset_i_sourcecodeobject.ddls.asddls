@AbapCatalog.sqlViewName: 'ZADCOSET_ISRCOBJ'
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Repository object for code search'

define view ZADCOSET_I_SourceCodeObject
  as select from tadir as object
{
  key pgmid         as ProgramId,
  key object.object as ObjectType,
  key obj_name      as ObjectName,
      devclass      as DevelopmentPackage,
      created_on    as CreatedDate,
      author        as Owner
}
where
       pgmid   = 'R3TR'
  and  delflag = ''
  and(
       object  = 'CLAS'
    or object  = 'INTF'
    or object  = 'PROG'
    or object  = 'FUGR'
    or object  = 'TYPE'
    or object  = 'DDLS'
    or object  = 'DCLS'
    or object  = 'DDLX'
    or object  = 'BDEF'
    or object  = 'XSLT'
  )

union

select from  tadir             as object
  inner join ZADCOSET_I_Tables as tabl on object.obj_name = tabl.ObjectName
{
  key pgmid      as ProgramId,
  key ObjectType,
  key obj_name   as ObjectName,
      devclass   as DevelopmentPackage,
      created_on as CreatedDate,
      author     as Owner
}
where
      pgmid   = 'R3TR'
  and delflag = ''
  and object  = 'TABL'
