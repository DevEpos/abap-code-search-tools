@AbapCatalog.sqlViewName: 'ZADCOSET_ISRCOBJ'
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Repository object for code search'

define view ZADCOSET_I_SourceCodeObject
  as select from tadir as Object
{
  key Object.pgmid      as ProgramId,
  key Object.object     as ObjectType,
  key Object.obj_name   as ObjectName,
      Object.devclass   as DevelopmentPackage,
      Object.created_on as CreatedDate,
      Object.author     as Owner
}
where
       Object.pgmid   = 'R3TR'
  and  Object.delflag = ''
  and(
       Object.object  = 'CLAS'
    or Object.object  = 'INTF'
    or Object.object  = 'PROG'
    or Object.object  = 'FUGR'
    or Object.object  = 'TYPE'
    or Object.object  = 'DDLS'
    or Object.object  = 'DCLS'
    or Object.object  = 'DDLX'
    or Object.object  = 'BDEF'
    or Object.object  = 'XSLT'
  )

union all

select from  tadir                       as Object
  inner join ZADCOSET_I_SearchableTables as Tabl on Object.obj_name = Tabl.ObjectName
{
  key Object.pgmid      as ProgramId,
  key Tabl.ObjectType,
  key Object.obj_name   as ObjectName,
      Object.devclass   as DevelopmentPackage,
      Object.created_on as CreatedDate,
      Object.author     as Owner
}
where
      Object.pgmid   = 'R3TR'
  and Object.delflag = ''
  and Object.object  = 'TABL'
