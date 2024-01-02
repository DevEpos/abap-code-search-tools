@AbapCatalog.sqlViewName: 'ZADCOSET_SRCDOBJ'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Repository object for code search'

define view ZADCOSET_SourceCodeObject
  as select from tadir
{
  key pgmid      as ProgramId,
  key object     as ObjectType,
  key obj_name   as ObjectName,
      devclass   as DevelopmentPackage,
      created_on as CreatedDate,
      author     as Owner
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
