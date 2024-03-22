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

select from  tadir as object
  inner join dd02l as tabl on object.obj_name = tabl.tabname
                           and(
                             tabl.tabclass    = 'INTTAB'
                             or tabl.tabclass = 'TRANSP'
                             or tabl.tabclass = 'APPEND'
                           )
{
  key pgmid      as ProgramId,
  key case
      when tabl.tabclass  = 'INTTAB'
        or tabl.tabclass  = 'APPEND' then 'STRU'
      when tabl.tabclass  = 'TRANSP' then 'DTAB'
  end            as ObjectType,
  key obj_name   as ObjectName,
      devclass   as DevelopmentPackage,
      created_on as CreatedDate,
      author     as Owner
}
where
      pgmid   = 'R3TR'
  and delflag = ''
  and object  = 'TABL'
