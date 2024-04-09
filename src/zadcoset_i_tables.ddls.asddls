@AbapCatalog.sqlViewName: 'ZADCOSET_TABLES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Table Data for Code Search'
define view ZADCOSET_I_Tables
  as select from dd02l as object
    inner join   cvers as rel on rel.component = 'SAP_BASIS'

{
  key case
      when tabclass  = 'INTTAB'
        or tabclass  = 'APPEND' then 'STRU'
      when tabclass  = 'TRANSP' then 'DTAB'
  end         as ObjectType,
  key tabname as ObjectName
}
where
  (
    (
         tabclass = 'INTTAB'
      or tabclass = 'APPEND'
    )
    and  release  > '740'
  )
  or(
         tabclass = 'TRANSP'
    and  release  > '751'
  )
