@AbapCatalog.sqlViewName: 'ZADCOSETSCTABLE'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tables with Source Code'
define view ZADCOSET_I_SearchableTable
  as select from dd02l as Object
{
  key case
      when tabclass  = 'INTTAB'
        or tabclass  = 'APPEND' then 'STRU'
  end         as ObjectType,
  key tabname as ObjectName
}
where
     tabclass = 'INTTAB'
  or tabclass = 'APPEND'
