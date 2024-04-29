@AbapCatalog.sqlViewName: 'ZADCOSETSCTABLES'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tables with Source Code'
define view ZADCOSET_I_SearchableTables
  as select from dd02l as Object
    inner join   cvers as Release on Release.component = 'SAP_BASIS'

{
  key case
      when Object.tabclass  = 'INTTAB'
        or Object.tabclass  = 'APPEND' then 'STRU'
      when Object.tabclass  = 'TRANSP' then 'DTAB'
  end                as ObjectType,
  key Object.tabname as ObjectName
}
where
  (
    (
         Object.tabclass = 'INTTAB'
      or Object.tabclass = 'APPEND'
    )
    and  Release.release > '740'
  )
  or(
         Object.tabclass = 'TRANSP'
    and  Release.release > '751'
  )
