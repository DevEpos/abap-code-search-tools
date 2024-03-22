@AbapCatalog.sqlViewName: 'ZADCOSET_ITRSCO'
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rep. obj. of Tr. Request for Code Search'

define view ZADCOSET_I_TransportSrcCodObj
  as select from e071  as TransportObject
    inner join   tadir as object on  TransportObject.pgmid    = object.pgmid
                                 and TransportObject.object   = object.object
                                 and TransportObject.obj_name = object.obj_name
{
  key TransportObject.trkorr   as Request,
  key TransportObject.pgmid    as ProgramId,
  key TransportObject.object   as ObjectType,
  key TransportObject.obj_name as ObjectName,
      object.devclass          as DevelopmentPackage,
      object.author            as Owner,
      object.created_on        as CreatedDate
}
where
       TransportObject.obj_name not like '______________________________VC'
  and  TransportObject.pgmid    = 'R3TR'
  and  object.delflag           = ''
  and(
       object.object            = 'CLAS'
    or object.object            = 'INTF'
    or object.object            = 'PROG'
    or object.object            = 'FUGR'
    or object.object            = 'TYPE'
    or object.object            = 'DDLS'
    or object.object            = 'DCLS'
    or object.object            = 'DDLX'
    or object.object            = 'BDEF'
    or object.object            = 'XSLT'
  )

union

select from  e071  as TransportObject
  inner join tadir as object on  TransportObject.pgmid    = object.pgmid
                             and TransportObject.object   = object.object
                             and TransportObject.obj_name = object.obj_name
  inner join dd02l as tabl   on object.obj_name = tabl.tabname
                             and(
                               tabl.tabclass    = 'INTTAB'
                               or tabl.tabclass = 'TRANSP'
                               or tabl.tabclass = 'APPEND'
                             )
{
  key TransportObject.trkorr   as Request,
  key TransportObject.pgmid    as ProgramId,
  key case
      when tabl.tabclass  = 'INTTAB'
        or tabl.tabclass  = 'APPEND' then 'STRU'
      when tabl.tabclass  = 'TRANSP' then 'DTAB'
  end                          as ObjectType,
  key TransportObject.obj_name as ObjectName,
      devclass                 as DevelopmentPackage,
      author                   as Owner,
      created_on               as CreatedDate

}
where
      TransportObject.pgmid  = 'R3TR'
  and delflag                = ''
  and TransportObject.object = 'TABL'

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
