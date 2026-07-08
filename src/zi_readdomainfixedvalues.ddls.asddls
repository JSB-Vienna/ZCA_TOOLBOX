@VDM.lifecycle.contract.type: #PUBLIC_LOCAL_API
@AbapCatalog.sqlViewName: 'ZIREADDOMFIXVAL'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType.sizeCategory: #S
@ObjectModel.usageType.serviceQuality: #D
@ObjectModel.usageType.dataClass:#MIXED
@ClientHandling.algorithm: #SESSION_VARIABLE
@EndUserText.label: 'Read Domain Fixed Values'

@VDM.private: false
@VDM.viewType: #BASIC
define view zi_readdomainfixedvalues  //Is an enhanced copy of standard view I_READDOMAINFIXEDVALUES 
  as select from dd07t

{
  key domname    as DomainName,
  key domvalue_l as DomainValue,
  key ddlanguage as Language,
  key valpos     as ValuePos,
      ddtext     as DomainText
}
where
      ddlanguage = $session.system_language
  and as4local   = 'A'
  and as4vers    = '0000'
