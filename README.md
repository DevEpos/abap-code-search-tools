![](https://img.shields.io/badge/version-ALPHA-orange)
![](https://img.shields.io/badge/ABAP-v7.40sp09+-green)
# abap-code-search
ABAP Advanced Code Search Tools
> **ATTENTION**: The API is basically useable but not all features are implemented or tested yet

## Description
Searches Code in ABAP sources like
- [X] Classes (CLAS) 
- [X] Interfaces (INTF)
- [X] Programs (PROG)
- [X] Type Groups (TYPE)
- [X] Function Groups (FUGR)
- [X] Data Definitions (DDLS)
- [X] Metadata Extensions (DDLX)
- [X] Access Controls (DCLS)
- [X] Behavior Definitions (BDEV)
- [X] Simple Transformations (XSLT)

The search is done entirely via ABAP (as opposed to the ADT Source Search). There is an option to run the search in parallel for better performance

### Search features
Search can be done via normal String search or with Regular Expressions. If supported by the system it is also possible to activate PCRE (Perl Compatible Regular Expressions)

### UI
You can execute the search via report `ZADCOSET_SEARCH`, where most of the options are available as well, but the main focus of the UI will be ADT (see [abap-code-search-ui](http://github.com/stockbal/abap-code-search-ui)). 

### Package overview

- **/src**  
  Root package which currently also holds the search API  
  Important Objects in package:
  
  Object Name               | Purpose
  --------------------------|------------------------------------
  ZCL_ADCOSET_SEARCH_ENGINE | Access point to code search
  
- **/src/adt**  
  Implementation of the ADT backend which is needed for future ADT plugin
  
- **/src/parl**  
  Implementation of parallel processing - needed for the code search

## Installation

Install this repository using [abapGit](https://github.com/abapGit/abapGit#abapgit).
