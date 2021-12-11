![](https://img.shields.io/badge/version-ALPHA-orange)
![](https://img.shields.io/badge/ABAP-v7.40sp09+-orange)
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


The search will be done entirely via ABAP (as opposed to the ADT Source Search). There is an option to run the search in parallel for better performance

There will also be an option to Search-n-Replace coding
> **Note**: This feature will probably not be in the initial release

### Search features
Search can be done via normal String search or with Regular Expressions. If supported by the system it is also possible to activate PCRE (Perl Compatible Regular Expressions)

### UI
The main focus will be ADT (ABAP Development Tools), but the repository may include also a simple selection report for executing the code search

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
