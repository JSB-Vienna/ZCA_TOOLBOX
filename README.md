# Library with reusable objects
This package contains the base components for any of the ZCA_TBX_*-packages. It builds the bracket for them, which means that each of these packages is defined as a sub-package of the ZCA_TOOLBOX package. 

***Please always start with the import of this package or update it before you install another ZCA_TBX_*-package!!!***

## Purpose / Objective
Each of the ZCA_TBX_*-tools should make a developer's life better and/or bring in a uniform codebase. Here is a rough list of the included objects in this package:
* ***DDIC elements***
  * Transparent table for "GUID to Business Object" relations. Some object keys are longer than 32 characters. To use them e.g. in workflows, they can be converted into a persistent GUID using the class _ZCL_CA_MAP_BO_KEY_2_GUID_ of this package.
  * Some structures to include them in transparent tables to persist there who and when has something created or changed in different combinations.

* ***Message class*** that contains all messages of all ZCA_TBX_*-packages

* ***Classes***
  * Some CVC classes (Constants and Value Check) in regard to the Clean ABAP rules.
  * Especially remarkable is the class _ZCL_CA_C_SCREEN_FIELD_ATTR_, which was planned to contain the constants for GUI screen manipulation, e.g. changing an input field to display only. It grows to a class providing several methods to manipulate a bunch of fields or field groups, avoiding coding the manipulation of table SCREEN by yourself. Of course, in some special cases, you need to do it by yourself, but most of the cases are covered by this class.
  * _ZCL_CA_CONV_ converts simple data (= single fields) from the internal into the external format and vice versa. E.g., to provide data for HTML output, providing screen fields after a value request or maybe batch input scenarios.
  * _ZCL_CA_DDIC_ creates object type descriptors using the SAP class hierarchy CL_ABAP_xxxxxDESC. On one hand, it wraps the methods with classic exceptions into class-based exceptions. On the other hand, it provides some methods that may used more often, e.g. getting the fixed values of a domain or the components of a structure.
  * _ZCL_CA_UTILS_ provides methods to create an icon with quick info, asking via a GUI popup for a decision so that you can use it in an IF- or CASE statement, or several check methods for GUI availability, active update task, etc.
 
* ***Interfaces***
  * _ZIF_APACK_MANIFEST_ is still included since I tried to use the APACK techniques to make it more convenient to provide the packages to you. But unfortunately, it does not work as I expected.
  * The others are relics of older days, but I have still to check if they are still in use. You can find more details about [APACK together with ABAPGit here](https://docs.abapgit.org/user-guide/reference/apack.html).

* ***Exception hierarchy***
  * I've created a personnel exception hierarchy with the purpose of providing recurring messages by inheritance and, furthermore, methods to convert results from different sources into an exception instance of choice. This choice has either to be an inheritor of the 2nd ERROR hierarchy level or an inheritor of the INTERN error class.

  * ***Why this effort?***
    * For me, there were at least two intentions to build this up. At the very first, the high number of repetitions to create the exception classes with at least some equal messages. In the second place, to recreate over and over again the coding to analyse a classic exception, either as a simple message or as a table from a BAPI or a class-based exception like a system exception, to make a class-based exception out of it for the active application.

  * ***Simplified overview of the exception hierarchy***
    * _ZCX_CA_INTERN_ inherits from _CX_NO_CHECKS_ -> Exception classes inheriting from this branch are comparable with system exceptions that do not need to be declared but can be caught anywhere. Create only a class inheriting from ZCX_CA_INTERN if you need additional messages not covered by this class.
    * _ZCX_CA_ERROR_ inherits from _CX_STATIC_CHECK_ -> Exception classes inheriting from this branch are the typical way to create exceptions. They have either to be caught at a specific point of your needs, or they have to be declared in the signature (interface) of methods, function modules or subroutines.  ***N E V E R !!!  inherit from this class!!***
     
    * **Second exception hierarchy level**
        * _ZCX_CA_DBACC_ for any kind of data access error, foremost database accesses.
        * _ZCX_CA_UI_ for any kind of dialogue information, e.g. select only a single row.
        * _ZCX_CA_PARAM_ for any kind of exception that does not fit into the range of the two classes before. ***This is the class you should inherit from in nearly all cases.***
     
* ***Hint to the creation of a class to this model***
  * Since I like very much to use code templates, all my exception classes have a constant with the name and the value of its own class name, e.g. C_ZCX_CA_PARAM TYPE seoclsname VALUE 'ZCX_CA_PARAM'. Like a lot of my classes, they follow the Hungarian notation, and so do the constants. If you follow the Hungarian notation, too, take care that the name of a new exception class has only 27 (= CV_) or 28 (= C_) characters. These constants are used in my code template '*creexc' to implement the method _CREATE_EXCEPTION_ of either the class _ZCA_CA_ERROR or _ZCX_CA_INTERN_, e.g. behind a return code comparison or CATCH statement. You can find more details on [code templates in SAP here](https://github.com/JSB-Vienna/code_templates.git).
     
## Required packages
This package is the requirement for any ZCA_TBX_*-package.



