A project to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model to draw for instance diagrams like this ([Usage grouped by packages (Use Case)](https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/wiki/Usage-grouped-in-packages-(Use-Case))): 

New 04 November 2016: The local classes of the extractor are now generated using global classes. This allows ABAP Unit to be used to improve the quality and reduce the time needed for implementation. There is now also a new program z_moose_translate_to_local for this conversion. This program can also be used for other projects.

New 29 July 2016: usage to Web Dynpro ABAP is included in the model.

![Packages with invocations of methods and accesses to attributes](https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/blob/master/wiki_pictures/Packages_with_classes_and_usages.png?raw=true)

See [Wiki](https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/wiki) for further details and the [Blog on SAP developer network](https://scn.sap.com/community/abap/custom-code-management/blog/2016/03/13/solving-sap-problems-without-reading-code--extract-a-famix-model-to-moose).

I would like to thank especially:

*Damir Majer* for many helpfull discussions and especially for inviting Tudor Girba to the SAP Inside Track Munich 2015, thus indirectly initiating this project

*Tudor Girba* for many valuable informations and tips on how to use Moose and FAMIX

*Enno Wulff* for making a 7.31 version available and many discussions to improve the extractor and to give hints on what can be done

And all *colleagues* that made this tool possible!
