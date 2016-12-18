# Dependency graphs

Make diagrams for real world applications build from classes, methods, attributes, database tables, files, domain specific objects (for instance SAP BW DSO, MultiProvider, transformations, ...). Currently diagrams are oft made manually. This is costly, time consuming and error prone. The goal is to generate diagrams like this automatically:
![An example of a manually created diagram](https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/blob/master/wiki_pictures/DemoApplication2.png)

Currently diagrams like this can be generated (With methods, attributes, classes, packages, Web Dynpro ABAP and database tables:
![An example for a dependency graph](https://raw.githubusercontent.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/master/wiki_pictures/SAP_Extractor_dependency_all.png)

Another diagram with all currently supported diagram elements: ![All currently supported diagram elements](https://raw.githubusercontent.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/master/wiki_pictures/All%20features%2018%20December%202016.png)

Positions of elements can be changed and stored to file. Comments to elements can be added. Connected elements can be highlighted. See specification for further logic. Further types of elements are planned to be added.

# Extract informations to Moose

The SAP extractor for this project is hosted in this repository.

This repository provides a program to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model.

# Adaptable dependency graphs

The logic to display dependency graphs is hosted on Smalltalk Hub [RainerWinkler/RW-Moose-Diagram](http://www.smalltalkhub.com/#!/~RainerWinkler/RW-Moose-Diagram)

# Further

The local classes of the extractor are now generated using global classes. This allows ABAP Unit to be used to improve the quality and reduce the time needed for implementation. There is now also a new program z_moose_translate_to_local for this conversion. This program can also be used for other projects.

See [Wiki](https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/wiki) for further details and the [Blog on SAP developer network](https://scn.sap.com/community/abap/custom-code-management/blog/2016/03/13/solving-sap-problems-without-reading-code--extract-a-famix-model-to-moose).

# Thanks

I would like to thank especially:

*Damir Majer* for many helpfull discussions and especially for inviting Tudor Girba to the SAP Inside Track Munich 2015, thus indirectly initiating this project

*Tudor Girba* for many valuable informations and tips on how to use Moose and FAMIX

*Enno Wulff* for making a 7.31 version available and many discussions to improve the extractor and to give hints on what can be done

My company CubeServ for supporting this project.

And all *colleagues* that made this tool possible!
