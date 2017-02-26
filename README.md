# Dependency graphs
See [YouTube video on how this application is typically used during development](https://youtu.be/0jLN-2AVIvo) for a 2.5 minute demo.

See [YouTube video on how to install this application](https://www.youtube.com/watch?v=_RMeqd5-ZQ4&t=95s) for a complete 14 minute description on how to install and run.

Make diagrams for real world applications build from classes, methods, attributes, database tables, files, domain specific objects (for instance SAP BW DSO, MultiProvider, transformations, ...). Currently diagrams are often made manually. This is costly, time consuming and error prone. The goal is to generate diagrams like this automatically:
![An example of a manually created diagram](https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/blob/master/wiki_pictures/DemoApplication2.png)

Currently diagrams like this can be generated (With methods, attributes, classes, packages, Web Dynpro ABAP and database tables):
![An example for a dependency graph](https://raw.githubusercontent.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/master/wiki_pictures/SAP_Extractor_dependency_all.png)

This diagrams use model informations extracted with this tool. But they are generated with a second tool, see the point "Adaptable dependency graphs" a few lines below for this.

Another diagram with all currently supported diagram elements: ![All currently supported diagram elements](https://raw.githubusercontent.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/master/wiki_pictures/All%20features%2018%20December%202016.png)

Positions of elements can be changed and stored to file. Comments to elements can be added. Connected elements can be highlighted. See specification for further logic. Further types of elements are planned to be added.

There is a new context menu available for groups of classes: ![Where to find the diagram](https://raw.githubusercontent.com/RainerWinkler/Moose-FAMIX-SAP-Extractor/master/wiki_pictures/WhereToFindInMenu.png)

See [Moose](http://www.moosetechnology.org/) for further informations on the analysis platform that is used.

# Extract informations to Moose

The SAP extractor for this project is hosted in this repository.

This repository provides a program to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model.

# Adaptable dependency graphs

The logic to display dependency graphs is hosted on Smalltalk Hub [RainerWinkler/RW-Moose-Diagram](http://www.smalltalkhub.com/#!/~RainerWinkler/RW-Moose-Diagram)

It can be used for all suitable Moose models, not only models generated from SAP.

# ABAPGit for development

This repository will use ABAPGit for all objects relevant to the development. The extractor itself is a simple program and can be installed with copy and paste. All classes and programs needed for the development are contained in the folder src. It is needed to use ABAPGit to replicate this files to a SAP System with at least ABAP 7.50. The main logic has to contain statements that run in ABAP 7.30. TEST-SEAM and END-TEST-SEAM is allowed and will be replaced while generating the extraction programm. The unit tests should be written with actual ABAP statemements.

The generating of the extraction program is done with the report Z2MSE_TRANSLATE_TO_LOCAL in package Z2MSE_TOOLS. 

There are (26.02.2017) two templates: Z2MSE_MOOSE_EXTRACTOR2 will contain the refactored and improved logic. Z2MSE_MOOSE_EXTRACTOR is used to contain the current extraction program and shall be replaced in the near future.

# Further

The local classes of the extractor are now generated using global classes. This allows ABAP Unit to be used to improve the quality and reduce the time needed for implementation. There is now also a new program z_moose_translate_to_local for this conversion. This program can also be used for other projects.

See [Blog on SAP developer network](https://scn.sap.com/community/abap/custom-code-management/blog/2016/03/13/solving-sap-problems-without-reading-code--extract-a-famix-model-to-moose).

# Thanks

I would like to thank especially:

*Damir Majer* for many helpfull discussions and especially for inviting Tudor Girba to the SAP Inside Track Munich 2015, thus indirectly initiating this project

*Tudor Girba* for many valuable informations and tips on how to use Moose and FAMIX

*Enno Wulff* for making a 7.31 version available and many discussions to improve the extractor and to give hints on what can be done

My company *CubeServ* for encouraging and supporting this project.

And all *colleagues* that made this tool possible!
