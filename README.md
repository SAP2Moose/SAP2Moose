# Dependency graphs

This repository provides a program to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model.

Last not stable release 0.4.0: [Extractor](../../releases/download/v0.4.0/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.4.0/z_moose_extractor_texts.txt).. This is the first release where data can be analyzed without using Moose. It also provides a Where-Used analysis over multiple levels. This is an early version that is incomplete and should not find all dependencies.

Last release 0.3.0: [Extractor](../../releases/download/v0.3.0/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.3.0/z_moose_extractor_texts.txt).. This is the first release where also usage by SAP BW routines is added to the model.

Last stable release 0.2.1: [Extractor](../../releases/download/v0.2.1/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.2.1/z_moose_extractor_texts.txt). Create a new program in your SAP system. Open file z_moose_extractor.abap with a text editor and paste the coding in your program. Add texts for convenience. Run the program, select a package and download the file (preferable with extension .mse). Open with Moose to analyze it.

Should work in ABAP 7.02 SP6, but there is currently only a limited test for this release done.

See [YouTube video on how this application is typically used during development](https://youtu.be/0jLN-2AVIvo) for a 2.5 minute demo.

See [YouTube video on how to install this application](https://www.youtube.com/watch?v=_RMeqd5-ZQ4&t=95s) for a complete 14 minute description on how to install and run.

Make diagrams for real world applications build from classes, methods, attributes, database tables, files, domain specific objects (for instance SAP BW DSO, MultiProvider, transformations, ...). Currently diagrams are often made manually. This is costly, time consuming and error prone. The goal is to generate diagrams like this automatically:
![An example of a manually created diagram](../../wiki/figures/DemoApplication2.png)

Currently diagrams like this can be generated (With methods, attributes, classes, packages, Web Dynpro ABAP and database tables):
![An example for a dependency graph](../../wiki/figures/SAP_Extractor_dependency_all.png)

This diagrams use model informations extracted with this tool. But they are generated with a second tool, see the point "Adaptable dependency graphs" a few lines below for this.

Another diagram with all currently supported diagram elements: ![All currently supported diagram elements](../../wiki/figures/All%20features%20SAP2Moose.png)

Positions of elements can be changed and stored to file. Comments to elements can be added. Connected elements can be highlighted. See specification for further logic. Further types of elements are planned to be added.

There is a new context menu available for groups of classes: ![Where to find the diagram](../../wiki/figures/WhereToFindInMenu.png)

See [Moose](http://www.moosetechnology.org/) for further informations on the analysis platform that is used.

# Extract informations to Moose

The SAP extractor for this project is hosted in this repository. It is currently `z_moose_extractor.abap`. The content of this file can be copied into a report and executed.

# Adaptable dependency graphs

The logic to display dependency graphs is hosted on Smalltalk Hub [RainerWinkler/RW-Moose-Diagram](http://www.smalltalkhub.com/#!/~RainerWinkler/RW-Moose-Diagram)

It can be used for all suitable Moose models, not only models generated from SAP.

# ABAPGit for development

This repository will use ABAPGit for all objects relevant to the development. The extractor itself is a simple program and can be installed with copy and paste. See wiki for details.

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
