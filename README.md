# Dependency graphs

**License problems regarding SAP AS ABAP 7.50 SP02 Developer Edition are probably solved**

There is currently a bug fix for the Minisap in preparation that should solve the license issues that occured after 30 June 2017. I will therefore again able to support this application.

This repository provides a program to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model.

Help wanted - Just open an Issue, I (Rainer Winkler) will be happy to answer any question even if the answer may be somewhere to be found here. - Or make a Fork and propose changes or improvements - Or make changes to the Wiki and improve the documentation, the Wiki is open to be edited by everyone.

There are currently not many changes in the extractor, just because I am happily using it nearto every day (Rainer Winkler).

Last stable release 0.4.3: [Extractor](../../releases/download/v0.4.3/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.4.3/z_moose_extractor_texts.txt).. This is the first release where data can be analyzed without using Moose. It also provides a Where-Used analysis over multiple levels. This is an early version that is incomplete and should not find all dependencies. ![An example of multi level Where-Used analysis](../../wiki/figures/v0.4.0_MultiLevelWhereUsed.png)

Last release 0.3.0: [Extractor](../../releases/download/v0.3.0/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.3.0/z_moose_extractor_texts.txt).. This is the first release where also usage by SAP BW routines is added to the model.

Create a new program in your SAP system. Open file z_moose_extractor.abap with a text editor and paste the coding in your program. Add texts for convenience. Run the program, select a package and download the file (preferable with extension .mse). Open with Moose to analyze it. Or use release 0.4.0 or higher to have features available that do not require Moose.

Should work in ABAP 7.02 SP6, but there is currently only a limited test for this release done.

# Installation

See [YouTube video on how to install this application](https://www.youtube.com/watch?v=_RMeqd5-ZQ4&t=95s) for a complete 14 minute description on how to install and run.

## By downloading Moose 6

Go to [Moose Analysis Installation](http://www.moosetechnology.org/#install) and download an image for Moose 6.0. Extract the zip file, no installer is needed.

## By using the Pharo Launcher

Install the Pharo Launcher from https://ci.inria.fr/pharo/view/Launcher/job/Launcher/ When the installation is done, choose a Moose 6.0 image.

## Add the logic for RW-Moose-Diagram

Execute the Pharo.exe in the extracted folder. Make a left mouseclick into the Pharo desktop and select Playground. Paste the following code into the Playground:

    Gofer new
        smalltalkhubUser: 'RainerWinkler' project: 'RW-Moose-Diagram';
        package: 'RWMooseDiagram';
        load.

Select the complete coding with the mouse. This is mandatory because the Playground will execute only the marked part of the coding. Make a click with the right mouse button and select Do It. 

## A first model

Make a left mouse click in the Pharo desktop and select Moose -> Moose Panel. Click in the upper right on the icon with MSE (Import model from file). Select the mse file that is generated with the SAP extractor. Click on the name of the file. There will now be a list with "All accesses ... All attributes ..."  Make a right click on All classes and choose Visualize -> RW Dependency graph. If the model is very big, it may need some time until the diagram is displayed. See next chapter how to work with it.

## Adaptable dependency graphs

The logic to display dependency graphs is hosted on Smalltalk Hub [RainerWinkler/RW-Moose-Diagram](http://www.smalltalkhub.com/#!/~RainerWinkler/RW-Moose-Diagram) (The link may work only in Firefox).

It can be used for all suitable Moose models, not only models generated from SAP.

# How to use

See [YouTube video on how this application is typically used during development](https://youtu.be/0jLN-2AVIvo) for a 2.5 minute demo.

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

# abapGit for development

This repository will use [abapGit](http://abapgit.org) for all objects relevant to the development. The extractor itself is a simple program and can be installed with copy and paste. See wiki for details.

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
