<code>This project is actively maintained and used (September 2020). Please open an issue to get support :-)</code>

# SAP2Moose

This repository provides a program to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model.

It allows drawing diagrams that visualize SAP applications with Moose Analysis. It is currently used mainly to generate customizable Dependency Diagrams with the application RW-Moose-Diagram. But it is open to other usages.

This is not a tool made or owned by SAP.

With [Moose2Model](http://www.moose2model.org) diagrams like this can be made:

Currently diagrams like this can be generated (With methods, attributes, classes, packages, Web Dynpro ABAP and database tables):
![An example for a dependency graph](../../wiki/figures/SAP_Extractor_dependency_all.png)

# Documentation

The documentation is in the Wiki of this repository (Switch to the tab Wiki).

[The architecture documentation is here.](Documentation/ArchitectureDocumentation.asciidoc)

## General

There are currently not many changes in the extractor, just because I am happily using it nearto every day (Rainer Winkler).

Last stable release v1.0.0: [Extractor](../../releases/download/v1.0.0/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v1.0.0/z_moose_extractor_texts.txt).. This is the first release where ADT links to ABAP classes and class components are extracted. Moose2Model v0.2.0 is required to use this links.

Earlier release v0.5.8: [Extractor](../../releases/download/v0.5.8/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.5.8/z_moose_extractor_texts.txt).. This is the first release where data can be analyzed without using Moose. It also provides a Where-Used analysis over multiple levels. This is an early version that is incomplete and should not find all dependencies. ![An example of multi level Where-Used analysis](../../wiki/figures/v0.4.0_MultiLevelWhereUsed.png)

# Compatibility

ABAP releases higher than 7.02 SP6.

There may be minor issues in older releases due to eroneous usage of modern ABAP statements, this should be easy to be fixed. You can do it yourself or open an issue for this.

# Installation

## To analyze applications

Create a new program in your SAP system. You need only the files z_moose_extractor.abap and z_moose_extractor_texts.txt. You find the most actual version on the tab Code. Or go to releases to get the code of a certain release.

Open file z_moose_extractor.abap with a text editor and paste the coding in your program. Add texts for convenience. Run the program, select a package and download the file (preferable with extension .mse). Open with Moose Analysis or Moose2Model to analyze it.

## To participate in development

Use abapGit to install the coding in a local system. The package name has to be Z2MSE (Required so that test objects have the correct names).

# abapGit for development

This repository will use [abapGit](http://abapgit.org) for all objects relevant to the development. The extractor itself is a simple program and can be installed with copy and paste. See wiki for details.

# Further

The local classes of the extractor are now generated using global classes. This allows ABAP Unit to be used to improve the quality and reduce the time needed for implementation. There is now also a new program z_moose_translate_to_local for this conversion. This program can also be used for other projects.

See [Blog on SAP developer network](https://blogs.sap.com/2017/07/23/software-exploration-tool-next-steps/).

# Thanks

I would like to thank especially:

*Damir Majer* for many helpfull discussions and especially for inviting Tudor Girba to the SAP Inside Track Munich 2015, thus indirectly initiating this project

*Tudor Girba* for many valuable informations and tips on how to use Moose and FAMIX

*Enno Wulff* for making a 7.31 version available and many discussions to improve the extractor and to give hints on what can be done

And all *colleagues* that made this tool possible!

# Funding

*CubeServ* is encouraging and supporting this project.
