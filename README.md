# SAP2Moose

This repository provides a program to extract model data from a SAP system into the analysis platform Moose on Pharo Smalltalk. It uses FAMIX as a flexible enhancable meta model.

It allows drawing diagrams that visualize SAP applications with Moose Analysis. It is currently used mainly to generate customizable Dependency Diagrams with the application RW-Moose-Diagram. But it is open to other usages.

This is not a tool made or owned by SAP.

With [Moose2Model](http://www.moose2model.org) diagrams like this can be made:

Currently diagrams like this can be generated (With methods, attributes, classes, packages, Web Dynpro ABAP and database tables):
![An example for a dependency graph](../../wiki/figures/SAP_Extractor_dependency_all.png)

# Documentation

The documentation is in the Wiki of this repository (Switch to the tab Wiki).

## General

There are currently not many changes in the extractor, just because I am happily using it nearto every day (Rainer Winkler).

Last stable release v0.5.8: [Extractor](../../releases/download/v0.5.8/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.5.8/z_moose_extractor_texts.txt).. This is the first release where ADT links to ABAP classes and class components are extracted. Moose2Model v0.2.0 is required to use this links.

Earlier release v0.4.4: [Extractor](../../releases/download/v0.4.4/z_moose_extractor.abap) and [Texts for extractor](../../releases/download/v0.4.4/z_moose_extractor_texts.txt).. This is the first release where data can be analyzed without using Moose. It also provides a Where-Used analysis over multiple levels. This is an early version that is incomplete and should not find all dependencies. ![An example of multi level Where-Used analysis](../../wiki/figures/v0.4.0_MultiLevelWhereUsed.png)

# Compatibility

Should work in ABAP 7.02 SP6, but there is currently only a limited test for this release done.

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
