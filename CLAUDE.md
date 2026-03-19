# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SAP2Moose extracts architectural model data from SAP systems into SOMIX (or legacy FAMIX) format for visualization with Moose2Model (moose2model.org). The extractor is an ABAP program that reads SAP database tables directly (read-only, non-invasive). It supports ABAP releases 7.02 SP6 and higher.

## Development Setup

- Use **abapGit** to install the code into a SAP system
- Package name **must be Z2MSE** (test objects depend on this naming)
- Folder logic: prefix

## Build & Deployment

Development uses global ABAP classes. For deployment, these are converted to local classes inside a single report file:

1. Develop/modify global classes in `src/main/`
2. Run program `z_moose_translate_to_local` to convert global classes to local classes
3. This generates `z_moose_extractor.abap` — the single-file deployment artifact (paste into SAP)

## Testing

ABAP Unit tests are embedded alongside classes in `*.testclasses.abap` files. Run them through the SAP system's ABAP Unit test runner (SE80 or ADT). No external CI/CD pipeline exists.

## Architecture

The extraction pipeline flows: **Element Selection → Association Building → Model Harmonization → MSE Export**

### Core Extraction Layer (`src/main/extr/`)

- `z2mse_extract3.clas.abap` — Main extraction entry point
- `z2mse_extr3.clas.abap` — Orchestrator class
- `z2mse_extr3_element_manager.clas.abap` — Central registry for all extracted elements and relationships
- `z2mse_extr3_model_builder.clas.abap` — Tracks elements by search level (initial, up-search, down-search)
- `z2mse_extr3_initial_elements.clas.abap` — Handles initial element selection from user input

### Element Types (`src/main/extr/elements/`)

Each SAP object type (classes, programs, packages, tables, functions, BW objects) has its own extractor class. These read from SAP database tables and register elements with the element manager.

### Association Builders (`src/main/extr/assocbuild/`)

- `z2mse_extr3_where_used_builder.clas.abap` — Up-search: "what uses this element"
- `z2mse_extr3_tadir_builder.clas.abap` — Direct TADIR relationships
- Supports bidirectional search (up-search and down-search)

### Metamodel Layers

- **SOMIX** (`src/main/somix/`) — Current format. 9 core classes: Entity, Element, Grouping, Code, Data, Coupling, Calls, Access, ParentChild
- **FAMIX** (`src/main/famix/`) — Legacy format (being phased out). 12+ entity types

### Model Export (`src/main/mse/`)

- `z2mse_somix_harmonize.clas.abap` — SOMIX output harmonization
- `z2mse_mse_harmonize.clas.abap` — FAMIX output harmonization
- `z2mse_output_model.clas.abap` — File output handling

## Models Directory

- `*.m2m` files — Transformation models for Moose2Model visualization tool
- `*.json` files — Focused circuit diagrams of this project's own code in condensed SOMIX format, exported from Moose2Model for AI analysis. Each file covers a specific aspect of the codebase (indicated by its `diagramTitle`), not the complete model. They contain nodes (classes, methods, attributes with their SOMIX type), calls (method-to-method), and accesses (method-to-data). The `AwN` flag ("Added with Neighbors") is critical: only elements with this flag have **all** their calls and accesses shown in the diagram. Elements without `AwN` appear as context but may have relationships in the code that are not visible in that diagram. To get the full picture for a given element, look for it with `AwN` in the relevant diagram, or fall back to reading the source code.

### Working with Diagrams

The JSON diagrams are created and maintained by the user in Moose2Model. Claude Code can request changes:

- **New diagram:** Ask the user to create a diagram for a specific aspect not yet covered.
- **Add element with AwN:** If an element's relationships are needed but it only appears as context (without `AwN`), ask the user to add it with all neighbors in the relevant diagram.
- **Comment corrections:** If a diagram contains wrong or outdated comments, ask the user to update them.
- **New comments:** Ask the user to add comments to diagrams where they would help clarify intent.

Always specify which element or diagram you need changed so the user can act on it in Moose2Model.

## Session Summaries

When the user asks to save/summarize a session, write it to `ai/issues/<issue-number>-<short-name>.md` using this structure:

```markdown
# Context
Short description of the problem

# Current Understanding
What we found out so far

# Decisions
- ...

# Open Questions
- ...

# Next Steps
- ...

# Relevant Files
- src/...
- ...

# Notes
Free text / thoughts
```

The user may say "save the session under ai/issues/..." or "update ai/issues/...". Create or update the file accordingly.

## Key Conventions

- All ABAP classes use prefix `z2mse_`
- Class naming: `z2mse_extr3_*` for extraction layer, `z2mse_somix_*` for SOMIX metamodel, `z2mse_famix_*` for FAMIX metamodel
- abapGit XML metadata files (`.xml`) accompany each ABAP source file
