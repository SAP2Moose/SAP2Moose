# Context
Issue #106: Find classes to read dynamic usages automatically. The current dynamic usage mechanism (`p_dyn` parameter) requires provider classes to return data in `wbcrossgt` format (SAP where-used table structure). This is inflexible — it only supports relations expressible as wbcrossgt entries and can't add new element types or SOMIX-native relations. The goal is a new, more flexible mechanism where provider classes deliver elements and relations directly in SOMIX notation.

# Current Understanding

## Existing mechanism (keep for backward compatibility)
- Parameter `p_dyn` accepts space-separated class names
- Each class must implement static method `WHERE_USED` exporting a table of `wbcrossgt` entries
- `z2mse_extr3_where_used_builder.set_dynamic_read()` calls these classes and stores results in `g_dynamic_usage`
- Dynamic usages are merged into both `search_up` and `search_down` in the where_used_builder
- Works only for relations expressible in wbcrossgt format (otype + name + include)

## Architecture for new mechanism
- The extraction pipeline: Element Selection -> Association Building (up/down search) -> Model Harmonization -> MSE Export
- `z2mse_extr3_model_builder` orchestrates search via `association_builders` list (currently: `where_used_builder`, `tadir_builder`)
- Each builder has `search_up(element_id)` and `search_down(element_id)` methods called at every search level
- A new builder can be added to this list cleanly

## Code smell identified
- `z2mse_extr3_planing` adds SOMIX elements during `make_model()` (late) — this mixes element discovery with model serialization
- Elements added late are invisible to the search machinery and can never be followed further
- The new mechanism should avoid this pattern

## Key tension: SOMIX vs extraction layer
- The search machinery operates on the **extraction layer** (element IDs, element types, associations)
- SOMIX is the **output format**
- A provider returning pure SOMIX data can't feed into further up/down search because those elements aren't registered in the extraction layer

# Decisions
- Keep existing `p_dyn` mechanism unchanged for backward compatibility
- New mechanism should integrate at the `association_builders` level in `model_builder.initialize()`, participating in search phases (not late in `make_model()`)

# Open Questions
- **Two-part provider contract?** Provider could return:
  - Part A: SAP elements (class names, program names, table names) registered in extraction layer -> participate in further search
  - Part B: SOMIX-only elements and relations (external/non-SAP things) -> added to model but no further search (correct because nothing to search)
- Is this two-part split the right approach, or is there a simpler design?
- How should the provider classes be discovered? By naming convention? By a new parameter?
- Should there be an attribute marking elements/relations as coming from a dynamic provider rather than plain SAP2Moose?
- Should the list of provider classes be specifiable (like `p_dyn` allows multiple classes)?

# Next Steps
- Finalize the provider class contract (method signatures, return types)
- Decide on Part A / Part B split or alternative
- Create new association builder class
- Wire it into `model_builder.initialize()`
- Create test provider class
- Write ABAP Unit tests

# Relevant Files
- `src/main/extr/z2mse_extr3_model_builder.clas.abap` — orchestrator, `initialize()` wires builders, `search()` runs phases
- `src/main/extr/assocbuild/z2mse_extr3_where_used_builder.clas.abap` — current dynamic usage integration (`set_dynamic_read`, `g_dynamic_usage`)
- `src/main/extr/assocbuild/z2mse_extr3_association_build.clas.abap` — base class for builders
- `src/main/extr/z2mse_extr3_element_manager.clas.abap` — central registry, holds SOMIX objects (`somix_code`, `somix_call`, `somix_access`, etc.)
- `src/main/extr/elements/z2mse_extr3_planing.clas.abap` — existing pattern of adding SOMIX directly (code smell to avoid)
- `src/main/somix/z2mse_somix_code.clas.abap` — SOMIX Code add interface
- `src/main/somix/z2mse_somix_call.clas.abap` — SOMIX Call interface
- `src/main/somix/z2mse_somix_access.clas.abap` — SOMIX Access interface
- `src/template/z2mse_moose_extractor2.prog.abap` — report with parameter `p_dyn`
- `src/tools/z2mse_dynamic_usage.clas.abap` — existing dynamic usage provider example
- `src/test/no_initial_selectn/z2mse_test_dynamic_usage.clas.abap` — test dynamic usage provider
- `models/Invocations and Accesses.json` — diagram showing how associations flow
- `models/Make Model.json` — diagram showing make_model phase
- `models/Planing RSPLAN.json` — diagram showing the planing SOMIX pattern

# Notes
- The `z2mse_extr3_planing` pattern (adding SOMIX elements during make_model) works but is architecturally wrong. The new feature is an opportunity to do it correctly from the start.
- Provider use cases: dynamic ABAP calls resolved via DB table entries, external applications consuming SAP code, BW-specific relations not visible in standard where-used.
- The SOMIX classes handle duplicates gracefully (`somix_code->add()` returns `exists_already_with_id`), so adding elements that already exist is safe.
