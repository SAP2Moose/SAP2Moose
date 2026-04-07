# Context
FAMIX export is legacy — fully replaced by SOMIX. Removing it will reduce code complexity and maintenance effort. SOMIX is the only actively used export format.

# Current Understanding

## Scope of FAMIX Code

**Files to delete (~56):**
- `src/main/famix/` — 15 FAMIX metamodel classes + 2 test files + 32 XML metadata files
- `src/main/mse/z2mse_mse_harmonize*` — FAMIX harmonization + maker + tests (6 files)
- `src/obsolete/z2mse_famix_*` — 4 obsolete FAMIX classes

**Files to modify (~15):**
- `z2mse_extr3_element_manager.clas.abap` — Central control point. Contains 8 FAMIX class references (lines 22-28) and `use_somix` flag (line 41) with IF/ELSE branching in constructor
- 7 extraction files with `IF use_somix EQ 'X'` checks (~16 checks total):
  - `z2mse_extr3_access.clas.abap` (2 checks)
  - `z2mse_extr3_invocation.clas.abap` (2 checks)
  - `z2mse_extr3_classes.clas.abap` (5 checks)
  - `z2mse_extr3_packages.clas.abap` (1 check)
  - `z2mse_extr3_programs.clas.abap` (2 checks)
  - `z2mse_extr3_tables.clas.abap` (2 checks)
  - `z2mse_extr3_web_dynpro_comp.clas.abap` (2 checks)
- 6 test files that reference FAMIX output or harmonization
- `z2mse_main_test.clas.abap` — Integration test with FAMIX expectations

## Key Coupling Points
1. **`z2mse_extr3_element_manager`** — instantiates FAMIX or SOMIX objects based on `use_somix` flag
2. **`z2mse_mse_harmonize`** — 765 lines of FAMIX-specific harmonization with hard-coded `'FAMIX.Class'`, `'FAMIX.Method'` etc.
3. **Element extractors** — conditional branches producing FAMIX or SOMIX output

# Decisions
- No refactoring for readability — focus on functional removal only

# Open Questions
- Can `use_somix` flag and all related IF/ELSE be removed completely, or do we need to keep backward compatibility for any users?
- Should `z2mse_model.clas.abap` and `z2mse_output_model.clas.abap` be simplified after FAMIX removal? (They handle MSE format which was FAMIX-oriented)
- What about `z2mse_extr3_association.clas.abap` and `z2mse_extr3_access_or_invocatn.clas.abap` — do they have FAMIX dependencies?
- Is the MSE output format itself still needed, or does SOMIX use a different output mechanism?

# Proposed Approach — Incremental Removal in 5 Phases

## Phase 1: Preparation
- Ensure all SOMIX tests pass independently of FAMIX
- Verify `z2mse_somix_harmonize` covers all harmonization needs

## Phase 2: Delete FAMIX Metamodel
- Delete entire `src/main/famix/` directory (15 classes + metadata)
- Delete `src/obsolete/z2mse_famix_*` files
- Delete FAMIX harmonization: `z2mse_mse_harmonize*`

## Phase 3: Simplify Element Manager
- Remove FAMIX data declarations from `z2mse_extr3_element_manager`
- Remove ELSE branch in constructor — keep only SOMIX initialization
- Remove `use_somix` flag (always SOMIX)

## Phase 4: Simplify Extraction Layer
- Remove `IF use_somix EQ 'X'` checks from 7 element/association files
- Keep only SOMIX branches as the sole code path

## Phase 5: Update Tests
- Remove FAMIX-specific test methods and expectations
- Update `z2mse_main_test` to SOMIX-only
- Run full ABAP Unit test suite

# Next Steps
- Clarify open questions before starting
- Begin with Phase 1 (test verification)

# Relevant Files
- src/main/famix/ (entire directory)
- src/main/mse/z2mse_mse_harmonize.clas.abap
- src/main/mse/z2mse_mse_harmonize_maker.clas.abap
- src/main/extr/z2mse_extr3_element_manager.clas.abap
- src/main/extr/associations/z2mse_extr3_access.clas.abap
- src/main/extr/associations/z2mse_extr3_invocation.clas.abap
- src/main/extr/elements/z2mse_extr3_classes.clas.abap
- src/main/extr/elements/z2mse_extr3_packages.clas.abap
- src/main/extr/elements/z2mse_extr3_programs.clas.abap
- src/main/extr/elements/z2mse_extr3_tables.clas.abap
- src/main/extr/elements/z2mse_extr3_web_dynpro_comp.clas.abap
- src/main/test/z2mse_main_test.clas.abap

# Notes
- The `use_somix` flag in element_manager is the central switch — once removed, all conditional branches become dead code
- FAMIX classes themselves (src/main/famix/) have low coupling — they can be deleted cleanly
- The harder part is cleaning up the 16 conditional checks in the extraction layer and updating tests
- z2mse_model.clas.abap uses MSE format which was originally FAMIX-oriented — may need review
