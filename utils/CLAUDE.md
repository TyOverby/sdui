# Utils Directory

This directory contains shared utility modules used across the entire SDUI
application. These are foundational components that provide common
functionality for effects, UI elements, file handling, and data processing.

## Core Utilities

**Effect Management (`effect_utils.ml/mli`)**
- `parallel_both` - Run two effects concurrently and collect results
- `parallel_all` - Run a list of effects concurrently
- `parallel_n` - Run N effects with progress updates via map
- `while_running` - Execute a background effect while main effect runs
- Essential for managing concurrent API calls and background tasks

**File Handling**
- `file_upload_zone.ml/mli` - Drag-and-drop file upload attributes with MIME type filtering
- `file_data_url.ml/mli` - Bridge between browser File objects and Bonsai file handling
- Used throughout the app for image uploads and file processing

**Layout System (`snips.ml/mli`)**
- Advanced CSS Grid-based layout system for complex UI arrangements
- Supports flexible sizing with `Fr`, `Auto`, `Min_content`, `Max_content`, `Fit_content`
- Scrolling configuration with gutter support for consistent scrollbar handling
- Directional layout: `top`, `right`, `bottom`, `left`, `split_h`, `split_v`
- `body` for main content areas, `render` for final layout output
- Used for the main workspace layouts throughout SDUI

**JSON Processing (`yojson_safe.ml/mli`)**
- Extended Yojson.Safe with additional utilities
- `merge_objects` for combining JSON templates with data
- Sexp derivation for debugging JSON structures
- Critical for API request/response handling

**Advanced UI Components**
- `fancy_box.ml` - Sophisticated layered box component with blur effects, opacity, and dual borders
- `raw_textarea.ml` - Custom textarea with syntax highlighting, backdrop blur, and advanced styling
- Both provide polished, modern UI elements beyond standard web controls

## When to Edit Files Here

**Concurrency patterns:** Modify `effect_utils.ml/mli` for new parallel processing needs
**File upload features:** Update `file_upload_zone.ml/mli` for new MIME types or upload behaviors
**Layout requirements:** Extend `snips.ml/mli` for new layout patterns or responsive features
**JSON handling:** Modify `yojson_safe.ml/mli` for new JSON processing utilities
**Custom UI components:** Add new sophisticated widgets following `fancy_box.ml` and `raw_textarea.ml` patterns
**Cross-cutting concerns:** Add new utilities here if they're used by multiple directories

These utilities form the foundation of SDUI's architecture - changes here can
affect the entire application.
