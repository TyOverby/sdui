# UI Directory

This directory contains reusable UI components and utilities used throughout
the SDUI application. It focuses on common interface patterns, resource
management, and shared widgets.

## Core UI Components

**Workspace Management (`workspace.ml/mli`)**
- Layout system for organizing gallery views, forms, and painting tools
- Provides structured layouts for different application modes
- Contains `make` function for creating workspaces with gallery, form, color
  picker, pen controls
- `for_first_node` for initial setup layouts
- `finalize` combines workspace with hosts, queue, and routing

**Painting Interface (`paint.ml/mli`)**
- Complete painting/editing widget for image modification
- Handles masks, blur masks, and underlying images
- Provides color picker, pen size controls, layer management
- Key for image-to-image workflows where users paint on generated images
- Returns paint images via `get_images` effect

**Resource Management (`lease_pool.ml/mli`)**
- Generic pool system for managing limited resources (like GPU hosts)
- Prevents resource conflicts by lending out values for effect duration
- Queues requests when resources are busy
- Critical for managing multiple SD server hosts efficiently

**Navigation & Routing**
- `navigation.ml/mli` - Navigation components and routing utilities
- `route.ml/mli` - Route definitions and path handling

**Generation Chains (`chain.ml/mli`, `sd_chain.ml`)**
- `chain.ml/mli` - Image generation chains for img2img workflows
- `sd_chain.ml` - Stable Diffusion specific chain implementations
- Handles sequences of image transformations

**Specialized Components**
- `single.ml/mli` - Single image display and interaction components
- `first_image.ml/mli` - Initial image generation interface
- `inc.ml/mli` - Incremental value utilities
- `parameters.ml/mli` - UI-specific parameter forms and validation
- `lease_pool_small_viz.ml/mli` - Visualization for resource pool status

**Utilities**
- `skip.ml` - Utility functions for UI logic

## When to Edit Files Here

**Layout changes:** Modify `workspace.ml/mli` for new workspace arrangements
**Painting features:** Edit `paint.ml/mli` for new drawing tools or mask functionality
**Resource management:** Update `lease_pool.ml/mli` for new resource types or pooling logic
**Navigation:** Modify `navigation.ml/mli` and `route.ml/mli` for new pages or routing
**Image chains:** Edit `chain.ml/mli` for new transformation sequences
**Form improvements:** Update `parameters.ml/mli` for new parameter controls
**New reusable widgets:** Add to this directory following existing patterns
