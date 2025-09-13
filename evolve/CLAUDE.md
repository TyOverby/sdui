# Evolve Directory

This is the main UI directory containing the desktop interface for SDUI. It
provides the full-featured image generation and editing workflow with
sophisticated tree-based image management, multiple generation modes, and
advanced editing capabilities.

## Core Architecture

**Main Interface (`evolve.ml/mli`)**
- Primary desktop component that orchestrates the entire evolve workflow
- Accepts tabs and renders the full desktop layout
- Entry point for the main SDUI experience

**Generation Screens**
- `txt2img_screen.ml/mli` - Text-to-image generation interface with state tree integration
- `img2img_screen.ml/mli` - Image-to-image editing with full card-based workflow system
- Both integrate with lease pools for server management and provide keyboard shortcuts

## Image Tree System (`image_tree.ml/mli`)

The heart of the evolve interface - a sophisticated tree-based image management system:

**Data Structure**
- `Model` - Maintains tree of images, parent-child relationships, and metadata
- `Unique_id` - Type-safe identifiers for images with DOM integration
- `Stage` - Image lifecycle states (Initial → Enqueued → In_progress → Finished/Error)
- `Stage.Kind` - Generation types (Prompt, Txt2img, Img2img, Resize, ControlNet, Edit)

**Tree Operations**
- `Tree_structure` - Hierarchical view with navigation utilities (`preceding`, `succeeding`)
- Parent-child relationships with automatic tree maintenance
- Root management for starting new generation chains

**Actions System**
- `Add_root` - Start new generation chain
- `Add` - Create child image with async generation dispatch
- `Remove` - Delete images (with keyboard support)
- `Set` - Update image state/stage
- `Toggle_starred` - Mark favorites
- `Set_seen` - Track viewed images

**Rendering**
- Visual tree representation with current selection
- Click handling and navigation
- Integration with seen/starred states

## Specialized Components

**Image Editing Cards (`img2img_screen.ml/mli`)**
- `basic_card` - Standard parameter modification cards (refine, reimagine, upscale, etc.)
- `resize_card` - Image resizing with dimension controls
- `controlnet_fix_card` - ControlNet parameter adjustment
- Each card provides parameter modifiers and custom UI views

**Resize Interface (`resize_screen.ml/mli`)**
- Dedicated component for image dimension adjustment
- Integrates with paint system for getting source images
- Provides callbacks for setting results

## Key Features

- **Tree-based workflow** - Generate variations and build on successful results
- **Multiple generation modes** - txt2img, img2img, ControlNet, resize, editing
- **Advanced parameter control** - Fine-grained control over all SD parameters
- **Visual state tracking** - Clear indication of generation progress and results
- **Keyboard shortcuts** - Efficient navigation and operations
- **Server management** - Integrated lease pool system for managing GPU resources
- **Favorites and history** - Star system and seen tracking

## When to Edit Files Here

**Main workflow changes:** Modify `evolve.ml/mli` for overall desktop interface structure
**Generation interfaces:** Update `txt2img_screen.ml/mli` or `img2img_screen.ml/mli` for new generation features
**Tree management:** Modify `image_tree.ml/mli` for new tree operations, states, or navigation
**Card system:** Extend `img2img_screen.ml/mli` for new editing card types
**Resize functionality:** Update `resize_screen.ml/mli` for dimension control features
**Keyboard shortcuts:** Add new shortcuts to screen components
**State management:** Extend `Stage.Kind` or `Action` types for new workflows

This is the primary interface that most users interact with - changes here
directly impact the main SDUI experience.
