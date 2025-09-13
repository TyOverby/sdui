# UI/JS Directory

This directory contains JavaScript code for the browser-based painting and
image manipulation system. It provides the client-side painting engine that
powers SDUI's image editing capabilities, implementing sophisticated brush
tools, layer management, and pixel-level operations.

## Core Functionality

**Painter System (`index.js`)**
- Advanced painting engine with pressure-sensitive drawing
- Multi-layer canvas management (background, draw, mask, blur_mask, outline, composite layers)
- Brush tools with variable radius and pressure sensitivity
- Support for paint, mask, and blur mask modes
- Real-time visual feedback with cursor outline

**Drawing Operations**
- `drawPill` - Creates smooth brush strokes between points with variable radii
- Pressure-sensitive drawing with `event.pressure` support
- Anti-aliased brush rendering using elliptical shapes
- Smooth stroke interpolation for fluid drawing experience

**Layer Management**
- Background image layer for original content
- Paint layer for user modifications
- Mask layer for selection areas (black/white masks)
- Blur mask layer for selective blur effects
- Outline layer for cursor preview and visual feedback
- Composite operations for combining layers

**Advanced Features**

**Color Operations**
- Color picker with shift+click sampling from background image
- RGB to hex conversion utilities
- Inverse color calculation for cursor visibility

**Scramble/Shuffle Tool**
- `scramble` function implements texture randomization within circular areas
- Uses Gaussian distribution for natural texture variation
- `randomPointInCircle` generates statistically distributed sample points
- Pixel swapping algorithm for texture scrambling effects

**Image Handling**
- `sanatize_url` ensures proper data URL formatting
- `on_image_init` and `on_images_init` handle asynchronous image loading
- Dynamic canvas resizing based on image dimensions
- Support for high-DPI displays with `devicePixelRatio` scaling

**State Management**
- Comprehensive state object with painting configuration
- Layer compositing with caching for performance
- Dirty state tracking to optimize updates
- Mode switching between paint, mask, and blur operations

**User Interaction**
- Pointer event handling for stylus and touch support
- Context menu prevention for painting area
- Keyboard modifiers (ctrl for erase/shuffle, shift for color picking)
- Visual feedback with cursor outline and color preview

## Global Interface

**Painter API**
- `globalThis.painter.init` - Initialize painting system
- Returns state object and DOM element for integration
- Supports optional paint, mask, and blur mask inputs

**Utility Functions**
- `empty_white_image` - Generate blank canvas for new images
- Canvas manipulation utilities
- Base64 image data handling

## When to Edit Files Here

**Brush improvements:** Modify drawing functions and pressure handling in `index.js`
**New painting modes:** Extend mode switching and tool behavior
**Layer operations:** Add new canvas layers or compositing modes
**Touch/stylus support:** Update pointer event handling
**Performance optimization:** Improve canvas operations and caching
**Visual effects:** Add new pixel manipulation algorithms like scramble
**UI integration:** Modify the painter API for OCaml bindings
**Mobile support:** Enhance touch interaction and responsive behavior

This JavaScript code provides the foundation for SDUI's image editing
capabilities, bridging between the OCaml frontend and browser canvas APIs for
sophisticated painting tools.
