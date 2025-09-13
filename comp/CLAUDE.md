# Comp Directory

This directory contains the image composition interface for stitching multiple
images together into a single combined image. It's useful for creating
panoramas, collages, or combining generated images.

## Main Component (`comp.ml`)

The composition tool provides:

**File Upload Interface**
- Drag-and-drop zone for uploading multiple images
- Support for PNG images via file selection
- Automatic processing and preview of uploaded images
- Reorderable list for arranging images before composition

**Smart Image Positioning**
- `find_offset` function that automatically detects optimal positioning
- Uses pixel analysis to find non-overlapping placement
- Handles white/transparent pixel detection for seamless stitching
- Binary search algorithm for efficient offset calculation

**Composition Controls**
- Adjustable gap slider for spacing between images
- Real-time preview of final composition
- "darken" composite operation for better blending
- Automatic canvas sizing based on content

**Output**
- Exports composed image as SD-compatible format
- Maintains proper dimensions and base64 encoding
- Integration with SDUI's image processing pipeline

## Key Features

- **Automatic alignment:** Images are positioned to avoid overlapping content
- **White space handling:** Transparent or near-white pixels are ignored during positioning
- **Responsive gap control:** Fine-tune spacing between images
- **Reorderable workflow:** Drag to rearrange image order before processing
- **Real-time preview:** See composition results immediately

## Technical Implementation

Uses Canvas2D for:
- Pixel-level analysis for positioning
- Image composition and blending
- Export to data URLs for SD integration

The composition algorithm analyzes pixel data to find optimal horizontal
positioning, preventing content overlap while maintaining visual coherence.

## When to Edit Files Here

**New composition modes:** Add vertical stacking or grid layouts
**Enhanced algorithms:** Improve automatic positioning or blending
**Format support:** Add support for JPEG or other image formats
**Composition effects:** Add filters, borders, or artistic effects
**Batch processing:** Add support for processing multiple compositions
**Export options:** Add different output formats or sizes

This is a specialized tool for users who want to combine multiple generated
images into cohesive compositions.
