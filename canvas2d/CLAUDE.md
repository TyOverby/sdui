# Canvas2D Directory

This directory provides OCaml bindings to the browser's Canvas 2D API. It's
a low-level graphics layer used for image manipulation, painting interfaces,
and pixel-level operations throughout SDUI.

## Core Types and APIs

**Canvas (`Canvas` module)**
- `create ~width ~height` - Creates new canvas with specified dimensions
- `ctx2d` - Gets 2D rendering context (set `~will_read_frequently:true` for pixel access)
- `clone` - Duplicates canvas by drawing onto new canvas
- `of_image` - Creates canvas from existing image
- `to_data_url` - Exports canvas as base64 data URL
- `dom_element` - Access underlying DOM canvas element

**2D Context (`Ctx2d` module)**
- Drawing operations: `fill_rect`, `draw_canvas`, `draw_image`
- Style control: `set_fill_style`, `set_global_composite_operation`, `set_filter`
- Pixel access: `get_image_data`, `put_image_data`
- Image conversion: `to_image` (async with callback)
- Size queries: `width`, `height`

**Image Data (`Image_data` module)**
- Pixel-level read/write: `get_rgba`, `set`, `get_r/g/b/a`, `set_r/g/b/a`
- Efficient batch operations with `get_rgba'` into arrays
- Dimensions: `width`, `height`
- Raw data access via `Pixel_array`

**Pixel Array (`Pixel_array` module)**
- Direct byte-level access to RGBA pixel data
- Row-major order with RGBA byte sequence
- Bigarray interface: `as_bigarray` for efficient operations
- Direct get/set operations on individual bytes

**Image (`Image` module)**
- Loading: `of_url`, `of_ctx` (both async)
- Export: `to_data_url` for base64 encoding
- Manipulation: `add_padding`, `resize` (both async)
- DOM access: `dom_element`

## Usage Patterns

This module is typically used for:
- **Painting interfaces** - The paint module uses this for mask editing
- **Image processing** - Pixel-level manipulation and filtering
- **Composition operations** - The comp module uses this for image stitching
- **Format conversion** - Between web images and SD API formats

## When to Edit Files Here

**New canvas operations:** Add drawing primitives or effects to `Ctx2d`
**Pixel algorithms:** Extend `Image_data` and `Pixel_array` for new processing
**Image utilities:** Add new image manipulation functions to `Image`
**Performance optimization:** Improve bigarray or async operations
**Browser API updates:** Add new Canvas 2D API features as they become available

This is a foundational layer - changes here affect painting, composition, and
any pixel-level image operations throughout SDUI.
