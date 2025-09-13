# API Directory

This directory contains all the API bindings and types for interfacing with the
Stable Diffusion API. It's where you'll find everything needed to make requests
to SD servers and handle responses.

## Core API Files

**Main API Interface (`sd.ml/mli`)**
- Central module that exports all other API modules
- Entry point for all Stable Diffusion functionality

**Request Types**
- `txt2img.ml/mli` - Text-to-image generation requests and query types
- `img2img.ml/mli` - Image-to-image generation with support for masks and input images
- `parameters.ml/mli` - Form components for SD parameters (steps, CFG, dimensions, etc.)

**Server Management**
- `hosts.ml/mli` - Manages available SD server hosts, health checks, and current models
- `models.ml/mli` - Model selection forms and current model queries
- `progress.ml/mli` - Progress tracking for long-running generation tasks

**Specialized Features**
- `alwayson_scripts.ml/mli` - ControlNet and Regional Prompter integration
- `controlnet_*` files - ControlNet detection, models, and modules
- `samplers.ml/mli` - Available sampling methods (Euler, DPM++, etc.)
- `styles.ml/mli` - Predefined style templates
- `upscaler.ml/mli` - Upscaling methods for high-res generation

**Utilities**
- `image.ml/mli` - Image handling and conversion utilities
- `constants.ml` - Default query templates and configuration
- `custom_form_elements.ml/mli` - Reusable form widgets (textareas, sliders, checkboxes)
- `load_image_effect.ml` - Image loading effects
- `strip_images_from_json.ml/mli` - JSON processing utilities

## When to Edit Files Here

**Add new API endpoints:** Extend `txt2img.ml/mli` or `img2img.ml/mli`
**Add new parameters:** Modify `parameters.ml/mli` and update query types
**New server features:** Update `hosts.ml/mli` for host management
**ControlNet extensions:** Modify `controlnet_*` and `alwayson_scripts.ml/mli`
**Form improvements:** Edit `custom_form_elements.ml/mli`
**Default settings:** Update `constants.ml`
