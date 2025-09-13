# Swipe Directory

This directory contains a mobile-optimized version of the SDUI interface
designed for touch interactions and simplified workflows. It's a stripped-down
alternative to the main desktop interface.

## Core Mobile Components

**Main Mobile Interface (`swipe.ml/mli`)**
- Primary mobile component that accepts tabs and renders the mobile layout
- Simplified interface optimized for smaller screens and touch interaction

**Touch Interactions (`swiper.ml/mli`)**
- Swipe gesture handling component
- Provides `on_remove` effect for swipe-to-dismiss functionality
- Core touch interaction primitive for mobile workflows

**Mobile Text-to-Image (`txt2img_screen.ml/mli`)**
- Mobile-specific text-to-image generation interface
- Simplified parameter controls suitable for mobile use
- Integrates with lease pool for server management
- Provides `generate_action` for triggering generation
- Includes host monitoring view

**Touch Input (`tapper.ml/mli`)**
- Touch event handling utilities
- Complements swiper for comprehensive mobile input

**Responsive Utilities (`visibility_attr.ml/mli`)**
- Attribute utilities for responsive design
- Controls element visibility based on screen size or mobile context

## Architecture

The swipe interface is designed as a simplified, mobile-first alternative to the main `evolve/` interface. It focuses on:
- Touch-friendly interactions
- Simplified parameter controls
- Streamlined generation workflow
- Optimized layouts for mobile screens

## When to Edit Files Here

**Mobile layout changes:** Modify `swipe.ml/mli` for overall mobile interface structure
**Touch gestures:** Edit `swiper.ml/mli` and `tapper.ml/mli` for new gesture handling
**Mobile generation:** Update `txt2img_screen.ml/mli` for mobile-specific generation features
**Responsive behavior:** Modify `visibility_attr.ml/mli` for new responsive utilities
**Mobile-specific features:** Add new components following the simplified mobile patterns

This directory should be preferred when working on mobile-specific features or
when the desktop interface in `evolve/` is too complex for mobile users.
