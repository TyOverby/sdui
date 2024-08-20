# Features
- [x] remove toplevel params (easy)
- [x] "resize by 2x / 0.5x" buttons (easy)
- [x] better queue UI (medium)
- [x] paint for initial image (medium)
- [x] bigger drag/drop target for initial image (easy)
- [x] min-size on initial image preview (easy)
- [x] "dev mode" (detect localhost) (easy)
  - [x] set parallelism to 1 (easy)
  - [x] set steps to 1 (easy)
  - [x] default size to 128x128 (easy)
- [x] configurable parallelism (easy)
- [ ] add a "commit canvas" to apply filters to
      https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/filter (hard)
- [ ] "favorites" pane (medium)
- [ ] background upscale / downscale (hard)
- [ ] kill unneeded jobs (medium)
- [ ] single-workspace UI (hard)
- [ ] tree-based workflow (hard)
- [ ] parameterize lease_pool on info about work (easy)
- paint
  - [ ] opacity (medium)
  - [ ] color picker samples painting layer (easy)
  - [ ] layers (hard)
  - [ ] blur (unknown)
  - [ ] pan
  - [ ] clear paint and mask layers separately

# Bugs
- [ ] race on incomplete image
- [ ] incomplete image size is wrong
- paint
  - [ ] sync state on component reset

# Behavior
- [ ] why is the "preview" image updated so infrequently?
