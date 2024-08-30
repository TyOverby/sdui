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
- [x] single-workspace UI (hard)
- [ ] "favorites" pane (medium)
  - [ ] send to compositor (unknown)
- [ ] background upscale / downscale (hard)
- [ ] kill unneeded jobs (medium)
- [ ] tree-based workflow (hard)
- [ ] parameterize lease_pool on info about work (easy)
- [ ] initialize paint zoom to fit canvas on screen
- [ ] clean up initial screen (medium)
- [ ] 

## paint
- [ ] opacity (medium)
- [ ] color picker samples painting layer (easy)
- [ ] layers (hard)
- [ ] blur (unknown)
- [ ] soft brush (unknown)
- [ ] pan
- [ ] clear paint and mask layers separately
- [ ] add a "commit canvas" to apply filters to
      https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/filter (hard)

## Compositor
- [ ] basic horizontal compositing of same-size images 
- [ ] background overlap compositing 

# Cleanup
- [x] collapse `Bonsai.t` in `paint.ml`
- [x] split navigation out of `pair.ml`
- [x] rename `pair` to `chain`
- [ ] use upstreamed `snips.ml` 
- [x] upstream `file_data_url.ml`
  - [ ] use upstreamed `file_data_url.ml`
- [x] upstsream `file_upload_zone.ml`
  - [ ] use upstsreamed `file_upload_zone.ml`
- [x] delete old "sd" binary

# Bugs
- [ ] race on incomplete image
- [ ] incomplete image size is wrong
- paint
  - [ ] sync state on component reset


# Behavior
- [ ] why is the "preview" image updated so infrequently?
