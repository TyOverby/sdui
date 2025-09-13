# SDUI Project

SDUI is primarily a web UI for the Stable Diffusion API.  The typical user flow
is that they start with a textual prompt and  then modify them either directly
by painting on the image or by resubmitting it to the image-to-image API.

## Subdirectories
- `evolve`: the main UI for the project.  Starting with a text prompt, users
  generate many images, selecting some of them to edit and send back to the API
- `api`: defines types and helpers for dispatching to the stable diffusion API
- `ui`: misc ui components, including a tablet painting component
- `swipe`: a stripped-down version of the UI that is intended for mobile use
- `canvas2d`: bindings to the browsers `canvas2d` API
- `comp`: an image composition UI for stitching images together

## Working on this project

Sdui is written in OCaml using the Bonsai frontend framework.  The author of
SDUI is also the person who made Bonsai, so if you have any questions, please ask!

IMPORTANT: NEVER run git commands unless explicitly instructed to

### OCaml tips
- A long-lived `dune` build is running, so run `dune diagnostics` to fetch the
  current error messages.
- IMPORTANT: check for errors after every change!
- If you want to see the type of a value, you can write `let () = Some_module.some_function` 
  and then check the error messages to see the type mismatch.
