# Developer tools

## Visualising DFA graphs in Emacs

The expect tests in `test/codegen/` include inline DOT graphs for each
lexer. The file `emacs/sedlex-dot.el` lets you render them as SVG images
directly inside the test buffer.

Load it once:

```elisp
(load "/path/to/sedlex/emacs/sedlex-dot.el")
```

Then, in any test buffer containing `DOT:` / `CODE:` blocks:

- `M-x sedlex-render-dot-blocks` — replace DOT source with rendered SVG.
- `M-x sedlex-remove-dot-images` — remove the overlays and show the source again.

Requires `graphviz` (`dot`) in your PATH and Emacs compiled with SVG
support (check with `(image-type-available-p 'svg)`).

## Rendering DOT graphs to files

`test/codegen/render_dots.sh` extracts all DOT blocks from the expect
tests and renders them as SVG files:

```
./test/codegen/render_dots.sh [output_dir]
```

Output defaults to `test/codegen/dot_images/`.
