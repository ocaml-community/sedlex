# Unicode specification extraction

The file `src/syntax/unicode.ml` is generated using the data available at [unicode.org](https://www.unicode.org/Public/).

The rule at `src/syntax/dune` is the main entry point for this process. It triggers:
* download of data files at `src/generator/data`
* build of `src/generator/gen_unicode.exe`
* generation `unicode.ml` and places a copy in the source tree and a copy in the build tree

The rule is ignoed when using the `--ignore-promoted-rules` option. This option is also implied when using
`-p`/`--for-release-of-packages`.

See: [dune documentation](https://dune.readthedocs.io/en/latest/dune-files.html#modes) for more information.
