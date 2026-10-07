# Kiln

A minimal static blog generator written in Haskell, distributed as a Nix flake.

## Features

- `kiln init` — scaffold the bundled `hello-kiln` template into the current (empty) directory
- `kiln build` — render Markdown posts with pandoc, assemble HTML component templates, generate the homepage and search index, and subset the configured fonts down to the characters actually used on the site (`pyftsubset`)
- `kiln clean` — remove build output
- running `kiln` with no arguments is equivalent to `kiln build`

## Installation & usage

This project is only distributed as a Nix flake — no Hackage package or standalone binary is published.

```sh
# run directly
nix run github:XilongYang/kiln -- init
nix run github:XilongYang/kiln -- build

# or add the flake's overlay / packages.kiln to your own configuration
```

Runtime dependencies (`pandoc`, `fonttools`/`pyftsubset`, `brotli`) are wrapped into the executable via the flake's `wrapProgram`, so nothing extra needs to be installed.

For development, `nix develop` opens a shell with `cabal-install`, HLS, and the same runtime dependencies.

## Configuration

`kiln build` and `kiln clean` read `kiln-config.json` from the current directory. See `template/hello-kiln/kiln-config.json` for a working example:

```json
{
  "path": {
    "in": {
      "src": "src",
      "template": "template",
      "fonts": "res/fonts"
    },
    "out": {
      "post": "post",
      "fonts-subset": "fonts-subset",
      "searchdb": "searchdb.json",
      "index": "index.html",
      "cache": ".cache"
    }
  },
  "webroot": "/",
  "toc": {
    "enable": true,
    "depth": 3,
    "number-sections": false
  },
  "fonts": {
    "JetBrainsMono-Regular.ttf": "JetBrains Mono",
    "MaterialIcons.woff2": "Material Icons",
    "SourceHanSerifCN-Regular.otf": "Source Han Serif CN"
  }
}
```

- `path.in.src` — directory of Markdown source files to render as posts
- `path.in.template` — directory of HTML component templates used to assemble pages
- `path.in.fonts` — directory containing the full font files to be subset
- `path.out.post` — output directory for rendered post HTML
- `path.out.fonts-subset` — output directory for the subset `.woff2` fonts
- `path.out.searchdb` — output path for the generated search index JSON
- `path.out.index` — output path for the generated homepage
- `path.out.cache` — directory used to cache intermediate build state
- `webroot` — the site's root path, used when generating absolute links
- `toc.enable` — whether pandoc generates a table of contents for each post (optional, default `true`)
- `toc.depth` — depth of the table of contents pandoc generates for each post (optional, default `3`)
- `toc.number-sections` — whether pandoc numbers section headings in posts (optional, default `false`)
- `fonts` — maps each font file name (relative to `path.in.fonts`) to the `font-family` name it is declared under in the site's own CSS, so kiln knows which font to subset for which family

## Project layout

- `app/` — executable entry point, dispatches the `init`/`build`/`clean` subcommands
- `src/Kiln/` — core logic (`Init`, `Config`, `Clean`, `Build`, and the `Build.*` modules for template substitution, post rendering, the index, the search database, and font subsetting)
- `template/hello-kiln/` — the default site template scaffolded by `kiln init`
- `test/` — hspec test suite

## Testing

```sh
cabal test
```

## License

MIT, see [LICENSE](LICENSE).

## Third-party resources

The bundled `hello-kiln` template ships with the following third-party resources:

- [latex.css](https://latex.css/) — base page styling
- [Prism](https://prismjs.com/) — code block syntax highlighting
- [MathJax](https://www.mathjax.org/) — math typesetting
- [JetBrains Mono](https://www.jetbrains.com/lp/mono/) — monospace font
- [Material Symbols](https://fonts.google.com/icons) — icon font
- [Source Han Serif](https://github.com/adobe-fonts/source-han-serif) — CJK serif font
