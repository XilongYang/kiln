# Kiln

A minimal static blog generator written in Haskell, distributed as a Nix flake.

## Features

- `kiln init [default|flow]` — scaffold one of the bundled templates into the current (empty) directory: `default` is a blog, `flow` is a card-flow feed; `kiln init` with no argument scaffolds `default`
- `kiln build` — render Markdown posts and every configured page (see `pages` below) with pandoc, generate the search index, and subset the configured fonts down to the characters actually used on the site (`pyftsubset`); posts, pages, and font subsets are cached under `target.cache-dir`, so a rebuild skips anything whose inputs haven't changed. A post removed from `input.src-dir` is warned about but its already-rendered page is left in place; only once that rendered page is also gone does `kiln build` drop its cache entry
- `kiln clean` — remove build output, including the cache (so the next `kiln build` is a full rebuild)
- running `kiln` with no arguments is equivalent to `kiln build`

## Installation & usage

This project is only distributed as a Nix flake — no Hackage package or standalone binary is published.

```sh
# run directly
nix run github:XilongYang/kiln -- init
nix run github:XilongYang/kiln -- build

# or add the flake's overlay / packages.kiln to your own configuration
```

Runtime dependencies (`pandoc`, `fonttools`/`pyftsubset`, `brotli`, `coreutils`'s `b2sum` for build caching) are wrapped into the executable via the flake's `wrapProgram`, so nothing extra needs to be installed.

For development, `nix develop` opens a shell with `cabal-install`, HLS, and the same runtime dependencies.

## Configuration

`kiln build` and `kiln clean` read `kiln-config.json` from the current directory. See `template/default/kiln-config.json` (or `template/flow/kiln-config.json`) for a working example:

```json
{
  "input": {
    "src-dir": "src",
    "template-dir": "template"
  },
  "opt": {
    "webroot": "/",
    "toc": {
      "enable": true,
      "depth": 3
    }
  },
  "target": {
    "searchdb": "searchdb.json",
    "cache-dir": ".cache",
    "post": { "template": "post.html", "output-dir": "post" },
    "pages": [
      { "template": "index.html", "output": "index.html" },
      { "template": "404.html", "output": "404.html" }
    ],
    "fonts": {
      "subset-dir": "fonts-subset",
      "sources": [
        "res/fonts/JetBrainsMono-Regular.ttf",
        "res/fonts/MaterialIcons.woff2",
        "res/fonts/SourceHanSerifCN-Regular.otf"
      ]
    }
  }
}
```

- `input.src-dir` — directory of Markdown source files to render as posts
- `input.template-dir` — directory of HTML component templates used to assemble pages
- `opt.webroot` — the site's root path, used when generating absolute links
- `opt.toc.enable` — whether pandoc generates a table of contents for each post (optional, default `true`)
- `opt.toc.depth` — depth of the table of contents pandoc generates for each post (optional, default `3`)
- `target.searchdb` — output path for the generated search index JSON
- `target.cache-dir` — directory used to track rendered posts', pages', and subset fonts' inputs, so unchanged ones are skipped on the next build; delete it (or run `kiln clean`) to force a full rebuild
- `target.post` — the standalone page rendered for every markdown file in `input.src-dir`, or JSON `null` to skip it entirely (every post is still available to `pages`' `$for(posts)$`, just with no page of its own -- see `flow`'s `kiln-config.json`). When not `null`: `template` is a path under `input.template-dir`, and `output-dir` is the directory each post's `<slug>.html` is written into. This key must always be present -- either an object or explicit `null`, never omitted -- so a config always states its choice rather than relying on an implicit default
- `target.pages` — every non-post page to generate: `template` is a path under `input.template-dir` (rendered through pandoc the same way `post`'s template is), and `output` is where to write it -- also this page's own identity, since two pages can't share an output path anyway (that's already a hard correctness requirement, independent of caching). Every page template gets a `webroot` variable and a `posts` metadata list (newest post first) it can lay out itself with pandoc's own `$for(posts)$`; each post in that list has `title`, `date`, `monthDay`, `url`, `tags` (the post's frontmatter `tags`, verbatim -- empty if it didn't set one), `abstract` (rendered HTML, or null if the post has no `<!--more-->` marker), `content` (the post's full rendered body HTML), `year`, `newYear`, `lastOfYear` (precomputed so a template can open/close a per-year wrapper, like `default`'s `index.html` does, without any stateful looping of its own), and `last` (true only for the very last post overall, e.g. for a flat chronological feed that wants a separator between posts but not a trailing one, like `flow`'s `index.html`)
- `target.fonts` — the local fonts to subset down to the characters actually used on the site, or JSON `null` if the site has none (like `flow`'s `kiln-config.json`). When not `null`: `subset-dir` is the output directory for the subset `.woff2` fonts, and `sources` lists the font files (full paths) to subset into it

## Project layout

- `app/` — executable entry point, dispatches the `init`/`build`/`clean` subcommands
- `src/Kiln/` — core logic (`Init`, `Config`, `Clean`, `Build`, `Str`, and the `Build.*` modules for post rendering, pages, the search database, font subsetting, and the incremental build cache)
- `template/default/`, `template/flow/` — the site templates `kiln init` can scaffold
- `test/` — hspec test suite

## Testing

```sh
cabal test
```

## License

MIT, see [LICENSE](LICENSE).

## Third-party resources

The bundled `default` template ships with the following third-party resources:

- [latex.css](https://latex.css/) — base page styling
- [Prism](https://prismjs.com/) — code block syntax highlighting
- [MathJax](https://www.mathjax.org/) — math typesetting
- [JetBrains Mono](https://www.jetbrains.com/lp/mono/) — monospace font
- [Material Symbols](https://fonts.google.com/icons) — icon font
- [Source Han Serif](https://github.com/adobe-fonts/source-han-serif) — CJK serif font
