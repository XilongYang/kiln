# Kiln

A minimal static blog generator, written in Haskell and distributed as a Nix flake. It turns a folder of Markdown posts into an incrementally-built site — post listing, search index, Atom feed, and font subsetting. Every byte of HTML/CSS/JS stays in your own [pandoc](https://pandoc.org/) templates.

The two bundled templates, `default` (a classic per-post blog with search, a table of contents, and an Atom feed) and `flow` (a flat, tag-filterable card feed with no per-post pages at all), look like entirely different sites, but are built by the exact same pipeline.

## Features

- `kiln init [default|flow]` — scaffold one of the bundled templates into the current (empty) directory; `kiln init` with no argument scaffolds `default`
- `kiln build` — render Markdown posts and every configured page with pandoc, generate a search index, and subset any configured fonts down to the characters actually used on the site (`pyftsubset`); posts, pages, and font subsets are cached under `target.cache-dir`, so a rebuild skips anything whose inputs haven't changed. A post removed from `input.src-dir` is warned about but its already-rendered page is left in place; only once that rendered page is also gone does `kiln build` drop its cache entry
- `kiln clean` — remove the files `kiln build` generated, including the cache (so the next `kiln build` is a full rebuild). Only files kiln recorded building are removed, never a configured directory wholesale — so an output directory shared with hand-written files, or set to the project root itself, loses only kiln's own output; a directory left empty afterwards is removed too
- `kiln help` — print usage
- `kiln version` — print the running build's version
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

## Writing templates

A page or post template is a plain [pandoc template](https://pandoc.org/MANUAL.html#templates) — `$if$`/`$for$`/variable interpolation all work exactly as pandoc defines them, and kiln adds nothing of its own on top. Reusable fragments (navbars, a search panel, footers, …) are ordinary [pandoc partials](https://pandoc.org/MANUAL.html#partials): put them under `component/` inside `input.template-dir` and reference them as `${ component/name() }`. Kiln watches the whole `component/` directory as part of a page's or post's build cache, so editing a partial correctly invalidates everything that references it.

Every page template gets a `$webroot$` variable, a `$site-url$` variable (`opt.site-url`, with no trailing slash), and a `posts` metadata list (every post, newest first) to lay out itself with `$for(posts)$`. Each entry has:

- `title`, `date` ("YYYY-MM-DD"), `monthDay`
- `url` — where the post is published, derived from `target.post.output-dir`, or `null` when `target.post` is `null` and the post therefore has no page of its own (so `$if(posts.url)$` is the way to ask)
- `tags` — the post's frontmatter `tags`, verbatim (empty string if unset)
- `abstract` — rendered HTML up to the post's `<!--more-->` marker, or `null` if it has none
- `content` — the post's full rendered body HTML
- `year`, `newYear`, `lastOfYear` — precomputed so a template can open/close a per-year wrapper (like `default`'s `index.html`) without any stateful looping of its own
- `first`, `last` — true only for the newest and the oldest post overall: `last` for a flat feed that wants a separator between posts but not a trailing one (like `flow`'s `index.html`), `first` for a timestamp that means "the site's own", like a feed's `<updated>`

A standalone post page (if `target.post` isn't `null`) additionally gets `$title$`, `$date$`, `$slug$`, `$body$`, and `$toc$` (if `opt.toc.enable`).

### Feeds

A feed is not a special kind of output — it's an ordinary entry in `target.pages` whose template happens to emit XML, laid out from the same `posts` list, cached and cleaned like any other page. `default` ships one as `template/atom.xml`; see it for a complete Atom 1.0 feed in ~30 lines.

The one thing a feed needs that an on-site page doesn't is absolute URLs, because it's read on someone else's domain, where a root-relative `/res/img.png` resolves against the reader. So a page can ask for them:

```json
{ "template": "atom.xml", "output": "atom.xml", "absolute-urls": true }
```

which, for that page only, resolves every URL in its `posts` list against `opt.site-url`: each post's `url`, and the links and media inside its `abstract`/`content` — including the document-relative ones (`../res/img.png`) and the bare fragments pandoc generates for footnotes (`#fn1`), which are resolved against the post's own page. URLs that already carry a scheme are left alone, and so is anything outside an HTML tag, so a post quoting markup in a code block keeps what it quoted.

Two things worth knowing when writing a feed template of your own: wrap titles and bodies in `<![CDATA[...]]>` so no amount of markup in a post can produce XML that doesn't parse, and remember that a post's `date` carries no time, so an Atom timestamp is `$posts.date$T00:00:00Z`.

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
    "site-url": "https://example.com",
    "toc": {
      "enable": true,
      "depth": 3,
      "ordered": true
    },
    "prism": true,
    "math": true
  },
  "target": {
    "cache-dir": ".cache",
    "pages": [
      { "template": "index.html", "output": "index.html" },
      { "template": "404.html", "output": "404.html" },
      { "template": "atom.xml", "output": "atom.xml", "absolute-urls": true }
    ],
    "post": { "template": "post.html", "output-dir": "post" },
    "searchdb": "searchdb.json",
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

Three top-level groups: `input` (where kiln reads its source material from — never written to, never cleaned), `opt` (site-wide behavior), and `target` (everything kiln generates, and everything `kiln clean` removes).

**`input`** — read-only source directories:

- `src-dir` — directory of Markdown source files to render as posts. Read non-recursively: a subdirectory in here holds no posts, and kiln says so rather than passing over it silently
- `template-dir` — directory of HTML templates used to assemble pages (see [Writing templates](#writing-templates))

**`opt`** — site-wide behavior:

Every key here is optional except `webroot`, and every default matches what the bundled `default` template needs — so a site resembling it can leave them all out, and one that doesn't (like `flow`) turns off what it isn't using.

- `webroot` — the site's root path, used when generating absolute links. Whether it's written with a trailing slash makes no difference (`/blog` and `/blog/` behave the same)
- `site-url` — the site's own origin (`https://example.com`), with or without a trailing slash. Needed only by a page that asked for `absolute-urls` — i.e. in practice by a feed, the one output read on a domain that isn't yours; a site generating none can leave it out (default empty, which leaves every URL as relative as it is now)
- `toc.enable` — whether pandoc generates a table of contents for each post (default `true`)
- `toc.depth` — depth of that table of contents (default `3`)
- `toc.ordered` — whether that table of contents is rewritten into an ordered (`<ol>`) list; pandoc's `--toc` only ever emits `<ul>`, and [latex.css](https://latex.css/) numbers a nested toc from CSS counters keyed on `nav ol`, which never match a `<ul>` (default `true`)
- `prism` — whether a fenced code block's language mark is rewritten into the pandoc attributes [Prism](https://prismjs.com/)'s line-numbers and match-braces plugins key off of (default `true`). A fence that already carries its own pandoc attributes, and any fence quoted inside a longer one, is left exactly as written either way
- `math` — whether post pages are rendered with pandoc's `--mathjax` (default `true`)

**`target`** — everything generated, every key either a value or an explicit JSON `null` (never silently defaulted, so a config always states its choice):

- `cache-dir` — directory tracking rendered posts', pages', and subset fonts' inputs, so unchanged ones are skipped on the next build; delete it (or run `kiln clean`) to force a full rebuild. The scratch space one build needs also lives in here, so it falls inside the boundary this config declares. kiln's own version is part of what's tracked, so upgrading kiln invalidates what the old one rendered
- `pages` — every non-post page to generate: `template` is a path under `input.template-dir`, and `output` is where to write it — also this page's own identity, since two pages can't share an output path without one silently overwriting the other. An optional `absolute-urls` (default `false`) hands that page every URL resolved against `opt.site-url` instead of the webroot — see [Feeds](#feeds)
- `post` — the standalone page rendered for every markdown file in `input.src-dir`, or `null` to skip it entirely (every post is still available to `pages`' `$for(posts)$`, just with no page of its own, and with a `null` `url` — like `flow`'s). When not `null`: `template` is a path under `input.template-dir`, and `output-dir` is the directory each post's `<slug>.html` is written into, and also the directory its `url` points into
- `searchdb` — output path for the generated search index JSON, or `null` if the site has no search feature to feed (like `flow`'s)
- `fonts` — the local fonts to subset down to the characters actually used on the site, or `null` if the site has none (like `flow`'s). When not `null`: `subset-dir` is the output directory for the subset `.woff2` fonts, and `sources` lists the font files (full paths) to subset into it

## Project layout

- `app/` — executable entry point, dispatches the `init`/`build`/`clean`/`help`/`version` subcommands
- `src/Kiln/` — core logic (`Init`, `Config`, `Clean`, `Build`, `Str`, and the `Build.*` modules: `Env` for one build's resolved settings and derived paths, `Markdown` for the pure transforms of a post's source and of pandoc's rendered output, plus post rendering, pages, the search database, font subsetting, and the incremental build cache)
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
