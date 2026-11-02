# Kiln

A minimal static blog generator, written in Haskell and distributed as a Nix flake. It turns a folder of Markdown posts into an incrementally-built site — post listing, search index, Atom feed, and font subsetting. Every byte of HTML/CSS/JS stays in your own [pandoc](https://pandoc.org/) templates.

Two templates ship with it, built by the exact same pipeline: `default`, a classic per-post blog with a table of contents, an Atom feed, and search over boolean expressions (`a & (b | ~c)`); and `flow`, a flat, tag-filterable card feed with no per-post pages at all, documented in [its own README](template/flow/README.md).

## Installation

Only distributed as a Nix flake — no Hackage package or standalone binary is published.

```sh
nix run github:XilongYang/kiln -- init
nix run github:XilongYang/kiln -- build
```

Or add the flake's overlay / `packages.kiln` to your own configuration. Runtime dependencies (`pandoc`, `fonttools`/`pyftsubset`, `brotli`) are wrapped into the executable, so nothing extra needs installing. For development, `nix develop` opens a shell with `cabal-install`, HLS, and the same dependencies.

## Commands

- `kiln init [default|flow]` — scaffold a bundled template into the current (empty) directory; no argument means `default`
- `kiln build` — render posts and pages with pandoc, write the search index, and subset fonts down to the characters the site actually uses. Everything is cached under `target.cache-dir`, so a rebuild skips whatever inputs haven't changed. Running `kiln` with no arguments does this
- `kiln clean` — remove what `kiln build` generated, cache included, so the next build is a full one. Only files kiln recorded building are removed, never a configured directory wholesale — an output directory shared with hand-written files, or set to the project root, loses only kiln's own output
- `kiln help`, `kiln version`

## Writing templates

A template is a plain [pandoc template](https://pandoc.org/MANUAL.html#templates) — `$if$`/`$for$`/interpolation work exactly as pandoc defines them, and kiln adds nothing of its own on top. Shared fragments (navbar, search panel, footer, …) are ordinary [pandoc partials](https://pandoc.org/MANUAL.html#partials): put them under `component/` inside `input.template-dir` and call them as `${ component/name() }`. Editing any partial invalidates every page and post.

Every page in `target.pages` gets `$webroot$`, `$site-url$` (no trailing slash), and a `posts` list (every post, newest first) to lay itself out with `$for(posts)$`. Each entry has:

- `title`, `date` ("YYYY-MM-DD"), `monthDay`, `year`
- `url` — where the post is published, or `null` when `target.post` is `null` and it has no page of its own (so `$if(posts.url)$` is the way to ask)
- `tags` — the post's frontmatter `tags`, comma-joined: `tags: long,image` and `tags: [long, image]` both come out as `long,image` (empty if unset)
- `abstract` — rendered HTML up to the post's `<!--more-->` marker, `null` if it has none
- `content` — the post's rendered body: everything after `<!--more-->` when it has one, so `abstract` + `content` is the whole post
- `newYear`, `lastOfYear`, `first`, `last` — position flags, so a template can open and close a per-year wrapper, or skip a trailing separator, without stateful looping of its own

A post page (`target.post`) is rendered from one post's own markdown rather than from that list, so it gets a different set: `$webroot$`, `$slug$`, `$body$`, `$abstract$`, `$toc$` (if `opt.toc.enable`), plus whatever the post's frontmatter declares, straight from pandoc's metadata — `$title$`, `$date$`, `$author$`, and anything else it sets. No `$site-url$` and no `posts`: a post page is read on the site itself, and it is one post.

### Feeds

A feed is not a special kind of output — it's an ordinary entry in `target.pages` whose template happens to emit XML, laid out from the same `posts` list and cached and cleaned like any other page. The one thing it needs that an on-site page doesn't is absolute URLs, because it's read on someone else's domain:

```json
{ "template": "atom.xml", "output": "atom.xml", "absolute-urls": true }
```

For that page only, every URL in its `posts` list is resolved against `opt.site-url`: each post's `url`, and the `href`, `src` and `poster` inside its `abstract`/`content` — including document-relative ones (`../res/img.png`) and the bare fragments pandoc generates for footnotes (`#fn1`), resolved against the post's own page. A URL that already carries a scheme, a protocol-relative `//cdn/…`, a hand-written `srcset`, and anything outside an HTML tag are all left exactly as written.

`template/default/template/atom.xml` is a complete Atom 1.0 feed to copy from. Three things it has to get right: titles and bodies go in `<![CDATA[...]]>`, so no amount of markup in a post can produce XML that doesn't parse; a post's `date` carries no time, so a timestamp is `$posts.date$T00:00:00Z`; and if posts use `<!--more-->`, an entry needs both `$posts.abstract$` and `$posts.content$`, since a feed carries the whole article.

## Configuration

`kiln build` and `kiln clean` read `kiln-config.json` from the current directory. Each bundled template ships one as a working example:

```json
{
  "input": {
    "src-dir": "src",
    "template-dir": "template"
  },
  "opt": {
    "webroot": "/",
    "site-url": "https://example.com",
    "toc": { "enable": true, "depth": 3, "ordered": true },
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
      "sources": ["res/fonts/JetBrainsMono-Regular.ttf"]
    }
  }
}
```

Three groups: `input` is only ever read from, `opt` is site-wide behavior, and `target` is everything kiln generates — and everything `kiln clean` removes.

**`input`**

- `src-dir` — the Markdown posts, read non-recursively: a subdirectory in here holds no posts, and kiln says so rather than passing over it silently
- `template-dir` — the templates, and their `component/` partials

**`opt`** — every key optional except `webroot`, each default matching what `default` needs, so a site resembling it can leave them all out and one that doesn't (like `flow`) turns off what it isn't using:

- `webroot` — the site's root path, used when generating absolute links; a trailing slash makes no difference
- `site-url` — the site's own origin, needed only by a page that asked for `absolute-urls`, i.e. in practice by a feed (default empty, leaving every URL as relative as it already is)
- `toc.enable`, `toc.depth`, `toc.ordered` — pandoc's table of contents per post (defaults `true`, `3`, `true`). `ordered` rewrites it into an `<ol>`, which is what [latex.css](https://latex.css/)'s counters number; pandoc's own `--toc` only ever emits `<ul>`
- `prism` — rewrite a fence's language mark into the pandoc attributes [Prism](https://prismjs.com/)'s line-numbers and match-braces plugins key off of (default `true`). A fence that already carries its own attributes, or that is quoted inside a longer one, is left as written either way
- `math` — render post pages with pandoc's `--mathjax` (default `true`)

**`target`** — every key either a value or an explicit `null`, never silently defaulted, so a config always states its choice:

- `cache-dir` — tracks what each rendered post, page and subset font was built from, kiln's own version included, so an upgrade invalidates everything the old one rendered. A build's scratch space lives here too; delete the directory, or run `kiln clean`, to force a full rebuild
- `pages` — the non-post pages: `template` is a path under `input.template-dir`, `output` is where it's written and also the page's identity, since two pages sharing one output path would silently overwrite each other. Optional `absolute-urls` (default `false`) — see [Feeds](#feeds)
- `post` — the page rendered for every file in `src-dir`: `template`, plus the `output-dir` each post's `<slug>.html` is written into and its `url` points at. `null` skips post pages entirely — every post still reaches `pages` through `posts`, just with a `null` `url`, like `flow`'s
- `searchdb` — output path for the search index JSON, or `null` if the site has no search
- `fonts` — the local fonts to subset down to the characters the site uses: `subset-dir` for the resulting `.woff2`, `sources` for the font files to subset. `null` if the site has none

## Project layout

- `app/` — the executable's entry point, dispatching the subcommands
- `src/Kiln/` — `Config`, `Init`, `Clean`, `FileTree`, `Str`, and `Build` with its `Build.*` modules (`Env`, `Markdown`, `PostEntry`, `Post`, `Page`, `SearchDb`, `FontSubset`, `Cache`)
- `template/default/`, `template/flow/` — the templates `kiln init` scaffolds
- `test/` — hspec suite, run with `cabal test`

## License

MIT, see [LICENSE](LICENSE). The `default` template bundles [latex.css](https://latex.css/), [Prism](https://prismjs.com/), [MathJax](https://www.mathjax.org/), [JetBrains Mono](https://www.jetbrains.com/lp/mono/), [Material Symbols](https://fonts.google.com/icons) and [Source Han Serif](https://github.com/adobe-fonts/source-han-serif), each under its own license, shipped alongside it.
