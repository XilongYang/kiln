# xilong.site

Source repository for [xilong.site](https://xilong.site), a statically generated personal blog.

This project uses a custom Haskell builder (under `builder/`) to transform markdown posts in `src/` into HTML pages in `post/`, generate `index.html`, build `searchdb.json`, and create a subset Chinese font for production.

## Highlights

- Custom static-site pipeline written in Haskell (`runghc -ibuilder builder/Main.hs`)
- Incremental post rebuilds based on source/target modification time
- Template component expansion (`template/component/*.html`)
- Markdown to HTML rendering via `pandoc`
- Auto-generated TOC for post pages
- Search index generation (`searchdb.json`) from plain-text post content
- Font subsetting via `pyftsubset` to reduce shipped CJK font size

## Repository Layout

```text
.
├─ src/                 # Markdown sources with front matter
├─ post/                # Generated post pages
├─ template/            # HTML templates and components
├─ style/               # CSS assets
├─ scripts/             # Front-end scripts
├─ res/                 # Static resources (fonts, images, third-party libs)
├─ builder/             # Haskell build system + unit tests
├─ index.html           # Generated homepage
└─ searchdb.json        # Generated search database
```

## Build Pipeline (Builder Behavior)

`builder/Main.hs` executes the following steps:

1. Recreate temp workspace (`temp/`) with `withTempDir`.
2. Warn about orphan generated pages in `post/` that no longer have matching `src/*.md`.
3. Render templates by expanding component placeholders like `<!--<navbar>-->`.
4. Parse every markdown file in `src/`:
   - Requires front matter delimited by `---`
   - Reads `title`, `author`, `date`
   - Splits abstract from body at the first `## ` heading
5. Build post pages (incremental):
   - Skip when target `post/*.html` exists and is newer than source `src/*.md`
   - Preprocess markdown (inject abstract block + `[[toc]]` + code fence class rewrite)
   - Render with `pandoc`
   - Inject generated TOC HTML into `[[toc]]`
6. Build `index.html` from all posts, grouped by year and sorted by date.
7. Generate `searchdb.json` from pandoc plain-text output.
8. Build `res/fonts/SourceHanSerifCN-Subset.woff2` from characters used in generated pages.

## Post Source Format

Each post in `src/` must start with front matter:

```md
---
title: Your Post Title
author: Your Name
date: 2026-03-25
---

Abstract paragraph(s)...

## First Section
Main content...
```

Notes:

- The content before the first `## ` heading is treated as abstract.
- The abstract is wrapped in `<div class="abstract">...</div>` by the builder.
- Fenced code blocks like ```` ```haskell ```` are rewritten to include Prism classes.
- Date format is expected to be `YYYY-MM-DD` for index grouping/sorting.

## Requirements

Required tools:

- `make`
- `runghc` (GHC)
- `pandoc` (project historically requires `>= 2.17`)
- `pyftsubset` (from Python `fonttools`)
- `brotli` (needed by some fonttools setups)

If you use Nix, this repo already provides a dev shell with these dependencies:

```bash
nix develop
```

## Usage

Build the site:

```bash
make
```

Run unit tests:

```bash
make test
```

Clean generated artifacts:

```bash
make clean
```

Direct builder commands (without `make`):

```bash
runghc -ibuilder builder/Main.hs
runghc -ibuilder -i. builder/UT/RunTest.hs
```

## Incremental Build Rule

For each post page, rebuild happens when:

- target HTML does not exist, or
- source markdown modification time is newer than target HTML.

`index.html`, `searchdb.json`, and font subset generation run on every build.

## Third-Party Libraries / Acknowledgements

- Latex.css: <https://github.com/vincentdoerig/latex-css>
- Prism: <https://github.com/PrismJS/prism>
- MathJax: <https://github.com/mathjax/MathJax>

## Fonts

- JetBrains Mono: [OFL](./res/fonts/JetBrainsMono-Regular-OFL.txt)
- Material Icons: [License](./res/fonts/MaterialIcons-LICENSE.txt)
- Source Han Serif CN: [OFL](./res/fonts/SourceHanSerifCN-OFL.txt)

## License

- Source code: MIT, see [LICENSE](./LICENSE)
- Articles and written content: CC BY-NC 4.0, see [CONTENT_LICENSE.txt](./CONTENT_LICENSE.txt)
