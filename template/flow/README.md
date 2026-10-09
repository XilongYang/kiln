# Flow

A "card flow" example site built with [Kiln](https://github.com/xilongyang/kiln): a flat, reverse-chronological feed of short dated entries on `index.html`, each optionally carrying images/videos, with a lightbox, an archive side-nav, and long entries collapsed by default.

Unlike the `default` scaffold's blog layout, a flow post has no standalone "title" and no permalink page of its own — every entry only ever lives inline in the index's card flow. `kiln-config.json` sets `"post": null` to say so explicitly, so `kiln build` never renders a standalone page per entry in the first place (contrast `default`, which sets `"post": {"template": "post.html", "output": "post"}`).

## Directory layout

```text
.
├─ kiln-config.json
├─ src/                        # one markdown file per entry; its `date` frontmatter controls sort order
├─ template/
│  └─ index.html                # the card-flow feed, laid out via pandoc's $for(posts)$
├─ style/base.css               # feed layout, card/media/grid styles
├─ style/archive.css            # side-nav month index
├─ style/lightbox.css           # media lightbox
└─ scripts/                     # feed behavior: collapse, tag filter, archive nav, lightbox, footer year
```

## Build

```sh
kiln build
```

See the main [kiln README](../../README.md) for `kiln-config.json`'s general shape; this template's only notable choices are `"post": null` (no standalone page per entry, see above), `toc.enable: false` (flow entries have no table of contents), and an empty `fonts` map (no custom fonts to subset -- `style/base.css` just uses a system CJK font stack).

## Writing an entry

Each `src/*.md` file needs only a `date` in its frontmatter. `tags` is optional -- a plain comma-separated string, passed straight through to `data-tags` on the rendered `.post` so `scripts/post-tags.js` can filter by it (see `src/2024-01-05-00.md` for `tags: long`, or `src/2024-02-18-00.md` for `tags: short,image`):

```markdown
---
date: 2024-03-02
tags: short
---

今天天气不错。
```

## Images and videos

```html
<img class="post-image" src="https://your-cdn.example/images/example.jpg" alt="" loading="lazy" decoding="async">
<video class="post-video" src="https://your-cdn.example/videos/example.mp4" controls preload="none"></video>
```

- Images: `loading="lazy"` + `decoding="async"` to reduce first-paint cost.
- Videos: `preload="none"` -- no video data is fetched until playback starts.
- Leave `src=""` as a placeholder before media is ready; `style/base.css` hides an empty-`src` image/video automatically (see `src/2024-02-18-00.md` for an example).

### 4-up / 9-up grids

```html
<div class="post-media-grid grid-4">

<img src="https://your-cdn.example/images/1.jpg" alt="" loading="lazy" decoding="async">

<img src="https://your-cdn.example/images/2.jpg" alt="" loading="lazy" decoding="async">

<video src="https://your-cdn.example/videos/1.mp4" controls preload="none"></video>

<video src="https://your-cdn.example/videos/2.mp4" controls preload="none"></video>

</div>
```

- `grid-4`: 2 columns. `grid-9`: 3 columns (collapses to 2 on narrow screens).
- Images and videos can be mixed freely within one grid.
- **Each grid item needs a blank line before and after it** (see `src/2024-01-27-00.md`). Pandoc's markdown reader wraps a bare inline tag like `<img>`/`<video>` in its own `<p>` rather than passing it through completely unchanged -- unlike the original hand-authored static HTML this template is based on, which had no markdown layer in between. `style/base.css`'s grid rules already account for that extra `<p>` (see the `.post-media-grid > p > img` rules), so visually nothing changes; just don't put multiple grid items on consecutive lines with no blank line between them, or they'll be merged into a single paragraph instead of separate grid cells.

## License

MIT, see the [kiln project's own LICENSE](../../LICENSE) -- this template ships as part of kiln, under the same license.
