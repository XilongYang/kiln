---
title: Markdown Formatting Showcase
author: Kiln Example
date: 2026-02-10
---

This post is a quick tour of the markdown building blocks a Kiln post can use: nested headings, lists, a table, a blockquote, and an image. It exists only to show how each element renders through the default template.

## Headings and the Table of Contents

Every heading on this page becomes an entry in the collapsible "Contents" panel above, nested by level. Kiln inserts that navigation block automatically — a post never has to build its own table of contents.

### A Nested Subsection

Subsections like this one show up indented under their parent heading in the table of contents.

## Text and Lists

Plain markdown emphasis works as expected: *italic*, **bold**, and `inline code`.

Unordered list:

- First point
- Second point
  - A nested point
  - Another nested point
- Third point

Ordered list:

1. Parse the markdown front matter
2. Render the body through Pandoc
3. Inject the table of contents
4. Write the final HTML page

## A Blockquote

> Static sites stay simple when the build has as few moving parts as possible.

## A Table

| Feature       | Powered by     |
|---------------|-----------------|
| Code blocks   | Prism            |
| Math          | MathJax          |
| Search        | `searchdb.klb`   |
| CJK text      | subsetted woff2  |

## An Image

The diagram below is a plain `.svg` file stored next to this post's assets. In dark mode it is colour-inverted automatically by `style/img-dark.css`, so it stays readable either way.

![build pipeline](../res/post-imgs/Markdown_Formatting_Showcase/pipeline.svg)

## Links

Kiln itself has no opinions about outbound links — this is a plain [reference link](https://pandoc.org) to Pandoc, the tool that turns this file into HTML.
