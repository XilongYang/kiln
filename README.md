# xilongyang.site

This repo is the source code of my personal site (xilong.site).

## Work flow

### 1. Edit Source Files

To create new posts, just put `.md` files in the `src` folder.

The metadata is surrounded by '---' and it has a structure such as `"key":value` per line.

The content between the metadata and the first level 2 title (`## title`) will be considered as an abstract. An auto-generated TOC will be placed after the abstract.

### 2. Generate Posts

Dependencies: `python3`, `python3-fonttools`, `pandoc (version >= 2.17)`

Run `make` to build posts.

Run `make clean` to remove all posts already built.

Posts are sorted by their creation date and grouped by their creation year.

## Third-Party Libraries / Acknowledgements

This project uses the following open source projects:

- Latex.css ([https://github.com/vincentdoerig/latex-css](https://github.com/vincentdoerig/latex-css))
- Prism([https://github.com/PrismJS/prism/](https://github.com/PrismJS/prism/))
- MathJax([https://github.com/mathjax/MathJax](https://github.com/mathjax/MathJax))

## Fonts Used

- JetBrains Mono: [LICENSE](./res/fonts/JetBrainsMono-Regular-OFL.txt)
- Material Icons: [LICENSE](./res/fonts/MaterialIcons-LICENSE.txt)
- Source Han Serif CN: [LICENSE](./res/fonts/SourceHanSerifCN-OFL.txt)

## License

- **Source code**: MIT License. See [LICENSE](./LICENSE).
- **Articles & other written content**: Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0). See [CONTENT_LICENSE](./CONTENT_LICENSE.txt).

