function loadFont() {
    const candidates = [
        '/res/fonts/cn-subset.woff2',
        '/res/fonts/SourceHanSerifCN-Regular.otf'
    ]

    const loadWithUrl = (url) => {
        const font = new FontFace('Kiln CJK Subset', `url(${url})`)
        font.load().then(f => {
            document.fonts.add(f)
            const fontFamily = "'Latin Modern', $TARGET$, Georgia, Cambria, 'Times New Roman', Times, serif"
            document.body.style.fontFamily = fontFamily.replace("$TARGET$", "'Kiln CJK Subset'")
        })
    }

    const tryLoad = (idx) => {
        if (idx >= candidates.length) {
            return
        }

        const url = candidates[idx]
        fetch(url, { method: 'HEAD' })
            .then(resp => {
                if (resp.ok) {
                    loadWithUrl(url)
                } else {
                    tryLoad(idx + 1)
                }
            })
            .catch(() => tryLoad(idx + 1))
    }

    tryLoad(0)
}

export {loadFont}
