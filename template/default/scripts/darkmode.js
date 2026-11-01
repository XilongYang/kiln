function refreshDarkMode() {
    var classList = document.body.classList
    // The button carries the id and the label; the glyph it wraps is a separate
    // element, so writing the ligature name must not go through the button.
    var button = document.getElementById('darkmode')
    var icon = document.getElementById('darkmode-icon')
    var codeStyle = document.getElementById('code-style')
    var imgInvert = document.getElementById('img-invert')
    if (sessionStorage.getItem("darkmode") == "true") {
        classList.add("latex-dark")
        icon.innerText = 'light_mode'
        button.setAttribute('aria-pressed', 'true')
        codeStyle.href = '/res/latex-css-1.13.0/prism/prism-dracula.css'
        imgInvert.href = '/style/img-dark.css'
    } else {
        classList.remove("latex-dark")
        icon.innerText = 'dark_mode'
        button.setAttribute('aria-pressed', 'false')
        codeStyle.href = '/res/latex-css-1.13.0/prism/prism-one-light.css'
        imgInvert.href = ''
    }
}

function switchDarkMode() {
    if (sessionStorage.getItem("darkmode") == "true") {
        sessionStorage.setItem("darkmode", "false")
    } else {
        sessionStorage.setItem("darkmode", "true")
    }
    refreshDarkMode()
}

export {switchDarkMode, refreshDarkMode}
