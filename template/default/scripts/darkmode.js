function refreshDarkMode() {
    var classList = document.body.classList
    var icon = document.getElementById('darkmode')
    var codeStyle = document.getElementById('code-style')
    var imgInvert = document.getElementById('img-invert')
    if (sessionStorage.getItem("darkmode") == "true") {
        classList.add("latex-dark")
        icon.innerText = 'light_mode'
        codeStyle.href = '/res/latex-css-1.13.0/prism/prism-dracula.css'
        imgInvert.href = '/style/img-dark.css'
    } else {
        classList.remove("latex-dark")
        icon.innerText = 'dark_mode'
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
