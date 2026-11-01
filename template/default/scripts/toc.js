function switchTocMode() {
    // Same split as in darkmode.js: id and label on the button, ligature on the
    // glyph element inside it.
    var button = document.getElementById('toc-control')
    var icon = document.getElementById('toc-control-icon')
    var toc_items = document.getElementById('toc-items')
    var isCollapsed = icon.innerText == 'add'
    icon.innerText = isCollapsed ? 'remove' : 'add'
    button.setAttribute('aria-expanded', isCollapsed ? 'true' : 'false')
    toc_items.style.display = isCollapsed ? "inherit" : "none"
}

export {switchTocMode}
