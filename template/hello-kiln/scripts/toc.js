function switchTocMode() {
    var icon = document.getElementById('toc-control')
    var toc_items = document.getElementById('toc-items')
    if (icon.innerText == 'add') {
        icon.innerText = 'remove'
        toc_items.style.display = "inherit"
    } else {
        icon.innerText = 'add'
        toc_items.style.display = "none"
    }
}

export {switchTocMode}
