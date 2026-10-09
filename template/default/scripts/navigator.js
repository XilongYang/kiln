function backTop() {
    window.scrollTo({
        top: 0,
        behavior: 'smooth'
    })
}

function goBottom() {
    window.scrollTo({
        top: document.body.scrollHeight,
        behavior: 'smooth'
    })
}

export {backTop, goBottom}
