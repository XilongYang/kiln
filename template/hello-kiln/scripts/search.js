var isLoading = true
var db = []

function keyboardCatch(event) {
    var box = document.getElementById('search-box')
    var button = document.getElementById('search-close')
    if (event.key == 'Tab') {
        event.preventDefault();
        if (document.activeElement === box) {
            button.focus();
        } else {
            box.focus();
        }
    }
    if (event.key == 'Escape') {
        closePanel()
    }
}

function loadDb(callback) {
    fetch('/searchdb.json')
        .then(response => {
            if (!response.ok) {
                throw new Error('Network response was not ok');
            }
            return response.json()
        })
        .then (posts => {
            db = posts
        })
        .then (callback)
        .catch(error => {
            console.error('There was a problem with the fetch operation:', error);
        });
}

function openPanel() {
    var scrollBarWidth = window.innerWidth - document.documentElement.clientWidth
    document.documentElement.style.paddingRight = scrollBarWidth + 'px'

    document.getElementById("search-box").value = ""
    document.body.style.overflow = "hidden"
    var panel = document.getElementById("search-panel")
    panel.style.display = "flex"
    document.getElementById("search-box").focus()
    panel.addEventListener('keydown', keyboardCatch)

    if (isLoading) {
        loadDb(()=> {
            isLoading = false
            document.getElementById("search-loading").style.display = "none"
        })
    }
}

function closePanel() {
    document.documentElement.style.paddingRight = ''

    document.body.style.overflow = "auto"
    var panel = document.getElementById("search-panel")
    panel.style.display = "none"
    panel.removeEventListener('keydown', keyboardCatch)
    document.getElementById("search-box").value = ""
    document.getElementById("search-result").innerHTML = ""
}

function show(title, content) {
    var titleDiv = document.createElement('div')
    titleDiv.classList = "search-result-item-title"
    titleDiv.innerHTML = title

    var contentDiv = document.createElement('div')
    contentDiv.classList = "search-result-item-content"
    contentDiv.innerHTML = content + '<br>---'

    var result = document.createElement('div')
    result.classList = "search-result-item"
    result.appendChild(titleDiv)
    result.appendChild(contentDiv)

    document.getElementById("search-result").appendChild(result)
}

function search() {
    document.getElementById("search-result").innerHTML = ''
    var text = document.getElementById("search-box").value
    if (text == '') {
        return
    }
    var posts = db
    for (var i = 0; i < posts.length; i++) {
        var title = posts[i].title
        var url = posts[i].url
        var content = posts[i].content
        var pos = content.indexOf(text)
        if (pos != -1) {
            title = '<a href="' + url + '">' + title + '</a>'
            if (pos > 10) {
                content = '...' + content.slice(pos - 10)
            }
            if (content.length > 100) {
                content = content.slice(0, 100) + '...'
            }
            content = content.replaceAll(text, '<span class="search-key">'+text+'</span>')
            show(title, content)
        }
    }
}

export {closePanel, openPanel, search}
