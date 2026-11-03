import {expressionMatch} from './express-matcher.js'
import {bindExpressionInput} from './express-input.js'

var isLoading = true
var db = []

// Set once the search box is bound; lets closePanel and the db loader drive the
// box outside of its own events.
var searchInput = {commit: () => {}, refresh: () => {}}

// Characters of context kept on each side of a hit, and the cap on a snippet
// that has no hit to center on.
const SNIPPET_CONTEXT = 10
const SNIPPET_LIMIT = 100

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

    document.getElementById("search-box").textContent = ""
    document.body.style.overflow = "hidden"
    var panel = document.getElementById("search-panel")
    panel.style.display = "flex"
    document.getElementById("search-box").focus()
    panel.addEventListener('keydown', keyboardCatch)

    if (isLoading) {
        loadDb(()=> {
            isLoading = false
            // Typing before the db arrives clears the result area, spinner included.
            var loading = document.getElementById("search-loading")
            if (loading != null) {
                loading.style.display = "none"
            }
            // An expression committed while the db was still empty matched
            // nothing; list it again now that there is something to match. A
            // refresh rather than a commit, so a db that lands mid-typing does
            // not report an error over a half-typed expression.
            searchInput.refresh()
        })
    }
}

function closePanel() {
    document.documentElement.style.paddingRight = ''

    document.body.style.overflow = "auto"
    var panel = document.getElementById("search-panel")
    panel.style.display = "none"
    panel.removeEventListener('keydown', keyboardCatch)
    document.getElementById("search-box").textContent = ""
    // Commit the emptied box rather than clearing the results directly: it also
    // forgets the expression, so reopening and retyping the same one still runs.
    searchInput.commit()

    // The panel took focus on open; hand it back so a keyboard user resumes at
    // the icon instead of at the top of the document.
    var button = document.getElementById("search-button")
    if (button != null) {
        button.focus()
    }
}

function escapeHtml(text) {
    var div = document.createElement('div')
    div.innerText = text
    return div.innerHTML
}

function show(post, content) {
    var link = document.createElement('a')
    link.href = post.url
    link.innerText = post.title

    var titleDiv = document.createElement('div')
    titleDiv.classList = "search-result-item-title"
    titleDiv.appendChild(link)

    var contentDiv = document.createElement('div')
    contentDiv.classList = "search-result-item-content"
    contentDiv.innerHTML = content + '<br>---'

    var result = document.createElement('div')
    result.classList = "search-result-item"
    result.appendChild(titleDiv)
    result.appendChild(contentDiv)

    document.getElementById("search-result").appendChild(result)
}

// A snippet is one line, so a line break and the indentation hugging it collapse
// into a single space. Done when a piece is emitted, never on the content being
// measured, so offsets stay valid.
function escapeText(text) {
    return escapeHtml(text.replace(/\s*\n\s*/g, ' '))
}

// Every occurrence of every term, as [start, end) offsets sorted by start.
function termRanges(content, terms) {
    var ranges = []
    for (var term of terms) {
        for (var pos = content.indexOf(term); pos != -1; pos = content.indexOf(term, pos + term.length)) {
            ranges.push([pos, pos + term.length])
        }
    }
    return ranges.sort((a, b) => a[0] - b[0])
}

// Gives each hit SNIPPET_CONTEXT characters of room on both sides. Windows that
// reach each other merge, so the text between two near hits is shown as it reads
// rather than cut in half by an ellipsis.
function hitWindows(content, ranges) {
    var windows = []
    for (var [start, end] of ranges) {
        var from = Math.max(start - SNIPPET_CONTEXT, 0)
        var to   = Math.min(end + SNIPPET_CONTEXT, content.length)
        var last = windows[windows.length - 1]
        if (last != undefined && from <= last[1]) {
            last[1] = Math.max(last[1], to)
        } else {
            windows.push([from, to])
        }
    }
    return windows
}

// Terms are marked by offset rather than by replace, so a hit inside the markup
// of an earlier hit cannot be rewritten.
function renderWindow(content, ranges, from, to) {
    var html = ""
    var done = from
    for (var [start, end] of ranges) {
        if (end <= done || start >= to) {
            continue
        }
        // Overlapping terms: emit only the part not covered yet.
        start = Math.max(start, done)
        html += escapeText(content.slice(done, start))
             + '<span class="search-key">' + escapeText(content.slice(start, end)) + '</span>'
        done = end
    }
    return html + escapeText(content.slice(done, to))
}

function snippet(content, terms) {
    var ranges = termRanges(content, terms)
    // A match carried entirely by negated terms has no hit to center on.
    if (ranges.length == 0) {
        return escapeText(content.slice(0, SNIPPET_LIMIT))
             + (content.length > SNIPPET_LIMIT ? '...' : '')
    }

    var windows = hitWindows(content, ranges)
    var parts = windows.map(([from, to]) => renderWindow(content, ranges, from, to))

    // The same ellipsis stands for text skipped between windows and for text cut
    // off at either end.
    var head = windows[0][0] > 0 ? '...' : ''
    var tail = windows[windows.length - 1][1] < content.length ? '...' : ''
    return head + parts.join('...') + tail
}

// A null tree means an empty box: there is nothing to list.
function showResults(matchTree) {
    document.getElementById("search-result").innerHTML = ''
    if (matchTree == null) {
        return
    }
    for (var post of db) {
        var hits = []
        if (expressionMatch(matchTree, post.content, hits)) {
            show(post, snippet(post.content, [...new Set(hits)]))
        }
    }
}

function bindSearchInput(box) {
    searchInput = bindExpressionInput(box, showResults)
}

export {closePanel, openPanel, bindSearchInput}
