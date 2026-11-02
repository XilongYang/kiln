import {expressionValidate, expressionParse, expressionMatch} from './express-matcher.js'

// ============= Private Method =================


// What the user typed. The error tooltip is a child of the input, hidden by CSS
// alone, so its text is skipped here rather than read back as part of the
// expression -- refreshFilter runs once more while the error is still shown. A
// contenteditable also keeps a NBSP where a plain space would not survive as
// markup, so fold those back.
function inputText(input) {
    var text = ""
    for (var node of input.childNodes) {
        if (node.nodeType != Node.ELEMENT_NODE) {
            text += node.textContent
        } else if (!node.classList.contains('error-msg')) {
            text += inputText(node)
        }
    }
    return text.replace(/\n/g, "").replace(/\u00a0/g, " ")
}

// Built as nodes rather than as a string: the expression is shown back to the
// user inside the input it came from, so anything that looks like a tag has to
// stay text. "C++ & vector<int> &" lost its <int> when this was innerHTML.
function showErrorMsg(msg, exp, index) {
    var errorMsg = document.createElement('span')
    errorMsg.className = 'error-msg'
    errorMsg.textContent = "Error: " + msg

    var error = document.createElement('span')
    error.className = 'error'
    // A bare space would collapse away, leaving nothing to highlight.
    error.textContent = exp[index] == ' ' ? '\u00a0' : exp[index]
    error.appendChild(errorMsg)

    var invalid = document.createElement('span')
    invalid.className = 'invalid-expression'
    invalid.textContent = exp.slice(index + 1)

    var input = document.getElementById("post-filter-input")
    input.textContent = exp.slice(0, index)
    input.appendChild(error)
    input.appendChild(invalid)
}

function moveCursorToEnd(target) {
    const range = document.createRange();
    const selection = window.getSelection();

    range.selectNodeContents(target);
    range.collapse(false);
    selection.removeAllRanges();
    selection.addRange(range);
}

// ==============================================

function toggleInput() {
    var filterIcon = document.getElementById("post-filter")
    var input = document.getElementById("post-filter-input")
    if(filterIcon.classList.contains("icon-active")) {
        filterIcon.classList.add("icon")
        filterIcon.classList.remove("icon-active")
        filterIcon.setAttribute("aria-expanded", "false")
        input.style.display = "none"
        input.innerHTML = ""
        refreshFilter()
    } else {
        filterIcon.classList.add("icon-active")
        filterIcon.classList.remove("icon")
        filterIcon.setAttribute("aria-expanded", "true")
        input.style.display = "inline-block"
        // Reached by Enter or Space now, so hand over the caret instead of
        // leaving focus on the button.
        input.focus()
    }
}

function inputFilter(e) {
    if (e.type == "keypress" && e.key == "Enter") {
        e.preventDefault();
        moveCursorToEnd(e.target)
        e.target.blur()
        refreshFilter()
    }

    if (e.type == "input") {
        const input = e.target
        if (input.innerText.includes('\n')) {
            input.innerText = input.innerText.replace(/\n/g, '');
            moveCursorToEnd(input)
        }
    }
}

function refreshFilter() {
    var input = document.getElementById("post-filter-input")
    var exp = inputText(input).trim()
    if (exp == "") {
        input.innerHTML = ""
        var postYears = document.getElementsByClassName('post-year-wrapper')
        for (var postYear of postYears) {
            var posts = postYear.getElementsByClassName('post-wrapper')
            for (var post of posts) {
                post.style.display = 'block'
            }
            postYear.style.display = 'block'
        }
        return
    }

    var validateResult = expressionValidate(exp)
    if (validateResult != "OK") {
        showErrorMsg(validateResult.msg, validateResult.exp, validateResult.index)
        return
    }

    var matchTree = expressionParse(exp)

    var postYears = document.getElementsByClassName('post-year-wrapper')
    for (var postYear of postYears) {
        var atLeastOne = false
        var posts = postYear.getElementsByClassName('post-wrapper')
        for (var post of posts) {
            var isSuccess = expressionMatch(matchTree, post.innerText)
            atLeastOne = atLeastOne || isSuccess
            post.style.display = isSuccess ? 'block' : 'none'
        }
        postYear.style.display = atLeastOne ? 'block' : 'none'
    }

}

function clearErrorMsg() {
    var input = document.getElementById("post-filter-input")
    // Nothing to strip: leave the DOM alone so the caret keeps its position.
    if (input.getElementsByClassName('error').length == 0) {
        return
    }
    for (var msg of Array.from(input.getElementsByClassName('error-msg'))) {
        msg.remove()
    }
    // Assigned as text, so the expression comes back exactly as it was typed.
    input.textContent = inputText(input)
    moveCursorToEnd(input)
}

export {toggleInput, inputFilter, refreshFilter, clearErrorMsg}
