import {expressionValidate, expressionParse, expressionMatch} from './express-matcher.js'

// ============= Private Method =================


function showErrorMsg(msg, exp, index) {
    var errorMsg = "Error: " + msg
    var errorSymbol = exp[index] == ' ' ? '&nbsp;' : exp[index]
    var htmlMsg = exp.slice(0, index)
                + "<span class='error'>" + errorSymbol
                + "<span class='error-msg'>" + errorMsg + "</span>"
                + "</span>"
                + "<span class = 'invalid-expression'>" + exp.slice(index + 1) + "</span>"
    document.getElementById("post-filter-input").innerHTML = htmlMsg
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
    var exp = input.innerText.replace("\n", "").trim()
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
    input.innerHTML = input.innerText.replace(/\n/g, "")
    moveCursorToEnd(input)
}

export {toggleInput, inputFilter, refreshFilter, clearErrorMsg}
