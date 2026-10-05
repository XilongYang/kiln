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
        input.style.display = "none"
        input.innerHTML = ""
        refreshFilter()
    } else {
        filterIcon.classList.add("icon-active")
        filterIcon.classList.remove("icon")
        input.style.display = "inline-block"
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
    var errs = input.getElementsByClassName('error')
    for (var err of errs) {
        var msgs = err.getElementsByClassName('error-msg')
        for (var msg of msgs) {
            err.removeChild(msg)
        }
    }
    var exp = input.innerText.replace("\n", "")
    input.innerHTML = exp
}

export {toggleInput, inputFilter, refreshFilter, clearErrorMsg}
