import {expressionValidate, expressionParse} from './express-matcher.js'

// A contenteditable box holding a search expression: reading back what was
// typed, marking a bad character in place, and committing on Enter or on blur.
// The post filter and the search panel each drive one, so the
// read -> validate -> report -> parse cycle lives here and a caller only says
// what to do with the tree that comes out.

// What the user typed. The error tooltip is a child of the box, hidden by CSS
// alone, so its text is skipped here rather than read back as part of the
// expression -- a commit may run while the error is still shown. A
// contenteditable also keeps a NBSP where a plain space would not survive as
// markup, so fold those back.
function inputText(el) {
    var text = ""
    for (var node of el.childNodes) {
        if (node.nodeType != Node.ELEMENT_NODE) {
            text += node.textContent
        } else if (!node.classList.contains('error-msg')) {
            text += inputText(node)
        }
    }
    return text.replace(/\n/g, "").replace(/\u00a0/g, " ")
}

function moveCursorToEnd(target) {
    const range = document.createRange();
    const selection = window.getSelection();

    range.selectNodeContents(target);
    range.collapse(false);
    selection.removeAllRanges();
    selection.addRange(range);
}

// Built as nodes rather than as a string: the expression is shown back to the
// user inside the box it came from, so anything that looks like a tag has to
// stay text. "C++ & vector<int> &" lost its <int> when this was innerHTML.
function showError(el, info) {
    var errorMsg = document.createElement('span')
    errorMsg.className = 'error-msg'
    errorMsg.textContent = "Error: " + info.msg

    var error = document.createElement('span')
    error.className = 'error'
    // A bare space would collapse away, leaving nothing to highlight.
    error.textContent = info.exp[info.index] == ' ' ? '\u00a0' : info.exp[info.index]
    error.appendChild(errorMsg)

    var invalid = document.createElement('span')
    invalid.className = 'invalid-expression'
    invalid.textContent = info.exp.slice(info.index + 1)

    el.textContent = info.exp.slice(0, info.index)
    el.appendChild(error)
    el.appendChild(invalid)
}

// Answers whether a marker was actually stripped.
function clearError(el) {
    // Nothing to strip: leave the DOM alone so the caret keeps its position.
    if (el.getElementsByClassName('error').length == 0) {
        return false
    }
    for (var msg of Array.from(el.getElementsByClassName('error-msg'))) {
        msg.remove()
    }
    // Assigned as text, so the expression comes back exactly as it was typed.
    el.textContent = inputText(el)
    moveCursorToEnd(el)
    return true
}

// `apply` receives the match tree, or null for an empty box. A bad expression
// is reported in the box and apply is not called, so whatever the last good
// expression produced stays on screen.
function commitExpression(el, apply) {
    var exp = inputText(el).trim()
    if (exp == "") {
        // Drops any error markup along with the text.
        el.innerHTML = ""
        apply(null)
        return
    }

    var validateResult = expressionValidate(exp)
    if (validateResult != "OK") {
        showError(el, validateResult)
        return
    }

    apply(expressionParse(exp))
}

// Enter and blur commit; focus strips the error marker so the caret can move
// through the expression again. Returns the two ways a caller drives the box
// outside of those events: commit reads it, refresh does not.
function bindExpressionInput(el, apply) {
    // The expression as of the last commit, or null once focus has wiped an
    // error marker. A commit replaces what it produced, which would cancel a
    // click landing on that output, so blurring on an unchanged expression
    // commits nothing.
    var lastText = null

    // The tree of the last expression that was actually applied, so it can be
    // applied again without reading the box. A rejected expression leaves it
    // alone, which is why apply is wrapped rather than called directly.
    var lastTree = null
    const record = tree => {
        lastTree = tree
        apply(tree)
    }

    const commit = () => {
        lastText = inputText(el).trim()
        commitExpression(el, record)
    }

    el.addEventListener('keypress', e => {
        if (e.key != 'Enter') {
            return
        }
        // Commit first, so the blur below finds nothing left to do.
        e.preventDefault()
        commit()
        moveCursorToEnd(el)
        el.blur()
    })

    el.addEventListener('input', () => {
        // A pasted newline would turn the single line into a block.
        if (el.innerText.includes('\n')) {
            el.innerText = el.innerText.replace(/\n/g, '')
            moveCursorToEnd(el)
        }

        // Deleting the last character leaves a <br> behind, and the box counts
        // as :empty only while it has no children at all -- which is the one
        // thing holding the placeholder up.
        if (el.textContent == "" && el.childNodes.length > 0) {
            el.innerHTML = ""
            moveCursorToEnd(el)
        }
    })

    el.addEventListener('focus', () => {
        if (clearError(el)) {
            // The marker is gone, so blurring has to report the error again.
            lastText = null
        }
    })

    el.addEventListener('focusout', () => {
        if (inputText(el).trim() != lastText) {
            commit()
        }
    })

    return {
        // Read the box and apply what it says.
        commit,
        // Apply the last committed expression again, leaving the box untouched:
        // for when what apply draws on has changed rather than the expression.
        // Reading the box here would report an error over a half-typed one.
        refresh: () => apply(lastTree),
    }
}

export {bindExpressionInput}
