import {expressionMatch} from './express-matcher.js'
import {bindExpressionInput} from './express-input.js'

// Set once the filter box is bound; lets toggleInput drive the box outside of
// its own events.
var filterInput = {commit: () => {}}

// ============= Private Method =================

// A null tree means an empty expression: every post matches. A year heading
// goes away with the last post under it.
function applyFilter(matchTree) {
    var postYears = document.getElementsByClassName('post-year-wrapper')
    for (var postYear of postYears) {
        var atLeastOne = false
        var posts = postYear.getElementsByClassName('post-wrapper')
        for (var post of posts) {
            var isSuccess = matchTree == null || expressionMatch(matchTree, post.innerText)
            atLeastOne = atLeastOne || isSuccess
            post.style.display = isSuccess ? 'block' : 'none'
        }
        postYear.style.display = atLeastOne ? 'block' : 'none'
    }
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
        filterInput.commit()
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

function bindFilterInput(input) {
    filterInput = bindExpressionInput(input, applyFilter)
}

export {toggleInput, bindFilterInput}
