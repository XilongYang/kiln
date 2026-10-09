window.addEventListener("DOMContentLoaded", function () {
    "use strict";

    var posts = document.querySelectorAll(".post");

    posts.forEach(function (post) {
        var fullText = (post.textContent || "").replace(/\s+/g, " ").trim();
        if (!fullText) {
            return;
        }

        var fullContent = document.createElement("div");
        fullContent.className = "post-full-content";
        while (post.firstChild) {
            fullContent.appendChild(post.firstChild);
        }

        var preview = document.createElement("p");
        preview.className = "post-preview";
        preview.textContent = fullText;
        post.appendChild(preview);

        // `.post-preview`'s CSS line-clamp caps it at a fixed number of
        // rendered lines -- comparing scrollHeight/clientHeight here (rather
        // than guessing from character count) is what actually tells us
        // whether `fullText` overflowed that clamp, since the same
        // character count can render as very different line counts
        // depending on font, viewport width, and CJK vs. Latin script.
        var overflowing = preview.scrollHeight > preview.clientHeight + 1;

        if (!overflowing) {
            // Not collapsing: restore the original flat structure instead
            // of leaving the `.post-full-content` wrapper in place, since
            // that extra layer breaks CSS selectors written against
            // `.post > p > .post-image` et al.
            post.removeChild(preview);
            while (fullContent.firstChild) {
                post.appendChild(fullContent.firstChild);
            }
            return;
        }

        // The expand button lives as `preview`'s sibling, not a child
        // appended inline after its text: a child placed past the 5th
        // line would itself fall inside the clamp's `overflow: hidden`
        // and never be visible or clickable.
        var expandButton = document.createElement("button");
        expandButton.type = "button";
        expandButton.className = "post-toggle";
        expandButton.textContent = "展开";

        var collapseButton = document.createElement("button");
        collapseButton.type = "button";
        collapseButton.className = "post-toggle";
        collapseButton.textContent = "收起";

        post.appendChild(fullContent);
        post.appendChild(expandButton);
        post.appendChild(collapseButton);

        post.classList.add("is-collapsed");
        fullContent.hidden = true;
        collapseButton.hidden = true;

        expandButton.addEventListener("click", function () {
            post.classList.remove("is-collapsed");
            preview.hidden = true;
            fullContent.hidden = false;
            expandButton.hidden = true;
            collapseButton.hidden = false;
        });

        collapseButton.addEventListener("click", function () {
            post.classList.add("is-collapsed");
            preview.hidden = false;
            fullContent.hidden = true;
            expandButton.hidden = false;
            collapseButton.hidden = true;
            preview.scrollIntoView({ behavior: "smooth", block: "nearest" });
        });
    });
});
