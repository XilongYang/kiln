window.addEventListener("DOMContentLoaded", function () {
    "use strict";

    var posts = document.querySelectorAll(".post");

    posts.forEach(function (post) {
        var fullText = (post.textContent || "").replace(/\s+/g, " ").trim();
        if (!fullText) {
            return;
        }

        // Measured on the original markup, before the surgery below: the
        // flattened preview can read as short where the real post is tall
        // (a poem of one-line `<p>`s). Media is hidden for the measurement
        // only -- one image's min-height alone can exceed the line budget.
        var media = post.querySelectorAll(".post-image, .post-video, .post-media-grid");
        media.forEach(function (el) {
            el.style.display = "none";
        });
        var originalHeight = post.scrollHeight;
        media.forEach(function (el) {
            el.style.display = "";
        });

        var fullContent = document.createElement("div");
        fullContent.className = "post-full-content";
        while (post.firstChild) {
            fullContent.appendChild(post.firstChild);
        }

        var preview = document.createElement("p");
        preview.className = "post-preview";
        preview.textContent = fullText;
        post.appendChild(preview);

        var previewStyle = getComputedStyle(preview);
        var lineHeight = parseFloat(previewStyle.lineHeight) || 0;
        var clampLines = parseInt(previewStyle.webkitLineClamp, 10) || 5;
        var overflowing = originalHeight > lineHeight * clampLines + 1;

        if (!overflowing) {
            // Restore the flat structure rather than leave the wrapper:
            // the extra layer breaks `.post > p > .post-image` et al.
            post.removeChild(preview);
            while (fullContent.firstChild) {
                post.appendChild(fullContent.firstChild);
            }
            return;
        }

        // A sibling of `preview`, not a child past its 5th line: that
        // would fall inside the clamp's `overflow: hidden`, unclickable.
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
