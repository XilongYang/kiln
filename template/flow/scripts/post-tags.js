window.addEventListener("DOMContentLoaded", function () {
    "use strict";

    /**
     * Tag/search filtering module.
     * Owns post visibility and exposes a single integration event:
     * `archive:filter-changed`.
     */
    var FILTER_CHANGED_EVENT = "archive:filter-changed";
    var ALL_TAG_LABEL = "All";
    var ALL_TAG_KEY = ALL_TAG_LABEL.toLowerCase();
    var TAG_STATE = {
        NEUTRAL: "neutral",
        INCLUDE: "include",
        EXCLUDE: "exclude",
    };

    var main = document.querySelector("main");
    if (!main) {
        return;
    }

    var postNodes = Array.prototype.slice.call(main.querySelectorAll(".post"));
    if (!postNodes.length) {
        return;
    }

    var model = buildPostModel(postNodes);
    if (!model.availableTags.length) {
        return;
    }

    var ui = renderFilterUI(main, model.availableTags);
    bindFilterEvents(ui, model.records);
    resetToAll(ui, model.records);

    function buildPostModel(posts) {
        var tagSet = new Set();
        var records = posts.map(function (postNode) {
            var parsedTags = parseTags(postNode.dataset.tags || "");
            parsedTags.forEach(function (tag) {
                tagSet.add(tag);
            });

            var dateNode = findSiblingByClass(postNode, "previousElementSibling", "post-date");
            var dividerNode = findSiblingByClass(postNode, "nextElementSibling", "post-divider");

            if (dateNode && parsedTags.length) {
                appendTagBadgesToDate(dateNode, parsedTags);
            }

            return {
                post: postNode,
                date: dateNode,
                divider: dividerNode,
                tags: parsedTags,
                text: (postNode.textContent || "").toLowerCase(),
            };
        });

        return {
            records: records,
            availableTags: Array.from(tagSet).sort(function (a, b) {
                return a.localeCompare(b);
            }),
        };
    }

    function parseTags(rawTags) {
        return String(rawTags)
            .trim()
            .split(",")
            .map(function (tag) {
                return tag.trim().toLowerCase();
            })
            .filter(function (tag) {
                return tag.length > 0;
            });
    }

    function findSiblingByClass(node, direction, className) {
        var current = node[direction];
        while (current && !current.classList.contains(className)) {
            current = current[direction];
        }
        return current || null;
    }

    function appendTagBadgesToDate(dateNode, tags) {
        var wrapper = document.createElement("span");
        wrapper.className = "post-tag-list";

        tags.forEach(function (tag) {
            var badge = document.createElement("span");
            badge.className = "post-tag";
            badge.textContent = tag;
            wrapper.appendChild(badge);
        });

        dateNode.appendChild(document.createTextNode(" "));
        dateNode.appendChild(wrapper);
    }

    function renderFilterUI(container, tags) {
        var filterRoot = document.createElement("section");
        filterRoot.className = "post-tag-filter";
        filterRoot.setAttribute("aria-label", "Post tag filter");

        var title = document.createElement("span");
        title.className = "post-tag-filter-title";
        title.textContent = "Tags:";
        filterRoot.appendChild(title);

        var group = document.createElement("div");
        group.className = "post-tag-filter-group";
        filterRoot.appendChild(group);

        var searchLabel = document.createElement("span");
        searchLabel.className = "post-filter-search-label";
        searchLabel.textContent = "Search:";
        filterRoot.appendChild(searchLabel);

        var searchInput = document.createElement("input");
        searchInput.type = "search";
        searchInput.className = "post-filter-search";
        searchInput.placeholder = "";
        searchInput.setAttribute("aria-label", "Search posts by keyword");
        filterRoot.appendChild(searchInput);

        var controls = [];
        [ALL_TAG_LABEL].concat(tags).forEach(function (tagLabel, index) {
            var button = document.createElement("button");
            button.type = "button";
            button.className = "post-tag-filter-btn";
            button.dataset.tag = tagLabel.toLowerCase();
            button.dataset.state = TAG_STATE.NEUTRAL;
            button.textContent = tagLabel;

            if (index === 0) {
                button.dataset.state = TAG_STATE.INCLUDE;
                button.classList.add("is-active");
                button.setAttribute("aria-pressed", "true");
            } else {
                button.setAttribute("aria-pressed", "false");
            }

            controls.push(button);
            group.appendChild(button);
        });

        container.insertBefore(filterRoot, container.firstChild);
        return {
            root: filterRoot,
            group: group,
            searchInput: searchInput,
            controls: controls,
        };
    }

    function bindFilterEvents(uiState, records) {
        uiState.group.addEventListener("click", function (event) {
            var button = event.target.closest(".post-tag-filter-btn");
            if (!button) {
                return;
            }

            if (button.dataset.tag === ALL_TAG_KEY) {
                resetToAll(uiState, records);
                return;
            }

            cycleTagState(button);
            setAllButtonNeutral(uiState.controls);
            syncButtonStyles(uiState.controls);
            applyFilter(uiState, records);
        });

        uiState.searchInput.addEventListener("input", function () {
            applyFilter(uiState, records);
        });
    }

    function resetToAll(uiState, records) {
        uiState.controls.forEach(function (button) {
            button.dataset.state =
                button.dataset.tag === ALL_TAG_KEY ? TAG_STATE.INCLUDE : TAG_STATE.NEUTRAL;
        });
        syncButtonStyles(uiState.controls);
        applyFilter(uiState, records);
    }

    function cycleTagState(button) {
        var current = button.dataset.state || TAG_STATE.NEUTRAL;
        if (current === TAG_STATE.NEUTRAL) {
            button.dataset.state = TAG_STATE.INCLUDE;
            return;
        }
        if (current === TAG_STATE.INCLUDE) {
            button.dataset.state = TAG_STATE.EXCLUDE;
            return;
        }
        button.dataset.state = TAG_STATE.NEUTRAL;
    }

    function setAllButtonNeutral(controls) {
        var allButton = controls.find(function (control) {
            return control.dataset.tag === ALL_TAG_KEY;
        });
        if (allButton) {
            allButton.dataset.state = TAG_STATE.NEUTRAL;
        }
    }

    function syncButtonStyles(controls) {
        controls.forEach(function (button) {
            var state = button.dataset.state || TAG_STATE.NEUTRAL;
            button.classList.toggle("is-active", state === TAG_STATE.INCLUDE);
            button.classList.toggle("is-excluded", state === TAG_STATE.EXCLUDE);
            button.setAttribute("aria-pressed", state === TAG_STATE.INCLUDE ? "true" : "false");
        });
    }

    function getSelectedTags(controls, targetState) {
        return controls
            .filter(function (button) {
                return button.dataset.tag !== ALL_TAG_KEY && button.dataset.state === targetState;
            })
            .map(function (button) {
                return button.dataset.tag;
            });
    }

    function applyFilter(uiState, records) {
        var keyword = (uiState.searchInput.value || "").trim().toLowerCase();
        var includeTags = getSelectedTags(uiState.controls, TAG_STATE.INCLUDE);
        var excludeTags = getSelectedTags(uiState.controls, TAG_STATE.EXCLUDE);

        records.forEach(function (record) {
            var included =
                includeTags.length === 0 ||
                includeTags.every(function (tag) {
                    return record.tags.indexOf(tag) !== -1;
                });
            var excluded = excludeTags.some(function (tag) {
                return record.tags.indexOf(tag) !== -1;
            });
            var matchesKeyword = !keyword || record.text.indexOf(keyword) !== -1;
            var visible = included && !excluded && matchesKeyword;

            record.post.hidden = !visible;
            if (record.date) {
                record.date.hidden = !visible;
            }
        });

        syncDividers(records);
        window.dispatchEvent(new CustomEvent(FILTER_CHANGED_EVENT));
        updateHighlights(records, keyword);
    }

    /**
     * Divider visibility must be derived after post visibility is final,
     * otherwise stale trailing separators can remain at the bottom.
     */
    function syncDividers(records) {
        var lastVisibleIndex = -1;
        for (var index = records.length - 1; index >= 0; index -= 1) {
            if (!records[index].post.hidden) {
                lastVisibleIndex = index;
                break;
            }
        }

        records.forEach(function (record, index) {
            if (!record.divider) {
                return;
            }
            record.divider.hidden = record.post.hidden || index >= lastVisibleIndex;
        });
    }

    function clearHighlights(rootNode) {
        var highlights = rootNode.querySelectorAll(".post-search-hit");
        highlights.forEach(function (highlight) {
            var replacement = document.createTextNode(highlight.textContent || "");
            var parent = highlight.parentNode;
            if (!parent) {
                return;
            }
            parent.replaceChild(replacement, highlight);
            parent.normalize();
        });
    }

    function highlightKeywordInTextNode(textNode, keyword) {
        var original = textNode.nodeValue || "";
        var lowered = original.toLowerCase();
        var start = lowered.indexOf(keyword);
        if (start === -1) {
            return;
        }

        var fragment = document.createDocumentFragment();
        var cursor = 0;

        while (start !== -1) {
            if (start > cursor) {
                fragment.appendChild(document.createTextNode(original.slice(cursor, start)));
            }

            var hit = document.createElement("span");
            hit.className = "post-search-hit";
            hit.textContent = original.slice(start, start + keyword.length);
            fragment.appendChild(hit);

            cursor = start + keyword.length;
            start = lowered.indexOf(keyword, cursor);
        }

        if (cursor < original.length) {
            fragment.appendChild(document.createTextNode(original.slice(cursor)));
        }

        if (textNode.parentNode) {
            textNode.parentNode.replaceChild(fragment, textNode);
        }
    }

    function updateHighlights(records, keyword) {
        records.forEach(function (record) {
            clearHighlights(record.post);
            if (!keyword || record.post.hidden) {
                return;
            }

            var walker = document.createTreeWalker(record.post, NodeFilter.SHOW_TEXT, {
                acceptNode: function (node) {
                    var value = node.nodeValue || "";
                    if (!value.trim()) {
                        return NodeFilter.FILTER_REJECT;
                    }

                    var parent = node.parentElement;
                    if (!parent || parent.closest(".post-search-hit")) {
                        return NodeFilter.FILTER_REJECT;
                    }

                    return NodeFilter.FILTER_ACCEPT;
                },
            });

            var textNodes = [];
            var currentNode;
            while ((currentNode = walker.nextNode())) {
                textNodes.push(currentNode);
            }

            textNodes.forEach(function (textNode) {
                highlightKeywordInTextNode(textNode, keyword);
            });
        });
    }
});
