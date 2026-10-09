window.addEventListener("DOMContentLoaded", function () {
    "use strict";

    /**
     * Archive side nav rebuilds itself from currently visible post dates.
     * It listens to `archive:filter-changed` emitted by filtering logic.
     */
    var FILTER_CHANGED_EVENT = "archive:filter-changed";
    var MONTH_KEY_PATTERN = /^(\d{4})-(\d{2})-(\d{2})$/;
    var MONTH_ANCHOR_PREFIX = "month-";
    var VIEWPORT_PROBE_RATIO = 0.22;

    var dateNodes = Array.prototype.slice.call(document.querySelectorAll(".post-date"));
    if (!dateNodes.length) {
        return;
    }

    var ui = createArchiveUI();
    var navLinks = [];
    var activeMonthKey = null;
    var isTicking = false;

    initializeMonthKeys();
    rebuildNavFromVisibleDates();
    updateActiveMonthFromViewport();
    bindEvents();

    function createArchiveUI() {
        var topJump = createJumpControl("archive-jump-top", "Scroll to top");
        var bottomJump = createJumpControl("archive-jump-bottom", "Scroll to bottom");
        var aside = document.createElement("aside");
        aside.className = "archive-nav";

        var nav = document.createElement("nav");
        nav.className = "archive-nav-inner";
        nav.setAttribute("aria-label", "Posts archive navigation");

        aside.appendChild(nav);
        document.body.appendChild(topJump);
        document.body.appendChild(aside);
        document.body.appendChild(bottomJump);

        return {
            topJump: topJump,
            bottomJump: bottomJump,
            aside: aside,
            nav: nav,
        };
    }

    function createJumpControl(className, label) {
        var control = document.createElement("a");
        control.className = "archive-jump " + className;
        control.href = "#";
        control.setAttribute("aria-label", label);
        control.innerHTML = '<span class="archive-jump-icon" aria-hidden="true"></span>';
        return control;
    }

    function parseDateNode(dateNode) {
        var rawDateText = "";
        for (var i = 0; i < dateNode.childNodes.length; i += 1) {
            var childNode = dateNode.childNodes[i];
            if (childNode.nodeType !== Node.TEXT_NODE) {
                continue;
            }
            rawDateText = (childNode.nodeValue || "").trim();
            if (rawDateText) {
                break;
            }
        }

        var match = rawDateText.match(MONTH_KEY_PATTERN);
        if (!match) {
            return null;
        }

        return {
            year: match[1],
            month: match[2],
            monthKey: match[1] + "-" + match[2],
        };
    }

    function initializeMonthKeys() {
        dateNodes.forEach(function (dateNode) {
            var parsedDate = parseDateNode(dateNode);
            if (!parsedDate) {
                return;
            }
            dateNode.dataset.monthKey = parsedDate.monthKey;
        });
    }

    /**
     * Anchors are assigned only to visible dates so month links always jump
     * to an actually visible target under filtering.
     */
    function assignVisibleMonthAnchors() {
        dateNodes.forEach(function (dateNode) {
            if (dateNode.id && dateNode.id.indexOf(MONTH_ANCHOR_PREFIX) === 0) {
                dateNode.removeAttribute("id");
            }
        });

        var anchoredMonthKeys = new Set();
        dateNodes.forEach(function (dateNode) {
            if (dateNode.hidden) {
                return;
            }

            var monthKey = dateNode.dataset.monthKey;
            if (!monthKey || anchoredMonthKeys.has(monthKey)) {
                return;
            }

            dateNode.id = MONTH_ANCHOR_PREFIX + monthKey;
            anchoredMonthKeys.add(monthKey);
        });
    }

    function collectVisibleYearMonthMap() {
        var yearToMonths = new Map();
        var monthSeen = new Set();

        dateNodes.forEach(function (dateNode) {
            if (dateNode.hidden) {
                return;
            }

            var parsedDate = parseDateNode(dateNode);
            if (!parsedDate || monthSeen.has(parsedDate.monthKey)) {
                return;
            }

            monthSeen.add(parsedDate.monthKey);
            if (!yearToMonths.has(parsedDate.year)) {
                yearToMonths.set(parsedDate.year, new Set());
            }
            yearToMonths.get(parsedDate.year).add(parsedDate.month);
        });

        return yearToMonths;
    }

    function rebuildNavFromVisibleDates() {
        assignVisibleMonthAnchors();

        var yearToMonths = collectVisibleYearMonthMap();
        ui.nav.textContent = "";
        navLinks = [];

        Array.from(yearToMonths.keys())
            .sort(function (a, b) {
                return Number(b) - Number(a);
            })
            .forEach(function (year) {
                var yearLabel = document.createElement("div");
                yearLabel.className = "archive-nav-year";
                yearLabel.textContent = year;
                ui.nav.appendChild(yearLabel);

                Array.from(yearToMonths.get(year))
                    .sort(function (a, b) {
                        return Number(b) - Number(a);
                    })
                    .forEach(function (month) {
                        var monthKey = year + "-" + month;
                        var monthLink = document.createElement("a");
                        monthLink.className = "archive-nav-month";
                        monthLink.href = "#" + MONTH_ANCHOR_PREFIX + monthKey;
                        monthLink.dataset.monthKey = monthKey;
                        monthLink.textContent = String(Number(month));
                        ui.nav.appendChild(monthLink);
                        navLinks.push(monthLink);
                    });
            });

        if (!navLinks.length) {
            ui.nav.hidden = true;
            activeMonthKey = null;
            return;
        }
        ui.nav.hidden = false;
    }

    function setActiveMonth(monthKey) {
        activeMonthKey = monthKey || null;
        navLinks.forEach(function (link) {
            link.classList.toggle("is-active", link.dataset.monthKey === activeMonthKey);
        });
    }

    function getFirstVisibleMonthKey() {
        return navLinks.length ? navLinks[0].dataset.monthKey : null;
    }

    function getMonthKeyFromViewport() {
        var probeY = window.innerHeight * VIEWPORT_PROBE_RATIO;
        var currentMonthKey = null;

        for (var i = 0; i < dateNodes.length; i += 1) {
            var node = dateNodes[i];
            if (node.hidden) {
                continue;
            }

            if (node.getBoundingClientRect().top <= probeY) {
                currentMonthKey = node.dataset.monthKey || currentMonthKey;
                continue;
            }
            break;
        }

        return currentMonthKey || getFirstVisibleMonthKey();
    }

    function scrollNavToActiveMonth() {
        if (!activeMonthKey) {
            return;
        }

        var activeLink = ui.nav.querySelector(
            '.archive-nav-month[data-month-key="' + activeMonthKey + '"]'
        );
        if (!activeLink) {
            return;
        }

        var targetTop = activeLink.offsetTop - ui.aside.clientHeight / 2 + activeLink.offsetHeight / 2;
        var maxTop = Math.max(0, ui.aside.scrollHeight - ui.aside.clientHeight);
        ui.aside.scrollTo({
            top: Math.max(0, Math.min(maxTop, targetTop)),
            behavior: "smooth",
        });
    }

    function updateActiveMonthFromViewport() {
        if (!navLinks.length) {
            return;
        }

        var nextMonthKey = getMonthKeyFromViewport();
        if (
            !nextMonthKey ||
            !ui.nav.querySelector('.archive-nav-month[data-month-key="' + nextMonthKey + '"]')
        ) {
            nextMonthKey = getFirstVisibleMonthKey();
        }

        var changed = nextMonthKey !== activeMonthKey;
        setActiveMonth(nextMonthKey);
        if (changed) {
            scrollNavToActiveMonth();
        }
    }

    function requestActiveMonthSyncOnScroll() {
        if (isTicking) {
            return;
        }

        isTicking = true;
        window.requestAnimationFrame(function () {
            updateActiveMonthFromViewport();
            isTicking = false;
        });
    }

    function bindEvents() {
        window.addEventListener("scroll", requestActiveMonthSyncOnScroll, { passive: true });
        window.addEventListener("resize", updateActiveMonthFromViewport);

        window.addEventListener(FILTER_CHANGED_EVENT, function () {
            rebuildNavFromVisibleDates();
            updateActiveMonthFromViewport();
        });

        ui.topJump.addEventListener("click", function (event) {
            event.preventDefault();
            window.scrollTo({ top: 0, behavior: "smooth" });
        });

        ui.bottomJump.addEventListener("click", function (event) {
            event.preventDefault();
            window.scrollTo({ top: document.documentElement.scrollHeight, behavior: "smooth" });
        });
    }
});
