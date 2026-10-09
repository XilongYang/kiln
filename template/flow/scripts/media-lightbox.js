window.addEventListener("DOMContentLoaded", function () {
    var overlay = document.createElement("div");
    overlay.className = "media-lightbox";
    overlay.hidden = true;
    overlay.setAttribute("aria-hidden", "true");

    var panel = document.createElement("div");
    panel.className = "media-lightbox-panel";
    panel.setAttribute("role", "dialog");
    panel.setAttribute("aria-modal", "true");
    panel.setAttribute("aria-label", "Media preview");

    var prevButton = document.createElement("button");
    prevButton.type = "button";
    prevButton.className = "media-lightbox-nav media-lightbox-prev";
    prevButton.setAttribute("aria-label", "Previous media");
    prevButton.textContent = "<";

    var nextButton = document.createElement("button");
    nextButton.type = "button";
    nextButton.className = "media-lightbox-nav media-lightbox-next";
    nextButton.setAttribute("aria-label", "Next media");
    nextButton.textContent = ">";

    var stage = document.createElement("div");
    stage.className = "media-lightbox-stage";
    var viewport = document.createElement("div");
    viewport.className = "media-lightbox-viewport";
    stage.appendChild(viewport);

    var zoomToolbar = document.createElement("div");
    zoomToolbar.className = "media-lightbox-zoom-toolbar";
    var zoomOutButton = document.createElement("button");
    zoomOutButton.type = "button";
    zoomOutButton.className = "media-lightbox-zoom-btn";
    zoomOutButton.setAttribute("aria-label", "Zoom out");
    zoomOutButton.textContent = "-";
    var zoomInButton = document.createElement("button");
    zoomInButton.type = "button";
    zoomInButton.className = "media-lightbox-zoom-btn";
    zoomInButton.setAttribute("aria-label", "Zoom in");
    zoomInButton.textContent = "+";
    var zoomResetButton = document.createElement("button");
    zoomResetButton.type = "button";
    zoomResetButton.className = "media-lightbox-zoom-btn";
    zoomResetButton.setAttribute("aria-label", "Fit to screen");
    zoomResetButton.textContent = "Fit";
    zoomToolbar.appendChild(zoomOutButton);
    zoomToolbar.appendChild(zoomResetButton);
    zoomToolbar.appendChild(zoomInButton);

    var minimap = document.createElement("div");
    minimap.className = "media-lightbox-minimap";
    var minimapImage = document.createElement("img");
    minimapImage.className = "media-lightbox-minimap-image";
    minimapImage.alt = "";
    var minimapRect = document.createElement("div");
    minimapRect.className = "media-lightbox-minimap-rect";
    minimap.appendChild(minimapImage);
    minimap.appendChild(minimapRect);

    panel.appendChild(prevButton);
    panel.appendChild(nextButton);
    panel.appendChild(zoomToolbar);
    panel.appendChild(stage);
    panel.appendChild(minimap);
    overlay.appendChild(panel);
    document.body.appendChild(overlay);

    var mediaList = [];
    var currentIndex = -1;
    var activeMedia = null;
    var zoom = 1;
    var zoomMin = 1;
    var zoomMax = 4;
    var panX = 0;
    var panY = 0;
    var baseWidth = 0;
    var baseHeight = 0;
    var isDragging = false;
    var dragStartX = 0;
    var dragStartY = 0;
    var dragPanX = 0;
    var dragPanY = 0;
    var isDraggingMinimapRect = false;
    var minimapDragOffsetX = 0;
    var minimapDragOffsetY = 0;
    var minimapRectWidth = 0;
    var minimapRectHeight = 0;

    function pauseAllPostVideos() {
        var videos = document.querySelectorAll(".post-video");
        videos.forEach(function (video) {
            if (video && !video.paused) {
                video.pause();
            }
        });
    }

    function isVideoControlsClick(event, video) {
        if (!event || !video || video.tagName !== "VIDEO" || !video.controls) {
            return false;
        }
        var rect = video.getBoundingClientRect();
        if (!rect || rect.height <= 0) {
            return false;
        }

        // Browser-native controls can appear at top and bottom depending on browser/UI state.
        var controlsBandHeight = Math.min(56, rect.height * 0.35);
        var yFromTop = event.clientY - rect.top;
        var inTopBand = yFromTop <= controlsBandHeight;
        var inBottomBand = yFromTop >= rect.height - controlsBandHeight;
        return inTopBand || inBottomBand;
    }

    function isFirstVideoClick(video) {
        if (!video || video.tagName !== "VIDEO") {
            return false;
        }
        if (video.dataset.lightboxPrimed === "1") {
            return false;
        }
        video.dataset.lightboxPrimed = "1";
        return true;
    }

    function isVideoPrimed(video) {
        return !!(video && video.dataset.lightboxPrimed === "1");
    }

    function isVisible(element) {
        if (!element || !element.isConnected) {
            return false;
        }
        if (element.hidden) {
            return false;
        }
        return element.getClientRects().length > 0;
    }

    function collectMedia() {
        var all = Array.prototype.slice.call(
            document.querySelectorAll(".post-image[src], .post-video[src]")
        );

        return all.filter(function (item) {
            return item.getAttribute("src") && isVisible(item);
        });
    }

    function cleanupActiveMedia() {
        if (activeMedia && activeMedia.tagName === "VIDEO") {
            activeMedia.pause();
            activeMedia.currentTime = 0;
        }
        activeMedia = null;
        viewport.textContent = "";
        minimap.classList.remove("is-visible");
        minimapImage.removeAttribute("src");
        zoom = 1;
        panX = 0;
        panY = 0;
        baseWidth = 0;
        baseHeight = 0;
        isDragging = false;
    }

    function clamp(value, min, max) {
        return Math.min(max, Math.max(min, value));
    }

    function constrainPan() {
        if (!activeMedia || zoom <= 1) {
            panX = 0;
            panY = 0;
            return;
        }
        var viewportRect = viewport.getBoundingClientRect();
        var contentWidth = baseWidth * zoom;
        var contentHeight = baseHeight * zoom;
        var maxOffsetX = Math.max(0, (contentWidth - viewportRect.width) / 2);
        var maxOffsetY = Math.max(0, (contentHeight - viewportRect.height) / 2);
        panX = clamp(panX, -maxOffsetX, maxOffsetX);
        panY = clamp(panY, -maxOffsetY, maxOffsetY);
    }

    function updateMinimap() {
        if (!activeMedia || activeMedia.tagName !== "IMG" || zoom <= 1) {
            minimap.classList.remove("is-visible");
            return;
        }

        var viewportRect = viewport.getBoundingClientRect();
        var contentWidth = baseWidth * zoom;
        var contentHeight = baseHeight * zoom;
        if (!viewportRect.width || !viewportRect.height || !contentWidth || !contentHeight) {
            minimap.classList.remove("is-visible");
            return;
        }

        var miniMaxWidth = 180;
        var miniMaxHeight = 140;
        var mediaRatio = contentWidth / contentHeight;
        var miniWidth = miniMaxWidth;
        var miniHeight = miniWidth / mediaRatio;
        if (miniHeight > miniMaxHeight) {
            miniHeight = miniMaxHeight;
            miniWidth = miniHeight * mediaRatio;
        }

        minimap.style.width = miniWidth + "px";
        minimap.style.height = miniHeight + "px";

        var left = (viewportRect.width - contentWidth) / 2 + panX;
        var top = (viewportRect.height - contentHeight) / 2 + panY;
        var visibleX = clamp(-left, 0, contentWidth);
        var visibleY = clamp(-top, 0, contentHeight);
        var visibleWidth = Math.min(viewportRect.width, contentWidth);
        var visibleHeight = Math.min(viewportRect.height, contentHeight);

        var scale = miniWidth / contentWidth;
        minimapRect.style.left = visibleX * scale + "px";
        minimapRect.style.top = visibleY * scale + "px";
        minimapRect.style.width = visibleWidth * scale + "px";
        minimapRect.style.height = visibleHeight * scale + "px";
        minimap.classList.add("is-visible");
    }

    function applyMediaTransform() {
        if (!activeMedia || !baseWidth || !baseHeight) {
            return;
        }
        constrainPan();
        var viewportRect = viewport.getBoundingClientRect();
        var contentWidth = baseWidth * zoom;
        var contentHeight = baseHeight * zoom;
        activeMedia.style.width = contentWidth + "px";
        activeMedia.style.height = contentHeight + "px";
        activeMedia.style.left = (viewportRect.width - contentWidth) / 2 + panX + "px";
        activeMedia.style.top = (viewportRect.height - contentHeight) / 2 + panY + "px";
        viewport.classList.toggle("is-zoomed", zoom > 1);
        updateMinimap();
    }

    function setupBaseSize() {
        if (!activeMedia) {
            return;
        }
        var viewportRect = viewport.getBoundingClientRect();
        if (!viewportRect.width || !viewportRect.height) {
            return;
        }

        var intrinsicWidth = 0;
        var intrinsicHeight = 0;
        if (activeMedia.tagName === "IMG") {
            intrinsicWidth = activeMedia.naturalWidth || 0;
            intrinsicHeight = activeMedia.naturalHeight || 0;
        } else if (activeMedia.tagName === "VIDEO") {
            intrinsicWidth = activeMedia.videoWidth || 0;
            intrinsicHeight = activeMedia.videoHeight || 0;
        }
        if (!intrinsicWidth || !intrinsicHeight) {
            return;
        }

        var fitScale = Math.min(
            viewportRect.width / intrinsicWidth,
            viewportRect.height / intrinsicHeight
        );
        baseWidth = intrinsicWidth * fitScale;
        baseHeight = intrinsicHeight * fitScale;
        zoom = 1;
        panX = 0;
        panY = 0;
        applyMediaTransform();
    }

    function zoomTo(nextZoom, anchorX, anchorY) {
        if (!activeMedia || !baseWidth || !baseHeight) {
            return;
        }
        var viewportRect = viewport.getBoundingClientRect();
        var oldZoom = zoom;
        nextZoom = clamp(nextZoom, zoomMin, zoomMax);
        if (Math.abs(nextZoom - oldZoom) < 0.001) {
            return;
        }

        var oldContentWidth = baseWidth * oldZoom;
        var oldContentHeight = baseHeight * oldZoom;
        var oldLeft = (viewportRect.width - oldContentWidth) / 2 + panX;
        var oldTop = (viewportRect.height - oldContentHeight) / 2 + panY;
        var pointerX = typeof anchorX === "number" ? anchorX : viewportRect.width / 2;
        var pointerY = typeof anchorY === "number" ? anchorY : viewportRect.height / 2;
        var relX = oldContentWidth ? (pointerX - oldLeft) / oldContentWidth : 0.5;
        var relY = oldContentHeight ? (pointerY - oldTop) / oldContentHeight : 0.5;

        zoom = nextZoom;
        var newContentWidth = baseWidth * zoom;
        var newContentHeight = baseHeight * zoom;
        var centeredLeft = (viewportRect.width - newContentWidth) / 2;
        var centeredTop = (viewportRect.height - newContentHeight) / 2;
        var newLeft = pointerX - relX * newContentWidth;
        var newTop = pointerY - relY * newContentHeight;
        panX = newLeft - centeredLeft;
        panY = newTop - centeredTop;
        applyMediaTransform();
    }

    function resetZoom() {
        zoom = 1;
        panX = 0;
        panY = 0;
        applyMediaTransform();
    }

    function panToContentPoint(contentX, contentY) {
        var viewportRect = viewport.getBoundingClientRect();
        var contentWidth = baseWidth * zoom;
        var contentHeight = baseHeight * zoom;
        if (!viewportRect.width || !viewportRect.height || !contentWidth || !contentHeight) {
            return;
        }

        var newLeft = viewportRect.width / 2 - contentX;
        var newTop = viewportRect.height / 2 - contentY;
        var centeredLeft = (viewportRect.width - contentWidth) / 2;
        var centeredTop = (viewportRect.height - contentHeight) / 2;
        panX = newLeft - centeredLeft;
        panY = newTop - centeredTop;
        applyMediaTransform();
    }

    function renderCurrent() {
        cleanupActiveMedia();
        if (currentIndex < 0 || currentIndex >= mediaList.length) {
            return;
        }

        var source = mediaList[currentIndex];
        var node;
        if (source.tagName === "VIDEO") {
            node = document.createElement("video");
            node.src = source.currentSrc || source.src;
            node.controls = true;
            node.preload = "metadata";
        } else {
            node = document.createElement("img");
            node.src = source.currentSrc || source.src;
            node.alt = source.alt || "";
            node.loading = "eager";
            node.decoding = "sync";
        }

        node.className = "media-lightbox-media";
        viewport.appendChild(node);
        activeMedia = node;
        if (source.tagName === "IMG") {
            minimapImage.src = node.src;
        }
        if (node.tagName === "IMG") {
            if (node.complete) {
                setupBaseSize();
            } else {
                node.addEventListener("load", setupBaseSize, { once: true });
            }
        } else {
            node.addEventListener("loadedmetadata", setupBaseSize, { once: true });
        }
    }

    function updateNavState() {
        var disabled = mediaList.length <= 1;
        prevButton.disabled = disabled;
        nextButton.disabled = disabled;
    }

    function openAt(index) {
        if (!mediaList.length || index < 0 || index >= mediaList.length) {
            return;
        }
        pauseAllPostVideos();
        overlay.hidden = false;
        overlay.classList.add("is-open");
        overlay.setAttribute("aria-hidden", "false");
        document.body.classList.add("lightbox-open");
        currentIndex = index;
        renderCurrent();
        updateNavState();
        requestAnimationFrame(setupBaseSize);
    }

    function closeLightbox() {
        overlay.classList.remove("is-open");
        overlay.hidden = true;
        overlay.setAttribute("aria-hidden", "true");
        document.body.classList.remove("lightbox-open");
        cleanupActiveMedia();
        currentIndex = -1;
    }

    function move(step) {
        if (mediaList.length <= 1) {
            return;
        }
        currentIndex = (currentIndex + step + mediaList.length) % mediaList.length;
        renderCurrent();
    }

    document.addEventListener("click", function (event) {
        var target = event.target;
        if (!target) {
            return;
        }

        var media = target.closest(".post-image, .post-video");
        if (!media) {
            return;
        }
        if (media.tagName === "VIDEO" && isFirstVideoClick(media)) {
            return;
        }
        if (media.tagName === "VIDEO" && isVideoControlsClick(event, media)) {
            return;
        }
        if (media.tagName === "VIDEO") {
            media.pause();
        }

        mediaList = collectMedia();
        var index = mediaList.indexOf(media);
        if (index === -1) {
            return;
        }

        event.preventDefault();
        openAt(index);
    });

    document.addEventListener("mousemove", function (event) {
        var target = event.target;
        if (!target) {
            return;
        }

        var video = target.closest(".post-video");
        if (!video) {
            return;
        }

        var shouldOpen = isVideoPrimed(video) && !isVideoControlsClick(event, video);
        video.style.cursor = shouldOpen ? "zoom-in" : "pointer";
    });

    document.addEventListener("mouseleave", function (event) {
        var target = event.target;
        if (!target) {
            return;
        }

        var video = target.closest(".post-video");
        if (!video) {
            return;
        }

        video.style.cursor = "pointer";
    }, true);

    prevButton.addEventListener("click", function (event) {
        event.stopPropagation();
        move(-1);
    });

    nextButton.addEventListener("click", function (event) {
        event.stopPropagation();
        move(1);
    });

    overlay.addEventListener("click", function (event) {
        var target = event.target;
        if (!target) {
            return;
        }
        if (
            target.closest(".media-lightbox-media") ||
            target.closest(".media-lightbox-nav") ||
            target.closest(".media-lightbox-zoom-btn") ||
            target.closest(".media-lightbox-minimap")
        ) {
            return;
        }
        closeLightbox();
    });

    viewport.addEventListener("wheel", function (event) {
        if (!activeMedia) {
            return;
        }
        event.preventDefault();
        var rect = viewport.getBoundingClientRect();
        var anchorX = event.clientX - rect.left;
        var anchorY = event.clientY - rect.top;
        var factor = event.deltaY < 0 ? 1.14 : 0.88;
        zoomTo(zoom * factor, anchorX, anchorY);
    }, { passive: false });

    viewport.addEventListener("mousedown", function (event) {
        if (!activeMedia || zoom <= 1 || event.button !== 0) {
            return;
        }
        isDragging = true;
        dragStartX = event.clientX;
        dragStartY = event.clientY;
        dragPanX = panX;
        dragPanY = panY;
        viewport.classList.add("is-dragging");
        event.preventDefault();
    });

    document.addEventListener("mousemove", function (event) {
        if (isDraggingMinimapRect) {
            var miniRect = minimap.getBoundingClientRect();
            if (!miniRect.width || !miniRect.height) {
                return;
            }
            var frameLeft = clamp(
                event.clientX - miniRect.left - minimapDragOffsetX,
                0,
                Math.max(0, miniRect.width - minimapRectWidth)
            );
            var frameTop = clamp(
                event.clientY - miniRect.top - minimapDragOffsetY,
                0,
                Math.max(0, miniRect.height - minimapRectHeight)
            );
            var pointX = frameLeft + minimapRectWidth / 2;
            var pointY = frameTop + minimapRectHeight / 2;
            var contentWidth = baseWidth * zoom;
            var contentHeight = baseHeight * zoom;
            if (!contentWidth || !contentHeight) {
                return;
            }
            var targetX = (pointX / miniRect.width) * contentWidth;
            var targetY = (pointY / miniRect.height) * contentHeight;
            panToContentPoint(targetX, targetY);
            return;
        }
        if (!isDragging) {
            return;
        }
        panX = dragPanX + (event.clientX - dragStartX);
        panY = dragPanY + (event.clientY - dragStartY);
        applyMediaTransform();
    });

    document.addEventListener("mouseup", function () {
        if (isDraggingMinimapRect) {
            isDraggingMinimapRect = false;
        }
        if (!isDragging) {
            return;
        }
        isDragging = false;
        viewport.classList.remove("is-dragging");
    });

    zoomOutButton.addEventListener("click", function (event) {
        event.stopPropagation();
        zoomTo(zoom / 1.2);
    });

    zoomInButton.addEventListener("click", function (event) {
        event.stopPropagation();
        zoomTo(zoom * 1.2);
    });

    zoomResetButton.addEventListener("click", function (event) {
        event.stopPropagation();
        resetZoom();
    });

    minimap.addEventListener("mousedown", function (event) {
        if (!activeMedia || activeMedia.tagName !== "IMG" || zoom <= 1) {
            return;
        }
        event.preventDefault();
        event.stopPropagation();

        var miniRect = minimap.getBoundingClientRect();
        if (!miniRect.width || !miniRect.height) {
            return;
        }

        var clickX = clamp(event.clientX - miniRect.left, 0, miniRect.width);
        var clickY = clamp(event.clientY - miniRect.top, 0, miniRect.height);
        var rectLeft = parseFloat(minimapRect.style.left) || 0;
        var rectTop = parseFloat(minimapRect.style.top) || 0;
        var rectWidth = parseFloat(minimapRect.style.width) || 0;
        var rectHeight = parseFloat(minimapRect.style.height) || 0;

        // Drag when pointer is inside the viewport frame.
        if (
            clickX >= rectLeft &&
            clickX <= rectLeft + rectWidth &&
            clickY >= rectTop &&
            clickY <= rectTop + rectHeight
        ) {
            isDraggingMinimapRect = true;
            minimapDragOffsetX = clickX - rectLeft;
            minimapDragOffsetY = clickY - rectTop;
            minimapRectWidth = rectWidth;
            minimapRectHeight = rectHeight;
            return;
        }

        var contentWidth = baseWidth * zoom;
        var contentHeight = baseHeight * zoom;
        var targetX = (clickX / miniRect.width) * contentWidth;
        var targetY = (clickY / miniRect.height) * contentHeight;
        panToContentPoint(targetX, targetY);
    });

    document.addEventListener("keydown", function (event) {
        if (overlay.hidden) {
            return;
        }
        if (event.key === "Escape") {
            closeLightbox();
            return;
        }
        if (event.key === "ArrowLeft") {
            move(-1);
            return;
        }
        if (event.key === "ArrowRight") {
            move(1);
            return;
        }
        if (event.key === "+" || event.key === "=") {
            zoomTo(zoom * 1.2);
            return;
        }
        if (event.key === "-" || event.key === "_") {
            zoomTo(zoom / 1.2);
            return;
        }
        if (event.key === "0") {
            resetZoom();
        }
    });

    window.addEventListener("resize", function () {
        if (!overlay.hidden) {
            setupBaseSize();
        }
    });
});
