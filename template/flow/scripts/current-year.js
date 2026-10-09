window.addEventListener("DOMContentLoaded", function () {
    var currentYear = document.getElementById("current-year");
    if (currentYear) {
        currentYear.textContent = String(new Date().getFullYear());
    }
});
