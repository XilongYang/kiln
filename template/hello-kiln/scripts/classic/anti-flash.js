var perferDarkMode = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches
if (sessionStorage.getItem("darkmode") == null && perferDarkMode) {
    sessionStorage.setItem("darkmode", "true")
}

if (sessionStorage.getItem("darkmode") == "true") {
    var antiFlash = document.getElementById("anti-flash")
    antiFlash.href = "/style/anti-flash-dark.css"
}
