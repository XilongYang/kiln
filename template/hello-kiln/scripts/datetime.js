function getCurrentYear() {
    var now = new Date(Date.now())
    return now.getFullYear()
}

export {getCurrentYear}