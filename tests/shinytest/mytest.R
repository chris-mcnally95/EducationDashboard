app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

app$setInputs(input_school_id = "2230301")
app$snapshot()

