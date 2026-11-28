# Translate package-relative help links when rendering pkgdown articles.
local({
    render_document <- knitr::knit_hooks$get("document")
    knitr::knit_hooks$set(document = function(markdown) {
        gsub(
            "(\\]\\(|href=[\"'])\\.\\./help/([[:alnum:]_.-]+)",
            "\\1../reference/\\2.html",
            render_document(markdown)
        )
    })
})
