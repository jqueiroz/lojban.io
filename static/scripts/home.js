let setupCarousel = function(previousButton, nextButton, elements) {
    // TODO: dynamically compute the number of pages
    // TODO: implement animations
    let numberOfElementsPerPage = 3;
    let numberOfPages = Math.ceil(elements.length / numberOfElementsPerPage);

    let displayPage = function(pageNumber) {
        elements.hide();
        let indexOfFirstElement = pageNumber * numberOfElementsPerPage;
        for (let i = 0; i < numberOfElementsPerPage; ++i) {
            $(elements[indexOfFirstElement + i]).show();
        }
    }
    displayPage(0);

    let currentPage = 0;
    previousButton.click(function() {
        currentPage += numberOfPages;
        currentPage -= 1;
        currentPage %= numberOfPages;
        displayPage(currentPage);
    });
    nextButton.click(function() {
        currentPage += 1;
        currentPage %= numberOfPages;
        displayPage(currentPage);
    });
};

$(document).ready(function() {
    setupCarousel($(".courses .previous"), $(".courses .next"), $(".courses .course"));
    setupCarousel($(".decks .previous"), $(".decks .next"), $(".decks .deck"));
});
