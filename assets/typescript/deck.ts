import * as $ from "jquery";

let retrieveDeck = deck_id => {
    return $.ajax({
        url: "/api/v0/deck/" + deck_id,
        method: "GET",
        dataType: "json",
    });
};

let setupDeck = (root, deck_id) => {
    retrieveDeck(deck_id).done(deck => {
        console.log(deck);

        root.empty();
        let ul = $("<ul>");
        for (let i = 0; i < deck.cards.length; ++i) {
            let card = deck.cards[i];
            let li = $("<li>").text(card.title);
            ul.append(li);
        }
        root.append(ul);
    });
};

$(document).ready(function() {
    let root_element = $(".deck");
    let deck_id = "eng_contextualized-brivla";
    setupDeck(root_element, deck_id);
});
