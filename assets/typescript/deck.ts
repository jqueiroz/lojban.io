import * as $ from "jquery";

import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { Deck } from './components/deck'

$(document).ready(function() {
    let root_element = $(".deck");
    let deckProps = {
        deckId: "eng_contextualized-brivla",
    };
    let deckComponent = React.createElement(Deck, deckProps);
    ReactDOM.render(deckComponent, root_element[0]);
});
